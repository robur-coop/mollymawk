(** This code is copied from
    https://git.robur.coop/robur/tlstunnel/src/branch/mjav/mkernel/filesystem.ml,
    written by Hannes Mehnert. *)

module H = Digestif.SHA256

let s_version = 1
let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module IS = Set.Make (Int)

type superblock = {
  super_version : int; (* 2 byte *)
  (* padding - 6 byte *)
  super_counter : int; (* 8 byte *)
  timestamp : Ptime.t; (* 8 byte *)
  active_sector : int64; (* 8 byte *)
  data_length : int; (* 8 byte *)
  data_checksum : string;
  (* padding until length - 32 *)
  (* superblock_checksum : string ; *)
  used_sectors : IS.t; (* not persistent *)
}

let superblock_size = 40 + (2 * H.digest_size)

let empty_superblock () =
  {
    super_version = s_version;
    super_counter = 0;
    timestamp = Mkernel.now ();
    active_sector = 0L;
    data_length = 0;
    data_checksum = "";
    used_sectors = IS.empty;
  }

let ns_per_day = Int64.mul 86_000L 1_000_000_000L
let ps_per_ns = 1_000L

let decode_timestamp data off =
  let ns = String.get_int64_be data off in
  let d = Int64.unsigned_div ns ns_per_day
  and ps = Int64.(mul (unsigned_rem ns ns_per_day) ps_per_ns) in
  Ptime.v (Int64.to_int d, ps)

let encode_timestamp data off v =
  let d, ps = Ptime.Span.to_d_ps (Ptime.to_span v) in
  let ns = Int64.(add (mul (Int64.of_int d) ns_per_day) (div ps ps_per_ns)) in
  Bytes.set_int64_be data off ns

let safe_int ~msg d =
  if d > Int64.of_int max_int then Error (`Overflow msg)
  else Ok (Int64.to_int d)

type decode_err =
  [ `Overflow of string | `Bad_checksum | `Bad_superblock_version of int ]

let pp_decode_err ppf = function
  | `Overflow msg -> Fmt.pf ppf "integer overflow %s" msg
  | `Bad_checksum -> Fmt.string ppf "bad superblock checksum"
  | `Bad_superblock_version v ->
      Fmt.pf ppf "superblock version %d is not supported (supported is %d)" v
        s_version

let decode_superblock buf : (superblock, [> decode_err ]) result =
  let payload, checksum =
    let mid = String.length buf - H.digest_size in
    (String.sub buf 0 mid, String.sub buf mid H.digest_size)
  in
  if String.equal checksum H.(to_raw_string (digest_string payload)) then
    let super_version = String.get_uint16_be payload 0
    and super_counter = String.get_int64_be payload 8
    and timestamp = decode_timestamp payload 16
    and active_sector = String.get_int64_be payload 24
    and data_length = String.get_int64_be payload 32
    and data_checksum = String.sub payload 40 H.digest_size in
    let ( let* ) = Result.bind in
    if super_version = s_version then
      let* super_counter = safe_int ~msg:"superblock counter" super_counter in
      let* data_length = safe_int ~msg:"data length" data_length in
      Ok
        {
          super_version;
          super_counter;
          timestamp;
          active_sector;
          data_length;
          data_checksum;
          used_sectors = IS.empty;
        }
    else Error (`Bad_superblock_version super_version)
  else Error `Bad_checksum

let encode_superblock t buf =
  Bytes.set_uint16_be buf 0 t.super_version;
  Bytes.set_int64_be buf 8 (Int64.of_int t.super_counter);
  encode_timestamp buf 16 t.timestamp;
  Bytes.set_int64_be buf 24 t.active_sector;
  Bytes.set_int64_be buf 32 (Int64.of_int t.data_length);
  Bytes.blit_string t.data_checksum 0 buf 40 H.digest_size;
  let eop = Bytes.length buf - H.digest_size in
  let payload = Bytes.sub_string buf 0 eop in
  let checksum = H.(to_raw_string (digest_string payload)) in
  Bytes.blit_string checksum 0 buf eop H.digest_size

let read_data block =
  let ss = Mkernel.Block.sector_size block in
  assert (ss >= superblock_size);
  let data_per_sector = ss - 8 in
  (* each sector is prefixed by a next pointer *)
  let super_data_first, super_data_last = (Bstr.create ss, Bstr.create ss) in
  let first_super, last_super =
    (0, ((Mkernel.Block.length block / ss) - 1) * ss)
  in
  Mkernel.Block.atomic_read block ~src_off:first_super super_data_first;
  Mkernel.Block.atomic_read block ~src_off:last_super super_data_last;
  let* superblock, to_write =
    match
      ( decode_superblock (Bstr.to_string super_data_first),
        decode_superblock (Bstr.to_string super_data_last) )
    with
    | Ok a, Ok b -> (
        match compare a.super_counter b.super_counter with
        | 0 -> Ok (a, None)
        | 1 -> Ok (a, Some (last_super, super_data_first))
        | -1 -> Ok (b, Some (first_super, super_data_last))
        | _ -> assert false)
    | Error `Bad_checksum, Ok b -> Ok (b, Some (first_super, super_data_last))
    | Ok a, Error `Bad_checksum -> Ok (a, Some (last_super, super_data_first))
    | Error x, _ | _, Error x -> Error x
  in
  let scratch = Bstr.create ss in
  let rec read_one sectors data src_sector_off =
    match (src_sector_off = 0, Cstruct.length data = 0) with
    | true, true -> Ok sectors
    | false, false ->
        Mkernel.Block.atomic_read block ~src_off:(src_sector_off * ss) scratch;
        let next = Bstr.get_int64_be scratch 0 in
        let next = Int64.to_int next in
        let len = Int.min (Cstruct.length data) data_per_sector in
        Cstruct.(blit (of_bigarray scratch)) 8 data 0 len;
        read_one (IS.add src_sector_off sectors) (Cstruct.shift data len) next
    | true, false -> error_msgf "early end of data"
    | false, true -> error_msgf "sector chain exceeds data"
  in
  let data = Cstruct.create superblock.data_length in
  let* used_sectors =
    read_one IS.empty data (Int64.to_int superblock.active_sector)
  in
  let data = Cstruct.to_string data in
  if
    String.equal superblock.data_checksum H.(to_raw_string (digest_string data))
  then (
    let fn (dst_off, data) = Mkernel.Block.atomic_write block ~dst_off data in
    Option.iter fn to_write;
    Ok ({ superblock with used_sectors }, data))
  else error_msgf "bad data checksum"

let write_data block old_superblock data =
  let ss = Mkernel.Block.sector_size block in
  let sectors = Mkernel.Block.length block / ss in
  assert (ss >= superblock_size);
  let data = Cstruct.of_string data in
  let data_per_sector = ss - 8 in
  (* each sector is prefixed by a next pointer *)
  let sectors_needed =
    (Cstruct.length data + (data_per_sector - 1)) / data_per_sector
  in
  if 2 + sectors_needed + IS.cardinal old_superblock.used_sectors > sectors then
    error_msgf "not enough blocks"
  else
    (* write data *)
    let data_sector = Bstr.create ss in
    let rec is_free i =
      if succ i >= sectors then error_msgf "no more sectors"
      else if IS.mem i old_superblock.used_sectors then is_free (succ i)
      else Ok i
    in
    let rec write_one dst_sector data acc =
      let* next =
        if Cstruct.length data <= data_per_sector then Ok 0
        else is_free ((dst_sector / ss) + 1)
      in
      let next = next * ss in
      Bstr.set_int64_be data_sector 0 (Int64.of_int next);
      let len = Int.min (Cstruct.length data) data_per_sector in
      Cstruct.blit data 0 (Cstruct.of_bigarray data_sector) 8 len;
      Mkernel.Block.atomic_write block ~dst_off:(dst_sector * ss) data_sector;
      let acc' = IS.add dst_sector acc in
      if next = 0 then Ok acc'
      else write_one next (Cstruct.shift data data_per_sector) acc'
    in
    let* first_sector = is_free 1 in
    let* used_sectors = write_one first_sector data IS.empty in
    let superblock =
      let empty = empty_superblock () in
      {
        empty with
        super_counter = succ old_superblock.super_counter;
        active_sector = Int64.of_int first_sector;
        data_length = Cstruct.length data;
        data_checksum =
          H.(to_raw_string (digest_string (Cstruct.to_string data)));
        used_sectors;
      }
    in
    let s = Bytes.create ss in
    encode_superblock superblock s;
    let s = Bstr.of_string (Bytes.unsafe_to_string s) in
    Mkernel.Block.atomic_write block ~dst_off:((sectors - 1) * ss) s;
    Mkernel.Block.atomic_write block ~dst_off:0 s;
    Ok superblock

let init block =
  let superblock =
    let empty = empty_superblock () in
    { empty with data_checksum = H.(to_raw_string (digest_string "")) }
  in
  let ss = Mkernel.Block.sector_size block in
  assert (ss >= superblock_size);
  let s = Bytes.create ss in
  encode_superblock superblock s;
  let last_sector = (Mkernel.Block.length block / ss) - 1 in
  let s = Bstr.of_string (Bytes.unsafe_to_string s) in
  Mkernel.Block.atomic_write block ~dst_off:(last_sector * ss) s;
  Mkernel.Block.atomic_write block ~dst_off:0 s;
  superblock
