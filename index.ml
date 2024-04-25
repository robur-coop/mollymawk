open Tyxml

let index_page =
  let home =
    Html.(
      html
        (head
            (title (txt "MollyMawk"))
            [ meta ~a:[a_charset "UTF-8"] ()
            ; meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1.0"] ()
            ; link ~rel:[`Stylesheet] ~href:"https://unpkg.com/aos@2.3.1/dist/aos.css" ()
            ; script ~a:[a_src "https://cdn.tailwindcss.com"] (txt "")
            ; script ~a:[a_src "main.js"] (txt "")
            ])

        (body ~a:[a_class["bg-black px-10 py-10 max-w-7xl mx-auto"]]
            [ section
                [ h1 ~a:[a_class ["font-bold text-3xl text-center text-gray-100"]] [txt "MollyMawk"]
                ; p ~a:[a_class ["font-semibold text-md text-gray-200 text-center"]] [txt "A MirageOS unikernel to....oh just have some Molly..."] ]
            ; p ~a:[a_id "no-molly"; a_class ["font-semibold text-sm text-gray-200 text-center hidden"]] [txt "Nothing yet..you gotta deploy some unikernels. Come on chop chop"]
            ; section ~a:[a_id "unikernel-container"; a_class ["my-5 grid grid-cols-4 gap-4"]  ][]]))
    in
    Format.asprintf "%a" (Html.pp()) (home)