type 'a testable = 'a Alcotest.testable

let testable = Alcotest.testable
let check = Alcotest.check
let result = Alcotest.result
let list = Alcotest.list
let pair = Alcotest.pair
let option = Alcotest.option
let string = Alcotest.string
let bool = Alcotest.bool
let int = Alcotest.int
let unit = Alcotest.unit
let () = Mirage_crypto_rng_unix.use_default ()

(* this dump was gotten from a locally running instance of mollymawk. in store.ml, the function read_disk outputs a string. this is the string that is contained in the value raw_dump below. *)
let raw_dump =
  {|{
  "version": 10,
  "users": [
    {
      "name": "user",
      "email": "user@robur.net",
      "email_verified": null,
      "password": "vn2hllXeTYpW2n9+gPf2Bnal6T2qms8acPQHb+0=",
      "uuid": "1d0a80e3-323b-4b45-9efd-bad03762e87f",
      "tokens": [],
      "cookies": [
        {
          "name": "molly_csrf",
          "created_at": "2026-04-13 19:29:58-00:00",
          "value": "MGM5MDZmYjAtMWZjYy00MzE1LTliODEtMjVkNDhiNjFiNWZh",
          "expires_in": 3600,
          "uuid": "f208c5b3-3307-417a-81d9-bfcb665155c7",
          "last_access": "2026-04-13 19:29:58-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-13 19:29:57-00:00",
          "value": "OTQ2OWRhZGQtODhjZS00NTg4LWEwMjUtZGQ5NDNkNTYwYWI3",
          "expires_in": 3600,
          "uuid": "d9a63a9b-ad57-4759-ad05-01b64f9572ba",
          "last_access": "2026-04-13 19:29:57-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-13 19:29:46-00:00",
          "value": "YjNhZjI4ZDktODY1NC00NDllLTlkNDYtZjdiMjdmMjBiNTFj",
          "expires_in": 3600,
          "uuid": "82bae6d9-75c5-4c7e-9328-f9f582002f36",
          "last_access": "2026-04-13 19:29:46-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-13 19:29:36-00:00",
          "value": "OGZkZjg4NDYtZGQyNS00MjFjLTg5NGQtZjk3OTAwODllMWM3",
          "expires_in": 3600,
          "uuid": "f4c766d0-a9df-4bfd-986e-1b3cdc5a9414",
          "last_access": "2026-04-13 19:29:36-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2025-12-28 18:18:24-00:00",
          "value": "MDQwZmNiZmMtOGM3MC00OTM2LTlmMjctOTExMDQwZGM3MDNl",
          "expires_in": 604800,
          "uuid": "1d0a80e3-323b-4b45-9efd-bad03762e87f",
          "last_access": "2025-12-28 18:18:24-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2025-12-28 18:15:54-00:00",
          "value": "OGZjNmY2YzktMWNjYy00NzQ4LWFhMzAtNzk1NmQwNDE4ODYz",
          "expires_in": 604800,
          "uuid": "1d0a80e3-323b-4b45-9efd-bad03762e87f",
          "last_access": "2025-12-28 18:15:54-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2026-04-13 19:25:22-00:00",
          "value": "MWI4MzYwOWItNmE4MS00ZGUzLWE5NmQtZTJjYWRmNWMzMTcw",
          "expires_in": 604800,
          "uuid": "1d0a80e3-323b-4b45-9efd-bad03762e87f",
          "last_access": "2026-04-13 19:25:22-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-13 19:25:22-00:00",
          "value": "NTI3MThjYzgtYWVhOC00MDY4LTk0OTgtOWM4ZDIxN2UyODhl",
          "expires_in": 3600,
          "uuid": "903bab07-a5bb-4dd5-bf6f-c2cf59e1d8bd",
          "last_access": "2026-04-13 19:25:22-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-13 19:26:46-00:00",
          "value": "MGY4NmVhODEtYjVkMC00ODM4LTk3OTEtNmE4Y2QxOWNlOTk5",
          "expires_in": 3600,
          "uuid": "703c46ce-4601-42b7-a8d2-8d5d4c9dc8e1",
          "last_access": "2026-04-13 19:26:46-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-13 19:31:50-00:00",
          "value": "YjlkYTg3YzgtMDFlZi00MTdjLTgxNGUtOTFmNThhZmM1Mjg3",
          "expires_in": 3600,
          "uuid": "c028d665-2b5e-4ec6-bfc5-bdaf75b025e7",
          "last_access": "2026-04-13 19:31:50-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        }
      ],
      "created_at": "2026-04-13 19:31:50-00:00",
      "updated_at": "2026-04-13 19:29:58-00:00",
      "email_verification_uuid": "fbf1fd08-0a19-464e-a7bc-e79d1d5f7fc7",
      "active": true,
      "super_user": false,
      "unikernel_updates": [],
      "scaling_policies": []
    },
    {
      "name": "user2",
      "email": "user2@robur.coop",
      "email_verified": "2025-12-28 13:34:10-00:00",
      "password": "WomWAonZcOsQ9ueSRwFbT7nvQAtcsfNFl/o7T66=",
      "uuid": "28538638-0136-43e1-9146-bf7649917a33",
      "tokens": [],
      "cookies": [
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:38:45-00:00",
          "value": "MWIyYWNiNzctMzUxYS00NWU0LTgzZjctOTUxODhjNmM5NGUx",
          "expires_in": 3600,
          "uuid": "aeb59d61-7992-42c6-8f64-91f1cfbfe476",
          "last_access": "2025-12-28 13:38:45-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:38:35-00:00",
          "value": "ODc5ZmQyNTQtMmQ4MS00NDFhLTgxMzQtODNlMWY5NzVkMzMw",
          "expires_in": 3600,
          "uuid": "ab2ed4bc-757b-4a62-8edf-2a612f6f8862",
          "last_access": "2025-12-28 13:38:35-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:38:33-00:00",
          "value": "OTJhNzUyYzYtNTQ2Ny00ZmNmLWE0NGQtMjNiZTNlNmY0Yjk2",
          "expires_in": 3600,
          "uuid": "210c522e-d1cb-4f07-9039-ab1dada57f7b",
          "last_access": "2025-12-28 13:38:33-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2025-12-08 16:52:44-00:00",
          "value": "NmUyYjlmZTctOGZiYi00Y2JhLWIzMDctMzk3ZjRiYjRlZWM4",
          "expires_in": 604800,
          "uuid": "28538638-0136-43e1-9146-bf7649917a33",
          "last_access": "2025-12-08 16:52:44-00:00",
          "user_agent": "PostmanRuntime/7.49.1"
        },
        {
          "name": "molly_session",
          "created_at": "2025-12-07 10:37:04-00:00",
          "value": "OTQ0M2QyNTgtNjgyNy00ZjBmLWEwOTYtMGE0OGIwYmI1NTc3",
          "expires_in": 604800,
          "uuid": "28538638-0136-43e1-9146-bf7649917a33",
          "last_access": "2025-12-07 10:37:04-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2025-12-28 13:33:06-00:00",
          "value": "MzNjYTM1ZGItYWVkYS00YTBkLThmMTctMDBiNWM3MWY3MDg2",
          "expires_in": 604800,
          "uuid": "28538638-0136-43e1-9146-bf7649917a33",
          "last_access": "2025-12-28 13:33:06-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2025-12-28 13:33:37-00:00",
          "value": "NzRhYTE0ZGUtNjJlYi00NmI0LWIyYmQtOTg4N2NhYTEzNzg0",
          "expires_in": 604800,
          "uuid": "28538638-0136-43e1-9146-bf7649917a33",
          "last_access": "2025-12-28 13:33:37-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:33:37-00:00",
          "value": "MDI5Yjc5OTctYzdiOC00MjExLTg4MTAtNGJkYWY4YjgzZThh",
          "expires_in": 3600,
          "uuid": "2ca30d0e-4080-431b-a780-f97a4e7b4ec2",
          "last_access": "2025-12-28 13:33:37-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:33:45-00:00",
          "value": "YTJiZTQ3OGEtYmJjNC00YTIyLWJmYmMtN2RmNTQ4OWFmMTQw",
          "expires_in": 3600,
          "uuid": "c93882b5-5519-4a72-b8d0-8fbd641aa16c",
          "last_access": "2025-12-28 13:33:45-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:34:10-00:00",
          "value": "OGIzMThkNzctMjMyNi00ZjQ5LWExNDItMTY5NWIwMjc4YmIw",
          "expires_in": 3600,
          "uuid": "80402a15-206d-40ed-9268-87c80d043258",
          "last_access": "2025-12-28 13:34:10-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:34:14-00:00",
          "value": "Yzc2ZjIwYzMtMjY4My00N2UxLTg3ODMtY2FiNjA2NGVhNjUy",
          "expires_in": 3600,
          "uuid": "42673105-1496-49d0-b23f-00e74d87752f",
          "last_access": "2025-12-28 13:34:14-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2025-12-28 13:34:17-00:00",
          "value": "NDhkYmI3YmYtNzIyNC00MDQwLTg0NjktY2NjM2QyYjliMGZl",
          "expires_in": 3600,
          "uuid": "7bb5ec3c-51eb-427a-b2d0-cc2640c8928d",
          "last_access": "2025-12-28 13:34:17-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        }
      ],
      "created_at": "2025-12-28 13:34:17-00:00",
      "updated_at": "2025-12-28 13:38:45-00:00",
      "email_verification_uuid": "56245e21-0201-47fd-b7f3-5cbee960c6af",
      "active": true,
      "super_user": false,
      "unikernel_updates": [],
      "scaling_policies": []
    },
    {
      "name": "user3",
      "email": "user3@robur.coop",
      "email_verified": "2025-12-28 12:35:08-00:00",
      "password": "eVHL6CIc5phEoElSOACRkCFZD68vzHAt9StFI=",
      "uuid": "1f42ba42-5458-479b-8095-8450afde8d6b",
      "tokens": [
        {
          "token_type": "Bearer",
          "value": "3cb6bc72-a52d-4f4f-969c-c85465875774",
          "expires_in": 800000,
          "created_at": "2025-12-09 05:12:11-00:00",
          "last_access": "2025-12-09 05:12:11-00:00",
          "name": "updated-gitlab-token",
          "usage_count": 0
        },
        {
          "token_type": "Bearer",
          "value": "6bc27967-30cd-4090-af8f-3997c47c25e1",
          "expires_in": 2419200,
          "created_at": "2025-12-05 04:14:38-00:00",
          "last_access": "2025-12-05 04:14:38-00:00",
          "name": "fobud",
          "usage_count": 68
        },
        {
          "token_type": "Bearer",
          "value": "4ba2912d-be58-4e32-883b-f780cccea150",
          "expires_in": 2419200,
          "created_at": "2025-12-05 03:37:22-00:00",
          "last_access": "2025-12-05 03:37:22-00:00",
          "name": "Nobur",
          "usage_count": 0
        },
        {
          "token_type": "Bearer",
          "value": "31d35fc0-852a-43ee-b604-3fabecab1b83",
          "expires_in": 2419200,
          "created_at": "2025-12-05 03:31:33-00:00",
          "last_access": "2025-12-05 03:31:33-00:00",
          "name": "Robur",
          "usage_count": 0
        },
        {
          "token_type": "Bearer",
          "value": "785b1453-77ed-47fe-901e-f0098404fd4d",
          "expires_in": 2419200,
          "created_at": "2025-12-05 03:31:45-00:00",
          "last_access": "2025-12-05 03:31:45-00:00",
          "name": "Robur",
          "usage_count": 48
        },
        {
          "token_type": "Bearer",
          "value": "9bba7dad-6df2-4339-9ff4-26dccb8c1a4f",
          "expires_in": 2419200,
          "created_at": "2025-12-05 04:15:33-00:00",
          "last_access": "2025-12-05 04:15:33-00:00",
          "name": "sobur",
          "usage_count": 2
        }
      ],
      "cookies": [
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:10:15-00:00",
          "value": "NjgzOTI1ZDEtYTgyMy00MDY4LTlkMzgtZWY5YjRjYjNjNzcz",
          "expires_in": 3600,
          "uuid": "c0c0529c-e905-437a-82f9-4ae3be68856a",
          "last_access": "2026-08-25 23:10:15-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:10:09-00:00",
          "value": "N2NjY2UzNjQtNTIwYS00M2FmLTliNTYtZWFkNTI5ZDBlZjg3",
          "expires_in": 3600,
          "uuid": "194f17b1-8635-4151-a789-d57b7ea083fb",
          "last_access": "2026-08-25 23:10:09-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:10:05-00:00",
          "value": "NGYxMDk5OWUtMzEzZC00NjYxLWIwOWEtZmQyMzE1ZWNmYzZk",
          "expires_in": 3600,
          "uuid": "44f9f10d-68f4-49f5-bea8-78a127bf7e3c",
          "last_access": "2026-08-25 23:10:05-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:10:02-00:00",
          "value": "NzFiNzVhZDctOTcwNi00NTAxLWJmZDAtMDJkMGQ4YWRjZjY2",
          "expires_in": 3600,
          "uuid": "112752c3-db96-444c-875e-6a463377317f",
          "last_access": "2026-08-25 23:10:02-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:10:00-00:00",
          "value": "NDQyZTBlNjMtYzM1Ny00MmU5LWIwYTAtZmJjNTM3YzczZWYx",
          "expires_in": 3600,
          "uuid": "87c554ec-25f2-4a24-9c0a-99696d1ba26d",
          "last_access": "2026-08-25 23:10:00-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:09:56-00:00",
          "value": "NTRkNWExNzMtOWZkMC00OTU1LThkMTctZjc1ZWFlODNjODQx",
          "expires_in": 3600,
          "uuid": "6916db2d-cd33-4d53-a1ba-4fa8625ee710",
          "last_access": "2026-08-25 23:09:56-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:09:54-00:00",
          "value": "N2YzMmI5MWItYjllYi00YzhhLWI5ZDMtNWQ0YjE5YzI0OWNi",
          "expires_in": 3600,
          "uuid": "9a0296e3-761b-4aa9-aa17-81a3e313860e",
          "last_access": "2026-08-25 23:09:54-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-08-25 23:09:50-00:00",
          "value": "ZWZjNDU1NGMtMDA4NC00ODY1LWFlOTEtYTczNzI3YTFiMTk3",
          "expires_in": 3600,
          "uuid": "3b545fc6-0ca2-4217-9a12-6e0dfc853f71",
          "last_access": "2026-08-25 23:09:50-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2026-08-25 23:09:50-00:00",
          "value": "YzhlZmUyOGMtODQ0Mi00ZGNhLWI1NmYtM2FiMDNkMDQ1ODI5",
          "expires_in": 604800,
          "uuid": "1f42ba42-5458-479b-8095-8450afde8d6b",
          "last_access": "2026-08-25 23:09:50-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        }
      ],
      "created_at": "2026-07-28 07:20:46-00:00",
      "updated_at": "2026-08-25 23:10:15-00:00",
      "email_verification_uuid": "3e49c0d0-8c1c-4ed7-9763-355d73f62257",
      "active": true,
      "super_user": true,
      "unikernel_updates": [
        {
          "name": "hello",
          "job": "hello",
          "uuid": "6cc1a49d-eb1e-4f46-8471-6452206d1475",
          "config": {
            "typ": "solo5",
            "compressed": false,
            "fail_behaviour": {
              "restart": null,
              "exit_code": [],
              "all_exit_codes": false
            },
            "cpuids": [
              0
            ],
            "memory": 32,
            "block_devices": [],
            "network_interfaces": [],
            "arguments": [],
            "numcpus": 1,
            "linux_boot_partition": null
          },
          "timestamp": "2026-05-01 18:14:22-00:00"
        },
        {
          "name": "hello-4",
          "job": "hello",
          "uuid": "",
          "config": {
            "typ": "solo5",
            "compressed": false,
            "fail_behaviour": {
              "restart": null,
              "exit_code": [],
              "all_exit_codes": false
            },
            "cpuids": [
              0
            ],
            "memory": 1,
            "block_devices": [],
            "network_interfaces": [],
            "arguments": [],
            "numcpus": 1,
            "linux_boot_partition": null
          },
          "timestamp": "2025-12-08 17:58:24-00:00"
        },
        {
          "name": "mello",
          "job": "hello",
          "uuid": "6cc1a49d-eb1e-4f46-8471-6452206d1475",
          "config": {
            "typ": "solo5",
            "compressed": false,
            "fail_behaviour": {
              "restart": null,
              "exit_code": [],
              "all_exit_codes": false
            },
            "cpuids": [
              0
            ],
            "memory": 0,
            "block_devices": [],
            "network_interfaces": [],
            "arguments": [],
            "numcpus": 1,
            "linux_boot_partition": null
          },
          "timestamp": "2025-12-07 23:08:24-00:00"
        }
      ],
      "scaling_policies": [
        {
          "name": "hello",
          "primary_albatross_instance": "local-server",
          "max_instances": 3
        }
      ]
    },
    {
      "name": "user4",
      "email": "user4@robur.com",
      "email_verified": null,
      "password": "j3I8UShH8DkVKa5U8SHh5+oUXk3bcUrQJvDHos=",
      "uuid": "ad7bd096-e9e8-4a46-9e7a-ed8b3bf7bdd8",
      "tokens": [],
      "cookies": [
        {
          "name": "molly_csrf",
          "created_at": "2026-04-15 07:44:56-00:00",
          "value": "ZWQ1NGFjMDktYmQxZC00OTIzLWI1ZjYtMmYxZWRjNWJiNjA4",
          "expires_in": 3600,
          "uuid": "acd410b4-ace5-4277-aaac-6e84f2fe59af",
          "last_access": "2026-04-15 07:44:56-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-15 07:44:01-00:00",
          "value": "MGYyOWMxNmUtYzcwNy00ZTAyLWFjMmEtYWM3MWU1N2NjZDM2",
          "expires_in": 3600,
          "uuid": "b2bd669b-e66a-4583-aa3d-9485c8a68d9d",
          "last_access": "2026-04-15 07:44:01-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2026-04-14 06:03:25-00:00",
          "value": "YjQxNGE3ZTEtOGMyYy00NWNjLWIzNTEtN2QzZTc4ZDY5NGI5",
          "expires_in": 604800,
          "uuid": "ad7bd096-e9e8-4a46-9e7a-ed8b3bf7bdd8",
          "last_access": "2026-04-14 06:03:25-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_session",
          "created_at": "2026-04-14 06:04:18-00:00",
          "value": "NTM4NWZhZDktMTdkOS00MzQ3LTk3YzYtYTY5N2QzMzc4YTBl",
          "expires_in": 604800,
          "uuid": "ad7bd096-e9e8-4a46-9e7a-ed8b3bf7bdd8",
          "last_access": "2026-04-14 06:04:18-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        },
        {
          "name": "molly_csrf",
          "created_at": "2026-04-14 11:31:41-00:00",
          "value": "YjZmZmUzNjktZTQyMy00NjFhLWEyNjQtMDRkMDUwNzY5Mjlh",
          "expires_in": 3600,
          "uuid": "7b0c8fa4-e4c5-4c9d-9fab-31808236fb7f",
          "last_access": "2026-04-14 11:31:41-00:00",
          "user_agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
        }
      ],
      "created_at": "2026-04-14 11:31:41-00:00",
      "updated_at": "2026-04-15 07:45:00-00:00",
      "email_verification_uuid": "3e832348-e654-4929-b626-95c53e013d1b",
      "active": true,
      "super_user": false,
      "unikernel_updates": [],
      "scaling_policies": []
    }
  ],
  "configuration": [
    {
      "name": "local-server",
      "certificate": "-----BEGIN CERTIFICATE-----\nMIH1MIGooAMCAQICCh4RFZsMHQSV7UMwBQYDK2VwMA0xCzAJBgNVBAMMAmNhMB4X\nDTI1MTIwNTEzMTk0MVoXDTM1MTIwMzEzMTk1MVowDTELMAkGA1UEAwwCY2EwKjAF\nBgMrZXADIQCD6DablGzl/JEQqieuP0B4CWhVlmnLoe5c777q8cOCr6MkMCIwDwYD\nVR0PAQH/BAUDAwfGADAPBgNVHRMBAf8EBTADAQH/MAUGAytlcANBAHNte6yJzB5f\nNkq0L4w7rHhUEIWg1igHciNMXLzJTrxxpm5RxDFmnS3yEhARmMVd2N5h6N/K1200\nZDgtaQRpDQw=\n-----END CERTIFICATE-----\n",
      "private_key": "-----BEGIN PRIVATE KEY-----\nMC4CAQAwBQYDK2VwBCIEIPa9DMmlMckBoVrWvcxB3yq1dQHe9oGhFSLShZA18vvb\n-----END PRIVATE KEY-----\n",
      "server_ip": "10.0.0.1",
      "server_port": 1025,
      "updated_at": "2026-04-15 11:20:27-00:00"
    }
  ],
  "email": {
    "server": "10.0.0.1",
    "port": 25,
    "from_email": "no-reply@robur.coop",
    "base_url": "127.0.0.2"
  }
}|}

(** Helper exception unwrap *)
let of_string_exn f =
  match f with Ok a -> a | Error (`Msg err) -> failwith err

let json_of_string_exn f =
  match f with Ok a -> a | Error (`Msg err) -> failwith err

let label_of_string_exn label =
  of_string_exn (Vmm_core.Name.Label.of_string label)

let name_of_string_exn name = of_string_exn (Vmm_core.Name.of_string name)
let email_of_string_exn email = of_string_exn (Mrmime.Mailbox.of_string email)

let signing_request_exn pk =
  match X509.Signing_request.create [] pk with
  | Ok c -> c
  | Error _ -> failwith "invalid signing request"

let private_key =
  X509.Private_key.generate ~seed:"robur_in_essaouira_2026" `ED25519

let second_private_key =
  X509.Private_key.generate ~seed:"robur_is_great_2026" `ED25519

let certificate_exn pk =
  match
    X509.Signing_request.sign (signing_request_exn pk) ~valid_from:Ptime.epoch
      ~valid_until:(Mirage_ptime.now ()) pk []
  with
  | Ok c -> c
  | Error _ -> failwith "invalid certificate"

(** Alcotest Testables *)
let msg_t : [ `Msg of string ] testable =
  let pp ppf (`Msg s) = Fmt.string ppf s in
  testable pp (fun (`Msg a) (`Msg b) -> String.equal a b)

let yojson_t : Yojson.Basic.t testable =
  let pp ppf j = Fmt.pf ppf "%s" (Yojson.Basic.pretty_to_string j) in
  testable pp Yojson.Basic.equal

let user_t =
  let pp ppf (u : User_model.user) =
    Fmt.pf ppf "%a" Yojson.Basic.pp (User_model.user_to_json u)
  in
  testable pp (fun (u1 : User_model.user) (u2 : User_model.user) ->
      Vmm_core.Name.Label.equal u1.name u2.name
      && Mrmime.Mailbox.equal u1.email u2.email
      && String.equal u1.uuid u2.uuid
      && Bool.equal u1.active u2.active
      && Bool.equal u1.super_user u2.super_user)

let token_t =
  let pp ppf (t : User_model.token) =
    Fmt.pf ppf "%a" Yojson.Basic.pp (User_model.token_to_json t)
  in
  testable pp (fun (t1 : User_model.token) (t2 : User_model.token) ->
      String.equal t1.name t2.name
      && String.equal t1.value t2.value
      && Int.equal t1.expires_in t2.expires_in
      && Int.equal t1.usage_count t2.usage_count)

let cookie_t =
  let pp ppf (c : User_model.cookie) =
    Fmt.pf ppf "%s=%s (expires: %d)" c.name c.value c.expires_in
  in
  testable pp (fun (c1 : User_model.cookie) (c2 : User_model.cookie) ->
      String.equal c1.name c2.name
      && String.equal c1.value c2.value
      && Int.equal c1.expires_in c2.expires_in)

let email_config_t =
  let pp ppf (e : Utils.Email.t) =
    Fmt.pf ppf "%a" Yojson.Basic.pp (Utils.Email.to_json (Some e))
  in
  testable pp (fun (e1 : Utils.Email.t) (e2 : Utils.Email.t) ->
      Ipaddr.compare e1.server e2.server = 0
      && Int.equal e1.port e2.port
      && String.equal e1.base_url e2.base_url
      && Mrmime.Mailbox.equal e1.from_email e2.from_email)

let config_pair_t =
  let pp ppf (c : Configuration.t) =
    Fmt.pf ppf "%s (%a:%d)"
      (Configuration.name_to_str c.name)
      Ipaddr.pp c.server_ip c.server_port
  in
  testable pp (fun (c1 : Configuration.t) (c2 : Configuration.t) ->
      Vmm_core.Name.Label.equal c1.name c2.name
      && Ipaddr.compare c1.server_ip c2.server_ip = 0
      && Int.equal c1.server_port c2.server_port)

let policy_t =
  let pp ppf (p : Vmm_core.Policy.t) = Fmt.pf ppf "%a" Vmm_core.Policy.pp p in
  testable pp (fun (p1 : Vmm_core.Policy.t) (p2 : Vmm_core.Policy.t) ->
      Int.equal p1.unikernels p2.unikernels
      && Vmm_core.IS.equal p1.cpuids p2.cpuids
      && Int.equal p1.memory p2.memory
      && Option.equal Int.equal p1.block p2.block)

let pp_storage ppf (users, configuration, email) =
  Fmt.pf ppf "%a" Yojson.Basic.pp (Storage.t_to_json users configuration email)

let eq_config_pair (c1 : Configuration.t) (c2 : Configuration.t) =
  let {
    Configuration.name = n1;
    server_ip = si1;
    server_port = sp1;
    updated_at = ua1;
    certificate = cer1;
    private_key = pk1;
  } =
    c1
  and {
    Configuration.name = n2;
    server_ip = si2;
    server_port = sp2;
    updated_at = ua2;
    certificate = cer2;
    private_key = pk2;
  } =
    c2
  in
  Vmm_core.Name.Label.equal n1 n2
  && Ipaddr.compare si1 si2 = 0
  && Int.equal sp1 sp2 && Ptime.equal ua1 ua2
  && String.equal
       (X509.Certificate.fingerprint `SHA256 cer1)
       (X509.Certificate.fingerprint `SHA256 cer2)
  && String.equal
       (X509.Private_key.encode_der pk1)
       (X509.Private_key.encode_der pk2)

let eq_config (config_1 : Configuration.t list)
    (config_2 : Configuration.t list) =
  List.equal eq_config_pair config_1 config_2

let eq_user_pair (u1 : User_model.user) (u2 : User_model.user) =
  let {
    User_model.name = n1;
    email = e1;
    password = p1;
    uuid = id1;
    active = a1;
    super_user = s1;
    updated_at = uat1;
    created_at = cat1;
    email_verified = ev1;
    email_verification_uuid = evu1;
    tokens = _tk1;
    cookies = _ck1;
    unikernel_updates = _uk1;
    scaling_policies = _sp1;
  } =
    u1
  and {
    User_model.name = n2;
    email = e2;
    password = p2;
    uuid = id2;
    active = a2;
    super_user = s2;
    updated_at = uat2;
    created_at = cat2;
    email_verified = ev2;
    email_verification_uuid = evu2;
    tokens = _tk2;
    cookies = _ck2;
    unikernel_updates = _uk2;
    scaling_policies = _sp2;
  } =
    u2
  in
  Vmm_core.Name.Label.equal n1 n2
  && Mrmime.Mailbox.equal e1 e2 && String.equal p1 p2 && String.equal id1 id2
  && Bool.equal a1 a2 && Bool.equal s1 s2 && Ptime.equal uat1 uat2
  && Ptime.equal cat1 cat2
  && Option.equal Ptime.equal ev1 ev2
  && Option.equal Uuidm.equal evu1 evu2

let eq_users (users_1 : User_model.user list) (users_2 : User_model.user list) =
  List.equal eq_user_pair users_1 users_2

let eq_emails (e1 : Utils.Email.t) (e2 : Utils.Email.t) =
  let {
    Utils.Email.server = s1;
    port = p1;
    base_url = bu1;
    from_email = fe1;
    to_email = te1;
  } =
    e1
  and {
    Utils.Email.server = s2;
    port = p2;
    base_url = bu2;
    from_email = fe2;
    to_email = te2;
  } =
    e2
  in
  Ipaddr.compare s1 s2 = 0
  && Int.equal p1 p2 && String.equal bu1 bu2
  && Mrmime.Mailbox.equal fe1 fe2
  && Option.equal Mrmime.Mailbox.equal te1 te2

let eq_storage (u1, c1, e1) (u2, c2, e2) =
  eq_users u1 u2 && eq_config c1 c2 && Option.equal eq_emails e1 e2

let storage_t = testable pp_storage eq_storage

let mock_storage ?(version = 10) ?(users = []) ?(configuration = [])
    ?(email = None) () =
  Storage.t_to_json ~version users configuration email

let mock_email =
  {
    Utils.Email.server = Ipaddr.of_string_exn "10.0.0.1";
    port = 56;
    from_email = email_of_string_exn "test@robur.coop";
    base_url = "robur.coop";
    to_email = None;
  }

let mock_albatross_config =
  let cert = certificate_exn private_key in
  {
    Configuration.name = label_of_string_exn "default";
    certificate = cert;
    private_key;
    server_ip = Ipaddr.of_string_exn "10.0.0.1";
    server_port = 25;
    updated_at = Ptime.epoch;
  }

(** User Creation Helper *)
let make_mock_user ?(name = "testuser") ?(email = "test@example.com")
    ?(password = "securePassword123") ?(active = true) ?(super_user = false)
    ?(tokens = []) () =
  let name_lbl = label_of_string_exn name in
  let email_box = email_of_string_exn email in
  let now = Mirage_ptime.now () in
  let user, _cookie =
    User_model.create_user ~name:name_lbl ~email:email_box ~password ~active
      ~super_user ~created_at:now ~user_agent:(Some "Alcotest/1.0")
  in
  { user with tokens }
