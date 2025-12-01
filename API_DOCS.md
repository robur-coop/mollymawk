# Mollymawk API Documentation

This documentation outlines the endpoints for Mollymawk.

## 1. Authentication & Security

The API supports two methods of authentication:

**Session Cookies**: Used primarily by the browser UI (`molly-session`). Requires CSRF tokens (`X-MOLLY-CSRF` header or `form_csrf` field).

**Bearer Tokens**: Used for programmatic API access. When using a token, CSRF checks are skipped.

Endpoints

| Method | Endpoint                      | Auth Required | Description                                       |
| ------ | ----------------------------- | ------------- | ------------------------------------------------- |
| `POST` | `/api/register`               | No            | Register a new user account.                      |
| `POST` | `/api/login`                  | No            | Authenticate and retrieve a session/user details. |
| `POST` | `/logout`                     | Yes           | Log out the current session.                      |
| `POST` | `/account/password/update`    | Yes           | Update the current user's password.               |
| `POST` | `/api/account/session/close`  | Yes           | Revoke a specific session.                        |
| `POST` | `/api/account/sessions/close` | Yes           | Revoke all other sessions.                        |
| `GET`  | `/verify-email`               | Yes           | Trigger email verification process.               |
| `GET`  | `/auth/verify`                | Yes           | Verify email via token (Query param: token).      |

#### Request Bodies

- `/api/register`

  ```json
  {
    "email": "user@example.com",
    "password": "securepassword",
    "name": "username",
    "form_csrf": "csrf_token_string"
  }
  ```

- `/api/login`

  ```json
  {
    "email": "user@example.com",
    "password": "securepassword"
  }
  ```

## 2. Unikernel Management

Endpoints for deploying, managing, and monitoring unikernels.

### List Unikernels

- Endpoint: `/api/unikernels`

- Method: `GET`

- Auth: `Token Required`

- Description: Returns a list of unikernels running on the configured Albatross instances.

```json
Response:

{
  "data": [ ...unikernel objects... ],
  "errors": [ ...error objects... ]
}
```

### Deploy Unikernel

- Endpoint: `/api/unikernel/create`

- Method: `POST`

- Auth: `Token Required`

- Content-Type: `multipart/form-data`

- Description: Uploads a unikernel binary and configuration to deploy a new unikernel.

- Form Data Parts:

  - `albatross_instance`: (string) Target instance name.

  - `unikernel_name`: (string) Name for the new unikernel.

  - `unikernel_config`: (json) Configuration JSON string.

  - `unikernel_force_create`: (bool) "true" or "false".

  - `binary`: (file) The unikernel binary file.

## Update Unikernel

- Endpoint: `/api/unikernel/update`

- Method: `POST`

- Auth: `Token Required`

- Description: Updates a unikernel by taking the newer version from [builds.robur.coop](builds.robur.coop), performing liveliness checks before finalizing.

```json
Body:

{
  "albatross_instance": "string",
  "job": "string",
  "to_be_updated_unikernel": "uuid_string",
  "currently_running_unikernel": "uuid_string",
  "unikernel_name": "string",
  "http_liveliness_address": "string (optional)",
  "dns_liveliness": "string (optional)",
  "unikernel_arguments": "object (optional)"
}
```

### Rollback Unikernel

- Endpoint: `/api/unikernel/rollback`

- Method: `POST`

- Auth: `Token Required`

```json
Body:

{
  "unikernel_name": "string",
  "albatross_instance": "string"
}
```

#### Lifecycle Actions

| Action  | Method | Endpoint                 | Body                                                 |
| ------- | ------ | ------------------------ | ---------------------------------------------------- |
| Destroy | `POST` | `/api/unikernel/destroy` | `{"name": "string", "albatross_instance": "string"}` |
| Restart | `POST` | `/api/unikernel/restart` | `{"name": "string", "albatross_instance": "string"}` |

### Console Stream

- Endpoint: `/api/unikernel/console`

- Method: `GET`

- Query Params: `?unikernel=NAME&instance=INSTANCE_NAME`

- Response: `text/event-stream` (Server-Sent Events)

## 3. Volume (Block Device) Management

### List Volumes

- Endpoint: `/volumes`

- Method: `GET`

- Query Param: `?instance=INSTANCE_NAME`

- Description: Returns information about volumes created by the user on this albatross instance.

### Create or Upload data to a Volume

- Endpoint: `/api/volume/create` or `/api/volume/upload`

- Method: `POST`

- Auth: `Token Required` or `Cookie`

- Content-Type: `multipart/form-data`

Description: Creates an empty block device/Streams block data to Albatross.

- Form Data Parts:

  - `albatross_instance`: (string)

  - `json_data`: (string) JSON Metadata.

  - `block_data`: (stream) The volume content.

- Metadata (json_data):

  - Create: `{"block_name": "name", "block_size": int, "block_compressed": bool}`

  - Upload: `{"block_name": "name", "block_compressed": bool}`

### Delete Volume

- Endpoint: `/api/volume/delete`

- Method: `POST`

- Body: `{"block_name": "string", "albatross_instance": "string"}`

### Download Volume

- Endpoint: `/api/volume/download`

- Method: `POST`

- Body: `{"albatross_instance": "string", "block_name": "string", "compression_level": int}`

- Response: `application/octet-stream`

## 4. API Token Management

Endpoints for users to manage their programmatic access tokens.

- List Tokens: `GET /tokens` (HTML View)

- Create Token: `POST /api/tokens/create`

  - Body: `{"token_name": "string", "token_expiry": int}`

- Delete Token: `POST /api/tokens/delete`

  - Body: `{"token_value": "string"}`

- Update Token: `POST /api/tokens/update`

  - Body: `{"token_name": "string", "token_expiry": int, "token_value": "string"}`

## 5. Administration

These endpoints require the user to be an admin.

### User Management

- View Users: `GET /admin/users`

- View User Detail: `GET /admin/user?uuid=...`

- Toggle Active Status: `POST /api/admin/user/activate/toggle`

  - Body: `{"uuid": "user_uuid"}`

- Toggle Admin Status: `POST /api/admin/user/admin/toggle`

  - Body: `{"uuid": "user_uuid"}`

- Update User Policy: `POST /api/admin/u/policy/update`

  - Body: `{"user_uuid": "...", "albatross_instance": "...", ...policy_args...}`

### System Settings (Albatross Configuration)

- View Settings: `GET /admin/settings`

- View Error Logs: `GET /admin/albatross/errors?instance=...`

- Retry Connection: `GET /api/admin/albatross/retry?instance=...`

- Update Config: `POST /api/admin/settings/update`

- Create Config: `POST /api/admin/settings/create`

- Delete Config: `POST /api/admin/settings/delete`

  - Body: `{"name": "config_name"}`

## 6. UI Routes (Browser)

These endpoints are intended for browser navigation.

`GET /` - Landing Page

`GET /dashboard` - Main user dashboard

`GET /account` - Account settings

`GET /usage` - Resource usage view

`GET /select/instance` - Albatross instance selection screen

`GET /unikernel/info` - Unikernel details

`GET /unikernel/deploy` - Deployment form

`GET /unikernel/update` - Update form

`GET /admin/u/policy/edit` - Policy editor

## 7. Static Assets

`GET /main.js`

`GET /style.css`

`GET /images/molly_bird.jpeg`

`GET /images/albatross_1.png`

`GET /images/dashboard_1.png`

`GET /images/mirage_os_1.png`

`GET /images/robur.png`
