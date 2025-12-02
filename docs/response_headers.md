# Blossom Response Matrix

この文書は現在の `lib/shell/http_server.ml` と [公式 Blossom 仕様](https://github.com/hzrd149/blossom)（主に BUD-01, BUD-02）に基づき、Blossom サーバーが返すレスポンスの条件とヘッダー処理をまとめたものです。すべてのレスポンスは最終的に `Access-Control-Allow-Origin: *` を付けて返され、エラー系は `X-Reason` を含みます。

| メソッド & パス | 条件 | ステータス / ボディ | 主要ヘッダー | 実装 / 仕様メモ |
| --- | --- | --- | --- | --- |
| `OPTIONS *` | CORS プリフライト | `204 No Content`, 空ボディ | `Access-Control-Allow-Origin: *`, `Access-Control-Allow-Methods: GET, HEAD, PUT, DELETE, OPTIONS`, `Access-Control-Allow-Headers: Authorization, Content-Type, Content-Length, *`, `Access-Control-Max-Age: 86400` | `handle_cors_preflight` が BUD-01 の要求を実現 (`lib/shell/http_server.ml:33-40`, `hzrd149/blossom/buds/01.md`) |
| `GET /<sha256(.ext)>` | ハッシュ妥当 & blob 取得成功 | `200 OK`, ボディ=blob | `Content-Type`, `Content-Length`, `Access-Control-Allow-Origin: *` | メタデータからヘッダー作成 (`lib/shell/http_server.ml:50-69`)。BUD-01 の GET 仕様に合致 |
| `GET /<sha256>` | ハッシュ不正/パス不正 | `404 Not Found`, `"Invalid path or hash"` | `X-Reason: Invalid path or hash`, CORS | `Integrity.validate_hash` 失敗 (`lib/shell/http_server.ml:55-70`) |
| `GET /<sha256>` | blob 不在 | `404 Not Found`, `"Blob not found"` | `X-Reason: Blob not found`, CORS | `Local_storage.get` が `Blob_not_found` (`lib/shell/http_server.ml:60-68`) |
| `GET /<sha256>` | ストレージ等の内部エラー | `500 Internal Server Error`, `msg` または `"Internal error"` | `X-Reason: <msg>`, CORS | `Local_storage.get` からの `Storage_error` など (`lib/shell/http_server.ml:60-69`) |
| `HEAD /<sha256(.ext)>` | ハッシュ妥当 & blob あり | `200 OK`, ボディ無し | `Content-Type`, `Content-Length`, CORS | GET と同じヘッダーでボディ無し (`lib/shell/http_server.ml:71-85`)。BUD-01 HEAD 仕様 |
| `HEAD /<sha256>` | ハッシュ不正/パス不正 | `404 Not Found`, `"Invalid path or hash"` | `X-Reason: Invalid path or hash`, CORS | (`lib/shell/http_server.ml:74-89`) |
| `HEAD /<sha256>` | blob 不在 | `404 Not Found`, `"Blob not found"` | `X-Reason: Blob not found`, CORS | 同上 |
| `HEAD /<sha256>` | ストレージ等の内部エラー | `500 Internal Server Error`, `"Internal error"` または詳細 | `X-Reason`, CORS | 同上 |
| `PUT /upload` | `Authorization` ヘッダー欠如 | `401 Unauthorized`, `"Missing Authorization header"` | `X-Reason: Missing Authorization header`, CORS | 認証必須 (BUD-02 Upload Auth)。`lib/shell/http_server.ml:90-93` |
| `PUT /upload` | 認証イベント検証失敗 | `401 Unauthorized`, `"Authentication failed"` またはエラー文 | `X-Reason`, CORS | `Auth.validate_auth` 失敗 (`lib/shell/http_server.ml:94-98`)。BUD-02 の要件に対応 |
| `PUT /upload` | ポリシー (サイズ/MIME) 違反 | `400 Bad Request`, `"Invalid size: <n>"` など | `X-Reason`, CORS | `Policy.check_upload_policy` エラー (`lib/shell/http_server.ml:107-117`) |
| `PUT /upload` | 保存処理失敗 | `500 Internal Server Error`, `<msg>` | `X-Reason`, CORS | `Local_storage.save_stream` エラー (`lib/shell/http_server.ml:118-126`) |
| `PUT /upload` | 成功 | `200 OK`, JSON Blob Descriptor | CORS (追加ヘッダー無し) | BUD-02 の Blob Descriptor を返却 (`lib/shell/http_server.ml:126-140`, `hzrd149/blossom/buds/02.md`) |
| その他 (未実装パス) | どの分岐にも該当せず | `404 Not Found`, `"Not found"` | `X-Reason: Not found`, CORS | キャッチオール (`lib/shell/http_server.ml:140-142`) |

## 参考
- 実装: `lib/shell/http_server.ml`
- 仕様: `hzrd149/blossom/buds/01.md`, `hzrd149/blossom/buds/02.md`
