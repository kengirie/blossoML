# hzrd149/blossom-server Response Matrix

## 共通挙動
- **CORS 全面適用**: `src/index.ts` で `@koa/cors` をグローバル適用し、`origin: *`, `allowMethods: *`, `allowHeaders: Authorization,*`, `exposeHeaders: *`, `maxAge: 86400` を常時セット。Preflight (`OPTIONS`) もこの設定で応答する。
- **エラーハンドリング**: `http-errors` 由来の例外は `ctx.status = err.status` で処理され、`X-Reason` に `err.message`（5xx の一部は "Something went wrong"）を設定。その他の例外も 500 / `X-Reason: Something went wrong` を返す (`src/index.ts`)。
- **認証パース**: `Authorization: Nostr <base64>` があれば JSON を復元し、kind 24242 か, `t` タグ, `expiration` を検証した上で `ctx.state.auth*` に格納 (`src/api/router.ts`)。各エンドポイントで必要に応じて検証される。

## エンドポイントごとのレスポンス
| メソッド & パス | 条件 | ステータス / ボディ | 主要ヘッダー | 実装参照 |
| --- | --- | --- | --- | --- |
| `GET /:hash(.ext)` | Blob がローカル格納済み | `200 OK`, ボディ=Blob ストリーム | `Content-Type`（保存メタ）, `Content-Length`。koa-range で Range 対応 | `src/api/fetch.ts` (`searchStorage`, `readStoragePointer`)
| `GET /:hash(.ext)` | ストレージがリダイレクト指示を返す | `302 Found`（`ctx.redirect` 既定）, ボディ無し | `Location: <redirect>`, CORS | `src/api/fetch.ts` (`getStorageRedirect`)
| `GET /:hash(.ext)` | ローカルに無いが HTTP Pointer 取得に成功 | `200 OK`, body=Pointer からのストリーム | `Content-Type` は pointer/type/推測、`Content-Length` = pointer.size | `src/api/fetch.ts`（HTTP pointer ループ）
| `GET /:hash(.ext)` | どのバックエンド/Pointer でも見つからず | `404 Not Found`, `X-Reason: Cant find blob for hash` | `X-Reason`, CORS | 同上 (`HttpErrors.NotFound`)
| `HEAD /:hash(.ext)` | Blob あり | `200 OK`, ボディ無し | `Content-Type`, `Content-Length`, `Accept-Ranges: bytes`（koa-range が自動追加） | `src/api/has.ts`
| `HEAD /:hash(.ext)` | Blob 不在 | `404 Not Found`, ボディ無し | （明示ヘッダー無し / CORS のみ） | `src/api/has.ts`
| `PUT /upload` | 設定で upload 無効 | `404 Not Found`, `X-Reason: Uploads disabled` | `X-Reason`, CORS | `src/api/upload.ts` (冒頭 middleware)
| `HEAD /upload` | Upload 有効時のヘルスチェック | `200 OK`, 空ボディ | CORS のみ | `src/api/upload.ts`
| `PUT /upload` | 認証/ルール OK & 保存成功 | `200 OK`, JSON Blob Descriptor (`{sha256,size,type,uploaded,url}`) | `Content-Type: application/json`（koa 既定） | 同上 (`addFromUpload`, `getBlobDescriptor`)
| `PUT /upload` | 認証・ポリシー違反や保存失敗 | `4xx/5xx`, `X-Reason` に詳細 | `X-Reason` | `checkUpload` / `addFromUpload` 内の `HttpErrors`、および共通エラー
| `GET /list/:pubkey` | 条件に一致する Blob あり | `200 OK`, Blob Descriptor 配列 | JSON | `src/api/list.ts`
| `GET /list/:pubkey` | 認証必須だが欠如/不一致 | `401 Unauthorized`, `X-Reason` に不足理由 | `X-Reason` | `src/api/list.ts`
| `DELETE /:hash(.ext)` | 認証成功 & オーナー削除 | `200 OK`, `{ "message": "Deleted" }` | JSON | `src/api/delete.ts`
| `DELETE /:hash(.ext)` | Blob 未登録 | `404 Not Found`, `X-Reason: Blob does not exist` | `X-Reason` | `src/api/delete.ts`
| `DELETE /:hash(.ext)` | 認証欠如/誤り | `401 Unauthorized`, `X-Reason` に理由 | `X-Reason` | 同上
| `HEAD /media` | Media upload 有効 | `200 OK`, 空ボディ | CORS のみ | `src/api/media.ts`
| `PUT /media` | 認証/最適化/保存成功 | `200 OK`, JSON Blob Descriptor (最適化後) | JSON | `src/api/media.ts`
| `PUT /media` | Media upload 無効 | `404 Not Found`, `X-Reason: Media uploads disabled` | `X-Reason` | `src/api/media.ts`
| `PUT /media` | 認証・最適化・保存エラー | `4xx/5xx`, `X-Reason` | `X-Reason` | 同上
| `PUT /mirror` | ミラー元取得・保存成功 | `200 OK`, JSON Blob Descriptor | JSON | `src/api/mirror.ts`
| `PUT /mirror` | Upload 全体が無効 | `404 Not Found`, `X-Reason: Uploads disabled` | `X-Reason` | 同上
| `PUT /mirror` | 認証/URL/ルール違反やダウンロード失敗 | `4xx/5xx`, `X-Reason` に理由 | `X-Reason` | 同上

## 備考
- `/mirror` と `/media` は `koa-body` / ファイル最適化を通すため、失敗時にはアップロード済み一時ファイルを削除しつつ `HttpErrors` を投げる。
- `/upload`・`/media` ともに HEAD 応答で `200` を返し、クライアントがアップロード要件を事前確認できる（BUD-06）。
- すべての成功応答でも `koa/cors` により `Access-Control-Allow-Origin: *` 等が付与される。
