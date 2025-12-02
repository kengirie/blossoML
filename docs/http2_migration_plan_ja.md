# HTTP/2 移行計画

この計画は、Piafを使用してOCaml BlossomサーバーをHTTP/2に対応させるための手順をまとめたものです。
主要なブラウザ（Chrome、Firefoxなど）はTLS上のHTTP/2（h2）のみをサポートしているため、この移行にはHTTPSサポートの追加が含まれます。

## ユーザーレビューが必要な事項

> [!IMPORTANT]
> **ブラウザでのHTTP/2利用にはHTTPSが必須です。**
> WebブラウザでHTTP/2を使用するには、有効なSSL証明書と秘密鍵を提供する必要があります。
> この計画では、`--cert` と `--key` というコマンドライン引数を追加します。これらが省略された場合、サーバーはHTTP/1.1 over Cleartext (HTTP) にフォールバックします。

## 変更案

### 1. `lib/shell/http_server.ml` の更新

`start` 関数を修正し、オプションの証明書パスを受け取るようにします。

- **関数シグネチャの変更**:
  ```ocaml
  val start :
    sw:Eio.Switch.t ->
    env:Eio.Stdenv.t ->
    port:int ->
    clock:Eio.Time.clock ->
    dir:Eio.Fs.dir_ty Eio.Path.t ->
    db:Blossom_db.t ->
    ?cert:string ->  (* 追加 *)
    ?key:string ->   (* 追加 *)
    unit -> unit
  ```

- **実装詳細**:
  - `cert` と `key` が提供された場合:
    - `Server.Config.https` を `allow_insecure=false`（または設定可能）で作成します。
    - `max_http_version` を `Versions.HTTP.v2_0` に設定します。
    - PiafはALPNネゴシエーションを処理し、HTTP/2へのアップグレードを行います。
  - `cert` と `key` が提供されない場合:
    - 既存のHTTP/1.1の動作にフォールバックします（必要であればH2Cを有効にすることも可能ですが、ブラウザクライアントには推奨されません）。

### 2. `bin/main.ml` の更新

SSL証明書用のコマンドライン引数を追加します。

- **新しい引数**:
  - `--cert <path>`: SSL証明書ファイル（PEM）へのパス。
  - `--key <path>`: SSL秘密鍵ファイル（PEM）へのパス。

- **ロジック**:
  - これらの引数をパースします。
  - それらを `Http_server.start` に渡します。

## 検証計画

### 自動テスト
- `curl` コマンドで `--http2` と `-k`（自己署名証明書を使用する場合）を使ってプロトコルを検証します。
  ```bash
  curl -v --http2 -k https://localhost:8082/
  ```
- レスポンスヘッダーで `HTTP/2` が使用されていることを確認します。

### 手動検証
- 自己署名証明書を使用してサーバーを起動します。
- ブラウザ経由でアクセスし、ネットワークタブで `h2` プロトコルが使用されていることを確認します。
