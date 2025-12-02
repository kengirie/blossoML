# Blossom Server 起動方法

このドキュメントでは、OCaml Blossom Server の起動方法、特に HTTP/2 (HTTPS) を有効にするための SSL 証明書と秘密鍵の設定方法について説明します。

## 前提条件

- OCaml 環境 (opam) がセットアップされていること
- 依存ライブラリがインストールされていること (`dune build` が通ること)
- `openssl` コマンドが利用可能であること（自己署名証明書を作成する場合）

## ビルド

まず、プロジェクトをビルドします。

```bash
eval $(opam env)
dune build
```

## 起動オプション

サーバーは以下のコマンドライン引数を受け付けます。

- `--port <int>`: サーバーがリッスンするポート番号 (デフォルト: 8082)
- `--cert <path>`: SSL 証明書ファイル (PEM 形式) へのパス
- `--key <path>`: SSL 秘密鍵ファイル (PEM 形式) へのパス

## HTTP/2 (HTTPS) での起動

ブラウザで HTTP/2 を利用するには HTTPS が必須です。

### 1. 自己署名証明書の作成 (テスト用)

開発やテストのために、自己署名証明書を作成します。

```bash
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes -subj "/CN=localhost"
```

これにより、カレントディレクトリに `key.pem` (秘密鍵) と `cert.pem` (証明書) が生成されます。

### 2. サーバーの起動

生成した証明書と鍵を指定してサーバーを起動します。

```bash
dune exec ./bin/main.exe -- --cert cert.pem --key key.pem --port 8082
```

### 3. 動作確認

#### curl を使用する場合

`curl` コマンドで HTTP/2 が有効になっているか確認できます。自己署名証明書を使用している場合は `-k` (insecure) オプションが必要です。

```bash
curl -v --http2 -k https://localhost:8082/
```

出力に `> GET / HTTP/2` のような行が含まれていれば、HTTP/2 で通信できています。

#### ブラウザを使用する場合

Chrome や Firefox などのブラウザで `https://localhost:8082/` にアクセスします。
自己署名証明書の場合、セキュリティ警告が表示されますが、無視して進んでください。
開発者ツールの「Network」タブを開き、プロトコル (Protocol) 列が `h2` になっていることを確認してください。

## HTTP/1.1 (HTTP) での起動

証明書と鍵を指定しない場合、サーバーは通常の HTTP (HTTP/1.1) で起動します。

```bash
dune exec ./bin/main.exe -- --port 8082
```

この場合、`http://localhost:8082/` でアクセスできます。
