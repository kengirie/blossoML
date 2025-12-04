# Magic Bytes 検出の調査結果と実装計画

## 調査結果

### ocaml-magic-mime

- **リポジトリ**: https://github.com/mirage/ocaml-magic-mime
- **機能**: **拡張子ベースのみ**（magic bytes 検出は非対応）
- **使用方法**:
  ```ocaml
  Magic_mime.lookup "/foo/bar.txt"  (* -> "text/plain" *)
  ```
- **利点**: 軽量、依存関係なし、MirageOS 対応
- **欠点**: ファイル内容を見ない

### conan（推奨）

- **リポジトリ**: https://github.com/mirage/conan
- **機能**: **magic bytes 検出対応**（libmagic の OCaml 再実装）
- **最新バージョン**: 0.0.6（2025年4月）
- **OCaml 5.x**: 対応（≥ 4.08.0）
- **依存関係**: `re`, `uutf`, `ptime`

#### サブパッケージ

| パッケージ | 用途 |
|-----------|------|
| `conan` | コアライブラリ（決定木エンジン） |
| `conan-database` | プリコンパイル済み magic database |
| `conan-unix` | Unix ファイル I/O 対応 |

#### 使用例

```ocaml
open Rresult

(* データベースから決定木を構築 *)
let tree = R.failwith_error_msg @@ Conan_unix.tree_of_string magic_rules

(* ファイルから MIME 検出 *)
let metadata = Conan_unix.run_with_tree tree "image.png"

match Conan.Metadata.mime metadata with
| Some mime_type -> Printf.printf "%s\n" mime_type
| None -> Printf.eprintf "MIME type not found\n"
```

### magic（libmagic バインディング）

- **リポジトリ**: https://github.com/Chris00/ocaml-magic
- **機能**: C の libmagic へのバインディング
- **問題**: OCaml < 5.0 のみ対応 → **使用不可**

## 推奨アプローチ

**conan + conan-database** を採用する。

理由:
1. Pure OCaml（C バインディング不要）
2. OCaml 5.x 対応
3. MirageOS 対応（将来のポータビリティ）
4. 文字列/バイト列から直接 MIME 検出可能

## 実装計画

### Phase 1: 依存関係の追加

1. `dune-project` に依存関係を追加:
   - `conan`
   - `conan-database`
   - `magic-mime`（拡張子フォールバック用）

### Phase 2: MIME 検出モジュールの作成

新規モジュール `lib/core/mime_detect.ml` を作成:

```ocaml
(** MIME type 検出モジュール

    検出優先順位:
    1. magic bytes（conan）
    2. 拡張子（magic-mime）
    3. デフォルト値
*)

val detect_from_bytes : bytes -> string option
(** バイト列から MIME type を検出 *)

val detect_from_extension : string -> string
(** 拡張子から MIME type を取得（デフォルト: application/octet-stream） *)

val detect : ?filename:string -> bytes -> string
(** 総合判定: magic bytes -> 拡張子 -> デフォルト *)
```

### Phase 3: アップロード処理への統合

`lib/shell/http_server.ml` の PUT /upload ハンドラを更新:

```
現在の優先順位:
1. Content-Type ヘッダー
2. X-Content-Type ヘッダー
3. デフォルト (application/octet-stream)

新しい優先順位:
1. Content-Type ヘッダー
2. X-Content-Type ヘッダー
3. magic bytes 検出（conan）
4. デフォルト (application/octet-stream)
```

### Phase 4: ダウンロード時の拡張子対応

URL パスの拡張子から MIME type を推定（blossom-server 互換）:

```
GET /abc123.png -> Content-Type: image/png（拡張子から推定）
GET /abc123     -> Content-Type: DB保存値
```

### 実装タスク

- [ ] `dune-project` に `conan`, `conan-database`, `magic-mime` を追加
- [ ] `lib/core/mime_detect.ml` モジュールを作成
- [ ] `lib/core/mime_detect.mli` インターフェースを作成
- [ ] `lib/core/dune` に依存関係を追加
- [ ] ユニットテスト `test/test_mime_detect.ml` を作成
- [ ] `http_server.ml` のアップロード処理に統合
- [ ] ダウンロード時の拡張子対応を追加
- [ ] 統合テストの実行

### 注意事項

1. **conan-database のサイズ**: フルデータベースは比較的大きい（unikernel 向けに約 6MB）。必要に応じて軽量版を使用。

2. **ストリーム対応**: 現在の実装では body 全体を読み込んでから処理。magic bytes 検出には先頭数 KB で十分なため、パフォーマンス最適化の余地あり。

3. **キャッシュ**: conan の決定木はアプリ起動時に一度だけ構築し、リクエストごとに再利用する。

## 参考リンク

- [conan GitHub](https://github.com/mirage/conan)
- [conan opam](https://ocaml.org/p/conan/latest)
- [conan-database opam](https://ocaml.org/p/conan-database/latest)
- [magic-mime GitHub](https://github.com/mirage/ocaml-magic-mime)
- [magic-mime opam](https://opam.ocaml.org/packages/magic-mime/)
