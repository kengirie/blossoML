# hzrd149/blossom-server の MIME Type 判定調査

## 概要

[hzrd149/blossom-server](https://github.com/hzrd149/blossom-server) は TypeScript で実装された Blossom サーバーのリファレンス実装。MIME type の判定には複数のフォールバック戦略を採用している。

## 使用ライブラリ

| ライブラリ | バージョン | 用途 |
|-----------|-----------|------|
| `file-type` | v19.6.0 | ファイル内容（magic bytes）からの MIME type 検出 |
| `mime` | v4.0.6 | 拡張子 ↔ MIME type の相互変換 |

## アップロード時の MIME type 判定

### 判定フロー（優先順位順）

```
1. Content-Type ヘッダー
   ↓ (なければ)
2. X-Content-Type カスタムヘッダー
   ↓ (なければ)
3. file-type ライブラリによる magic bytes 検出
```

### 実装コード (`src/api/upload.ts`)

```typescript
// ヘッダーからの取得（優先）
const contentType = ctx.header["content-type"] || String(ctx.header["x-content-type"]);

// ...

// アップロード処理時
let type = contentType || upload.type;  // upload.type は file-type による検出結果
```

### 一時ファイル保存時 (`src/storage/upload.ts`)

```typescript
// リクエストからの保存
let type = message.headers["content-type"];

// レスポンスからの保存（リモートフェッチ時）
type = type || (await fileTypeFromFile(tempFile))?.mime;  // フォールバック

// 一時ファイル名に拡張子を付与
if (type) filename += "." + mime.getExtension(type);
```

## ダウンロード時の MIME type 判定

### 判定フロー（優先順位順）

```
1. ローカルストレージに保存された type
   ↓ (なければ)
2. URL パスの拡張子から mime.getType() で推定
   ↓ (application/octet-stream の場合)
3. リモートポインタの type 情報
```

### 実装コード (`src/api/fetch.ts`)

```typescript
// URLパスから拡張子ベースで推定
const search = { hash, type: mime.getType(ctx.path) };

// ローカルストレージから取得時
if (storageResult.type) ctx.type = storageResult.type;

// リモートフェッチ時のフォールバック
if (!ctx.type) {
  if (pointer.type === 'application/octet-stream' && search.type) {
    ctx.type = search.type;  // URLの拡張子を優先
  } else if (pointer.type) {
    ctx.type = pointer.type;
  } else if (search.type) {
    ctx.type = search.type;
  }
}
```

## データ構造

### BlobPointer 型

```typescript
type CommonPointer = {
  hash: string;
  type?: string;      // MIME type
  size: number;
  metadata?: PointerMetadata;
};
```

## ocaml-nostr-blossom との比較

| 機能 | blossom-server | ocaml-nostr-blossom |
|------|---------------|---------------------|
| Content-Type ヘッダー | ✅ | ✅ |
| X-Content-Type ヘッダー | ✅ | ❌ |
| magic bytes 検出 | ✅ (`file-type`) | ❌ |
| 拡張子からの推定 | ✅ (`mime`) | ❌ |
| デフォルト値 | `application/octet-stream` | `application/octet-stream` |

## 推奨改善点

1. **magic bytes 検出の追加**
   - OCaml では `magic-mime` や `conan` ライブラリが利用可能
   - ファイル内容から MIME type を検出することで、誤った Content-Type ヘッダーへの対策になる

2. **拡張子マッピングの追加**
   - ダウンロード時に URL の拡張子から MIME type を推定
   - `mime` ライブラリ相当の機能を実装

3. **X-Content-Type ヘッダーのサポート**
   - BUD-06 仕様との互換性向上

## 参考リンク

- [blossom-server リポジトリ](https://github.com/hzrd149/blossom-server)
- [file-type npm パッケージ](https://www.npmjs.com/package/file-type)
- [mime npm パッケージ](https://www.npmjs.com/package/mime)
