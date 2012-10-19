Qiita-mode.el 作りました #qiita #markdown
gongoさんに先を越されてしまいましたが、一応公開します。
https://github.com/uk-ar/qiita-mode

# これは何か

Emacs から qiita を操作出来ます。

# 使い方

## 投稿
```
M-x qiita-post-buffer
```
で現在開いているバッファを qiita に投稿します。
* 先頭行がタイトル
* 先頭行のなかで#を使用してタグを指定する
* 2行目以降のテキストが本文
になります。

例を出すと
```
Qiita-mode.el 作りました #qiita #markdown
https://github.com/uk-ar/qiita-mode

# これは何か
...
```
のようになります。

初回投稿時に記事のidがコメントとして埋め込まれるので、再度編集後に
```
M-x qiita-post-buffer
```
とすることで更新ができます。

## 一覧
anything.elのインストールした環境では

```
M-x qiita-my-items
```
で自分の記事一覧

```
M-x qiita-all-items
```
ですべての記事一覧
```
M-x qiita-tags-items emacs
```
でemacsのタグがついた記事一覧が取れます。

### 回覧

一覧でエンターを押すとデフォルトブラウザで記事を開きます。

### 削除

未実装です。

# 今後の予定

実用性を考えると投稿や一覧も非同期にしたいところです。
が、deferred.elを使おうとするとcurlを直に呼び出すのではなくurl-retrieve互換の層が欲しくなります。
モチベーションが続けば、そのあたりをがんばろうと思います。
