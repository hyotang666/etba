# ETBA 1.0.0
## What is this?
Top view action.

## History

### これまで
1. [Elona](http://ylvania.style.coocan.jp/elona_top.html)は面白いなぁ。
2. ターン制じゃなくてリアルタイムアクションならなお好きなんだがなぁ。
3. 作るか。
4. 全部作ると骨だから選択と集中をしよう。
5. 最小限の見下ろしアクションを手始めに作ってみるか。
6. とりあえず形になったぞ。
7. これをどう拡張しようか？
8. 本家との違いはリアルタイム性のアクションである点だからそこに力を入れなきゃ嘘だよな。
9. アニメーションに力を入れるのは大変そうだけどボタン操作で色々動かせるってのはまだできそうだな。
10. 手始めに現時点で使ってるボタンのみでできるアクションを増やそうか。

### ｲﾏｺｺ!!
11. とりあえずアクションは増えたぞ。
12. しかしつまらん。
13. トップビューアクションの面白さとは一体？
14. 戦略性？
15. とりあえず敵の行動にパターンを導入してみるか。

## 仕様
### 実装済み
* プレイヤーは一人。
* 敵も一人。
* 画面は固定。
* プレイヤーのHPがゼロになったらゲームオーバー。
* 敵のHPがゼロになったらクリア。
* カーソルキーを連続入力することで移動速度アップ。
* 攻撃ボタンを押しっぱなしにすることでチャージ攻撃。
  * チャージの溜まり具合で攻撃パターン切り替え。
* 攻撃ボタンを連打することでいわゆる百烈張り手的な攻撃。
* 攻撃ボタンを一定のテンポで入力することで前進を伴うコンボ追撃。

### 追加仕様
* 敵には行動パターンがある。

## Task

- [x] キノコらしく（？）規定行動は待機を実装。
  - [x] 移動せずに方向だけランダムに変更を実装。
- [x] Playerがテリトリに侵入したら遠隔攻撃を実装。
- [x] Playerが射程範囲にいれば近接攻撃を実装。
