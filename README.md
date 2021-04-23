# ETBA 1.9.1
## What is this?


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
11. とりあえずアクションは増えたぞ。
12. しかしつまらん。
13. トップビューアクションの面白さとは一体？
14. 戦略性？
15. とりあえず敵の行動にパターンを導入してみるか。
16. とりあえず導入できたぞ。
17. しかしまだつまらん。
18. 行動パターンが一つしかなく戦略も一つしかとりようがないのが原因だな。
19. しかし安易にアレコレ増やしたくない。
20. まずはゲームバランス調整を簡単に行うか。

### ｲﾏｺｺ!!
21. 簡単な調整は終わったぞ。
22. 調整を通して不具合が見えてきたのでFixしよう。

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
* 敵には行動パターンがある。

## Task as known issues.

- [ ] 攻撃が多段ヒットしてしまうのを修正。
- [ ] 見た目上衝突しているのに衝突判定が失敗するのを修正。
- [ ] 攻撃をするまでキャラクターが描画されないのを修正。
