# ETBA 1.0.0
## What is this?
Top view action.

## History

1. [Elona](http://ylvania.style.coocan.jp/elona_top.html)は面白いなぁ。
2. ターン制じゃなくてリアルタイムアクションならなお好きなんだがなぁ。
3. 作るか。
4. 全部作ると骨だから選択と集中をしよう。
5. 最小限の見下ろしアクションを手始めに作ってみるか。

6. とりあえず形になったぞ。
7. これをどう拡張しようか？
8. 本家との違いはリアルタイム性のアクションである点だからそこに力を入れなきゃ嘘だよな。
9. アニメーションに力を入れるのは大変そうだけどボタン操作で色々動かせるってのはまだできそうだな。
10. 手始めに現時点で使ってるボタンのみでできるアクションを増やそうか。<--- ｲﾏｺｺ!!

## 仕様
### 実装済み
* プレイヤーは一人。
* 敵も一人。
* <del>攻撃種類も一つ。</del>
* 画面は固定。
* プレイヤーのHPがゼロになったらゲームオーバー。
* 敵のHPがゼロになったらクリア。

### 追加仕様
* カーソルキーを連続入力することで移動速度アップ。
* 攻撃ボタンを押しっぱなしにすることでチャージ攻撃。
  * チャージの溜まり具合で攻撃パターン切り替え。
* 攻撃ボタンを連打することでいわゆる百烈張り手的な攻撃。
* 攻撃ボタンを一定のテンポで入力することで前進を伴うコンボ追撃。

## Task

- [x] 通常攻撃時一瞬固まる副作用スタンの実装。
- [x] ノックバックの実装。
- [x] ダッシュの実装。
- [x] チャージによる遠距離攻撃の実装。
  - [x] チャージ度合いにより飛距離変動の実装。
  - [x] 遠距離攻撃画像作成。
  - [x] チャージ中の移動速度低下を実装。
- [x] 連続攻撃の実装。
  - [x] 通常攻撃より長い副作用スタンの実装。
  - [x] 連続攻撃エフェクトの作成。
- [x] 追撃の実装。
- [ ] 違う操作をしていることがわかりやすいように効果音の導入。
  - [x] 通常攻撃（打撃）効果音作成。
  - [x] 連続攻撃効果音作成。
  - [x] 遠距離攻撃（波動）効果音作成。
  - [x] 追撃効果音作成。
  - [x] ダッシュ開始時効果音作成。
- [x] 敵AIが新行動を行うようにAIを拡張。
