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
21. 簡単な調整は終わったぞ。
22. 調整を通して不具合が見えてきたのでFixしよう。
23. 現状確認
  * テリトリーに侵入しない限りキノコは攻撃してこない。
  * 安全圏でチャージして遠距離攻撃でチクチク刺せば比較的安全に殺せる（ただし時間がかかる）
  * 被ダメ覚悟で突っ込んでいって殴り散らすのが最適解になってしまっている。
  * キノコの近接攻撃力を上げても戦略性はこれ以上増えない。
24. 敵の攻撃に予備動作があれば駆け引きが生まれる？
25. 防御ボタンを導入すれば駆け引きがより楽しくなる？
26. 現状確認
  * シールドバッシュ強すぎィ！
27. これは調整が必須。
28. 五秒のクールダウンタイムを実装したぜ！
29. 現状確認
  * 敵前まで近づいてタイミング良くガード/攻撃をくりかえすのが最適解。
  * 敵（キノコ）が動かないためこちらも動く必要がほとんどない。
30. 動き回る敵を導入してみよう。

### ｲﾏｺｺ!!

31. だいぶゲームとして形になってきたな。
32. 可能な限りシステムを小さく保ったまま面白さを追求するならデータを増やすことだな。

## 仕様
### 実装済み
* プレイヤーは一人。
* 画面は固定。
* プレイヤーのHPがゼロになったらゲームオーバー。
* カーソルキーを連続入力することで移動速度アップ。
* 攻撃ボタンを押しっぱなしにすることでチャージ攻撃。
  * チャージの溜まり具合で攻撃パターン切り替え。
* 攻撃ボタンを連打することでいわゆる百烈張り手的な攻撃。
* 攻撃ボタンを一定のテンポで入力することで前進を伴うコンボ追撃。
* 敵には行動パターンがある。
* 敵の攻撃には予備動作がある。
* 防御ボタンで敵の攻撃をガード可能。
* シールドバッシュにはクールタイムがある。
* ガード時は動けないものの方向転換だけはできる。
* ガード方向と攻撃方向でダメージが変動する。
  * 正面からの攻撃は無効。
  * 背後からの攻撃は倍。
  * 横からの攻撃は半減。
  * 斜め正面からの攻撃は1/4。
  * 斜め背後からの攻撃は3/4。
* <del>敵は二匹。</del>
* <del>敵を殲滅するとクリア。</del>

### 追加仕様

* 敵の数はランダム（5くらいを想定）。
* 敵を殲滅すると次ステージへ。

## Task

- [x] 階段画像を作成。
- [x] 敵を殲滅すると階段が現れ階段経由で次ステージへ移動を実装。
- [ ] NPCモンスターの増加。
  - [x] スライム。
  - [x] ネズミ。
  - [x] マンドレイク。
  - [x] 蛇。
  - [x] カブトムシ。
  - [x] 猫。
  - [ ] ワーム。
  - [ ] カラス。


