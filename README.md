# w32-ime.el の歴史

## はじめに

[本リポジトリ](https://github.com/trueroad/w32-ime.el) は、
[GNU Emacs](https://www.gnu.org/software/emacs/)
を Windows で使う際に、日本語入力用の IME を使うことができるようにする
IME パッチの一部である、 w32-ime.el および関連ファイルについて収集し、
その開発の流れを追えるようにしたものです。

## w32-ime.el とは

### IME パッチ

GNU Emacs は、かな漢字変換の機能 (IM: input method) を持っているので、
適切なインストールと設定がされていれば日本語の入力ができます。
これは Windows 上で動作する GNU Emacs でも同じですが、
基本的に Emacs の IM を使って日本語入力することになり、
Windows の MS-IME など IME (input method editor)
で日本語入力することは困難でした。
通常の Windows アプリケーションは IME で日本語入力するのに
Emacs では IME が使えないので IM で日本語入力する、となってしまうと
Emacs だけ日本語入力の操作性や変換辞書などが異なって使いにくい
という問題がありました。
（Emacs 26.1 までは表示上の問題などがあり本当に困難でしたが、
Emacs 26.2 以降は「とりあえず」レベルでは使えるようになってます。
ただし、後述の IM
との連携やモードライン切り替えなどには対応していないので不便です。）

Emacs はオープンソースなので自分で手を入れて改造することができます。
そこで、Emacs でも IME が使えるようにパッチを作る試みが行われてきました。
これが IME パッチです。
IME パッチについては Wurly 氏の
[NTEmacs IMEパッチ](http://cha.la.coocan.jp/doc/NTEmacsIMEpatch.html)
が参考になります。

### IME パッチと w32-ime.el

Emacs は大きく分けて、
C で書かれている部分と Emacs Lisp で書かれている部分があります。
基本的には API やウィンドウメッセージなど OS に関わる低レイヤの処理は
C で書かれていて、
UI など操作性に関わる部分は IM も含め Lisp で書かれています。
Windows の IME は OS と深くかかわる仕組みであるため、
IME を使えるようにするには
IME の API やウィンドウメッセージを処理する必要があり、
IME パッチは C のソースを変更してこうした処理を追加します。
そして、UI 関連や既存の IM との連携については Lisp を追加します。
Emacs の Lisp 部分は、
元々カスタマイズできるように意識された設計になっているため、
設定やロード関連など一部を除いて既存の Lisp ソースを変更するのではなく、
新規の Lisp ソースを追加することになります。

この IME パッチで新規に追加する Lisp ソースが、
[本リポジトリ](https://github.com/trueroad/w32-ime.el)
で扱う w32-ime.el です。
w32-ime.el には、既存の IM の処理と連携して
IME が IM の一つの入力メソッドとして振舞えるようにする処理や、
UI 関連ではモードラインに IME の状態を表示する処理などが入っています。
これにより、IM 側から、かな漢字変換の ON/OFF 要求（c-\\ など）があったら、
w32-ime.el が要求を受け取って
C で実装されている IME ON/OFF の関数を呼び出して切り替え、
モードラインの変更も行います。
逆に IME 側から、かな漢字変換の ON/OFF 要求
（半角/全角キーやマウス操作など）があったら、
C 実装が状態変更のウィンドウメッセージを受け取って w32-ime.el へ通知し、
w32-ime.el が IM の状態を切り替え、モードラインの変更も行う、
という動作になります。

このように、w32-ime.el は C で実装されている低レイヤ処理と、
元々 Emacs が持っている IM の処理を橋渡しする重要な役割を担っている
といえます。

## w32-ime.el の歴史

これまで様々な方々が IME パッチを作成・提供してこられました。
IME パッチの歴史については Wurly 氏の
[NTEmacs IMEパッチの歴史](http://cha.la.coocan.jp/doc/NTEmacsIMEpatchHistory.html)
にまとまっています。
そして、IME パッチの変遷とともに、
その一部である w32-ime.el も変遷を遂げてきました。
本リポジトリでは、w32-ime.el の他に、
その前身である meadow-ntemacs.el や、
w32-ime.el と併用してより高度な処理を行うために作られた smart-ime.el
についても取り扱います。

これらの歴史的なものも含めて IME パッチを収集し、
その日付や内容から世代や派生を推定して、
流れをいくつかのブランチにまとめました。
残念ながら入手できなかった版も存在しますが、
今後、収拾済のものを繋ぐ中間にあたる世代が入手できた場合には、
本リポジトリではコミット間に挟み込むことを考えており、
そうするとコミットのハッシュ値が変更になります。
そのため、特定の版の w32-ime.el などをハッシュ値で参照していると、
将来ハッシュ値が変更されて参照できなくなる可能性があります。
そこで、特定の版を参照するためにタグを付与し、
ハッシュ値が変更されても以前と同じ名前のタグで参照できるようにします。
また、複数の IME パッチが同一内容の w32-ime.el などを持っていた場合は、
同じコミットに複数のタグを付与しています。
タグの名称は基本的に IME パッチの日付と公開者名を
アンダーバーで繋いだものです。
一部の同一日に公開されたものは追加で通番を振ったり、
同一の公開者が同時期に複数系統の IME パッチを公開していたら
公開者名に追加でサフィックスを振ったりしているものもあります。
収集先・収集方法などはタグやコミットのメッセージに記載しています。

### 三好雅則氏

三好氏は Windows 用の Emacs 実装の一つである Meadow の開発メンバの一人です。
Meadow （GNU Emacs 20 ベース）のソースから一部を切り出して
GNU Emacs 21 で IME が使えるようにするパッチを作成されました。
この時代、Windows 上で Emacs を使いたい一般的なユーザは
主に Meadow を使用していて、
この IME パッチは実験的な要素が大きかったと思われます。
残念ながら既にサイトが無くなっているため Wayback Machine で拾ってきました。
そのため失われていて確認することができない版がいくつか存在します。
ファイル名は w32-ime.el ではなく前身の meadow-ntemacs.el です。

* [20011023_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20011023_Miyoshi) for Emacs 21.1
* [20020307_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20020307_Miyoshi) for Emacs 21.2.50
* [20030213_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20030213_Miyoshi) for Emacs 21.2
* [20030822_1_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20030822_1_Miyoshi) for Emacs 21.3
* [20030822_2_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20030822_2_Miyoshi) for Emacs 21.3

### KOBAYASHI Yasuhiro 氏

三好氏は Meadow2 （GNU Emacs 21 ベース）の開発に移行して
IME パッチの作成をやめられましたが、
KOBAYASHI Yasuhiro 氏が当時の GNU Emacs 開発版 (CVS HEAD) に対する
IME パッチを作成されています。
この時代の一般的なユーザは Meadow2 や Meadow3 （GNU Emacs 22 ベース）
を使用していて、IME パッチは開発版へのパッチということもあり
実験的なものであったと思われます。
当初は三好氏の IME パッチをベースにしていたようですが、
途中で Meadow3 の修正を取り込んだり、
medow-ntemacs.el から w32-ime.el
への改名（あわせて関数名や変数名なども改名）があったり、
現在の IME パッチへつながる大きな変更があったようです。
残念ながら既にサイトが無くなっているため Wayback Machine で拾ってきました。
そのため失われていて確認することができない版がいくつか存在します。
重要な変更があったのではないかと思われる版も失われています。
また、Meadow の SVN リポジトリも無くなっているので比較することも困難です。
（Meadow のリリースされたソースは Ring Server などで入手可能ですが…。）

* [20040313_Kobayashi](https://github.com/trueroad/w32-ime.el/tree/20040313_Kobayashi) for Emacs CVS HEAD (22.0.xx)
* （すみません、数が多いので途中は省略します。タグは付けてあります。）
* [20070401_Kobayashi-Unicode](https://github.com/trueroad/w32-ime.el/tree/20070401_Kobayashi-Unicode) for Emacs UNICODE-2 (22?)

### NTEmacsJP

Meadow とは別に
[NTEmacsJP](https://ja.osdn.net/projects/ntemacsjp/)
というコミュニティが IME パッチ適用済の
GNU Emacs 22 バイナリの配布を始め、
あわせて使用した IME パッチも配布していました。
コミュニティのメンバには上記の KOBAYASHI Yasuhiro 氏も含まれていたようで、
IME パッチは KOBAYASHI 版をベースにしたものが使われてたようです。
一方、Meadow は Meadow3 （GNU Emacs 22 ベース）を最後に開発が終了したため、
一般的なユーザも NTEmacsJP 版の GNU Emacs
を使うようになっていったと思われます。
NTEmacsJP のコミュニティサイト自体は残っていますが
GNU Emacs 23.1 向けパッチのリリースを最後に活動が停滞したと思われ、
GNU Emacs 23 のバイナリは配布されていません。
22 向けは古い meadow-ntemacs.el が、23 向けは新しい w32-ime.el が入っています。

* 22 向け
    * [20080218_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20080218_NTEmacsJP) for Emacs 22.1.90
    * （省略）
    * [20080327_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20080327_NTEmacsJP) for Emacs 22.2
* 23 向け
    * [20070630_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20070630_NTEmacsJP) for Emacs CVS HEAD (23.0.xx)
    * （省略）
    * [20090804_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20090804_NTEmacsJP) for Emacs 23.1

### gnupack

[gnupack](https://ja.osdn.net/projects/gnupack/)
は、当初 Cygwin のツール群と Windows native の（MinGW の）
Emacs を組み合わせたパッケージとしてスタートし、
途中から Emacs も MinGW から Cygwin へ変更したようです。
Emacs だけ独立したバイナリ配布も行っていたため、
特に MinGW 時代は Emacs だけ使う一般的なユーザも多かったものと思われます。
この MinGW 時代には独自の IME パッチを使っていたようなのですが、
非常に残念なことに配布サイトから Cygwin Emacs 移行前のファイルが
すべて消されてしまっているため確認することができません。
1 つだけ当時の gnupack で使われていたものと同じと思われる IME パッチが
別のサイトにあったため、それを収集しましたが、
どのように変化していったのかを確認することはできません。
Cygwin Emacs への移行後は次の rzl24ozi 氏の IME パッチをそのまま使い、
独自のものではなくなったようです。

* [20130316_gnupack](https://github.com/trueroad/w32-ime.el/tree/20130316_gnupack) for Emacs 24.3

### OOTA 氏

[OOTA 氏によるバイナリ配布](http://hp.vector.co.jp/authors/VA052357/emacs.html)
（ソース同梱）の最大の特徴は、w32-ime.el だけではなく、
それを補完するような smart-ime.el を使うようになっていることです。
本リポジトリではこの smart-ime.el もあわせて収集しました。
OOTA 氏は、いくつかの版を作られたようですが、
残念ながら最後の版しかサイトに残っていないため、
どのように変化していったのかを確認することはできません。

* [20120710_OOTA](https://github.com/trueroad/w32-ime.el/tree/20120710_OOTA) for Emacs 23.4

### rzl24ozi 氏

[rzl24ozi](https://gist.github.com/rzl24ozi)
氏による IME パッチ配布です。
OOTA 版とは異なり smart-ime.el がありません。
rzl24ozi 氏はバイナリ配布を行わず IME パッチのみを配布しました。
そのため、
rzl24ozi 氏の IME パッチを適用したバイナリ配布がいくつか現れ、
一般的なユーザはそうしたバイナリを使用することが多かったと思われます。
また、rzl24ozi 氏は、これまで IME の利用に必要な機能以外も搭載して
肥大化していた IME パッチを整理・分割し、必要な機能のみを搭載した
IME パッチを作成されました。
そして、rzl24ozi 版の最大の特徴は Cygwin Emacs にも対応したことです。
Cygwin Emacs は Linux 環境などでの Emacs と同様、
X の GUI を持ったものと、コンソールで動作するものがありました。
それが 2013 年頃から emacs-w32 という、
Cygwin のバイナリでありながら UI は Windows API を使ったものが登場しました。
rzl24ozi 版は、この emacs-w32 でも IME が使えるようになるパッチとして
使うことができたのです。
rzl24ozi 氏は Git ベースの gist でパッチを公開されたため、
過去にさかのぼってどのような変遷があったのかたどることが容易になっています。

* [20140515_rzl24ozi](https://github.com/trueroad/w32-ime.el/tree/20140515_rzl24ozi) for Emacs 24.3.91
* （省略）
* [20180410_rzl24ozi](https://github.com/trueroad/w32-ime.el/tree/20180410_rzl24ozi) for Emacs 26.1-rc1
* [20180418_rzl24ozi](https://github.com/trueroad/w32-ime.el/tree/20180418_rzl24ozi) for Emacs master

### Koichi Arakawa 氏

[Koichi Arakawa 氏による IME パッチ配布](https://github.com/K-Arakawa/emacs-ime-patch)
です。
OOTA 版と同じく smart-ime.el がありますが、
大きく手が加わっているように見えます。
また、C 実装部分にも smart-ime.el と連携する機能が追加されたようです。
また、Cygwin には非対応（MinGW のみ対応）です。
つまり rzl24ozi 版の系譜ではなくて OOTA 版の系譜ではないかと考えられます。
パッチのみの配布です。
Koichi Arakawa 氏は Git ベースの github でパッチを公開されているため、
過去にさかのぼってどのように変遷があったのかたどることが容易になっています。

* [20161003_Arakawa](https://github.com/trueroad/w32-ime.el/tree/20161003_Arakawa) for Emacs 25.1
* （省略）
* [20200806_Arakawa](https://github.com/trueroad/w32-ime.el/tree/20200806_Arakawa) for Emacs 27.1-rc2

そして、Emacs master (28.0.xx) 向けのパッチもあります。
こちらは C で実装されている Lisp 関数名が変更されています。

* [20200418_Arakawa-master](https://github.com/trueroad/w32-ime.el/tree/20200418_Arakawa-master) for Emacs master (28.0.xx)
* （省略）
* [20200703_Arakawa-master](https://github.com/trueroad/w32-ime.el/tree/20200703_Arakawa-master) for Emacs master (28.0.xx)

### TANE 氏

[TANE 氏による IME パッチ配布](http://tanehp.ec-net.jp/heppoko-lab/prog/zakki/emacs/emacs.html)
です。
smart-ime.el はありません。
Cygwin のみ対応（MinGW 非対応）です。
つまり rzl24ozi 版の系譜と思われますが、
gnupack 版（失われた版）をベースにしたという記述もあります。
C 実装部分も w32-ime.el も、独自に手を入れている部分があります。
残念ながら通常の Web サイトでの公開であり、
過去の版の一部は入手できませんでした。

* [20150419_TANE](https://github.com/trueroad/w32-ime.el/tree/20150419_TANE) for Emacs 24.5
* （省略）
* [20200815_TANE](https://github.com/trueroad/w32-ime.el/tree/20200815_TANE) for Emacs 27.1

## ライセンス

smart-ime.el は OOTA 氏によりソース中に GPL3+ と明示されており、
Koichi Arakawa 氏もそれを引き継いでいます。
一方、meadow-ntemacs.el や w32-ime.el は、
非常に残念なことにソースファイル中でライセンスが明示されておらず、
曖昧な状態であるとも言えます。
ただし、元となった Meadow や、さらにその元となった GNU Emacs は、
2007 年頃まで GPL2+ で、それ以降は GPL3+ であったため、
これに準じて考えればよいと認識しています。
また、GPL2+ は GPL3 やそれ以降として扱ってもよいため、
本サイトでは
[GPL3+](./COPYING)
と表示することにします。
