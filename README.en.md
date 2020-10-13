[![MELPA](https://melpa.org/packages/w32-ime-badge.svg)](https://melpa.org/#/w32-ime)

[ [Japanese](./README.md) / English ]

# w32-ime.el --- Windows IME UI/UX controler

w32-ime.el, formerly known as "Meadow features for NTEmacs", was part of
an IME patch for Emacs for Windows that allowed to input Japanese using
the Windows IME (Input Method Editor).  On Emacs 26.2 or later, Japanese
input using the IME is now possible even without the IME patch.  However,
some of the features of the IME patch are not implemented in current
Emacs, so it is possible to type Japanese on Emacs without the IME patch,
but it is not convenient.

w32-ime.el managed the upper layers of the IME patches and provided the
convenient features, but it is not contained in current Emacs.  It has
functions such as displaying the on/off state of the IME in the mode line
as UI/UX features.  It also has hooks to call when the IME state is
changed.  With these hooks, you can change the color and shape of the
cursor depending on the IME ON/OFF status to be visually known to the
IME state.

To use w32-ime.el, add the following code to your init.el or .emacs

```el
(setq default-input-method "W32-IME")
(w32-ime-initialize)
```

## History

### H.Miyashita

He wrote `meadow.el`, the predecessor to `w32-ime.el`.

According to `ChangeLog.Meadow` in
[Meadow-1.15 source
](http://www.ring.gr.jp/archives/pc/meadow/1.15/Meadow-1.15-1-src.tar.gz)
, it's around 1999.

### MIYOSHI Masanori

* [20011023_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20011023_Miyoshi) for Emacs 21.1
* [20020307_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20020307_Miyoshi) for Emacs 21.2.50
* [20030213_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20030213_Miyoshi) for Emacs 21.2
* [20030822_1_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20030822_1_Miyoshi) for Emacs 21.3
* [20030822_2_Miyoshi](https://github.com/trueroad/w32-ime.el/tree/20030822_2_Miyoshi) for Emacs 21.3

### KOBAYASHI Yasuhiro

* [20040313_Kobayashi](https://github.com/trueroad/w32-ime.el/tree/20040313_Kobayashi) for Emacs CVS HEAD (22.0.xx)
* ...
* [20070401_Kobayashi-Unicode](https://github.com/trueroad/w32-ime.el/tree/20070401_Kobayashi-Unicode) for Emacs UNICODE-2 (22?)

### NTEmacsJP

* For Emacs 22
    * [20080218_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20080218_NTEmacsJP) for Emacs 22.1.90
    * ...
    * [20080327_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20080327_NTEmacsJP) for Emacs 22.2
* For Emacs 23
    * [20070630_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20070630_NTEmacsJP) for Emacs CVS HEAD (23.0.xx)
    * ...
    * [20090804_NTEmacsJP](https://github.com/trueroad/w32-ime.el/tree/20090804_NTEmacsJP) for Emacs 23.1

### gnupack

* [20130316_gnupack](https://github.com/trueroad/w32-ime.el/tree/20130316_gnupack) for Emacs 24.3

### OOTA

* [20120710_OOTA](https://github.com/trueroad/w32-ime.el/tree/20120710_OOTA) for Emacs 23.4

### rzl24ozi

* [20140515_rzl24ozi](https://github.com/trueroad/w32-ime.el/tree/20140515_rzl24ozi) for Emacs 24.3.91
* ...
* [20180410_rzl24ozi](https://github.com/trueroad/w32-ime.el/tree/20180410_rzl24ozi) for Emacs 26.1-rc1
* [20180418_rzl24ozi](https://github.com/trueroad/w32-ime.el/tree/20180418_rzl24ozi) for Emacs master

### Koichi Arakawa

* [20161003_Arakawa](https://github.com/trueroad/w32-ime.el/tree/20161003_Arakawa) for Emacs 25.1
* ...
* [20200806_Arakawa](https://github.com/trueroad/w32-ime.el/tree/20200806_Arakawa) for Emacs 27.1-rc2

* [20200418_Arakawa-master](https://github.com/trueroad/w32-ime.el/tree/20200418_Arakawa-master) for Emacs master (28.0.xx)
* ...
* [20200703_Arakawa-master](https://github.com/trueroad/w32-ime.el/tree/20200703_Arakawa-master) for Emacs master (28.0.xx)

### TANE

* [20150419_TANE](https://github.com/trueroad/w32-ime.el/tree/20150419_TANE) for Emacs 24.5
* ...
* [20200815_TANE](https://github.com/trueroad/w32-ime.el/tree/20200815_TANE) for Emacs 27.1
* [20200901_TANE](https://github.com/trueroad/w32-ime.el/tree/20200901_TANE) for Emacs 27.1

### Masamichi Hosoda

* [Fix](https://github.com/trueroad/w32-ime.el/tree/20200824_Hosoda)
* [Fix](https://github.com/trueroad/w32-ime.el/tree/20200826_Hosoda)
* [Fix](https://github.com/trueroad/w32-ime.el/tree/20200829_Hosoda)

### MELPA

w32-ime.el is
[added to MELPA](https://melpa.org/#/w32-ime) on Oct. 2020.
We have made various modifications to that.

* [Modifications to comply with coding conventions.
](https://github.com/trueroad/w32-ime.el/commit/4265355ad0ac07c8723a0db8276b5c9340c6f2b0)
* [Rewrite w32-ime-wrap-function-to-control-ime using advice-add
](https://github.com/trueroad/w32-ime.el/pull/2)
* [Refactoring
](https://github.com/trueroad/w32-ime.el/pull/1)
* etc.

## License

[GPL3+](./COPYING)
