;;; w32-ime.el --- Windows IME UI/UX controler

;; Copyright (C) 1999      H.Miyashita
;; Copyright (C) 2001-2003 MIYOSHI Masanori
;; Copyright (C) 2004-2007 KOBAYASHI Yasuhiro
;; Copyright (C) 2007-2009 KOBAYASHI Yasuhiro (NTEmacsJP)
;; Copyright (C) 2013      ksugita (gnupack)
;; Copyright (C) 2014      rzl24ozi
;; Copyright (C) 2015-2020 TANE
;; Copyright (C) 2020      Masamichi Hosoda

;; Author:           H.Miyashita
;;                   MIYOSHI Masanori
;;                   KOBAYASHI Yasuhiro
;;                   NTEmacsJP
;;                   ksugita (gnupack)
;;                   rzl24ozi
;;                   TANE
;;                   Masamichi Hosoda <trueroad@trueroad.jp>
;; Maintainer:       Masamichi Hosoda <trueroad@trueroad.jp>
;; URL:              https://github.com/trueroad/w32-ime.el
;; Version:          20200929
;; Package-Requires: ((emacs "24.3"))

;; w32-ime.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; w32-ime.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tr-emacs-ime-module.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; w32-ime.el, formerly known as "Meadow features for NTEmacs", was part of
;; an IME patch for Emacs for Windows that allowed to input Japanese using
;; the Windows IME (Input Method Editor).  On Emacs 26.2 or later, Japanese
;; input using the IME is now possible even without the IME patch.  However,
;; some of the features of the IME patch are not implemented in current
;; Emacs, so it is possible to type Japanese on Emacs without the IME patch,
;; but it is not convenient.
;;
;; w32-ime.el managed the upper layers of the IME patches and provided the
;; convenient features, but it is not contained in current Emacs.  It has
;; functions such as displaying the on/off state of the IME in the mode line
;; as UI/UX features.  It also has hooks to call when the IME state is
;; changed.  With these hooks, you can change the color and shape of the
;; cursor depending on the IME ON/OFF status to be visually known to the
;; IME state.

;; To use w32-ime.el, add the following code to your init.el or .emacs
;;
;;   (setq default-input-method "W32-IME")
;;   (w32-ime-initialize)

;;; Code:

(defgroup W32-IME nil
  "w32-ime"
  :group 'emacs)

(define-obsolete-variable-alias 'w32-last-selection
  'w32-ime-last-selection "2020")

(defvar w32-ime-last-selection nil
  "It is stored the last data from Emacs.")

;; ----------

(defvar w32-ime-on-hook nil
  "Functions to eval when IME is turned on at least.
Even if IME state is not changed, these functiona are maybe called.")
(defvar w32-ime-off-hook nil
  "Functions to eval when IME is turned off at least.
Even if IME state is not changed, these functiona are maybe called.")
(defvar w32-ime-buffer-switch-p t
  "If this variable is nil, IME control when buffer is switched is disabled.")
(defvar w32-ime-show-mode-line t
  "When t, mode line indicates IME state.")
(defvar w32-ime-mode-line-state-indicator "[O]"
  "This is shown at the mode line.  It is regarded as state of ime.")
(make-variable-buffer-local 'w32-ime-mode-line-state-indicator)
(put 'w32-ime-mode-line-state-indicator 'permanent-local t)
(defvar w32-ime-mode-line-state-indicator-list '("-" "[|]" "[O]")
  "List of IME state indicator string.")
(defvar w32-ime-mode-line-format-original nil
  "Original mode line format.")
(defvar w32-ime-input-method-title nil
  "String denoting W32-IME input method.")

;;
;; Section: IME
;;

;; ;; This is temporal solution.  In the future, we will prepare
;; ;; dynamic configuration.
;; (defvar w32-ime-coding-system-language-environment-alist
;;   '(("Japanese" . japanese-shift-jis)
;;     ("Chinese-GB" . chinese-iso-8bit)
;;     ("Chinese-BIG5" . chinese-big5)
;;     ("Korean" . korean-iso-8bit)))

;;
;; IME state indicator
;;
(global-set-key [kanji] 'ignore)
(global-set-key [compend] 'ignore)

(define-obsolete-function-alias 'wrap-function-to-control-ime
  #'w32-ime-wrap-function-to-control-ime "2020")

(defun w32-ime-wrap-function-to-control-ime
  (fn &optional interactive-p interactive-arg suffix)
  "Wrap FN, and IME control is enabled when FUNCTION is called.
If INTERACTIVE-P is non-nil, FUNCTION is handled as interactive and uses
INTERACTIVE-ARG as its arguments.
An original function is saved to FUNCTION-SUFFIX when suffix is string.
If SUFFIX is nil, \"-original\" is added."
  (let ((original-function
	 (intern (concat (symbol-name fn) (or suffix "-original")))))
    (cond
     ((not (fboundp original-function))
      (fset original-function (symbol-function fn))
      (fset fn
	    (list
	     'lambda '(&rest arguments)
	     (when interactive-p
	       (list 'interactive interactive-arg))
	     `(cond
		((and (ime-get-mode)
		      (equal current-input-method "W32-IME"))
 		 (ime-force-off)
		 (unwind-protect
		     (apply ',original-function arguments)
		   (when (and (not (ime-get-mode))
			      (equal current-input-method "W32-IME"))
		     (ime-force-on))))
		(t
		 (apply ',original-function arguments)))))))))

(defvar w32-ime-toroku-region-yomigana nil
  "* if this variable is string, toroku-region regard this value as yomigana.")

(defun w32-ime-toroku-region (begin end)
  "Register words between BEGIN and END to the IME dictionary."
  (interactive "r")
  (let ((string (buffer-substring begin end))
	(w32-ime-buffer-switch-p nil)
	(reading w32-ime-toroku-region-yomigana))
    (unless (stringp reading)
      (w32-set-ime-mode 'hiragana)
      (setq reading
	    (read-multilingual-string
            (format "Input reading of \"%s\": " string) nil "W32-IME")))
    (w32-ime-register-word-dialog reading string)))

;; for IME management system.

(defun w32-ime-sync-state (window)
  "Sync IME and IM buffer state on WINDOW."
  (if w32-ime-buffer-switch-p
      (with-current-buffer (window-buffer window)
	(let* ((frame (window-frame window))
	       (ime-state (ime-get-mode)))
	  (cond
	   ((and (not ime-state)
		 (equal current-input-method "W32-IME"))
	    (ime-force-on nil)
	    (run-hooks 'w32-ime-on-hook))
	   ((and ime-state
		 (not (equal current-input-method "W32-IME")))
	    (ime-force-off nil)
	    (run-hooks 'w32-ime-off-hook)))))
    (let ((ime-state (ime-get-mode)))
      (dolist (frame (frame-list))
	(dolist (win (window-list frame))
	  (with-current-buffer (window-buffer win)
	    (cond
	     ((and (not ime-state)
		   current-input-method)
	      (deactivate-input-method))
	     ((and ime-state
		   (not current-input-method))
	      (activate-input-method "W32-IME")))))))))

(defun w32-ime-set-selected-window-buffer-hook (oldbuf newwin newbuf)
  "A function called by the IME patch's abnormal hook.
It is called when the buffer of the selected window is set.
OLDBUF is the previous buffer.  NEWWIN is the newly selected window.
NEWBUF is the newly set buffer."
  (w32-ime-sync-state newwin))

(defun w32-ime-select-window-hook (old new)
  "A function called by the IME patch's abnormal hook.
It is called when the window is selected.
OLD is the previous window.  NEW is the newly selected window."
  (w32-ime-sync-state new))

(defun w32-ime-mode-line-update ()
  "Update IME mode line."
  (if (fboundp 'ime-get-mode)
      (progn
	(cond
	 (w32-ime-show-mode-line
	  (if (or
	       (not w32-ime-buffer-switch-p)
	       (and w32-ime-buffer-switch-p
		    (not (window-minibuffer-p (selected-window)))))
	      (setq w32-ime-mode-line-state-indicator
		    (nth (if (ime-get-mode) 1 2)
			 w32-ime-mode-line-state-indicator-list))))
	 (t
	  (setq w32-ime-mode-line-state-indicator
		(nth 0 w32-ime-mode-line-state-indicator-list))))
	(force-mode-line-update))))

(defun w32-ime-init-mode-line-display ()
  "Initialize IME mode line."
  (unless (member 'w32-ime-mode-line-state-indicator mode-line-format)
    (setq w32-ime-mode-line-format-original
	  (default-value 'mode-line-format))
    (if (and (stringp (car mode-line-format))
	     (string= (car mode-line-format) "-"))
	(setq-default mode-line-format
		      (cons ""
			    (cons 'w32-ime-mode-line-state-indicator
				  (cdr mode-line-format))))
      (setq-default mode-line-format
		    (cons ""
			  (cons 'w32-ime-mode-line-state-indicator
				mode-line-format))))
    (force-mode-line-update t)))

;;;###autoload
(defun w32-ime-initialize ()
  "Initialize w32-ime.el."
   (when (and (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	      (eq window-system 'w32)
	      (fboundp 'ime-get-mode))
     (w32-ime-init-mode-line-display)
     (w32-ime-mode-line-update)
     (add-hook 'select-window-functions
	       'w32-ime-select-window-hook)
     (add-hook 'set-selected-window-buffer-functions
	       'w32-ime-set-selected-window-buffer-hook)
     (define-key global-map [kanji] 'toggle-input-method)))

(defun w32-ime-uninitialize ()
  "Uninitialize w32-ime.el."
  (when (and (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	     (eq window-system 'w32)
	     (fboundp 'ime-get-mode))
    (setq-default mode-line-format
		  w32-ime-mode-line-format-original)
    (force-mode-line-update t)
    (remove-hook 'select-window-functions
		 'w32-ime-select-window-hook)
    (remove-hook 'set-selected-window-buffer-functions
		 'w32-ime-set-selected-window-buffer-hook)
    (define-key global-map [kanji] 'ignore)))

(defun w32-ime-exit-from-minibuffer ()
  "A function called from \"minibuffer-exit-hook\"."
  (when w32-ime-buffer-switch-p
    (deactivate-input-method))
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'w32-ime-exit-from-minibuffer)))

(defun w32-ime-state-switch (&optional arg)
  "Switch IME state.
If ARG is omitted or nil, turn off the IME state.
Otherwise, turn off the IME state."
  (if arg
      (progn
	(setq deactivate-current-input-method-function
	      'w32-ime-state-switch)
	(run-hooks 'input-method-activate-hook)
	(run-hooks 'w32-ime-on-hook)
	(setq describe-current-input-method-function nil)
	(when (eq (selected-window) (minibuffer-window))
	  (add-hook 'minibuffer-exit-hook 'w32-ime-exit-from-minibuffer))
	(ime-force-on)
        (setq current-input-method-title w32-ime-input-method-title))
    (setq current-input-method nil)
    (run-hooks 'input-method-deactivate-hook)
    (run-hooks 'w32-ime-off-hook)
    (setq describe-current-input-method-function nil)
    (ime-force-off)
    (setq current-input-method-title nil))
  (w32-ime-mode-line-update))

(register-input-method "W32-IME" "Japanese" 'w32-ime-state-switch ""
		       "W32 System IME")

;;
;; Windows only functions
;;

(declare-function ime-get-mode "w32fns.c")
(declare-function ime-force-on "w32fns.c")
(declare-function ime-force-off "w32fns.c")
(declare-function w32-set-ime-mode "w32fns.c")
(declare-function w32-ime-register-word-dialog "w32fns.c")

;;
;; provide
;;

(provide 'w32-ime)

;;; w32-ime.el ends here
