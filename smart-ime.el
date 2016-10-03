;;; smart-ime.el - smart Input Method
;;
;; Copyright (C) 2011 HIROSHI OOTA

;; Author: HIROSHI OOTA
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; smart-im frees you from the frustration about IM.
;; It disables an IM automatically according to a situation.

;;; Install:
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'smart-ime)

;; Todo:

;;; Code:

;; variable
(defvar sime-saved-input-method nil)
(make-variable-buffer-local 'sime-saved-input-method)
(set-default 'sime-saved-input-method nil)

(defun sime-activate-input-method ()
  (setq sime-saved-input-method (list current-input-method))
  (deactivate-input-method))
(defun sime-inactivate-input-method ()
  (when sime-saved-input-method
    (activate-input-method (car sime-saved-input-method))
    (setq sime-saved-input-method nil)))

;;
;; minibuffer
(add-hook 'minibuffer-setup-hook 'sime-activate-input-method)
(add-hook 'minibuffer-exit-hook 'sime-inactivate-input-method)
;;
;; incremental search
(add-hook 'isearch-mode-hook 'sime-activate-input-method)
(add-hook 'isearch-mode-end-hook 'sime-inactivate-input-method)

;;
;; query-replace, query-replace-regexp, query-replace-regexp-eval ...
(defadvice perform-replace
  (around sime-ad-perform-replace compile activate)
  (let ((im current-input-method))
    (if (or (not im)
	    (not (ad-get-arg 2)))
	ad-do-it
      (deactivate-input-method)
      ad-do-it
      (activate-input-method im))))

;;
;; read-passwd
(defadvice read-passwd
  (around sime-ad-read-passwd compile activate)
  (let ((sime-ad-toggle-ime-mapped-key-list nil)
	(im current-input-method))
    (dolist (k (where-is-internal
                'toggle-input-method overriding-local-map nil nil
                (command-remapping 'toggle-input-method)))
      (when (integerp (aref k 0))
	(setq sime-ad-toggle-ime-mapped-key-list
	      (cons (aref k 0) sime-ad-toggle-ime-mapped-key-list))))

    (ad-enable-advice 'read-key 'around 'sime-ad-read-key)
    (ad-activate 'read-key)
    (deactivate-input-method)
    ad-do-it
    (activate-input-method im)
    (ad-disable-advice 'read-key 'around 'sime-ad-read-key)
    (ad-activate 'read-key)))

(defadvice read-key
  (around sime-ad-read-key compile)
  (let ((flag t)
	(msg (current-message)))
    (while flag
      ad-do-it
      (if (not (memq ad-return-value
                     sime-ad-toggle-ime-mapped-key-list))
	  (setq flag nil)
	(toggle-input-method)
	(message msg)))))

;;
;; universal-argument
(defadvice universal-argument
    (before sime-ad-universal-argument compile activate)
  (setq sime-saved-input-method
        (or sime-saved-input-method
            (list current-input-method)))
  (deactivate-input-method))

(defun universal-argument--mode ()
  (setq sime-saved-input-method
	(or sime-saved-input-method
	    (list current-input-method)))
  (deactivate-input-method)
  (prefix-command-update)
  (set-transient-map universal-argument-map #'sime-inactivate-input-method))

;; a keyboard which has no KANJI-KEY
;; entering/leaving KANJI-mode key-sequence is <kanji><M-kanji>
;; then we should pass prefix-arg to next command.
(global-set-key
  [M-kanji]
  #'(lambda (arg)
      (interactive "P")
      (setq prefix-arg arg)))

;;
;; others
(defmacro wrap-function-to-control-input-method (fcn)
  `(defadvice ,fcn (around
                    ,(make-symbol (concat "sime-ad-" (symbol-name fcn)))
                    compile activate)
     (let ((im current-input-method))
       (deactivate-input-method)
       ad-do-it
       (activate-input-method im))))

(wrap-function-to-control-input-method map-y-or-n-p)
(wrap-function-to-control-input-method y-or-n-p)
(wrap-function-to-control-input-method read-key-sequence)

;; disable W32-IME control during processing the timer handler
(defadvice timer-event-handler
  (around sime-ad-timer-event-handler compile activate)
  (let ((w32-ime-buffer-switch-p nil))
    ad-do-it))

(provide 'smart-ime)
;; -*- mode: Emacs-Lisp; coding: euc-jp-unix -*-
