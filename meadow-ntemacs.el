;;;;; meadow-ntemacs.el ---- Meadow features for NTEmacs.
;;
;;   Author H.Miyashita
;;
;;;;;

(defgroup Meadow-ntemacs nil
  "Meadow-ntemacs"
  :group 'emacs)

(defvar mw32-last-selection nil
  "It is stored the last data from Emacs.")

;----------

(defvar mw32-ime-on-hook nil
  "Functions to eval when IME is turned on at least.\n\
Even if IME state is not changed, these functiona are maybe called.")
(defvar mw32-ime-off-hook nil
  "Functions to eval when IME is turned off at least.\n\
Even if IME state is not changed, these functiona are maybe called.")
(defvar mw32-ime-buffer-switch-p t
  "If this variable is nil, IME control when buffer is switched is disabled.")
(defvar mw32-ime-state nil
  "This shows IME state of the buffer.(buffer local variable)")
(make-variable-buffer-local 'mw32-ime-state)
(defvar mw32-ime-show-mode-line t
  "When t, mode line indicates IME state.")
(defvar mw32-ime-mode-line-state-indicator "[O]"
  "This is shown at the mode line. It is regarded as state of ime.")
(make-variable-buffer-local 'mw32-ime-mode-line-state-indicator)
(defvar mw32-ime-mode-line-state-indicator-list '("-" "[|]" "[O]")
  "List of IME state indicator string.")
(defvar mw32-ime-mode-line-format-original nil
  "Original mode line format.")


;; isearch ime keymap

(setq isearch-ime-keymap (copy-keymap minibuffer-local-map))
(nconc isearch-ime-keymap (list (make-vector 26 'isearch-exit-win32ime)))
(define-key isearch-ime-keymap [compend] 'exit-minibuffer)
(define-key isearch-ime-keymap [kanji] 'isearch-exit-win32ime)
(define-key isearch-ime-keymap "\C-s" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-r" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\177" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-g" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-q" 'isearch-command-win32ime)

(define-key isearch-ime-keymap "\r" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-j" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\t" 'isearch-command-win32ime)
(define-key isearch-ime-keymap " " 'isearch-command-win32ime)

(define-key isearch-ime-keymap "\C-w" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\C-y" 'isearch-command-win32ime)

(let ((meta-map (make-sparse-keymap)))
  (define-key isearch-ime-keymap (char-to-string meta-prefix-char) meta-map)
  (define-key isearch-ime-keymap [escape] meta-map))
(define-key isearch-ime-keymap
  (vector meta-prefix-char t)  'isearch-exit-win32ime)
(define-key isearch-ime-keymap "\M-n" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\M-p" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\M-y" 'isearch-command-win32ime)
(define-key isearch-ime-keymap "\M-\t" 'isearch-command-win32ime)

;;
;; Section: IME
;;

;; This is temporal solution.  In the future, we will prepare
;; dynamic configuration.
(defvar mw32-ime-coding-system-language-environment-alist
  '(("Japanese" . japanese-shift-jis)
    ("Chinese-GB" . chinese-iso-8bit)
    ("Chinese-BIG5" . chinese-big5)
    ("Korean" . korean-iso-8bit)))

;;
;; IME state indicator
;;
(global-set-key [kanji] 'ignore)
(global-set-key [compend] 'ignore)

(defun wrap-function-to-control-ime
  (function interactive-p interactive-arg &optional suffix)
  "Wrap FUNCTION, and IME control is enabled when FUNCTION is called. \n\
An original function is saved to FUNCTION-SUFFIX when suffix is string. \n\
If SUFFIX is nil, \"-original\" is added. "
  (let ((original-function
	 (intern (concat (symbol-name function)
			 (if suffix suffix "-original")))))
    (cond ((not (fboundp original-function))
	   (fset original-function
		 (symbol-function function))
	   (fset function
		 (list
		  'lambda '(&rest arguments)
		  (if interactive-p
		      (list 'interactive interactive-arg))
		  (` (cond ((fep-get-mode)
			    (fep-force-off)
			    (unwind-protect
				(apply '(, original-function) arguments)
			      (if mw32-ime-state
				  (fep-force-on))))
			   (t
			    (apply '(, original-function)
				   arguments))))))))))

(defvar mw32-ime-toroku-region-yomigana nil
  "* if this variable is string, toroku-region regard this value as yomigana.")

(defun mw32-ime-toroku-region (begin end)
  (interactive "r")
  (let ((string (buffer-substring begin end))
	(mw32-ime-buffer-switch-p nil)
	(reading mw32-ime-toroku-region-yomigana))
    (save-excursion
      (set-buffer (window-buffer (minibuffer-window)))
      (fep-force-on nil)
      (mw32-ime-toggle)
      (w32-set-ime-mode 'hiragana)
      (if (not (stringp reading))
	  (setq reading (read-from-minibuffer
			 (format "Input reading of \"%s\":" string))))
      (w32-ime-register-word-dialog reading string))
    (if (not mw32-ime-state) (fep-force-off nil))))

;; for IME management system.

(defun mw32-ime-set-selected-window-buffer-hook (oldbuf newwin newbuf)
  (save-excursion
    (set-buffer newbuf)
    (if mw32-ime-buffer-switch-p
	(if (not (eq (fep-get-mode) mw32-ime-state))
	    (cond
	     (mw32-ime-state
	      (fep-force-on nil)
	      (run-hooks 'mw32-ime-on-hook))
	     (t
	      (if (= (w32-ime-undetermined-string-length) 0)
		  (progn
		    (fep-force-off nil)
		    (run-hooks 'mw32-ime-off-hook)))))))))

(defun mw32-ime-select-window-hook (old new)
  (save-excursion
    (set-buffer (window-buffer new))
    (if mw32-ime-buffer-switch-p
	(if (not (eq (fep-get-mode) mw32-ime-state))
	    (cond
	     (mw32-ime-state
		(fep-force-on nil)
		(run-hooks 'mw32-ime-on-hook))
	     (t
	      (if (= (w32-ime-undetermined-string-length) 0)
		  (progn
		    (fep-force-off nil)
		    (run-hooks 'mw32-ime-off-hook)))))))
    (if (and (eq old (minibuffer-window))
	     (not (eq new (minibuffer-window))))
	(progn
	  (set-buffer (window-buffer (minibuffer-window)))
;	(fep-force-off)
	  (setq mw32-ime-state
		nil
;		mode-line-win32ime-mode-in-minibuffer
;		transparent-mode-indicator
;		minibuffer-preprompt 
;		nil
		))))
;  (if (eq new (minibuffer-window))
;      (setq minibuffer-window-selected t)
;    (setq minibuffer-window-selected nil))
  )

(defun mw32-ime-mode-line-update ()
  (cond (mw32-ime-show-mode-line
	 (if (window-minibuffer-p (selected-window))
;;; for minibuffer ...
	     nil
	   (setq mw32-ime-mode-line-state-indicator
		 (if mw32-ime-state
		     (nth 1 mw32-ime-mode-line-state-indicator-list)
		   (nth 2 mw32-ime-mode-line-state-indicator-list)))))
	(t
	 (setq mw32-ime-mode-line-state-indicator
	       (nth 0 mw32-ime-mode-line-state-indicator-list))))
  (force-mode-line-update))

(defun mw32-ime-init-mode-line-display ()
  (if (not (member 'mw32-ime-mode-line-state-indicator
		   mode-line-format))
      (progn
	(setq mw32-ime-mode-line-format-original
	      (default-value 'mode-line-format))
	(if (and (stringp (car mode-line-format))
		 (string= (car mode-line-format) "-"))
	    (setq-default mode-line-format
			  (cons ""
				(cons 'mw32-ime-mode-line-state-indicator
				      (cdr mode-line-format))))
	  (setq-default mode-line-format
			(cons ""
			      (cons 'mw32-ime-mode-line-state-indicator
				    mode-line-format))))
	(force-mode-line-update t))))

(defun mw32-ime-toggle ()
  (interactive)
  (let ((ime-state (fep-get-mode)))
    (if ime-state
	(run-hooks 'mw32-ime-on-hook)
      (run-hooks 'mw32-ime-off-hook))
    (if (not (eq ime-state mw32-ime-state))
	(progn
	  (setq mw32-ime-state ime-state)
	  (mw32-ime-mode-line-update)))))

(defun mw32-ime-initialize ()
  (cond ((and (eq system-type 'windows-nt)
	      (eq window-system 'w32)
	      (featurep 'meadow-ntemacs))
	 (let ((coding-system
		(assoc current-language-environment
		       mw32-ime-coding-system-language-environment-alist)))
	   (mw32-ime-init-mode-line-display)
	   (mw32-ime-mode-line-update)
	   (add-hook 'select-window-functions
		     'mw32-ime-select-window-hook)
	   (add-hook 'set-selected-window-buffer-functions
		     'mw32-ime-set-selected-window-buffer-hook)
	   (define-key global-map [kanji] 'mw32-ime-toggle)
	   (define-key global-map [C-kanji] 'mw32-ime-toggle) ; remove me!
	   (define-key global-map [M-kanji] 'mw32-ime-toggle) ; remove me!
	   (if coding-system
	       (set-keyboard-coding-system (cdr coding-system)))))))

(defun mw32-ime-uninitialize ()
  (cond ((and (eq system-type 'windows-nt)
	      (eq window-system 'w32)
	      (featurep 'meadow-ntemacs))
	 (setq-default mode-line-format
		       mw32-ime-mode-line-format-original)
	 (force-mode-line-update t)
	 (remove-hook 'select-window-functions
		      'mw32-ime-select-window-hook)
	 (remove-hook 'set-selected-window-buffer-functions
		      'mw32-ime-set-selected-window-buffer-hook)
	 (define-key global-map [kanji] 'ignore))))

(defun mw32-ime-state-switch (&optional arg)
  (if arg
      (progn
	(setq inactivate-current-input-method-function
	      'mw32-ime-state-switch)
	(run-hooks 'input-method-activate-hook)
	(setq describe-current-input-method-function nil)
	(fep-force-on t))
    (setq current-input-method nil)
    (run-hooks 'input-method-inactivate-hook)
    (setq describe-current-input-method-function nil)
    (fep-force-off t)))

(register-input-method "MW32-IME" "Japanese" 'mw32-ime-state-switch ""
		       "MW32 System IME")

(provide 'meadow-ntemacs)
