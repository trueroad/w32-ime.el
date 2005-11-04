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
  "Functions to eval when IME is turned on at least.
Even if IME state is not changed, these functiona are maybe called.")
(defvar mw32-ime-off-hook nil
  "Functions to eval when IME is turned off at least.
Even if IME state is not changed, these functiona are maybe called.")
(defvar mw32-ime-buffer-switch-p t
  "If this variable is nil, IME control when buffer is switched is disabled.")
(defvar mw32-ime-show-mode-line t
  "When t, mode line indicates IME state.")
(defvar mw32-ime-mode-line-state-indicator "[O]"
  "This is shown at the mode line. It is regarded as state of ime.")
(make-variable-buffer-local 'mw32-ime-mode-line-state-indicator)
(put 'mw32-ime-mode-line-state-indicator 'permanent-local t)
(defvar mw32-ime-mode-line-state-indicator-list '("-" "[|]" "[O]")
  "List of IME state indicator string.")
(defvar mw32-ime-mode-line-format-original nil
  "Original mode line format.")

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
  "Wrap FUNCTION, and IME control is enabled when FUNCTION is called.
An original function is saved to FUNCTION-SUFFIX when suffix is string.
If SUFFIX is nil, \"-original\" is added. "
  (let ((original-function
	 (intern (concat (symbol-name function)
			 (if suffix suffix "-original")))))
    (cond
     ((not (fboundp original-function))
      (fset original-function
	    (symbol-function function))
      (fset function
	    (list
	     'lambda '(&rest arguments)
	     (when interactive-p
	       (list 'interactive interactive-arg))
	     (`(cond
		((and (fep-get-mode)
		      (equal current-input-method "MW32-IME"))
 		 (fep-force-off)
		 (unwind-protect
		     (apply '(, original-function) arguments)
		   (when (and (not (fep-get-mode))
			      (equal current-input-method "MW32-IME"))
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
    (unless (stringp reading)
      (w32-set-ime-mode 'hiragana)
      (setq reading
	    (read-multilingual-string
            (format "Input reading of \"%s\":" string) nil "MW32-IME")))
    (w32-ime-register-word-dialog reading string)))

;; for IME management system.

(defun mw32-ime-sync-state (window)
  (when mw32-ime-buffer-switch-p
    (with-current-buffer (window-buffer window)
      (let* ((frame (window-frame window))
	     (ime-state (fep-get-mode)))
	(cond
	 ((and (not ime-state)
	       (equal current-input-method "MW32-IME"))
	  (fep-force-on nil)
	  (run-hooks 'mw32-ime-on-hook))
	 ((and ime-state
	       (not (equal current-input-method "MW32-IME")))
	  (when (= (w32-ime-undetermined-string-length) 0)
	    (fep-force-off nil)
	    (run-hooks 'mw32-ime-off-hook))))))))

(defun mw32-ime-set-selected-window-buffer-hook (oldbuf newwin newbuf)
  (mw32-ime-sync-state newwin))

(defun mw32-ime-select-window-hook (old new)
  (mw32-ime-sync-state new))

(defun mw32-ime-mode-line-update ()
  (cond
   (mw32-ime-show-mode-line
    (unless (window-minibuffer-p (selected-window))
      (setq mw32-ime-mode-line-state-indicator
	    (nth (if (fep-get-mode) 1 2)
		 mw32-ime-mode-line-state-indicator-list))))
   (t
    (setq mw32-ime-mode-line-state-indicator
	  (nth 0 mw32-ime-mode-line-state-indicator-list))))
  (force-mode-line-update))

(defun mw32-ime-init-mode-line-display ()
  (unless (member 'mw32-ime-mode-line-state-indicator mode-line-format)
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
    (force-mode-line-update t)))

(defun mw32-ime-toggle ()
  (interactive)
  (if (equal current-input-method "MW32-IME")
      (inactivate-input-method)
    (activate-input-method "MW32-IME")))

(defun mw32-ime-initialize ()
  (cond
   ((and (eq system-type 'windows-nt)
	 (eq window-system 'w32)
	 (featurep 'meadow-ntemacs))
    (let ((coding-system
	   (assoc-string current-language-environment
			 mw32-ime-coding-system-language-environment-alist
			 t)))
      (mw32-ime-init-mode-line-display)
      (mw32-ime-mode-line-update)
      (add-hook 'select-window-functions
		'mw32-ime-select-window-hook)
      (add-hook 'set-selected-window-buffer-functions
		'mw32-ime-set-selected-window-buffer-hook)
      (define-key global-map [kanji] 'mw32-ime-toggle)
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

(defun mw32-ime-exit-from-minibuffer ()
  (inactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'mw32-ime-exit-from-minibuffer)))

(defun mw32-ime-state-switch (&optional arg)
  (if arg
      (progn
	(setq inactivate-current-input-method-function
	      'mw32-ime-state-switch)
	(run-hooks 'input-method-activate-hook)
	(run-hooks 'mw32-ime-on-hook)
	(setq describe-current-input-method-function nil)
	(when (eq (selected-window) (minibuffer-window))
	  (add-hook 'minibuffer-exit-hook 'mw32-ime-exit-from-minibuffer))
	(fep-force-on))
    (setq current-input-method nil)
    (run-hooks 'input-method-inactivate-hook)
    (run-hooks 'mw32-ime-off-hook)
    (setq describe-current-input-method-function nil)
    (fep-force-off))
  (mw32-ime-mode-line-update))

(register-input-method "MW32-IME" "Japanese" 'mw32-ime-state-switch ""
		       "MW32 System IME")

(provide 'meadow-ntemacs)
