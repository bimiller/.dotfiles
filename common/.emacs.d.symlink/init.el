;;============================================================================
;; init.el -- emacs customizations
;;============================================================================

(require 'package)
(add-to-list 'package-archives  '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;----------------------------------------------------------------------------
;; Switch Command and Option keys on Macs.
;;----------------------------------------------------------------------------

(when (equal window-system 'ns)
  (setq  ns-command-modifier 'meta ; use Command as Meta
         ns-option-modifier 'super ; use Option as Command
         ns-right-command-modifier 'left ; right and left Command are the same
         ns-right-option-modifier 'left ; right and left Opt are the same
         ns-function-modifier 'hyper)
)

;;----------------------------------------------------------------------------
;; Enable Vim emulation
;;----------------------------------------------------------------------------

;; (setq evil-want-integration nil) ; required by evil-collection
;; (setq evil-want-keybinding nil)

;; (require 'evil)
;; (evil-mode t)
;; (setq evil-want-minibuffer t)		;; use VIM bindings in minibuffer

;; (when (require 'evil-collection nil t)
;;  (evil-collection-init)
;; ;; (require 'syndicate)
;;  )

;;----------------------------------------------------------------------------
;; Essential key bindings & editing behaviors
;;----------------------------------------------------------------------------

(setq mark-even-if-inactive nil	; only use mark when region is visible
      transient-mark-mode 1   ; region is not always active
      sentence-end-double-space nil
      )

(setq-default indent-tabs-mode nil)	; use SPCs instead of TABs

(delete-selection-mode 1) ; typing replaces the actively selected text

;; Use ^H to backspace (F1 for help).

;;(global-unset-key (kbd "C-h"))		; break the habit first
;;(define-key key-translation-map [?\C-h] [?\C-?])
;;(global-set-key (kbd "<f1>") 'help-command)

;; Make C-d erase active region.

(global-set-key (kbd "C-d") 'delete-forward-char)

;; Use ^W to kill previous word when no selection is active.

;; (defun my:kill-region-or-backward-word ()
;;   (interactive)
;;   (if (region-active-p)
;;       (kill-region (region-beginning) (region-end))
;;     (backward-kill-word 1)))
;; (global-set-key (kbd "C-w") 'my:kill-region-or-backward-word)

;; Use M-w to copy-line when no selection is active.

;; (defun my:copy-line (arg)
;;   "Copy to end of line, or as many lines as prefix argument"
;;   (interactive "P")
;;   (if (null arg)
;;       (progn (kill-ring-save (point)
;; 			     (line-end-position))
;; 	     (message "Copied to end of line"))
;;     (let ((cnt (prefix-numeric-value arg)))
;;       (kill-ring-save (line-beginning-position)
;; 		      (line-beginning-position (+ 1 arg)))
;;       (message "%d line%s copied" arg (if (= 1 arg) "" "s")))))

;; (defun my:save-region-or-current-line (arg)
;;   (interactive "P")
;;   (if (region-active-p)
;;       (kill-ring-save (region-beginning) (region-end))
;;     (my:copy-line arg)))
;; (global-set-key (kbd "M-w") 'my:save-region-or-current-line)

;; Swap zap-to-char for more generally useful zap-up-to-char.

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;;----------------------------------------------------------------------------
;; Environment settings
;;----------------------------------------------------------------------------

;; Use UTF-8 by default for new files, file detection, etc.

(prefer-coding-system 'utf-8)

;; Backup and other temporary files

(setq delete-by-moving-to-trash t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      tramp-auto-save-directory "~/.emacs.d/tramp-auto-save"
      )

;; Windowing system defaults.

(cond ((string-equal window-system "x")
       (set-face-attribute 'default nil :family "Source Code Pro" :height 120)
       )
      ((string-equal window-system "ns")
       (set-face-attribute 'default nil :family "Monaco" :height 140)
       (global-set-key (kbd "M-`") 'other-frame)
       )
      )

;; Preferred frame size.

(if (display-graphic-p)
    (setq default-frame-alist
	  '((top . 0.0) (left . 0.5) (width . 0.8) (height . 0.8)))
  )

;;----------------------------------------------------------------------------
;; Preferred look-and-feel
;;----------------------------------------------------------------------------

;; Color scheme

(load-theme 'zenburn 'no-confirm)
(set-face-attribute 'region nil :background "#666") ; make region easier to see

;; Window adornments

(when (not (equal window-system 'ns))
  (menu-bar-mode -1))			; hide menubar (except on MacOS)

(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))			; hide toolbar

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)	; vertical scroll bars only
  (setq vertical-scroll-bar t))

;; Miscellaneous features and options

(show-paren-mode t)			; highlight matching parens (etc)

(setq apropos-do-all t
      blink-cursor-blinks 3
      column-number-mode t
      inhibit-startup-screen t
      initial-scratch-message nil
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      mouse-yank-at-point t
      read-buffer-completion-ignore-case t
      ring-bell-function (quote ignore)
      save-interprogram-paste-before-kill t
      scroll-conservatively 1000
      scroll-error-top-bottom t
      split-width-threshold 240 ; minimum columns for horizontal splitting
      view-read-only t
      visible-bell t
      )

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;----------------------------------------------------------------------------
;; Protect against hangs when viewing large files.
;;----------------------------------------------------------------------------

(defun my:find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only and use fundamental mode."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'my:find-file-check-make-large-file-read-only-hook)

;;----------------------------------------------------------------------------
;; Improved navigation
;;----------------------------------------------------------------------------

(require 'mwim)

(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

;;----------------------------------------------------------------------------
;; Operate on current line if no region is active.
;;----------------------------------------------------------------------------

(require 'whole-line-or-region)

(add-to-list 'whole-line-or-region-extensions-alist
             '(comment-dwim whole-line-or-region-comment-dwim nil))
(whole-line-or-region-mode 1)

;;----------------------------------------------------------------------------
;; Expand/contract region selection based on "semantic" units
;;----------------------------------------------------------------------------

(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)

;; (require 'change-inner)

;; (global-set-key (kbd "M-i") 'change-inner)
;; (global-set-key (kbd "M-o") 'change-outer)

;;----------------------------------------------------------------------------
;; Display help for key sequences.
;;----------------------------------------------------------------------------

(require 'which-key)
(which-key-mode)

;;----------------------------------------------------------------------------
;; Use portion of directory path to make buffer names unique.
;;----------------------------------------------------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;----------------------------------------------------------------------------
;; Save place in files when closing Emacs.
;;----------------------------------------------------------------------------

;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (concat user-emacs-directory "places"))

;;----------------------------------------------------------------------------
;; Dired customizations
;;----------------------------------------------------------------------------

(with-eval-after-load "dired"
  
  (load "dired-x")        

  (setq dired-isearch-filenames 'dwim	; restrict search on/to filenames
	dired-dwim-target t		; copy/move files between split panes
	)

  ;; Open files in dired mode using 'open' on MacOS
  
  (when (eq system-type 'darwin) 
    (progn
      (define-key dired-mode-map (kbd "z")
	(lambda () (interactive)
	  (let ((fn (dired-get-file-for-visit)))
	    (start-process "default-app" nil "open" fn))))))
)

;;----------------------------------------------------------------------------
;; Initialize environment from user shell.
;;----------------------------------------------------------------------------

(when (string-equal system-type "darwin")
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  )

;;----------------------------------------------------------------------------
;; Shell options
;;----------------------------------------------------------------------------

(add-hook 'comint-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)               ; process ANSI color escapes

            (setq comint-completion-addsuffix t)          ; insert space/slash after file completion
            (setq comint-completion-autolist t)           ; show completion list when ambiguous
            (setq comint-input-ignoredups t)              ; no duplicates in command history
            (setq comint-prompt-read-only t)              ; no good reason to change prompts
            (setq comint-scroll-show-maximum-output t)    ; scroll to show max possible output
            (setq comint-scroll-to-bottom-on-input t)     ; always insert at the bottom
            (setq comint-scroll-to-bottom-on-output t)    ; always add output at the bottom
            (setq comint-terminfo-terminal "xterm-256color")
            (setq completion-ignore-case t)               ; case-insensitive file completions

	    ;; (global-set-key (kbd "s-n") 'comint-next-input)
	    ;; (global-set-key (kbd "s-p") 'comint-previous-input)
            ))


;;----------------------------------------------------------------------------
;; Org-mode customizations
;;----------------------------------------------------------------------------

(with-eval-after-load "org"
  ;;(setq-default org-log-done 'time)
  (setq-default org-todo-keywords
		'((sequence "TODO" "STARTED" "WAITING" "|"
			    "DONE" "CANCELLED")))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  )

;;----------------------------------------------------------------------------
;; Completion
;;----------------------------------------------------------------------------

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(global-set-key [remap execute-extended-command] #'smex)

;; (require 'ido)
;; (ido-mode t)
;; (ido-everywhere t)

;;(setq ido-enable-flex-matching t ; Enable fuzzy matching
      ;;ido-create-new-buffer 'always ; Create a new buffer if nothing matches
      ;;ido-use-filename-at-point 'guess
      ;; Visit buffers and files in the selected window
      ;;ido-default-file-method 'selected-window
      ;;ido-default-buffer-method 'selected-window
      ;;ido-use-faces nil ; Prefer flx ido faces
;;       ido-case-fold t ; be case-insensitive
;;       )
;; (add-to-list 'ido-ignore-files "\\.DS_Store")
;;(flx-ido-mode) ; Powerful IDO flex matching
;;(ido-vertical-mode) ; Show IDO completions vertically


;;----------------------------------------------------------------------------
;; Version control
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Auto-completion and snippets
;;----------------------------------------------------------------------------

;; Be sure to load YASnippet before Auto-Complete
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "ac-dict"))
;; (ac-config-default)

;; (setq-default ac-sources '(ac-source-abbrev
;;                            ac-source-dictionary
;;                            ac-source-words-in-same-mode-buffers))

;; from http://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/
; set the trigger key so that it can work together with yasnippet on
; tab key, if the word exists in yasnippet, pressing tab will cause
; yasnippet to activate, otherwise, auto-complete will
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")

;; Start auto-complete after N characters of a word.
;;(setq ac-auto-start 3)

;; Case sensitivity is important when finding matches.
;; (setq ac-ignore-case 'smart)


;;----------------------------------------------------------------------------
;; Ack configuration
;;----------------------------------------------------------------------------

;; (require 'ack-and-a-half)
;; (defalias 'ack 'ack-and-a-half)
;; (defalias 'ack-same 'ack-and-a-half-same)
;; (defalias 'ack-find-file 'ack-and-a-half-find-file)
;; (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)


;;----------------------------------------------------------------------------
;; Autopair options
;;----------------------------------------------------------------------------

;; (require 'autopair)
;; (autopair-global-mode)


;;----------------------------------------------------------------------------
;; C/C++ mode customizations
;;----------------------------------------------------------------------------

;; (setq c-default-style "bsd")	    ; standard for HANA development
;; (setq c-basic-offset 4)		        ; standard for HANA development


;;----------------------------------------------------------------------------
;; CSS mode customizations
;;----------------------------------------------------------------------------

;; (require 'css-mode)

;; (defun my:css-mode-hook ()
;;   ;; indent automatically
;;   (local-set g-key (kbd "RET") 'newline-and-indent)

;;   ;; indentation style
;;   (setq css-indent-offset 2)
;;   )


;;----------------------------------------------------------------------------
;; HTML mode customizations
;;----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(with-eval-after-load 'web-mode

  ;; (require 'flycheck)
  ;; (flycheck-add-mode 'html-tidy 'web-mode)

  (add-hook 'web-mode-hook
	    (lambda ()

	      ;; indent automatically
	      ;; (local-set-key (kbd "RET") 'newline-and-indent)

	      ;; http://web-mode.org/
	      (setq web-mode-markup-indent-offset 4)
	      (setq web-mode-css-indent-offset 4)
	      (setq web-mode-code-indent-offset 4)

	      ;; enable syntax-checking
	      ;; (flycheck-mode 1)
	      ))
    )

; Active auto-completion for HTML editing

;; (when (boundp 'ac-modes)
;;   (add-to-list 'ac-modes 'html-mode)
;;   (add-to-list 'ac-modes 'web-mode)
  
;;   ;; Remove inappropriate completion sources.
;;   (setq web-mode-ac-sources-alist
;;         '(("css" . (ac-source-css-property))
;;           ("html" . (ac-source-yasnippet ac-source-words-in-buffer ac-source-dictionary ac-source-abbrev)))
;;         )
;;   )


;;----------------------------------------------------------------------------
;; Javascript mode configuration
;;----------------------------------------------------------------------------

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; (with-eval-after-load 'js2-mode
;;   (require 'flycheck)
;;   (add-hook 'js2-mode-hook
;; 	    (lambda ()
;; 	      (add-node-modules-path)  ;; set path to node_modules automatically
;; 	      (flycheck-mode 1)
;; 	      ))
;;   ;; disable jshint (leaving only eslint checking)
;;   (setq-default flycheck-disabled-checkers
;; 		(append flycheck-disabled-checkers
;; 			'(javascript-jshint)))
;;   )

;;----------------------------------------------------------------------------
;; Python mode configuration
;;----------------------------------------------------------------------------

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt -i")

;; (with-eval-after-load 'python-mode
;;   (elpy-enable)
;;   )
  
(add-hook 'python-mode-hook
          (lambda ()
;;	    (flycheck-mode 1)
;;	    (require 'company-mode)
;;	    (company-mode)
;;            (require 'pyenv-mode)
	    ;;	    (electric-indent-mode)
	    ;; (define-key python-mode-map [remap backward-sentence] 'python-nav-backward-statement)
	    ;; (define-key python-mode-map [remap forward-sentence] 'python-nav-forward-statement)
            )
          )

;;(require 'python)

;; Ensure Python is on Emacs PATH.
;; (when (eq system-type 'windows-nt)
;;   (my:prepend-to-PATH (expand-file-name (getenv "PYTHON_HOME")))
;;   (my:prepend-to-PATH (concat (expand-file-name (getenv "PYTHON_HOME")) "/Scripts"))
;;   )


;; (setq using-ipython t) ; choose iPython versus std Python shell
;; (if using-ipython
;;     (progn ;; configure iPython shell
;;       (setq
;;        python-shell-interpreter "python"
;;        python-shell-interpreter-args (cond ((string-equal system-type "windows-nt") "-i -m IPython console --gui=wx --matplotlib=wx --colors=Linux")
;;                                            ((string-equal system-type "darwin") "--gui=osx --matplotlib=osx --colors=Linux"))
;;        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
;;        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
;;        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
;;        )
;;       )
;;   (progn ;; configure standard Python shell

;; 	;; Helpful key bindings
;; 	(eval-after-load 'python
;; 	  '(define-key python-mode-map (kbd "C-c !") 'python-shell-switch-to-shell))
;; 	(eval-after-load 'python
;; 	  '(define-key python-mode-map (kbd "C-c |") 'python-shell-send-region))

;;     ;; 	(add-hook 'inferior-python-mode-hook
;;     ;;               (lambda (setq temporary-file-directory "/temp"))
;;     ;; 			     (lambda () (define-key inferior-python-mode-map "\t" 'insert-char))
;;     ;;               )
;;  	)
;; )

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq tab-width 4)

;;             (when (eq system-type 'darwin)
;;               (setq python-shell-interpreter "ipython"
;;                     ;; python-shell-interpreter-args "-i"
;;                     )
;;               )

;;             (require 'gud)
;;             (setq gud-pdb-command-name "python -m pdb")
;;             (local-set-key (kbd "<f8>") '(lambda () (interactive) (pdb (concat gud-pdb-command-name " " (buffer-file-name)))))

            ;; Remove inappropriate completion sources.
            ;; (setq ac-sources '(ac-source-yasnippet
            ;;                    ac-source-abbrev
            ;;                    ac-source-dictionary
            ;;                    ac-source-words-in-same-mode-buffers))
;;            (jedi:setup)
            ;; (set (make-local-variable 'compile-command)
            ;;      (concat "python " buffer-file-name)
            ;;      )
          ;;   )
          ;; )

;; (defun my:python-debug-buffer ()
;;   (interactive)
;;   (pdb (concat gud-pdb-command-name " " (buffer-file-name))))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (require 'gud)
;;             (setq gud-pdb-command-name "python -m pdb")
;;             (local-set-key (kbd "<f8>") #'my:python-debug-buffer)
;;             )
;;           )

;;----------------------------------------------------------------------------
;; Allow this Emacs process to be a server for emacsclient.
;;----------------------------------------------------------------------------

(require 'server)
(unless (or (server-running-p)
	    noninteractive (member "--daemon" command-line-args))
  (server-start))

;; Use C-x,k for killing buffers even if opened from emacsclient

(defun my:kill-emacsclient-buffer ()
  (interactive)
  (if server-buffer-clients
      (server-done)
    (kill-this-buffer)
    ))

(global-set-key (kbd "C-x k") 'my:kill-emacsclient-buffer)

;; Customize settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit whole-line-or-region python change-inner mwim add-node-modules-path exec-path-from-shell flycheck js2-mode json-mode web-mode which-key zenburn-theme expand-region syndicate))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
