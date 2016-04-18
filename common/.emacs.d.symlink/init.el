;;============================================================================
;; Emacs customizations
;;============================================================================

(setq inhibit-default-init t) ;; Disable the local default settings.

;;----------------------------------------------------------------------------
;; Esssential customizations
;;----------------------------------------------------------------------------

;; Switch Command and Option keys on Macs.
(when (equal window-system 'ns)
  (setq  ns-command-modifier 'meta ; use Command as Meta
         ns-option-modifier 'super ; use Option as Command
         ns-right-command-modifier 'left ; right and left Command are the same
         ns-right-option-modifier 'left ; right and left Opt are the same
         ns-function-modifier 'hyper)
  (global-set-key (kbd "M-`") 'other-frame)
)

;; Highlight region when mark is active.
(transient-mark-mode 1)

;; Delete the selection when inserting.
(delete-selection-mode 1)

;; Don't operate on regions unless the mark is active.
(setq mark-even-if-inactive nil)

;; Replace zap-to-char with zap-UP-to-char.
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Save the contents of the clipboard to kill ring before killing.
(setq save-interprogram-paste-before-kill t)

;; Sync cut/copied text with the MS Windows clipboard.
(setq x-select-enable-clipboard t)

;; Mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;; Make searches case-insensitive.
(setq-default case-fold-search t)

;; Use UTF-8 by default for new files, file detection, etc.
(prefer-coding-system 'utf-8)

;; I map this to the system tmp directory.
;(setq temporary-file-directory "/tmp/")

;; Keep backup and auto save files out of the way.
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups")))
      auto-save-file-name-transforms `((".*" ,(concat temporary-file-directory "\\2") t))
      )

;; Delete files to trash.
(setq delete-by-moving-to-trash t)

;; Allow this Emacs process to be a server for emacsclient
(require 'server)
(unless (or (server-running-p) noninteractive (member "--daemon" command-line-args)) (server-start))

;; Use C-x,k for killing buffers even if opened from emacsclient
(global-set-key (kbd "C-x k")
                '(lambda () (interactive)
                   (if server-buffer-clients
                       (server-done)
                     (kill-this-buffer)
                     )))

;; Disable tabs, but give them the "standard" width by default.
;; (Better to override the tab width for each mode as needed.)
(setq-default indent-tabs-mode nil
              tab-width 8)

;; View files in read-only mode.
(setq view-read-only t)

;; Protect against hangs when viewing large files.
(defun my:find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only and use fundamental mode."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hooks 'my:find-file-check-make-large-file-read-only-hook)

;; Automatically add final newline (if necessary) when saving.
(setq-default require-final-newline t)

;;----------------------------------------------------------------------------
;; Look-and-feel
;;----------------------------------------------------------------------------

;; Get rid of tool bar, scroll bars, and menu bar (text mode only).
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (display-graphic-p)))
  (menu-bar-mode -1))

;; Show cursor position and file size on mode line.
(line-number-mode)
(column-number-mode)
(size-indication-mode)

;; Limit cursor blinking.
(setq blink-cursor-blinks 3)

;; No beeping, no startup screen, and no scratch message.
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-message t
      visible-bell t
      )

;; Short Yes/No questions.
(fset 'yes-or-no-p 'y-or-n-p)

;; Configure scrolling.
(setq scroll-margin 0 ; Drag the point along while scrolling.
      scroll-conservatively 1000 ; Never recenter the screen while scrolling.
      scroll-error-top-bottom t ; Move to beg/end of buffer before signalling an error.
      ;; These settings make trackpad scrolling on OS X much more predictable and smooth.
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; Show matching delimiters.
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; General editing preferences
;;----------------------------------------------------------------------------

;; Disable tabs, but give them the "standard" width by default.
;; (Better to override the tab width for each mode as needed.)
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Sentences can end with a single space.
(setq-default sentence-end-double-space nil)

;; Make Tab complete if the line is indented.
(setq tab-always-indent 'complete)

;; Electric indenting, bracket pairing, and code layout.
(electric-indent-mode)
(electric-pair-mode)
(electric-layout-mode)

;; Enable region upper/lower-casing.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Package management
;;----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;;----------------------------------------------------------------------------
;; Fonts and color themes
;;----------------------------------------------------------------------------

;; Choose Font and color theme. Prefer Source Code Pro, and then Anonymous
;; Pro from http://www.marksimonson.com/fonts/view/anonymous-pro or Inconsolata
;; (from the Google Webfont directory). On OS X, give these fonts a larger size.
;; If neither is available, fall back to the standard faces of OS X (Monaco),
;; Linux (DejaVu Sans Mono) or Windows (Consolas, Courier New).
(defconst my:preferred-monospace-fonts
  `(("Source Code Pro" . ,(if (eq system-type 'darwin) 130 100))
    ("Anonymous Pro" . ,(if (eq system-type 'darwin) 140 110))
    ("Anonymous Pro Minus" . ,(if (eq system-type 'darwin) 140 110))
    ("Inconsolata" . ,(if (eq system-type 'darwin) 140 110))
    ("Monaco" . 140)
    ("Consolas" . 120)
    ("DejaVu Sans Mono" . 130)
    ("Courier New" . 130))
  "My preferred monospace fonts.
The `car' of each item is the font family, the `cdr' the preferred font size.")

(defconst my:preferred-proportional-fonts
  '(("Lucida Grande" . 120)
    ("DejaVu Sans" . 110))
  "My preferred proportional fonts.
The `car' of each item is the font family, the `cdr' the preferred font size.")

(defun my:first-existing-font (fonts)
  "Get the first existing font from FONTS."
  (let (font)
    (while (and fonts
                (not (setq font (when (x-family-fonts (caar fonts))
                                  (car fonts)))))
      (setq fonts (cdr fonts)))
    font))

(defmacro when-let (var-val &rest body)
  "If VAL evaluates to non-nil, bind it to VAR and execute body.
VAR-VAL should be a (VAR VAL) pair."
  (declare (debug ((symbolp form) body))
	   (indent 1))
  (let ((var (car var-val))
	(val (cadr var-val)))
    `(let ((,var ,val))
       (when ,var
	 ,@body))))

(defun my:choose-best-fonts ()
  "Choose the best fonts."
  (interactive)
  (when-let (font (my:first-existing-font my:preferred-monospace-fonts))
            (dolist (face '(default fixed-pitch))
              (set-face-attribute face nil :family (car font) :height (cdr font))))
  (when-let (font (my:first-existing-font my:preferred-proportional-fonts))
            (set-face-attribute 'variable-pitch nil
                                :family (car font) :height (cdr font))))
(my:choose-best-fonts)

;; Set preferred color scheme.
(load-theme 'zenburn 'no-confirm)

;;----------------------------------------------------------------------------
;; Platform-specific configuration settings
;;----------------------------------------------------------------------------

;; PATH modification helper function
(defun my:prepend-to-PATH (directory)
  "Prepends directory to both OS PATH and Emacs exec-path."
  (let ((new-path (concat directory path-separator (getenv "PATH"))))
    (setenv "PATH" new-path)
    (setq exec-path (split-string new-path path-separator))))

;; Ensure proper integration with external environment.
(cond
 
 ;; ------------ Windows emacs ------------
 
 ((string-equal system-type "windows-nt")
  ;; Ensure Cygwin binaries appear at the front of the Emacs exe search path
  ;; (and before other Windows system and app paths).
  (my:prepend-to-PATH "C:/cygwin64/bin")
  
  ;; Cygwin settings (for use with NT Emacs).
  (require 'setup-cygwin)
  (if (require 'cygwin-mount nil t)
      (progn (require 'setup-cygwin)
             (cygwin-mount-activate)
             (set-shell-bash)
             )
    )

  ;; TLS/SSL settings (help GNUTLS find Cygwin cert stores)
  (eval-after-load "gnutls"
    '(progn
       (setq gnutls-trustfiles '("c:/cygwin64/usr/ssl/certs/ca-bundle.trust.crt"
                                 "c:/cygwin64/usr/ssl/certs/ca-bundle.crt"))))
  
  ;; Tramp settings
  (cond  ((eq window-system 'w32)
          (setq tramp-default-method "scpx"))
         (t
          (setq tramp-default-method "scpc")))
  
  ;; Use Cygwin ASpell instead of ISpell
  (setq-default ispell-program-name "aspell")
  
  ;; Browser configuration
  (setq browse-url-browser-function 'browse-url-firefox)
  
  (setq-default comint-process-echoes 'on)
  
  ;; Cincinnati office printer
  (setq printer-name "//uschipr0.chi.sap.corp/CIN1_CO")
  )

 ;; ------------ Cygwin emacs ------------
 
 ((string-equal system-type "cygwin")
  ;; Ensure Cygwin binaries come before Windows paths.
  (my:prepend-to-PATH "/bin")
  
  ;; Include Windows file extensions when searching for executables.
  (setq-default exec-suffixes (append '(".exe" ".com" ".bat" ".cmd" ".btm") exec-suffixes))
  
  ;; Translate Windows file paths to Cygwin paths.
  ;;(load "custom/windows-path.el")
  ;;(windows-path-activate)

  ;; Set shell to Bash
  (setq shell-file-name "/bin/bash")

  ;; Use ASpell instead of ISpell
  (setq-default ispell-program-name "aspell")

  ;; Browser configuration
  (setq browse-url-browser-function 'browse-url-firefox)
  )

 ;; ------------ Linux ------------
 
 ((string-equal system-type "gnu/linux")
  )

 ;; ------------ OS X ------------
 
 ((string-equal system-type "darwin")
  (require 'exec-path-from-shell) ;; if not using the ELPA package
  (exec-path-from-shell-initialize)
       )
       ;; Use homebrew ASpell instead of ISpell (brew install aspell --lang=en)
       (setq-default ispell-program-name "aspell")
      )


;;----------------------------------------------------------------------------
;; Default windows size & position.
;;----------------------------------------------------------------------------

(cond ((string-equal window-system "w32")
       (setq default-frame-alist
             '((top . 20) (left . 100)
               (width . 150) (height . 36)
               ))

       ;; Initial frame is top-left.
       (setq initial-frame-alist '((top . 20) (left . 100)))
       (setq split-width-threshold 240)
       )
      ((string-equal window-system "x")
       (defun custom-set-frame-size ()
         (add-to-list 'default-frame-alist '(height . 36))
         (add-to-list 'default-frame-alist '(width . 150))
         (add-to-list 'default-frame-alist '(top . 20))
         (add-to-list 'default-frame-alist '(left . 100)))
       (custom-set-frame-size)
       (add-hook 'before-make-frame-hook 'custom-set-frame-size)
       (setq split-width-threshold 240)

       (x-focus-frame nil)
       )
      ((string-equal window-system "ns")
       (defun custom-set-frame-size ()
         (add-to-list 'default-frame-alist '(height . 36))
         (add-to-list 'default-frame-alist '(width . 150))
         (add-to-list 'default-frame-alist '(top . 20))
         (add-to-list 'default-frame-alist '(left . 100)))
       (custom-set-frame-size)
       (add-hook 'before-make-frame-hook 'custom-set-frame-size)
       (setq split-width-threshold 240)

       (x-focus-frame nil)
       )
      )

;;----------------------------------------------------------------------------
;; Configure preferred built-in packages
;;----------------------------------------------------------------------------

;; Use portion of directory path to make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save place in files when closing Emacs.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use expanded dired features.
(with-eval-after-load "dired"
  )

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (setq dired-isearch-filenames 'dwim)
            ))

;; Apropos commands should search more extensively.
(setq apropos-do-all t)


;;----------------------------------------------------------------------------
;; IDO options
;;----------------------------------------------------------------------------

(require 'ido)
(ido-mode t)
;; (ido-everywhere t)

(setq ido-enable-flex-matching t ; Enable fuzzy matching
      ;;ido-create-new-buffer 'always ; Create a new buffer if nothing matches
      ;;ido-use-filename-at-point 'guess
      ;; Visit buffers and files in the selected window
      ;;ido-default-file-method 'selected-window
      ;;ido-default-buffer-method 'selected-window
      ;;ido-use-faces nil ; Prefer flx ido faces
      ido-case-fold t ; be case-insensitive
      )
(add-to-list 'ido-ignore-files "\\.DS_Store")
;;(flx-ido-mode) ; Powerful IDO flex matching
;;(ido-vertical-mode) ; Show IDO completions vertically

;;----------------------------------------------------------------------------
;; Tramp configuration
;;----------------------------------------------------------------------------

;; Store Tramp auto save files locally.
(setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save"))

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

(setq c-default-style "bsd")	    ; standard for HANA development
(setq c-basic-offset 4)		        ; standard for HANA development


;;----------------------------------------------------------------------------
;; CSS mode customizations
;;----------------------------------------------------------------------------

(require 'css-mode)

(defun my:css-mode-hook ()
  ;; indent automatically
  (local-set g-key (kbd "RET") 'newline-and-indent)

  ;; indentation style
  (setq css-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my:web-mode-hook)


;;----------------------------------------------------------------------------
;; Expand/contract region selection based on "semantic" units
;;----------------------------------------------------------------------------

;; (require 'expand-region)
;; (global-set-key (kbd "M-<up>") 'er/expand-region)
;; (global-set-key (kbd "M-<down>") 'er/contract-region)


;;----------------------------------------------------------------------------
;; HTML mode customizations
;;----------------------------------------------------------------------------

;; (require 'web-mode)

; Use web-mode for all web page editing

;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

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

; Hook web mode activation

;; (defun my:web-mode-hook ()

;;   ;; indent automatically
;;   (local-set-key (kbd "RET") 'newline-and-indent)
  
;;   )
;; (add-hook 'web-mode-hook  'my:web-mode-hook)


;;----------------------------------------------------------------------------
;; Javascript mode configuration
;;----------------------------------------------------------------------------

;; Open JSON files in Javascript mode
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))


;;----------------------------------------------------------------------------
;; Org-mode customizations
;;----------------------------------------------------------------------------

(require 'org)
(setq org-log-done 'time)
(setq org-M-RET-may-split-line nil)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE"))))

(if (featurep 'org)
	(progn
	  ;; (define-key global-map "\C-cl" 'org-store-link)
	  ;; (define-key global-map "\C-ca" 'org-agenda)
	  ;; (define-key global-map "\C-cc" 'org-capture)
	  ))


;;----------------------------------------------------------------------------
;; Python mode configuration
;;----------------------------------------------------------------------------


;;(require 'python)

;; Ensure Python is on Emacs PATH.
(when (eq system-type 'windows-nt)
  (my:prepend-to-PATH (expand-file-name (getenv "PYTHON_HOME")))
  (my:prepend-to-PATH (concat (expand-file-name (getenv "PYTHON_HOME")) "/Scripts"))
  )


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

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)

            (when (eq system-type 'darwin)
              (setq python-shell-interpreter "ipython"
                    ;; python-shell-interpreter-args "-i"
                    )
              )

            (require 'gud)
            (setq gud-pdb-command-name "python -m pdb")
            (local-set-key (kbd "<f8>") '(lambda () (interactive) (pdb (concat gud-pdb-command-name " " (buffer-file-name)))))

            ;; Remove inappropriate completion sources.
            ;; (setq ac-sources '(ac-source-yasnippet
            ;;                    ac-source-abbrev
            ;;                    ac-source-dictionary
            ;;                    ac-source-words-in-same-mode-buffers))
;;            (jedi:setup)
            ;; (set (make-local-variable 'compile-command)
            ;;      (concat "python " buffer-file-name)
            ;;      )
            )
          )

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
;; Additional customizations
;;----------------------------------------------------------------------------

;(add-to-list 'load-path user-emacs-directory)

;;(load "custom/my-functions")

;; (load "~/.emacs.d/custom/sap-regi-integration")
;; (load "~/.emacs.d/custom/sap-tasdk-integration")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
