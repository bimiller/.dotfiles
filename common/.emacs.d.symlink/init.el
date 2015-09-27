;;============================================================================
;; Emacs customizations
;;============================================================================

(setq inhibit-default-init t) ;; Disable the default site settings.

;; Use Command as Meta on Macs. (set early in case init.el needs editing)
(when (equal window-system 'ns)
  (setq  mac-option-modifier 'super ; use Option as Command key
         mac-command-modifier 'meta ; use Command as Meta
         mac-command-key-is-meta t
         mac-right-command-modifier 'left ; right and left Command are the same
         mac-right-option-modifier 'none ; Keep right Option for accented input
         mac-function-modifier 'hyper)
  (global-set-key (kbd "M-`") 'other-frame))


;;----------------------------------------------------------------------------
;; Package management
;;----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun my:install-packages (packages)
  "Install a set of packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(defconst my:common-packages
  '(
    ;; ack-and-a-half ;; Code search
    ;; auto-complete
    ;; exec-path-from-shell ;; Set PATH from shell
    ;; expand-region ;; Expand region by semantic units
    ;; git-commit-mode ;; Git commit message mode
    ;; gitconfig-mode ;; Git configuration mode
    ;; git-rebase-mode ;; Mode for git rebase -i
    ;; js2-mode
    ;; magit ;; Git frontend
    ;; maxframe
    ;; web-mode
    ;; yasnippet
    zenburn-theme
    )
  "Packages needed by my configuration on all platforms.")

(defconst my:mswindows-packages
  '(
    ;; Cygwin integration
    cygwin-mount ; make NT Emacs recognize Cygwin paths
    setup-cygwin ; make NT Emacs use Cygwin for shell, etc.
    )
  "Packages needed by my configuration on MS Windows.")

(my:install-packages my:common-packages)

(when (eq system-type 'windows-nt)
  (my:install-packages my:mswindows-packages))


;;----------------------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------------------

;; Get rid of tool bar, menu bar and scroll bars. On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq window-system "ns")) (fboundp 'menu-bar-mode) (not (display-graphic-p)))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Highlight region when active
(transient-mark-mode)

;; Delete the selection instead of inserting
(delete-selection-mode)

;; No blinking and beeping, no startup screen, and no scratch message.
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-message t
      visible-bell t
      )

;; Short Yes/No questions.
(fset 'yes-or-no-p 'y-or-n-p)

;; Sentences can end with a single space.
(setq-default sentence-end-double-space nil)

;; Show cursor position and file size on mode line.
(line-number-mode -1)
(column-number-mode)
(size-indication-mode)

;; Make searches case-insensitive.
(setq-default case-fold-search t)

;; Save the contents of the clipboard to kill ring before killing.
(setq save-interprogram-paste-before-kill t)

;; Sync cut/copied text with the MS Windows clipboard.
(setq x-select-enable-clipboard t)

;; Move between windows with Shift + Arrow keys
(windmove-default-keybindings)


;;----------------------------------------------------------------------------
;; Fonts and color themes
;;----------------------------------------------------------------------------

;; Choose Font and color theme. We prefer Source Code Pro, and then Anonymous
;; Pro from http://www.marksimonson.com/fonts/view/anonymous-pro or Inconsolata
;; (from the Google Webfont directory). On OS X, we need to give these fonts a
;; larger size. If neither is available, we fall back to the standard faces of
;; OS X (Menlo), Linux (DejaVu Sans Mono) or Windows (Consolas, Courier New)
(defconst my:preferred-monospace-fonts
  `(("Source Code Pro" . ,(if (eq system-type 'darwin) 130 100))
    ("Anonymous Pro" . ,(if (eq system-type 'darwin) 140 110))
    ("Anonymous Pro Minus" . ,(if (eq system-type 'darwin) 140 110))
    ("Inconsolata" . ,(if (eq system-type 'darwin) 140 110))
    ("Menlo" . 130)
    ("Consolas" . 120)
    ("DejaVu Sans Mono" . 110)
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

(load-theme 'zenburn 'no-confirm)


;;----------------------------------------------------------------------------
;; File handling
;;----------------------------------------------------------------------------

;; Use UTF-8 by default for new files, file detection, etc.
(prefer-coding-system 'utf-8)

;; I map this to the Cygwin tmp directory on MS Windows.
(setq temporary-file-directory "/tmp/")

;; Keep backup and auto save files out of the way.
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups")))
      auto-save-file-name-transforms `((".*" ,(concat temporary-file-directory "\\2") t))
      )

;; Delete files to trash.
(setq delete-by-moving-to-trash t)

;; Power up dired.
(require 'dired-x)

;; Save place in files when closing Emacs.
;; (desktop-save-mode)

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

;; Store Tramp auto save files locally.
(setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save"))


;;----------------------------------------------------------------------------
;; Basic editing
;;----------------------------------------------------------------------------

;; Disable tabs, but give them the "standard" width by default.
;; (I prefer to override the tab width for each mode as needed.)
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Make Tab complete if the line is indented.
(setq tab-always-indent 'complete)

;; Electric indenting, bracket pairing, and code layout.
(electric-indent-mode)
(electric-pair-mode)
(electric-layout-mode)

;; Configure scrolling.
(setq scroll-margin 0 ; Drag the point along while scrolling
      scroll-conservatively 1000 ; Never recenter the screen while scrolling
      scroll-error-top-bottom t ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; Enable region upper/lower-casing.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Show matching delimiters.
(show-paren-mode)

;; Add custom highlights to buffers.
(global-hi-lock-mode 1)

;; Make `kill-whole-line' indentation aware
(defun my:smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.
Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key [remap kill-whole-line] #'my:smart-kill-whole-line)

;; Make `kill-line' indentation-aware.
(defun my:smart-backward-kill-line ()
  "Kill line backwards and re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))
(global-set-key (kbd "C-S-<backspace>") #'my:smart-backward-kill-line) ; C-S-backspace

;; Open empty line below the current line.
(defun my:smart-open-line ()
  "Insert empty line after the current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") #'my:smart-open-line)

;; Make C-a toggle between beginning of line and indentation
(defun my:back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


;;----------------------------------------------------------------------------
;; Platform-specific configuration
;;----------------------------------------------------------------------------

;; PATH modification helper function
(defun my:prepend-to-PATH (directory)
  "Prepends directory to both OS PATH and Emacs exec-path."
  (let ((new-path (concat directory path-separator (getenv "PATH"))))
    (setenv "PATH" new-path)
    (setq exec-path (split-string new-path path-separator))))

;; Ensure proper integration with external environment.
(cond ((string-equal system-type "windows-nt")
       ;; Ensure Cygwin binaries appear at the front of the Emacs exe search path
       ;; (and before other Windows system and app paths).
       (my:prepend-to-PATH "C:/Cygwin/bin")

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
            (setq gnutls-trustfiles '("c:/cygwin/usr/ssl/certs/ca-bundle.trust.crt"
                                      "c:/cygwin/usr/ssl/certs/ca-bundle.crt"))))
       
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
      ((string-equal system-type "gnu/linux")
       )
      ((string-equal system-type "darwin")
       ;; (require 'exec-path-from-shell) ;; if not using the ELPA package
       ;; (exec-path-from-shell-initialize)
       )
       ;; Use homebrew ASpell instead of ISpell (brew install aspell --lang=en)
       (setq-default ispell-program-name "aspell")
      )

;; Default windows size & position.
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
  (local-set-key (kbd "RET") 'newline-and-indent)

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
;; IDO options
;;----------------------------------------------------------------------------

(require 'ido)
(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t ; Enable fuzzy matching
      ido-create-new-buffer 'always ; Create a new buffer if nothing matches
      ido-use-filename-at-point 'guess
      ;; Visit buffers and files in the selected window
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-use-faces nil ; Prefer flx ido faces
      ido-case-fold t ; be case-insensitive
      )
(add-to-list 'ido-ignore-files "\\.DS_Store")
;(flx-ido-mode) ; Powerful IDO flex matching
;(ido-vertical-mode) ; Show IDO completions vertically


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
	  (define-key global-map "\C-cl" 'org-store-link)
	  (define-key global-map "\C-ca" 'org-agenda)
	  (define-key global-map "\C-cc" 'org-capture)
	  ))


;;----------------------------------------------------------------------------
;; Python mode configuration
;;----------------------------------------------------------------------------


(require 'python)

;; Ensure Python is on Emacs PATH.
(when (eq system-type 'windows-nt)
  (my:prepend-to-PATH (expand-file-name (getenv "PYTHON_HOME")))
  (my:prepend-to-PATH (concat (expand-file-name (getenv "PYTHON_HOME")) "/Scripts"))
  )

(when (eq system-type 'darwin)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i")
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


;;----------------------------------------------------------------------------
;; Uniquify settings
;;----------------------------------------------------------------------------

;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)


;;----------------------------------------------------------------------------
;; Additional customizations
;;----------------------------------------------------------------------------

;(add-to-list 'load-path user-emacs-directory)

;;(load "custom/my-functions")

;; (load "~/.emacs.d/custom/sap-regi-integration")
;; (load "~/.emacs.d/custom/sap-tasdk-integration")


;;----------------------------------------------------------------------------
;; Basic key bindings
;;----------------------------------------------------------------------------

;; Kill some useless bindings
(when (display-graphic-p)
  ;; `suspend-frame' is annoying in graphic displays
  (global-set-key (kbd "C-z") nil)
  )

;; (defun my:hard-newline ()
;;   "Insert 1 newline without identation, useful to start a fresh Python function."
;;   (interactive)
;;   (newline 1))
;; )
;; Improve standard bindings
;;(global-set-key (kbd "RET") 'newline-and-indent)
;;(global-set-key [(control return)] 'my:hard-newline)

;;(global-set-key [remap execute-extended-command] #'smex)
(global-set-key [remap list-buffers] #'ibuffer)

(global-set-key [remap move-beginning-of-line] #'my:back-to-indentation-or-beginning-of-line)
(global-set-key [remap dabbrev-expand] #'hippie-expand)
;; (global-set-key [remap isearch-forward] #'isearch-forward-regexp)
;; (global-set-key [remap isearch-backward] #'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)
;;(global-set-key [remap just-one-space] #'cycle-spacing)

;; Complement standard bindings (the comments indicate the related bindings)
;(global-set-key (kbd "M-X") #'smex-major-mode-commands) ; M-x

;;(global-set-key (kbd "M-Z") #'zap-up-to-char) ; M-z
;; (global-set-key (kbd "C-h A") #'apropos) ; C-h a
;; (global-set-key (kbd "C-x p") #'proced) ; C-x p

;; Steve Yegge's recommended bindings
;(global-set-key "\C-x\C-m" 'execute-extended-command)
;(global-set-key "\C-c\C-m" 'execute-extended-command)
;(global-set-key "\C-w" 'backward-kill-word)
;(global-set-key "\C-x\C-k" 'kill-region)
;(global-set-key "\C-c\C-k" 'kill-region)

;; Use C-x,k for killing buffers even if opened from emacsclient
(global-set-key (kbd "C-x k")
                '(lambda () (interactive)
                   (if server-buffer-clients
                       (server-done)
                     (kill-this-buffer)
                     )))

;; Home toggles between the first char and the first non-whitespace character in the line.
;(global-set-key [home] 'my:smart-beginning-of-line)
;(global-set-key (kbd "C-a") 'my:back-to-indentation-or-beginning-of-line)

;; Keys for paging in the "other" window.
;; (global-set-key "\M-n" "\C-u1\C-v")
;; (global-set-key "\M-p" "\C-u1\M-v")

;; Function Key bindings

(global-set-key (kbd "<f8>") 'compile)

;;(global-set-key [f11] 'previous-error)
;;(global-set-key [f12] 'next-error)

;;(global-set-key [C-tab] 'next-buffer)
;;(global-set-key [C-S-tab] 'previous-buffer)

;;(global-set-key [M-f4] 'save-buffers-kill-emacs)

;;----------------------------------------------------------------------------
;; Allow this Emacs process to be a server for emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (or (server-running-p) noninteractive (member "--daemon" command-line-args)) (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" default)))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
