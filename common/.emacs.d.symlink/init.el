;;----------------------------------------------------------------------------
;; Basic setup
;;----------------------------------------------------------------------------

(require 'package)
(require 'tls)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      load-prefer-newer t)
(package-initialize)

;; Backups & temporary files

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t
      backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups")))
      version-control 'never)

;; MacOS integration

(when (equal window-system 'ns)

  (setq ;ns-command-modifier 'meta       ; Cmd is Meta
        ;ns-option-modifier 'super       ; Opt is Super (bind as "s-")
        ns-function-modifier 'hyper     ; Fn  is Hyper (bind as "H-")
        ns-right-command-modifier 'left ; left/right Cmd keys are equivalent
        ns-right-option-modifier 'left) ; left/right Opt keys are equivalent

  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen) ; also F11
  (global-set-key (kbd "s-G") 'isearch-repeat-backward)
  (global-set-key (kbd "s-o") 'find-file)  ; but NOT using Finder dialog
  (global-set-key (kbd "s-S") 'write-file) ; but NOT using Finder dialog
  (global-set-key (kbd "s-[") 'previous-buffer)
  (global-set-key (kbd "s-]") 'next-buffer)

  (global-unset-key (kbd "s-e"))        ; (isearch-yank-kill)
  (global-unset-key (kbd "s-j"))        ; (exchange-point-and-mark)
  (global-unset-key (kbd "s-q"))        ; (save-buffers-kill-emacs)
  (global-unset-key (kbd "s-u"))        ; (revert-buffer)

  (setq delete-by-moving-to-trash t
        ns-pop-up-frames nil            ; open Finder files in current frame
        trash-directory "~/.Trash")     ; put deleted files in MacOS trash

  ;; Enable emoji, and stop the UI from freezing when trying to display them.

  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  (add-to-list 'completion-ignored-extensions ".DS_Store")) ; MacOS detritus

;; Basic editing and UX preferences

(setq-default blink-cursor-blinks 3
              column-number-mode t
              delete-active-region 'kill    ; put region text in yank ring
              disabled-command-function nil ; enable all Emacs commands
              fill-column 80                ; wrap lines to this length
              indent-tabs-mode nil          ; generally use SPCs to indent
              inhibit-startup-screen t
              initial-scratch-message nil
              line-number-mode t
              mark-even-if-inactive nil ; only use mark when region is visible
              mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(1)
              mouse-yank-at-point t
              read-buffer-completion-ignore-case t
              require-final-newline t   ; generally end all files with newline
              ring-bell-function 'ignore
              save-interprogram-paste-before-kill t
              scroll-conservatively 100000
              scroll-error-top-bottom t
              scroll-preserve-screen-position t ; only if point moves off-screen
              sentence-end-double-space nil ; can't rely on this convention!
              set-mark-command-repeat-pop t ; repeat mark pops "modally"
              size-indication-mode t        ; show buffer size in mode line
              split-width-threshold 240 ; minimum columns for horizontal splitting
              tab-always-indent 'complete ; indent if possible; otherwise complete
              transient-mark-mode t     ; region is not always active
              use-dialog-box nil        ; never use GUI dialog boxes
              view-read-only t  ; open RO files in view mode initially
              visible-bell t)

(prefer-coding-system 'utf-8)

(delete-selection-mode t) ; typing replaces the actively selected text
(global-hl-line-mode +1)  ; highlight line containing cursor
(show-paren-mode t)       ; highlight matching parens (etc)
(electric-pair-mode +1)   ; automatically insert closing brackets

(display-time-mode +1)                  ; show time in mode line

(if (not (equal window-system 'ns))
    (menu-bar-mode -1))                 ; hide menu (except on MacOS)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))                 ; hide toolbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))                 ; hide scroll bars (both)

(defalias 'yes-or-no-p 'y-or-n-p)       ; life in the fast lane

;; Preferred fonts

(cond ((equal window-system 'x)
       (set-face-attribute 'default nil :family "Source Code Pro" :height 120))
      ((equal window-system 'ns)
       (set-face-attribute 'default nil :family "Monaco" :height 140)))

;; Preferred frame size

(if (display-graphic-p)
    (setq default-frame-alist
          '((top . 0.0) (left . 0.5) (width . 0.8) (height . 0.8))))

;; Show path to file in title bar.

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Help tweaks

(with-eval-after-load "apropos"
  (setq apropos-do-all t))              ; apropos should search more extensively

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; Automatically revert (reload) buffers when files change outside of Emacs.

(global-auto-revert-mode t)

;; Enable automatic expansion of abbreviations.

(setq-default save-abbrevs 'silently)

(add-hook 'text-mode-hook 'abbrev-mode)

;; Basic text expansion (built-into Emacs)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Use portion of directory path to make buffer names unique.

(require 'uniquify)
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "/")

;; Use ibuffer to manage buffers.

(require 'ibuffer)
(setq ibuffer-expert t ; Don't ask for confirmation to delete marked buffers.
      ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))     ; automatically update buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Save place in files when closing Emacs.

(require 'saveplace)
(setq-default save-place t              ; activate for all buffers
              save-place-file (concat user-emacs-directory "saveplace"))

;; Save minibuffer history

(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-autosave-interval 60 ;; save every minute
      savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode +1)

;; Track recently opened files.

(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory "recentf")
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)      ; may cause problems w/ remote files
(recentf-mode +1)

;; Easier movement between windows.

(require 'windmove)

;; Jump to definitions in buffer

(setq imenu-auto-rescan t
      imenu-max-item-length 200)

;; Spell-checking

(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(dolist (mode-hook '(text-mode-hook
                     org-mode-hook
                     LaTeX-mode-hook))
  (add-hook mode-hook #'flyspell-mode))

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Automatically describe function or variable at point in echo area.

(require 'eldoc)
(global-eldoc-mode)

;; Code folding

(require 'hideshow)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Save bookmarks across sessions.

(require 'bookmark)
(setq bookmark-save-flag 1)

;; Ediff should use existing frame (instead of creating a new one).

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

;; Restore original window configuration after quitting Ediff.

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; Clean up obsolete buffers automatically

(require 'midnight)

;; Make a shell script executable automatically on save.

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Undo/redo support for windowing changes.

(require 'winner)
(winner-mode +1)

;; Protect against hangs when viewing large files.

(defun my:find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only and use fundamental mode."
  (when (> (buffer-size) (* 100 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'my:find-file-check-make-large-file-read-only-hook)

;; Help with building regular expressions.
;; (Also see PCRE package to augment this.)

(require 're-builder)
(setq reb-re-syntax 'string)

;; Use ^H to backspace (F1 and/or "C-x h" for help).  SOMEDAY/MAYBE

;; (define-key key-translation-map [?\C-h] [?\C-?])
;; (global-set-key (kbd "C-x h") 'help-command)

;; Use ^W to kill previous word when no selection is active.

(defun my:kill-region-or-backward-word ()
  "Kill region or previous word (if no region active)."
  (interactive)
  (if (region-active-p)
     (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'my:kill-region-or-backward-word)

;; Make C-d kill active region.

(global-set-key (kbd "C-d") 'delete-forward-char)

;; Smarter whitespace removal.

(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Swap zap-to-char for more generally useful zap-up-to-char.

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;;----------------------------------------------------------------------------
;; Global extensions and customizations
;;----------------------------------------------------------------------------

;; Bootstrap 'use-package'.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))

(use-package diminish
  :ensure t)

;; Color scheme

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  ;; make region easier to see
  (set-face-attribute 'region nil :background "#666"))

;; Enable transient modes.

(use-package hydra
  :ensure t)

;; Efficient window management.

(use-package ace-window
  :ensure t
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 3.0)
  (set-face-attribute 'aw-mode-line-face nil
                      :inherit 'mode-line-buffer-id
                      :foreground "lawn green")
  (setq aw-dispatch-when-more-than 2
        aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u (lambda () (progn (winner-undo) (setq this-command 'winner-undo))))
          (?r winner-redo)
          (?? aw-show-dispatch-help))
        ace-window-display-mode t)
  (when (package-installed-p 'hydra)
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame"))
    (defhydra hydra-window-scroll (:color red)
      "Scroll other window"
      ("n" (lambda () (interactive) (scroll-other-window 1)) "scroll up")
      ("p" (lambda () (interactive) (scroll-other-window-down 1)) "scroll down"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
  ()
  (global-set-key [remap other-window] 'ace-window)
  ;; NB: Swap windows with a single prefix argument C-u.
  ;;     Delete selected window with a double prefix argument, i.e. C-u C-u.
  (global-set-key (kbd "M-o") (lambda () (interactive)
                                (let ((aw-dispatch-always t))
                                  (ace-window t))))
)

;; (use-package avy
;;   :ensure t
;;   :pin gnu
;;   :bind (("C-'" . avy-goto-char)
;;          ("M-g g" . avy-goto-line)
;;          ("M-g w" . avy-goto-word-or-subword-1)
;;          :map isearch-mode-map
;;          ("C-'" . avy-isearch))
;;   :config
;;   (setq avy-background t          ; dim background to hilight markers
;;         avy-style 'at-full        ; show markers on top of target text
;;         ))

;; (use-package beginend
;;   :ensure t
;;   :config
;;   (cl-loop for (minor-mode lighter) in minor-mode-alist
;;            when (equal lighter " be")
;;            do (diminish minor-mode))
;;   (beginend-global-mode)
;;   )

;; (use-package browse-kill-ring
;;   :ensure t
;;   :config
;;   ;;(browse-kill-ring-default-keybindings)
;;   (global-set-key (kbd "s-y") 'browse-kill-ring))

(use-package company
  :ensure t
  :diminish ""
  :bind (;; ([remap completion-at-point] . company-complete-common)
         :map company-active-map
         ;; ([remap completion-at-point] . company-complete-common)
         ;; ([remap complete-symbol] . company-complete-common)
         ("C-f" . company-complete-common)
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort))
  :hook (after-init . global-company-mode)
  :config
  (setq company-echo-delay 0.0
        company-idle-delay 1.0
        company-minimum-prefix-length 3
        company-require-match 'never
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-transformers '(company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix)
        )
;;   (setq company-dabbrev-downcase nil
;;         company-dabbrev-ignore-case zt
;;         company-dabbrev-other-buffers t)
;;   (setq company-dabbrev-code-everywhere t)
  ;; (company-tng-configure-default) ; use TAB to select completions
  )

;; (use-package crux
;;   :ensure t
;;   :bind
;;   ("C-<backspace>" . crux-kill-line-backwards)
;;   ([(control return)] . crux-smart-open-line)
;;   ([(shift control return)] . crux-smart-open-line-above)
;;   ([remap kill-whole-line] . crux-kill-whole-line))

;; (use-package diff-hl
;;   :ensure t
;;   :pin gnu
;;   :config
;;   (global-diff-hl-mode +1)
;;   (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; (use-package dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config
;;   (setq dumb-jump-selector 'popup) ; 'ivy)
;;   (dumb-jump-mode))

;; (use-package easy-kill
;;   :ensure t
;;   :pin gnu
;;   :config
;;   (global-set-key [remap kill-ring-save] 'easy-kill)
;;   (global-set-key [remap mark-sexp] 'easy-mark))

(use-package evil
  :ensure t
  :init
  (setq evil-cross-lines t              ; let move ops move between lines
        evil-default-state 'normal
        evil-disable-insert-state-bindings t ; std Emacs bindings for editing
        evil-move-beyond-eol t               ; let cursor sit on ending NL
        evil-move-cursor-back nil            ; don't back cursor up like Vim
        evil-respect-visual-line-mode t      ; adapt to Emacs settings
        evil-want-C-d-scroll nil
        evil-want-C-i-jump nil
        evil-want-C-u-delete nil
        evil-want-C-u-scroll nil
        evil-want-C-w-delete nil
        evil-want-C-w-in-emacs-state nil
        evil-want-fine-undo t
        evil-want-keybinding nil        ; required for evil-collection
        evil-want-Y-yank-to-eol t)
  :diminish undo-tree-mode
  :config
  (evil-mode 0))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  )

(use-package exec-path-from-shell
  :ensure t
  :when (eq window-system 'ns)
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind (("C-=" . 'er/expand-region)))

(use-package flycheck
  :ensure t
  :disabled
  :hook ((after-init . global-flycheck-mode) ; (prog-mode . flycheck-mode)
         (emacs-lisp-mode . (lambda ()
                             (let ((file-name (buffer-file-name)))
                               (when (and file-name (string-match-p "/init\\.el\\'" file-name))
                                 (setq-local flycheck-checkers '(emacs-lisp)))))))
  :config
;;  (setq flycheck-mode-line-prefix "✔")
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package ido                ; Navigation and minibuffer completion
  :ensure t
  :config
  (setq ido-create-new-buffer 'always
        ido-enable-flex-matching t
        ido-everywhere t
        ido-use-filename-at-point 'guess)
  (ido-mode 1)
  (ido-everywhere 1))

(use-package flx-ido
  :ensure t
  :after ido
  :config
  (setq ido-use-faces nil)   ; disable IDO faces to see flx highlights
  (flx-ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :after ido
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (ido-vertical-mode 1))

;; (use-package move-text
;;   :ensure t
;;   :bind
;;   (([(meta shift up)] . move-text-up)
;;    ([(meta shift down)] . move-text-down)))

;; (use-package multiple-cursors
;; :ensure t
;; )

;; (defhydra hydra-multiple-cursors (:hint nil)
;;   "
;;  Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
;; ------------------------------------------------------------------
;;  [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
;;  [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
;;  [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
;;  [Click] Cursor at point       [_q_] Quit"
;;   ("l" mc/edit-lines :exit t)
;;   ("a" mc/mark-all-like-this :exit t)
;;   ("n" mc/mark-next-like-this)
;;   ("N" mc/skip-to-next-like-this)
;;   ("M-n" mc/unmark-next-like-this)
;;   ("p" mc/mark-previous-like-this)
;;   ("P" mc/skip-to-previous-like-this)
;;   ("M-p" mc/unmark-previous-like-this)
;;   ("s" mc/mark-all-in-region-regexp :exit t)
;;   ("0" mc/insert-numbers :exit t)
;;   ("A" mc/insert-letters :exit t)
;;   ("<mouse-1>" mc/add-cursor-on-click)
;;   ;; Help with click recognition in this hydra
;;   ("<down-mouse-1>" ignore)
;;   ("<drag-mouse-1>" ignore)
;;   ("q" nil))

;; (use-package mwim
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-a") 'mwim-beginning)
;;   (global-set-key (kbd "C-e") 'mwim-end))

(use-package projectile
  :ensure t
  ;;  :diminish projectile-mode
  :init
  ;; (setq projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map)
              ("s-p" . 'projectile-command-map))
  :config
  ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; (setq projectile-mode-line '(:eval (format " [%s] " (projectile-project-name)))
  ;;       projectile-require-project-root nil) ; allow use of projectile "anywhere"
  (projectile-mode +1))

(use-package rainbow-mode
  :ensure t
  :pin gnu
  :defer t
  :diminish rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; (use-package smartparens
;;   :ensure t
;;   :disabled
;;   ;; :diminish smartparens-mode
;;   :commands smartparens-mode
;;   :config
;;   (require 'smartparens-config)
;;   (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
;;   (setq sp-autoskip-closing-pair 'always
;;         base-key-bindings 'paredit
;;         sp-hybrid-kill-entire-symbol nil)
;;   ;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;; keybinding management
;;   (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
;;   (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
;;   (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

;;   (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

;;   (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
;;   (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;;   (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
;;   (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;;   (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;;   (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;;   (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

;;   (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
;;   (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
;;   (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

;;   (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
;;   (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

;;   (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
;;   (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

;;   (bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
;;   (bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

;;   (bind-key "C-M-s"
;;             (defhydra smartparens-hydra ()
;;               "Smartparens"
;;               ("d" sp-down-sexp "Down")
;;               ("e" sp-up-sexp "Up")
;;               ("u" sp-backward-up-sexp "Up")
;;               ("a" sp-backward-down-sexp "Down")
;;               ("f" sp-forward-sexp "Forward")
;;               ("b" sp-backward-sexp "Backward")
;;               ("k" sp-kill-sexp "Kill" :color blue)
;;               ("q" nil "Quit" :color blue))
;;             smartparens-mode-map)

;;   (bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
;;   (bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
;;   (bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
;;   (bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
;;   (bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
;;   (bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
;;   (bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
;;   (bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
;;   (bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
;;   (bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
;;   (bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
;;   (bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
;;   (bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
;;   (defvar hyp-s-x-map)
;;   (define-prefix-command 'hyp-s-x-map)
;;   (bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
;;   (bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
;;   (bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
;;   (bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

;;   (bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

;;   (bind-key ";" 'sp-comment emacs-lisp-mode-map)

;;   (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

;;   ;;;;;;;;;;;;;;;;;;
;;   ;; pair management

;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;   (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

;;   (sp-with-modes 'org-mode
;;     (sp-local-pair "=" "=" :wrap "C-="))

;;   (sp-with-modes 'textile-mode
;;     (sp-local-pair "*" "*")
;;     (sp-local-pair "_" "_")
;;     (sp-local-pair "@" "@"))

;;   (sp-with-modes 'web-mode
;;     (sp-local-pair "{{#if" "{{/if")
;;     (sp-local-pair "{{#unless" "{{/unless"))

;; ;;; tex-mode latex-mode
;;   (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
;;     (sp-local-tag "i" "\"<" "\">"))

;; ;;; lisp modes
;;   (sp-with-modes sp--lisp-modes
;;     (sp-local-pair "(" nil
;;                    :wrap "C-("
;;                    :pre-handlers '(my-add-space-before-sexp-insertion)
;;                    :post-handlers '(my-add-space-after-sexp-insertion)))

;;   (defun my-add-space-after-sexp-insertion (id action _context)
;;     (when (eq action 'insert)
;;       (save-excursion
;;         (forward-char (sp-get-pair id :cl-l))
;;         (when (or (eq (char-syntax (following-char)) ?w)
;;                   (looking-at (sp--get-opening-regexp)))
;;           (insert " ")))))

;;   (defun my-add-space-before-sexp-insertion (id action _context)
;;     (when (eq action 'insert)
;;       (save-excursion
;;         (backward-char (length id))
;;         (when (or (eq (char-syntax (preceding-char)) ?w)
;;                   (and (looking-back (sp--get-closing-regexp))
;;                        (not (eq (char-syntax (preceding-char)) ?'))))
;;           (insert " ")))))

;; ;;; C++
;;   (sp-with-modes '(malabar-mode c++-mode)
;;     (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
;;   (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
;;                                                       ("* ||\n[i]" "RET")))

;;   (setq-default sp-escape-quotes-after-insert nil)

;;   (sp-local-pair 'js2-mode "/**" "*/" :post-handlers '(("| " "SPC")
;;                                                        ("* ||\n[i]" "RET")))
;;   (smartparens-global-mode)
;;   (show-smartparens-global-mode))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  ;; (global-set-key [remap execute-extended-command] #'smex)
  ;; (global-set-key "\M-X" #'smex-major-mode-commands)
  )

(use-package super-save
  :ensure t
  :diminish ""
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

(use-package undo-tree
  :ensure t
  :pin gnu
  :config
  ;; automatically save the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  ;;(global-undo-tree-mode)
  ;;:diminish
  )

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  )

;; Highlight whitespace and automatically cleanup when saving.

(use-package whitespace
  :ensure nil
  :diminish ""
  :config
  (setq whitespace-style '(face tabs empty trailing lines-tail)
        whitespace-line-column 79)        ; highlight long lines
  )

(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook #'whitespace-mode))

(add-hook 'before-save-hook 'whitespace-cleanup)

;; TODO: decide between this and easy-mark
;; (use-package whole-line-or-region
;;   :ensure t
;;   :disabled
;;   :config
;;   (add-to-list 'whole-line-or-region-extensions-alist
;;                '(comment-dwim whole-line-or-region-comment-dwim nil))
;;   (whole-line-or-region-mode 1))


(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((text-mode prog-mode restclient-mode org-mode) . yas-minor-mode-on)
  :config
  ;; (setq-default yas-prompt-functions '(yas-completing-prompt))
  (setq yas-wrap-around-region t)

  ;; Jump to end of snippet definition
  (define-key yas-keymap (kbd "C-j") 'yas-exit-all-snippets)

  ;; Inter-field navigation
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas-active-snippets)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas-active-snippets)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))

  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

  (yas-reload-all)
  )

;;----------------------------------------------------------------------------
;; Hydras
;;----------------------------------------------------------------------------

(when (package-installed-p 'hydra)

  (defhydra my:hydra-window-management (:color red :columns nil)
    "Window Management"

    ("d" ace-delete-window "delete window")
    ("o" ace-delete-other-windows "delete other windows" :exit t)
    ("s" ace-swap-window "swap window")
    ("j" ace-select-window "jump to window")
    ("b" ido-switch-buffer "switch buffer in current window")
    ("^" enlarge-window "enlarge vertically")
    ("-" shrink-window-if-larger-than-buffer "shrink to fit buffer")
    ("+" balance-windows "balance-windows")
    ("}" enlarge-window-horizontally "enlarge horizontally")
    ("{" shrink-window-horizontally "shrink horizontally")
    ("2" (lambda () (interactive) (split-window-below) (windmove-down)) "split down")
    ("3" (lambda () (interactive) (split-window-right) (windmove-right)) "split right")
    ("r" (progn (winner-redo) (setq this-command 'winner-redo)) "redo")
    ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
    ("q" nil "quit"))

  (global-set-key (kbd "C-x w") 'my:hydra-window-management/body))

;;----------------------------------------------------------------------------
;; Dired
;;----------------------------------------------------------------------------

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil) ; reuse buffer by pressing 'a'

  (setq dired-isearch-filenames 'dwim   ; restrict search on/to filenames
        dired-dwim-target t)            ; copy/move files between split panes

  ;; Always delete and copy recursively. (TOO RISKY FOR ME RIGHT NOW)
  ;; (setq dired-recursive-deletes 'always)
  ;; (setq dired-recursive-copies 'always)

  (require 'dired-x)                    ; enable extensions

  ;; Open files in dired mode using 'open' on MacOS

  (when (eq system-type 'darwin)
    (progn
      (define-key dired-mode-map (kbd "z")
        (lambda () (interactive)
          (let ((fn (dired-get-file-for-visit)))
            (start-process "default-app" nil "open" fn)))))))

;;----------------------------------------------------------------------------
;; Shells
;;----------------------------------------------------------------------------

;; Common shell interaction settings

(add-hook 'comint-mode-hook
          (lambda ()
;;            (ansi-color-for-comint-mode-on)               ; process ANSI color escapes

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

(eval-after-load "shell"
  '(add-to-list 'explicit-bash-args "-l" t))

;;----------------------------------------------------------------------------
; Tramp
;;----------------------------------------------------------------------------

(eval-after-load "tramp"
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-save"
        tramp-default-method "ssh"))

;;----------------------------------------------------------------------------
;; Magit
;;----------------------------------------------------------------------------

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x G" . magit-dispatch-popup))
  )

;; (use-package git-timemachine
;;   :ensure t
;;   :disabled
;;   :bind (("s-g" . git-timemachine)))

;;----------------------------------------------------------------------------
;; Org
;;----------------------------------------------------------------------------

(use-package org
  :ensure t
  ;; :pin org
  :init
  (setq-default org-startup-with-inline-images t
                org-todo-keywords
                '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (define-key global-map "\C-c l" 'org-store-link)
  (define-key global-map "\C-c a" 'org-agenda)
  (define-key global-map "\C-c c" 'org-capture)

  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  )

;;----------------------------------------------------------------------------
;; REST client
;;----------------------------------------------------------------------------

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :config
  )

;;----------------------------------------------------------------------------
;; Common programming setup
;;----------------------------------------------------------------------------


;; (use-package column-enforce-mode
;;   :ensure t
;;   :hook (clojure-mode
;;          js2-mode
;;          shell-script-mode
;;          json-mode)
;;   :diminish column-enforce-mode
;;   :init
;;   (setq column-enforce-column 100
;;         column-enforce-comments nil))

;; (when (fboundp 'electric-pair-mode)
;;        (add-hook 'after-init-hook 'electric-pair-mode))

;; Compilation from Emacs.

;; (defun prelude-colorize-compilation-buffer ()
;;   "Colorize a compilation mode buffer."
;;   (interactive)
;;   ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
;;   (when (eq major-mode 'compilation-mode)
;;     (let ((inhibit-read-only t))
;;       (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)

(setq compilation-ask-about-save nil    ; just save before compiling
      compilation-always-kill t         ; just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error ; automatically scroll to first error
      )

;; Colorize output of Compilation Mode (see http://stackoverflow.com/a/3072831/355252)

;; (require 'ansi-color)

;; (add-hook 'compilation-filter-hook #'prelude-colorize-compilation-buffer)

;;----------------------------------------------------------------------------
;; Emacs LISP
;;----------------------------------------------------------------------------

(use-package lisp-mode
  :config
  (defun my:visit-ielm ()
    "Switch to default `ielm' buffer. Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'my:visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  )

;; (use-package elisp-slime-nav
;;   :ensure t
;;   :disabled
;;   :config
;;   (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;     (add-hook hook #'elisp-slime-nav-mode)))

;; (use-package paredit
;;   :ensure t
;;   :disabled
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;;   ;; enable in the *scratch* buffer
;;   (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
;;   (add-hook 'ielm-mode-hook #'paredit-mode)
;;   (add-hook 'lisp-mode-hook #'paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;; (use-package cask-mode
;;   :ensure t)

;; (use-package geiser
;;   :ensure t
;;   :config
;;   (setq geiser-active-implementations '(guile))
;;   )

;;----------------------------------------------------------------------------
;; HTML/CSS editing
;;----------------------------------------------------------------------------

(use-package web-mode
  :ensure t
  :mode ("\\.djhtml\\'"
         "\\.ejs\\'"
         "\\.erb\\'"
         "\\.html?\\'"
         "\\.j2\\'"
         "\\.jinja\\'"
         "\\.mustache\\'"
         "\\.php\\'"
         "\\.xhtml?\\'")
  :init
  ;; fix paren matching web-mode conflict for jinja-like templates
  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (setq-local electric-pair-inhibit-predicate
  ;;                         (lambda (c)
  ;;                           (if (char-equal c ?{) t (electric-pair-default-inhibit c))))))
  :config
  (setq css-indent-offset 2
        web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                                    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
        web-mode-block-padding 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-expanding t
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-pairing nil ; use smartparens instead
        web-mode-enable-auto-quoting t
        web-mode-enable-comment-keywords t
        web-mode-enable-css-colorization t
        web-mode-enable-current-column-hightlight t
        web-mode-enable-current-element-highlight t
        web-mode-engines-alist '(("django" . "\\.html\\'"))
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2)
  ;; (defun sp-web-mode-is-code-context (id action context)
  ;;   (and (eq action 'insert)
  ;;        (not (or (get-text-property (point) 'part-side)
  ;;                 (get-text-property (point) 'block-side)))))

  ;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
  )

;; (use-package web-beautify
;;   :ensure t
;;   :commands (web-beautify-html-buffer)
;;   )

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2)
  (electric-pair-mode 1))

;; (use-package scss-mode
;;   :ensure t
;;   :mode ("\\.scss\\'"
;;          "\\.sass\\'")
;;   :config
;;   (setq css-indent-offset 2)
;;   (electric-pair-mode 1))

;; (use-package emmet-mode
;;   :ensure t
;;   :config
;;   (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;;   (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;;   (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;;   )

;; CSS mode customizations

;;;; (defun my:css-mode-hook ()
;;   ;; indent automatically
;;   (local-set g-key (kbd "RET") 'newline-and-indent)

;;   ;; indentation style
;;   (setq css-indent-offset 2)
;;   )

;;----------------------------------------------------------------------------
;; Markdown & YAM
;;----------------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package yaml-mode
  :ensure t)

;;----------------------------------------------------------------------------
;; JavaScript mode configuration
;;----------------------------------------------------------------------------

(use-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :interpreter "node"
  :hook ((js2-mode . (lambda ()
                       (eldoc-mode)
                       (electric-pair-mode)
                       (flycheck-mode)
                       (company-mode))))
  :config
  (setq-default js-indent-level 4
                js2-basic-offset 4
                js2-bounce-indent-p t
                js2-chain-indent t
;;                js2-show-parse-errors nil
;;                js2-show-strict-warnings nil
                js2-strict-inconsistent-return-warning t
                js2-strict-missing-semi-warning t
                js2-strict-trailing-comma-warning nil)
  (setq-local flycheck-disabled-checkers (cl-union flycheck-disabled-checkers
                                                   '(javascript-jshint))) ; jshint doesn't work for JSX
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq-default js2-global-externs
                  '("module"
                    "exports"
                    "require"
                    "process"
                    "setTimeout"
                    "clearTimeout"
                    "setInterval"
                    "clearInterval"
                    "window"
                    "location"
                    "__dirname"
                    "console"
                    "JSON"))
  (electric-pair-mode 1)
  ;;; flycheck settings
  ;;(add-hook 'flycheck-mode-hook #'cs/use-linter-from-package-json)
  )

;; javascript eval and repl
(use-package indium
  :ensure t
  :defer t
  :diminish indium-interaction-mode
  :hook (((js2-mode rjsx-mode) . indium-interaction-mode))
  :config
  )

(use-package typescript-mode
  :ensure t)

;; typescript "IDE"

(use-package tide
  :ensure t
  :defer t
  :diminish tide-mode
  :hook (((typescript-mode) . tide-setup))
  :config
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  )

;; (use-package js2-mode
;;   :ensure t
;;   :mode ("\\.js\\'" "\\.jsx$")
;;   :interpreter ("node")
;;   :hook ((js2-mode . (lambda ()
;;                        (eldoc-mode)
;;                        (electric-pair-mode)
;;                        (flycheck-mode)
;;                        (company-mode))))
;;   :config
;;   (setq-default js-indent-level 2
;;                 js2-basic-offset 2
;;                 js2-bounce-indent-p t
;;                 js2-chain-indent t
;; ;;                js2-show-parse-errors nil
;; ;;                js2-show-strict-warnings nil
;;                 js2-strict-inconsistent-return-warning t
;;                 js2-strict-missing-semi-warning t
;;                 js2-strict-trailing-comma-warning nil)

;;   ;; (add-hook 'js-mode-hook 'js2-minor-mode)
;;   ;; (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
;;   )

;; (with-eval-after-load 'js2-mode
;;   (require 'flycheck)
;;   (add-hook 'js2-mode-hook
;;          (lambda ()
;;            (add-node-modules-path)  ;; set path to node_modules automatically
;;            (flycheck-mode 1)
;;            ))
;;   ;; disable jshint (leaving only eslint checking)
;;   (setq-default flycheck-disabled-checkers
;;              (append flycheck-disabled-checkers
;;                      '(javascript-jshint)))
;;   )

(use-package json-mode
  :ensure t
  :defer t
  :hook (json-mode . (lambda ()
                       (setq js-indent-level 2)))
  :config
  )

;;----------------------------------------------------------------------------
;; Python mode configuration
;;----------------------------------------------------------------------------

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable)
;;   ;; :bind (:map elpy-mode-map
;;   ;;             ("C-M-n" . elpy-nav-forward-block)
;;   ;;             ("C-M-p" . elpy-nav-backward-block))
;;   :config
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
;;   ;; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
;;   ;; (setq elpy-shell-echo-output nil)
;;   (setq elpy-rpc-python-command "python3")
;;   (setq elpy-rpc-timeout 2)
;;   )

(use-package python
  :ensure t
  :defer t
  :config
  (setq-default python-indent-offset 4)
  ;; (require 'flycheck)
  ;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-hook 'python-mode-hook
            (lambda ()
              ;; (setq python-shell-interpreter "ipython"
              ;;       python-shell-interpreter-args "--simple-prompt -i")

              ;; Use IPython for REPL
              ;; (setq python-shell-interpreter "jupyter"
              ;;       python-shell-interpreter-args "console --simple-prompt"
              ;;       python-shell-prompt-detect-failure-warning nil)
              ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
              ;;              "jupyter")

              (setq python-environment-virtualenv '("/usr/local/bin/python3" "-m" "venv"))

              ;; Set virtual environment when using Pyenv. (required for Jedi)
              (let ((virtualenv (and (> (length (string-trim (shell-command-to-string "pyenv virtualenv-prefix 2>/dev/null"))) 0)
                                     (string-trim (shell-command-to-string "pyenv prefix ")))))
                (setq-local python-shell-virtualenv-root virtualenv))

              ;; (setq forward-sexp-function nil)
              (setq-local comment-inline-offset 2)

              ;; (flycheck-select-checker 'python-flake8)
              ;; (flycheck-mode 1)

              ;; (yas-minor-mode)

              ;; (electric-indent-mode)

              ;; (define-key python-mode-map [remap backward-sentence] 'python-nav-backward-statement)
              ;; (define-key python-mode-map [remap forward-sentence] 'python-nav-forward-statement)
            ))
    )

(use-package pyenv-mode
  :disabled
  :ensure t
  :hook python-mode
  )

;; (use-package company-jedi
;;   :ensure t
;;   :after company
;;   :config
;;   (setq jedi:complete-on-dot t
;;         ; jedi:tooltip-method nil
;;         jedi:use-shortcuts t            ; enable goto-definition
;;         )
;;   :hook
;;   (python-mode . (lambda ()
;;                    (setq-local company-backends
;;                                '(company-jedi company-files company-capf company-keywords))
;;                    (jedi:setup)))
;;   )

;;----------------------------------------------------------------------------
;; C/C++ development
;;----------------------------------------------------------------------------

;; (use-package ggtags
;;   :ensure t
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (ggtags-mode 1)
;;                 (setq-local eldoc-documentation-function #'ggtags-eldoc-function))))
;;   (add-hook 'c-mode-common-hook 'hs-minor-mode))

;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; (defun my/c++-mode-hook ()
;;   (yas-minor-mode 1)
;;   (require 'company)
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   (company-mode)
;;   (setq c-default-style "stroustrup"
;;         c-basic-offset 4)
;;   )

;; (add-hook 'c++-mode-hook 'my/c++-mode-hook)
;; (add-hook 'c-mode-hook 'my/c++-mode-hook)

;;----------------------------------------------------------------------------
;; Allow this Emacs process to be a server for emacsclient.
;;----------------------------------------------------------------------------

(require 'server)
(unless (or (server-running-p)
            noninteractive
            (daemonp))
  (server-start))

;; Use C-x,k for killing current buffer (even if opened from emacsclient)

(defun my:kill-current-buffer ()
  (interactive)
  (if server-buffer-clients
      (let ((result (and server-clients (server-done))))
        (if (and result (car result))
            (apply 'server-switch-buffer result)
          (shell-command "open -a Terminal")))
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-x k") 'my:kill-current-buffer)

;; Customize settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme yasnippet-snippets yaml-mode which-key web-mode use-package tide super-save smex rjsx-mode restclient rainbow-mode projectile markdown-mode magit ido-vertical-mode hydra flx-ido expand-region exec-path-from-shell evil-collection diminish ace-window))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
