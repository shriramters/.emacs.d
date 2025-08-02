;;; Package Management Setup

;; Configure package archives.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize the package system.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;;; Custom Functions

(defun my-kill-emacs ()
  "Save some buffers, then exit unconditionally."
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun my-enumerate-fonts ()
  "Enumerate installed fonts in a new buffer."
  (interactive)
  (let ((font-buffer (get-buffer-create "*Available Fonts*")))
    (with-current-buffer font-buffer
      (erase-buffer)
      (dolist (font (x-list-fonts "*"))
        (insert (format "%s\n" font)))
      (display-buffer font-buffer))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/toggle-eat-other-window ()
  "Show *eat* in the other window, or hide it if it's already visible."
  (interactive)
  (let* ((buf   (get-buffer-create "*eat*"))
         (win   (get-buffer-window buf)))
    (if win
        (delete-window win)
      (eat-other-window))))

(defun eglot-format-buffer-before-save ()
  "Add a hook to format the buffer with eglot before saving."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))


;;; Core Emacs Configuration

(use-package emacs
  :init
  ;; These settings are applied before Emacs starts up fully.
  (setq inhibit-splash-screen t)
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (setq native-comp-async-report-warnings-errors nil)

  :config
  ;; These settings are applied after Emacs is loaded.
  ;; UI Customization
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)

  ;; Font settings
  (if (find-font (font-spec :name "JetBrainsMono Nerd Font Mono"))
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font Mono")
    (message "JetBrainsMono Nerd Font Mono font not found, using default."))
  (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font Mono"))

  ;; Editor behavior
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq column-number-mode t)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers in specific modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  eat-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

  :bind (("C-x C-c" . my-kill-emacs)
         ("C-c c" . my-edit-configuration)
         ("C-c f" . my-enumerate-fonts)
         ("M-<up>" . move-line-up)
         ("M-<down>" . move-line-down)
         ("M-s-<left>"  . windmove-left)
         ("M-s-<right>" . windmove-right)
         ("M-s-<up>"    . windmove-up)
         ("M-s-<down>"  . windmove-down)
         ("C-`" . my/toggle-eat-other-window)))


;;; External Package Configurations

;; Load theme and visual packages first to prevent white flash
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t)
  ;; (doom-themes-treemacs-config) ;; DISABLED to prevent override
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  ;; (doom-themes-treemacs-theme "doom-atom") ;; DISABLED to prevent override
  )

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

;; Show git blame info in the fringe
(use-package blamer
  :ensure t
  :after doom-themes ;; Ensure the theme is loaded before configuring the face
  :config
  (set-face-attribute 'blamer-face nil :inherit 'font-lock-comment-face :italic t)
  (global-blamer-mode 1))

;; Save command history across sessions
(use-package savehist
  :ensure t
  :config
  (savehist-mode 1))

;; Ensure shell environment variables are available in Emacs on macOS
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (add-to-list 'exec-path-from-shell-variables "GPG_TTY")
  (exec-path-from-shell-initialize))

;; A powerful Git client
(use-package magit
  :ensure t)

;; A modern tab bar
(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-height 32))

;; Provides icons for many packages
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  ;; Check if the font is findable by Emacs before trying to install.
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts)))

;; A file and project explorer tree
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-follow-mode t)
  :bind
  (("s-t" . treemacs)
   ("s-T" . treemacs-add-and-display-current-project-exclusively)))

;; Icon integration for treemacs
(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

;; Snippet expansion engine
(use-package yasnippet
  :ensure t)

;; Text completion framework
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; A mode for Google's Protocol Buffers
(use-package protobuf-mode
  :ensure t)

;; An Emacs terminal emulator
(use-package eat
  :ensure t)

;; Keeps the GPG keyring for ELPA up to date
(use-package gnu-elpa-keyring-update
  :ensure t)


;;; Programming Language and Tooling Setup

;; Tree-sitter for more advanced syntax parsing
(use-package treesit
  :config
  ;; NOTE: You must manually install the language grammars for tree-sitter.
  ;; Run M-x treesit-install-language-grammar and select the language (e.g., 'go').
  ;; This is a one-time setup step per language.
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (proto "https://github.com/mitchellh/tree-sitter-proto")))
  ;; Loop through the list and install any missing grammars.
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;; Org mode configuration
(use-package org
  :config
  ;; Enable beamer export for presentations
  (require 'ox-beamer))

;; Flymake diagnostics keybinding
(use-package flymake
  :bind (:map prog-mode-map
              ("C-c d" . flymake-show-buffer-diagnostics)))

;; Language Server Protocol client
(use-package eglot
  :ensure t
  :config
  ;; Associate major modes with language server executables.
  ;; You must have the language server installed on your system.
  (setq eglot-server-programs
        '((go-ts-mode . ("gopls"))
          ;; Uncomment and customize for other languages, e.g.:
          ;; (python-ts-mode . ("pyright"))
          ))
  :bind (("C-c e r" . eglot-rename)))

;; Go mode using tree-sitter
(use-package go-ts-mode
  :ensure t
  :mode ("\\.go\\'" . go-ts-mode)
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-format-buffer-before-save))
  :config
  ;; Custom function to find Go project root (go.mod)
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module)
  :custom
  (go-ts-mode-indent-offset 4))

;;; Debug Adapter Protocol client
(use-package dape
  :ensure t
  :commands (dape)
  :config
  ;; C++ configuration using GDB
  (add-to-list 'dape-configs
               '(gdb-debug
                 :modes (c-mode c++-mode)
                 :command "gdb"
                 :command-args '("--interpreter=dap")
                 :request "launch"
                 :type "cppdbg"
                 :cwd dape-cwd
                 :program (lambda () (read-file-name "Path to executable: " dape-cwd))
                 :stopOnEntry t))

  ;; Go configuration using Delve
  (add-to-list 'dape-configs
               '(go-dlv-debug
                 :modes (go-mode)
                 :command "dlv"
                 :command-args '("dap")
                 :request "launch"
                 :type "go"
                 :cwd dape-cwd
                 :program "${fileDirname}")))


;;; Finalization

;; Prevent Emacs from writing customizations to this file.
;; All configuration should be managed with use-package.
(setq custom-file (make-temp-file "emacs-custom-"))

;;; End of init.el
