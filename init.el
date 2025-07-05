;; MACOS caveats
;; eat-compile-terminfo
;; (when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;; ANY OS Caveats
;; all-the-icons-install-fonts
;; treesitter-install-grammar

;; disable menu on startup
;; (menu-bar-mode -1)

;; disable tools on startup
(tool-bar-mode -1)

;; disable scroll bar on startup
(toggle-scroll-bar -1)

;; inhibit splash screen
(setq inhibit-splash-screen t)

;; disable backup and lockfiles
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; disable native-comp errors
(setq native-comp-async-report-warnings-errors nil)

;; set font
(set-face-attribute 'default nil :font "-*-JetBrains Mono-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;; uncomment the next line to set font scaling. 160 means 1.6x.
(set-face-attribute 'default nil :height 160)

;; line numbers(buffer) and column numbers(modeline)
(global-display-line-numbers-mode t)
(setq column-number-mode t)

;; set tab-width to a reasonable 4
(setq-default tab-width 4)

;; insert spaces and not tabs
(setq-default indent-tabs-mode nil)

;; save command history
(savehist-mode)

;; disable annoying question modified buffers exist
(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; quickly open init.el with C-c c
(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c c") 'my-edit-configuration)

;; enumerate all installed fonts
(defun my-enumerate-fonts ()
  "Enumerate installed fonts"
  (interactive)
  (dolist (font (x-list-fonts "*"))
    (insert (format "%s\n" font))))
(global-set-key (kbd "C-c f") 'my-enumerate-fonts)

;; windmove
(global-set-key (kbd "M-s-<left>")  'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>")    'windmove-up)
(global-set-key (kbd "M-s-<down>")  'windmove-down)

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

(global-set-key (kbd "M-<up>")   'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; open terminal with C-`
(defun my/toggle-eat-other-window ()
  "Show *eat* in the other window, or hide it if it’s already visible."
  (interactive)
  (let* ((buf   (get-buffer-create "*eat*"))     ; reuse the same Eat buffer
         (win   (get-buffer-window buf)))
    (if win
        ;; It’s on-screen → just close that window
        (delete-window win)
      ;; Not visible → create (or switch to) it with the usual helper
      (eat-other-window))))

(global-set-key (kbd "C-`") #'my/toggle-eat-other-window)

(define-key prog-mode-map (kbd "C-c d") #'flymake-show-buffer-diagnostics)

;; setup Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; list the packages you want
(setq package-list '(company treemacs all-the-icons treemacs-all-the-icons
                             yasnippet doom-themes solaire-mode exec-path-from-shell
                             eat centaur-tabs gnu-elpa-keyring-update magit))

; activate all the packages (in particular autoloads)
(package-initialize)

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)

  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (add-to-list 'exec-path-from-shell-variables "GPG_TTY")

  ;; Run the function to import the variables
  (exec-path-from-shell-initialize))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'centaur-tabs)
(centaur-tabs-mode t)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-height 32)

;; tree-sitter
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
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; treemacs
(global-set-key (kbd "s-t") 'treemacs)
(global-set-key (kbd "s-T") 'treemacs-add-and-display-current-project-exclusively)
(setq treemacs-follow-mode t)
(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

;; disable line numbers for certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eat-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; themeing
(require 'doom-themes)
(load-theme 'doom-tomorrow-night t)
;;(load-theme 'doom-homage-black t)
(solaire-global-mode +1)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)

;; company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; go-eglot
(require 'project)
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; Optional: load other packages before eglot to enable eglot integrations.
(require 'company)
(require 'yasnippet)

(require 'go-ts-mode)
(require 'eglot)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(setq-default go-ts-mode-indent-offset 4)
(global-set-key (kbd "C-c e r") #'eglot-rename)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-before-save ()
(add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-ts-mode-hook #'eglot-format-buffer-before-save)
