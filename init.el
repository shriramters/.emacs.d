;; disable menu on startup
(menu-bar-mode -1)

;; disable tools on startup
(tool-bar-mode -1)

;; disable scroll bar on startup
(toggle-scroll-bar -1)

;; inhibit splash screen
(setq inhibit-splash-screen t)

;; disable backup files
(setq make-backup-files nil)

;; set default font and size
(set-face-attribute 'default nil :font "-*-JetBrains Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; line numbers
(global-display-line-numbers-mode t)


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


;; setup Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; list the packages you want
(setq package-list '(company lsp-mode treemacs treemacs-nerd-icons timu-caribbean-theme vterm vterm-toggle yasnippet yasnippet-snippets))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; lsp-mode
(require 'lsp-mode)
(add-hook 'typescript-mode-hook #'lsp-deferred)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; use tab to autocomplete
;; (with-eval-after-load 'company
;;   (define-key company-active-map (kbd "RET") nil)
;;   (define-key company-active-map [12] nil)
;;   (define-key company-active-map [return] nil)
;;   (define-key company-active-map (kbd "tab") 'company-complete-selection)
;;   (define-key company-active-map [tab] 'company-complete-selection)
;;   )

;; vterm
(require 'vterm)
;; disable line numbers in vterm
(add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode -1)))

;; vterm-toggle
(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)

;; you can cd to the directory where your previous buffer file exists
;; after you have toggle to the vterm buffer with `vterm-toggle'.
(define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

;Switch to next vterm buffer
(define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
;Switch to previous vterm buffer
(define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward)

;; treemacs
(global-set-key (kbd "s-t") 'treemacs)
(global-set-key (kbd "s-T") 'treemacs-add-and-display-current-project-exclusively)
(setq treemacs-follow-mode t)

;; nerd-icons
(require 'nerd-icons)
(setq nerd-icons-font-family "JetBrainsMonoNF")

(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

;; c++ IDE
;; set clang path
(setq company-clang-executable "/usr/bin/clang-16")
(setq lsp-clients-clangd-executable "/usr/bin/clangd-16")

(require 'company)
(require 'lsp-mode)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c++-mode-hook
          #'(lambda ()
	     (set (make-local-variable 'company-backends)
		  '((company-files :with company-yasnippet)
		    (company-capf :with company-yasnippet)
		    (company-dabbrev-code company-gtags company-etags company-keywords :with company-yasnippet)
		    (company-dabbrev :with company-yasnippet)
		    (company-lsp :with company-yasnippet)))))
;; c++ style
(defun my-c++-mode-hook ()
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(setq lsp-completion-provider :none)
