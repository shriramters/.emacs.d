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
(set-face-attribute 'default nil :font "JetbrainsMonoNF-14")

;; line numbers
(global-display-line-numbers-mode t)

;; setup Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-processes nil)
 '(custom-enabled-themes '(timu-caribbean))
 '(custom-safe-themes
   '("bc7d4cfb6d4bd7074a39f97f0b8a057c5b651c403950bbbc4ad35a609ad6268a" default))
 '(package-selected-packages
   '(yasnippet-snippets vterm-toggle vterm timu-caribbean-theme company typescript-mode vue-mode lsp-mode nerd-icons nerd-icons-dired treemacs treemacs-nerd-icons emmet-mode js2-mode php-mode web-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; disable annoying question modified buffers exist
(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; nerd-icons
(require 'nerd-icons)
(setq nerd-icons-font-family "JetBrainsMonoNF")

(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

;; quickly open init.el with C-c c
(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c c") 'my-edit-configuration)

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
          '(lambda ()
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
