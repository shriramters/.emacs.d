;; disable menu on startup
(menu-bar-mode -1)

;; disable tools on startup
(tool-bar-mode -1)

;; disable scroll bar on startup
(toggle-scroll-bar -1)

;; inhibit splash screen
(setq inhibit-splash-screen t)

;; disable backup and lockfiles
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; set default font and size
(set-face-attribute 'default nil :font "-JB-JetBrainsMono Nerd Font Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;; uncomment the next line to set font scaling. 160 means 1.6x.
;; (set-face-attribute 'default nil :height 160)

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

;; transpose line with above
(global-set-key (kbd "M-<up>") 'transpose-lines)

;; setup Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; list the packages you want
(setq package-list
	  '(company lsp-mode treemacs treemacs-nerd-icons timu-caribbean-theme vterm
				vterm-toggle yasnippet yasnippet-snippets geiser geiser-guile
				paredit clang-format gnu-elpa-keyring-update web-mode transpose-frame
                multiple-cursors string-inflection
                ))

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
;; (setq nerd-icons-font-family "Fira Mono Nerd Font")

(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

;; c++ IDE
;; set clang path
(setq company-clang-executable "/usr/bin/clang")
(setq lsp-clients-clangd-executable "/usr/bin/clangd")

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
(use-package clang-format)
(defun clang-format-save-hook()
  "Create a buffer local save hook to apply `clang-format-buffer'"
  ;; Only format if .clang-format is found
  (when (locate-dominating-file "." ".clang-format")
    (clang-format-buffer))
  ;; Continue to save
  nil)

(define-minor-mode clang-format-on-save-mode
  "Buffer-local mode to enable/disable automated clang format on save"
  :lighter " ClangFormat"
  (if clang-format-on-save-mode
      (add-hook 'before-save-hook 'clang-format-save-hook nil t)
    (remove-hook 'before-save-hook 'clang-format-save-hook t)))

;; Create a globalized minor mode to
;;   - Auto enable the above mode only for C/C++, or glsl in your case
;;   - Be able to turn it off globally if needed
(define-globalized-minor-mode clang-format-auto-enable-mode clang-format-on-save-mode
  (lambda()(clang-format-on-save-mode t))
  :predicate '(c-mode c++-mode c-or-c++-mode))
(clang-format-auto-enable-mode t)

(setq lsp-completion-provider :none)

;; Change Log Mode
(add-hook 'change-log-mode-hook
	  #'(lambda ()
		  (setq indent-tabs-mode nil)
	      (make-local-variable 'tab-width)
	      (make-local-variable 'left-margin)
	      (setq tab-width 2
				left-margin 2)))

;; Paredit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; string-inflection
(require 'string-inflection)
(global-set-key (kbd "s-<tab>") 'string-inflection-all-cycle)

(load-theme 'timu-caribbean t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(package-selected-packages
   '(string-inflection multiple-cursors transpose-frame ox-epub yasnippet-snippets web-mode vterm-toggle use-package treemacs-nerd-icons timu-caribbean-theme spacious-padding paredit lsp-mode gnu-elpa-keyring-update geiser-guile company clang-format centaur-tabs ac-geiser)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
