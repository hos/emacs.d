;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

(let ((default-directory site-lisp-dir))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; el-get 
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(setq el-get-sources
      '((:name el-get :branch "master")

	(:name yasnippet
	       :after (yas-global-mode 1))

	(:name auto-complete
	       :after (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))
        
        (:name fiplr
               :after (global-set-key (kbd "C-S-p") 'fiplr-find-file))
               
        (:name multiple-cursors
               :after (progn (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                             (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                             (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                             (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
        
        (:name expand-region
               :after (global-set-key (kbd "C-=") 'er/expand-region))

	(:name undo-tree
	       :after (global-undo-tree-mode 1))))

;; my packages
(setq nrs-packages
      (append
       ;; list of packages we use straight from official recipes
       '(magit
         auto-complete
         projectile
         undo-tree
         yasnippet
         expand-region
         evil
         fiplr
         org-mode
         multiple-cursors)
       
       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync nrs-packages)

;; Local units
(require 'appearance)
(require 'sane-defaults)
(require 'enhancements)


;; Language defaults
(require 'init-c)
(require 'init-perl)
(require 'init-python)
(require 'init-latex)

(require 'init-org-mode)

;; Site-lisp packages
(require 'turkish-mode)

(defun jump-to-register-other (reg)
  (other-window 1)
  (jump-to-register reg)
;;  (hilit-recenter (/ (window-height) 2))
)

(defun jump-to-register-here (reg)
  (jump-to-register reg)
;;  (hilit-recenter (/ (window-height) 2))
)

;;(add-hook 'c-mode-common-hook   'highlight-changes-mode)    ; in C-mode
;;(add-hook 'emacs-lisp-mode-hook 'highlight-changes-mode)    ; in Lisp-mode
;;(add-hook 'text-mode-hook       'highlight-changes-mode)    ; in Text-mode

;; Auto-fill in text mode
;;(add-hook 'text-mode-hook
;;         '(lambda ()
;;             (turn-on-auto-fill)
;;             (auto-fill-mode 1)
;;             ))

;; Make text-mode the default mode, so that we can use the tab-completion
;; feature in files that don't have an extension.
;; (setq default-major-mode 'text-mode)

;; Spelling
;; Do running spell check in text mode.
;;(setq ispell-program-name "/usr/bin/aspell")
;; (add-hook 'text-mode-hook  'flyspell-mode)

;; Set the dictionary for the spell check.
;;flamf(setq flyspell-mode-hook
;;      '(lambda () "Sets the dictionary for flyspell on startup."
;;	 (ispell-change-dictionary "svenska")
;;))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(font-use-system-font t)
 '(line-number-mode 1)
 '(load-home-init-file t t)
 '(menu-bar-mode nil)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-special-blocks org-vm org-wl org-w3m org-drill)))
 '(scroll-bar-mode (quote right))
 '(scroll-bar-position (quote left))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill (lambda nil (local-set-key (kbd "") (quote indent-or-complete))) flyspell-mode text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tool-bar-style (quote image)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Compilation command
(setq compilation-scroll-output 1)

;; Toggle fullscreen

(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(setq exec-path (append exec-path '("/usr/bin")))

;; (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/monokai-theme")
(load-theme 'monokai t)

(defun nolinum () (global-linum-mode 0))

(add-hook 'org-mode-hook 'nolinum)

;; load keybindings
(require 'keybindings)

;; activate evil-mode because some men just want to watch the world burn
;; (evil-mode)
