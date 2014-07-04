;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
;; (add-to-list 'load-path user-emacs-directory)
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

	;; (:name auto-complete
	;;        :after (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))
        
        (:name fiplr
               :after (global-set-key (kbd "C-S-p") 'fiplr-find-file))
               
        (:name multiple-cursors
               :after (progn (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                             (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                             (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                             (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
        (:name helm
               :after (progn  
                        (require 'helm)
                        (require 'helm-config)
                        (require 'helm-eshell)
                        (require 'helm-files)
                        (require 'helm-grep)

                        (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
                        (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
                        (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

                        (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
                        (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
                        (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

                        (setq
                         helm-google-suggest-use-curl-p t
                         helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
                         helm-quick-update t ; do not display invisible candidates
                         helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
                         helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
                         helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

                         helm-split-window-default-side 'other ;; open helm buffer in another window
                         helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
                         helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                                             '(picture-mode artist-mode))
                         helm-candidate-number-limit 200 ; limit the number of displayed canidates
                         helm-M-x-requires-pattern 0     ; show all candidates when set to 0
                         helm-boring-file-regexp-list
                         '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
                         helm-ff-file-name-history-use-recentf t
                         helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
                         ido-use-virtual-buffers t      ; Needed in helm-buffers-list
                         helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
                         )

                        (define-key helm-map (kbd "C-x 2") 'helm-select-2nd-action)
                        (define-key helm-map (kbd "C-x 3") 'helm-select-3rd-action)
                        (define-key helm-map (kbd "C-x 4") 'helm-select-4rd-action)

                        (global-set-key (kbd "M-x") 'helm-M-x)
                        (global-set-key (kbd "M-y") 'helm-show-kill-ring)
                        (global-set-key (kbd "C-x b") 'helm-mini)
                        (global-set-key (kbd "C-x C-f") 'helm-find-files)
                        (global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
                        (global-set-key (kbd "C-c h m") 'helm-man-woman)
                        (global-set-key (kbd "C-c h f") 'helm-find)
                        (global-set-key (kbd "C-c h l") 'helm-locate)
                        (global-set-key (kbd "C-c h o") 'helm-occur)
                        (global-set-key (kbd "C-c h r") 'helm-resume)
                        (define-key 'help-command (kbd "C-f") 'helm-apropos)
                        (define-key 'help-command (kbd "r") 'helm-info-emacs)

                        ;; use helm to list eshell history
                        (add-hook 'eshell-mode-hook
                                  #'(lambda ()
                                      (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))
                        
                        ;; Save current position to mark ring
                        (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

                        (helm-mode 1)))

        (:name nyan-mode
               :after (nyan-mode))

        (:name expand-region
               :after (global-set-key (kbd "C-=") 'er/expand-region))

	(:name undo-tree
	       :after (global-undo-tree-mode 1))))

;; my packages
(setq nrs-packages
      (append
       ;; list of packages we use straight from official recipes
       '(magit
         git-timemachine ;; switch through different versions of file
         auto-complete ;; auto complete for emacs
         epresent ;; emacs presentations with org-mode
         helm
         projectile
         undo-tree
         yasnippet ;; snipets
         expand-region
         nyan-mode ;; show nyan cat status bar
         evil ;; vi emulation
         fiplr ;; fuzzy file search
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
