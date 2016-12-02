;; init.el
;; top level initialization file for emacs

;; Set path to dependencies

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; el-get - smarter package management
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; (el-get 'sync)


;; my packages
(setq hos-packages
      (append
       ;; list of packages we use straight from official recipes
       '(git-timemachine ;; switch through different versions of file
         auto-complete ;; auto complete for emacs
         helm ;; incremental completion and selection narrowing framework
         auctex ;; most sophisticated TeX and LaTeX package for emacs
         reftex ;; references in LaTeX
         undo-tree ;; sane undoing
         yasnippet ;; snipets
         expand-region ;; context sensitive scoping
         nyan-mode ;; show nyan cat status bar
         markdown-mode ;; yep
         iy-go-to-char ;; jump to next occurrence of char
         ;; web-mode
         mmm-mode
         ;; psgml-mode
         emmet-mode ;; zen coding
         tabbar
         moe-theme
         monokai-theme
         solarized-emacs
         typescript-mode
         multiple-cursors) ;; multiple cursors
       ;; org-mode ;; organize the universe
       ;; slime ;; superior lisp interaction mode for emacs
       ;; aggressive-indent-mode ;; i love me some indentation
       ;; rainbow-delimiters ;; pretty colors for those scopes
       ;; asciidoc
       ;; adoc-mode
       ;; elpy ;; python packages
       ;; julia-mode
       ;; swiper
       ;; magit ;; better git support
       ;; epresent ;; emacs presentations with org-mode
       ;; ac-math ;; auto-complete math
       ;; projectile ;; manage projects easier
       ;; evil ;; vi emulation
       ;; fiplr ;; fuzzy file search


       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

;; py-yapf
(el-get-bundle py-yapf
  :url "https://github.com/paetzke/py-yapf.el.git"
  :features py-yapf)


(setq el-get-sources
      '((:name el-get :branch "master")

        (:name yasnippet
               :after (yas-global-mode 1))

        (:name auto-complete
               :after (progn
                        ;; (require 'yasnippet)
                        ;; (yas-global-mode 1)

                        ;; (ac-config-default)
                        (global-auto-complete-mode t)
                        (ac-flyspell-workaround)
                        ;; (defun auto-complete-mode-maybe ()
                        ;;   "No maybe for you. Only AC!"
                        ;;   (auto-complete-mode 1))
                        ;;
                        ;; LaTeX configuration
                        (add-to-list 'ac-modes 'latex-mode)
                        (add-to-list 'ac-modes 'octave-mode)
                        ;; (require 'ac-math) ; package should be installed first
                        (defun my-ac-latex-mode () ; add ac-sources for latex
                          (setq ac-sources
                                (append '(ac-source-math-unicode
                                          ac-source-math-latex
                                          ac-source-latex-commands)
                                        ac-sources)))
                        (add-hook 'LaTeX-mode-hook 'my-ac-latex-mode)
                        (setq ac-math-unicode-in-math-p t)
                        ;;
                                        ; (add-to-list 'ac-modes 'org-mode) ; auto-complete for org-mode
                        ;;
                        (require 'auto-complete-config)
                        ;; (define-key ac-menu-map (kbd "M-TAB") 'ac-next)
                        ;; (define-key ac-menu-map (kbd "<M-backtab>") 'ac-previous)
                        ))

        (:name fiplr
               :after (global-set-key (kbd "C-S-p") 'fiplr-find-file))

        (:name multiple-cursors
               :after (progn
                        (require 'multiple-cursors)
                        (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                        (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                        (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                        (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

        (:name tabbar
               :after (progn  ;;(require 'tabbar)
                        ;;(tabbar-mode t)
                        (global-set-key [M-left] 'tabbar-backward-tab)
                        (global-set-key [M-right] 'tabbar-forward-tab)))

        (:name mmm-mode
               :after (progn
                        (require 'mmm-mode)
                        (setq mmm-global-mode 'maybe)
                        ;;
                        ;; set up an mmm group for fancy html editing
                        (mmm-add-group
                         'fancy-html
                         '(
                           (html-php-tagged
                            :submode php-mode
                            :face mmm-code-submode-face
                            :front "<[?]php"
                            :back "[?]>")
                           (html-css-attribute
                            :submode css-mode
                            :face mmm-declaration-submode-face
                            :front "styleREMOVEME=\""
                            :back "\"")))
                        ;;
                        ;; What files to invoke the new html-mode for?
                        (add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
                        (add-to-list 'auto-mode-alist '("\\.phtml\\'" . html-mode))
                        (add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
                        (add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . html-mode))
                        (add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
                        ;;
                        ;; What features should be turned on in this html-mode?
                        (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
                        (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
                        (add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))
                        ;;
                        ))


        ;; (:name column-marker
        ;;        :after (progn (add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 80)))
        ;;                      (add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))))

        ;; config examples at: http://www.emacswiki.org/emacs/FillColumnIndicator
        ;;
        ;; (:name fill-column-indicator
        ;;        :after (progn (setq-default fci-rule-column 80)
        ;;                      (setq fci-rule-width 2)
        ;;                      (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
        ;;                      (global-fci-mode 1)))

        (:name helm
               :after (progn
                        (require 'helm)

                        ;; must set before helm-config,  otherwise helm use default
                        ;; prefix "C-x c", which is inconvenient because you can
                        ;; accidentially pressed "C-x C-c"
                        (setq helm-command-prefix-key "C-c h")

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
                         helm-mode-fuzzy-match t
                         helm-completion-in-region-fuzzy-match t

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

                        ;; (define-key helm-map (kbd "C-x 2") 'helm-select-2nd-action)
                        ;; (define-key helm-map (kbd "C-x 3") 'helm-select-3rd-action)
                        ;; (define-key helm-map (kbd "C-x 4") 'helm-select-4rd-action)

                        (global-set-key (kbd "M-x") 'helm-M-x)
                        (global-set-key (kbd "M-y") 'helm-show-kill-ring)
                        ;; (global-set-key (kbd "C-x b") 'helm-mini)
                        (global-set-key (kbd "C-x C-f") 'helm-find-files)
                        ;; (global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
                        ;; (global-set-key (kbd "C-c h m") 'helm-man-woman)
                        ;; (global-set-key (kbd "C-c h f") 'helm-find)
                        (global-set-key (kbd "C-c h g") 'helm-do-grep)
                        ;; (global-set-key (kbd "C-c h l") 'helm-locate)
                        ;; (global-set-key (kbd "C-c h o") 'helm-occur)
                        ;; (global-set-key (kbd "C-c h r") 'helm-resume)
                        ;; (define-key 'help-command (kbd "C-f") 'helm-apropos)
                        ;; (define-key 'help-command (kbd "r") 'helm-info-emacs)

                        ;; use helm to list eshell history
                        (add-hook 'eshell-mode-hook
                                  #'(lambda ()
                                      (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

                        ;; Save current position to mark ring
                        (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

                        (helm-mode 1)))

        ;; (:name auctex
        ;;        :after (setq texmathp-tex-commands (quote (("\\eqn"  arg-on)))))

        (:name reftex
               :after (progn (require 'reftex)
                             (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))


        ;; (:name adoc-mode
        ;;        :after (add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode)))

        (:name nyan-mode
               :after (nyan-mode))

        (:name iy-go-to-char
               :after (progn
                        (global-set-key (kbd "C-c f") 'iy-go-to-char)
                        (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
                        (global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
                        (global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)))


        ;; (:name swiper
        ;;        :after (progn
        ;;                 (global-set-key "\C-r" 'swiper)
        ;;                 (global-set-key "\C-s" 'swiper)))

        (:name expand-region
               :after (global-set-key (kbd "C-=") 'er/expand-region))

        (:name rainbow-delimiters
               :after (progn
                        (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

        (:name aggressive-indent-mode
               :after (progn
                        (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)))

        ;; (:name web-mode
        ;;        :after (progn
        ;;                 (require 'web-mode)
        ;;                 (setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
        ;;                 (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
        ;;                 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
        ;;                 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
        ;;                 (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
        ;;                 (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
        ;;                 (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
        ;;                 (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
        ;;                 (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
        ;;                 (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))))

        (:name emmet-mode
               :after (progn
                        (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
                        (add-hook 'css-mode-hook  'emmet-mode))) ;; enable Emmet's css abbreviation.

        (:name undo-tree
               :after (global-undo-tree-mode 1))))


;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync hos-packages)

;; Local units
(require 'appearance)
(require 'sane-defaults)
(require 'enhancements)
(require 'whitespace-cleanup)

;; Language defaults
(require 'init-c)
(require 'init-perl)
(require 'init-python)
(require 'init-latex)
(require 'init-html)
(require 'init-js)
; (require 'init-org-mode)
(require 'init-octave)

;; Site-lisp packages
(require 'turkish-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "087c9d7433a9e062098ef09894ea982db743172fb2d8a35b550f8ea01a4d3296" "c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab" "479eba125f9e97a0208b642a99eee1d816fa208fe3a06f73e444504beb0b17f7" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(font-use-system-font t)
 '(frame-background-mode (quote light))
 '(line-number-mode 1)
 '(load-home-init-file t t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode (quote right))
 '(scroll-bar-position (quote left))
 '(show-paren-mode t)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(tabbar-mode t nil (tabbar))
 '(text-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (local-set-key
        (kbd "")
        (quote indent-or-complete)))
     flyspell-mode text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tool-bar-style (quote image)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load keybindings last for love
(require 'keybindings)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
