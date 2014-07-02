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


(require 'appearance)
(require 'sane-defaults)
(require 'enhancements)
(require 'magit)

;; Language defaults
(require 'init-c)
(require 'init-perl)
(require 'init-python)
(require 'init-latex)

(require 'init-org-mode)

;; Get ()-matching
;; (require 'paren)
;; (show-paren-mode 1)
;; (setq show-paren-delay 0)

(defun jump-to-register-other (reg)
  (other-window 1)
  (jump-to-register reg)
;;  (hilit-recenter (/ (window-height) 2))
)

(defun jump-to-register-here (reg)
  (jump-to-register reg)
;;  (hilit-recenter (/ (window-height) 2))
)

(defun next-buffer () 
  "Go to the buffer which is at the end of the buffer-list. 
   This is the symmetric of burry-buffer." 
  (interactive)
  (switch-to-buffer (nth (- (length (buffer-list)) 1) (buffer-list))))

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
(setq default-major-mode 'text-mode)

;; Spelling

;; Do running spell check in text mode.
;;(setq ispell-program-name "/usr/bin/aspell")
(add-hook 'text-mode-hook  'flyspell-mode)

;; Set the dictionary for the spell check.
;;flamf(setq flyspell-mode-hook
;;      '(lambda () "Sets the dictionary for flyspell on startup."
;;	 (ispell-change-dictionary "svenska")
;;))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		          Small functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UNIX-DOS-UNIX end of line conversions
(defun dos-unix ()
	(interactive)
	(goto-char (point-min))
	(while (search-forward "\r" nil t) (replace-match "")))

(defun unix-dos ()
	(interactive)
	(goto-char (point-min))
	(while (search-forward "\n" nil t) (replace-match "\r\n")))

;; Poor mans TTCN reader
(defun ttcn ()
  "Makes a trivial reformating of TTCN logs."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "{" nil t) (replace-match "{\n\t"))
  (goto-char (point-min))
  (while (search-forward "," nil t) (replace-match ",\n\t"))
  (goto-char (point-min)))


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

;; Word count

(defun word-count (&optional filename)
  "Returns the word count of the current buffer.  If `filename' is not nil, returns the word count of that file."
  (interactive)
  (save-some-buffers) ;; Make sure the current buffer is saved
  (let ((tempfile nil))
    (if (null filename)
        (progn
          (let ((buffer-file (buffer-file-name))
                (lcase-file (downcase (buffer-file-name))))
            (if (and (>= (length lcase-file) 4) (string= (substring lcase-file -4 nil) ".tex"))
                ;; This is a LaTeX document, so DeTeX it!
                (progn
                  (setq filename (make-temp-file "wordcount"))
                  (shell-command-to-string (concat "detex < " buffer-file " > " filename))
                  (setq tempfile t))
              (setq filename buffer-file)))))
    (let ((result (car (split-string (shell-command-to-string (concat "wc -w " filename)) " "))))
      (if tempfile
          (delete-file filename))
      (message (concat "Word Count: " result))
      )))

(setq exec-path (append exec-path '("/usr/bin")))

;; (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/monokai-theme")
(load-theme 'monokai t)

;; site-lisp packages
(require 'multiple-cursors)
(require 'expand-region)
(require 'yasnippet)
(require 'undo-tree)
(require 'auto-complete)
(require 'evil)
(require 'fiplr)
(require 'projectile)

;; site-lisp package options
(setq yas/root-directory "~/.emacs.d/site-lisp/yasnippet/snippets/")
(yas-global-mode 1)
;; (yas/load-directory yas/root-directory)

(global-undo-tree-mode)

(defun nolinum () (global-linum-mode 0))

(add-hook 'org-mode-hook 'nolinum)

;; load keybindings
(require 'keybindings)

;; activate evil-mode because some men just want to watch the world burn
;; (evil-mode)
