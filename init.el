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

;; Language defaults
(require 'setup-c)
(require 'setup-perl)
(require 'setup-python)
(require 'setup-latex)

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

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Move to match if on (){}[] when pressing %, otherwise insert %.
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	((looking-at "\\s\{") (forward-list 1) (backward-char 1))
	((looking-at "\\s\}") (forward-char 1) (backward-list 1))
	((looking-at "\\s[") (forward-list 1) (backward-char 1))
	((looking-at "\\s]") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; Make sure that the file ends in a newline.
(setq require-final-newline t)

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
;;			      Tab completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a -hook for all modes where we want tab completion.
;; (add-hook 'c-mode-common-hook
;;           (function (lambda ()
;;                     (local-set-key (kbd "") 'indent-or-complete)
;;                      )))
;; (add-hook 'text-mode-hook
;;           (function (lambda ()
;;                     (local-set-key (kbd "") 'indent-or-complete)
;;                      )))
;; (add-hook 'emacs-lisp-mode-hook 
;;           (function (lambda ()
;;                     (local-set-key (kbd "") 'indent-or-complete)
;;                      )))
;; (add-hook 'LaTeX-mode-hook 
;;           (function (lambda ()
;;                     (local-set-key (kbd "") 'indent-or-complete)
;;                      )))
;; (add-hook 'TeX-mode-hook 
;;           (function (lambda ()
;;                     (local-set-key (kbd "") 'indent-or-complete)
;;                      )))

;; (defun indent-or-complete ()
;;   "Complete if point is at end of a word, otherwise indent line."
;;   (interactive)
;;   (if (looking-at "\\>")
;;     (dabbrev-expand nil)
;;   (indent-for-tab-command)
;;    ))

;; ;; The order that different completes are tested.
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-dabbrev-visible
;; 	try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         ; try-expand-all-abbrevs
;;         ; try-expand-line 
;;         ; try-expand-line-all-buffers
;;         ; try-expand-whole-kill
;;         ; try-expand-list
;;         ; try-expand-list-all-buffers
;;         ; try-complete-file-name-partially
;;         ; try-complete-file-name 
;;         ; try-complete-lisp-symbol-partially
;;         ; try-complete-lisp-symbol 
;; 	))

;; ;; TAB expands even during isearch (Ctrl-S)
;; (define-key isearch-mode-map [tab] 'isearch-yank-word)


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
 '(scroll-bar-mode (quote right))
 '(scroll-bar-position (quote left))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill (lambda nil (local-set-key (kbd "") (quote indent-or-complete))) flyspell-mode text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tool-bar-style (quote image)))

;; Compilation command
(setq compilation-scroll-output 1)

;; Copy current line

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

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

;auto-mode-alist (append (list '("\\.S$" . S-mode)
;; 			      '("\\.s$" . S-mode)
;; 			      '("\\.R$" . R-mode)
;; 			      '("\\.r$" . R-mode)
;; 	                )
;; 		      auto-mode-alist)
;; (setq-default inferior-S+6-program-name "Splus")
;; (setq-default inferior-R-program-name "R")

; (when (fboundp 'windmove-default-keybindings)
;   (windmove-default-keybindings))


; (package-initialize)


;; site-lisp packages
(require 'multiple-cursors)
(require 'expand-region)
(require 'yasnippet)
(require 'undo-tree)

(yas-global-mode 1)
(global-undo-tree-mode)


;; load keybindings
(require 'keybindings)
