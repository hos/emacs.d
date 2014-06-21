;; Wheel mouse moves up and down 4 lines
;; (define-key global-map [mouse-4] (lambda () (interactive) (scroll-down 4)))
;; (define-key global-map [mouse-5] (lambda () (interactive) (scroll-up 4)))

;; Wheel mouse +  Control change buffer we are looking at
(defun next-buffer () 
  "Go to the buffer which is at the end of the buffer-list. 
   This is the symmetric of burry-buffer." 
  (interactive)
  (switch-to-buffer (nth (- (length (buffer-list)) 1) (buffer-list))))

(define-key global-map [(control mouse-4)] 'next-buffer)
(define-key global-map [(control mouse-5)] 'bury-buffer)


;; Home and End selects buffer
(global-set-key [home] 'next-buffer)
(global-set-key [end]  'bury-buffer)

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;; Enable position saving through shortcuts.
;; Save current position with  Ctrl-F1 Ctrl-F2 Ctrl-F3 and Ctrl-F4
(global-set-key [C-f1] '(lambda () (interactive) (point-to-register ?1)))
(global-set-key [C-f2] '(lambda () (interactive) (point-to-register ?2)))
(global-set-key [C-f3] '(lambda () (interactive) (point-to-register ?3)))
(global-set-key [C-f4] '(lambda () (interactive) (point-to-register ?4)))


;; Move to saved position with F1 F2 F3 and F4
(global-set-key [f1] '(lambda () (interactive) (jump-to-register-here ?1)))
(global-set-key [f2] '(lambda () (interactive) (jump-to-register-here ?2)))
(global-set-key [f3] '(lambda () (interactive) (jump-to-register-here ?3)))
(global-set-key [f4] '(lambda () (interactive) (jump-to-register-here ?4)))


;; Meta-up/down to do Page Up and Page Down, as the regular Page Up and 
;; Page down does not repeat, making it tedious to scroll large documents.
(global-set-key [M-up] 'scroll-down)
(global-set-key [M-down] 'scroll-up)

;; Make changes since the file was opened stand out in red.
;; Press F12 to toggle on and off
;;(global-set-key [f12] 'highlight-changes-mode)


;; Highlight based on regexps
(global-set-key [M-f1] 'highlight-regexp)
(global-set-key [M-f2] 'highlight-lines-matching-regexp)
(global-set-key [M-f3] 'hi-lock-mode)
(global-set-key [M-f4] 'hi-lock-write-interactive-patterns)

;; Make changes since the file was opened stand out in red.
;; Press F12 to toggle on and off
;;(global-set-key [f12] 'highlight-changes-mode)
;;(add-hook 'c-mode-common-hook   'highlight-changes-mode)    ; in C-mode
;;(add-hook 'emacs-lisp-mode-hook 'highlight-changes-mode)    ; in Lisp-mode
;;(add-hook 'text-mode-hook       'highlight-changes-mode)    ; in Text-mode

;; Auto-fill in text mode
;;(add-hook 'text-mode-hook
;;         '(lambda ()
;;             (turn-on-auto-fill)
;;             (auto-fill-mode 1)
;;             ))
;; Move to match if on (){}[] when pressing %, otherwise insert %.

(global-set-key "%" 'match-paren)

;;(global-set-key [f1] 'ansi-term)

;; Put the cursor over the function/variable that you want to lookup.
;; Press F10, and then Enter to confirm the search. (And give the location
;; of the TAGS-file if needed.)
;; Shift-F10 will open the matches in another frame.
;; Then use F9 and F11 to browse back and forth between the matches.

(global-set-key [f9]  'pop-tag-mark)
(global-set-key [f10] 'find-tag)
(global-set-key [S-f10] 'find-tag-other-frame)
(global-set-key [f11] "\C-u\M-.")

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; multiple-cursors bindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand-region bindings
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-S-d") 'kill-whole-line)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Easier buffer switching
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'bury-buffer)

(global-set-key (quote [f6]) 'word-count)
;; (global-set-key (quote [f7]) 'linum-mode)

(global-set-key [(meta return)] 'toggle-fullscreen)

(global-set-key (kbd "C-S-t") 'copy-line)

(global-set-key [f5] 'compile)

;; Ctrl-Tab to spellcheck the word under the cursor.
;; (global-set-key [C-tab] 'ispell-word)


(provide 'keybindings)
