;; Wheel mouse moves up and down 4 lines
;; (define-key global-map [mouse-4] (lambda () (interactive) (scroll-down 4)))
;; (define-key global-map [mouse-5] (lambda () (interactive) (scroll-up 4)))


;; (global-set-key [control mouse-4] 'bury-buffer)
;; (global-set-key [control mouse-5] 'next-buffer)

;; (global-set-key [mouse-4] 'tabbar-backward-tab)
;; (global-set-key [mouse-5] 'tabbar-forward-tab)

;; Easier buffer switching
;; (global-set-key [home] 'next-buffer)
;; (global-set-key [end]  'bury-buffer)

(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'bury-buffer)

;; Meta-up/down to do Page Up and Page Down, as the regular Page Up and
;; Page down does not repeat, making it tedious to scroll large documents.
;; (global-set-key [M-up] 'scroll-down)
;; (global-set-key [M-down] 'scroll-up)

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;; Enable position saving through shortcuts.
;; Save current position with  Ctrl-F1 Ctrl-F2 Ctrl-F3 and Ctrl-F4
;; (global-set-key [C-f1] '(lambda () (interactive) (point-to-register ?1)))
;; (global-set-key [C-f2] '(lambda () (interactive) (point-to-register ?2)))
;; (global-set-key [C-f3] '(lambda () (interactive) (point-to-register ?3)))
;; (global-set-key [C-f4] '(lambda () (interactive) (point-to-register ?4)))


;; Move to saved position with F1 F2 F3 and F4
;; (global-set-key [f1] '(lambda () (interactive) (jump-to-register-here ?1)))
;; (global-set-key [f2] '(lambda () (interactive) (jump-to-register-here ?2)))
;; (global-set-key [f3] '(lambda () (interactive) (jump-to-register-here ?3)))
;; (global-set-key [f4] '(lambda () (interactive) (jump-to-register-here ?4)))

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

(global-set-key (kbd "C-%") 'match-paren)

;;(global-set-key [f1] 'ansi-term)

;; Put the cursor over the function/variable that you want to lookup.
;; Press F10, and then Enter to confirm the search. (And give the location
;; of the TAGS-file if needed.)
;; Shift-F10 will open the matches in another frame.
;; Then use F9 and F11 to browse back and forth between the matches.

;; (global-set-key [f9]  'pop-tag-mark)
;; (global-set-key [f10] 'find-tag)
;; (global-set-key [S-f10] 'find-tag-other-frame)
;; (global-set-key [f11] "\C-u\M-.")

;; toggling comments
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
;; (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)

(global-set-key (kbd "M-W") 'copy-and-comment-region)

;; kill whole line
(global-set-key (kbd "C-S-d") 'kill-whole-line)

;; copy whole line
(global-set-key (kbd "C-S-t") 'copy-line)

;; join line - it seems M-^ is already bound to this
;; (global-set-key (kbd "C-S-j") 'join-line)

;; word count
(global-set-key [f6] 'word-count)

;; fullscreen
(global-set-key [(meta return)] 'toggle-fullscreen)

;; compilation
;; (global-set-key [f5] 'compile)

;; run kmacro
;; (global-set-key [f7] 'kmacro-call-macro)

;; that dirty habit
(global-set-key [f2] 'save-buffer)

;; kill whitespace
(global-set-key (kbd "C-S-e") 'whack-whitespace)

;; Ctrl-Tab to spellcheck the word under the cursor.
;; (global-set-key [C-tab] 'ispell-word)

;; Moving lines up & down
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "C-S-o") 'insert-line-before)
(global-set-key (kbd "C-c C-o") 'insert-line-after)
;; (global-set-key (kbd "C-S-M-o") 'open-line)

(global-set-key (kbd "M-p") 'toggle-letter-case)

(provide 'keybindings)
