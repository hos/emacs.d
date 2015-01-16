;;; whitespace-cleanup.el --- perform whitespace cleanup on save by @skeeto

;;; Commentary:

;; Maybe this should be a minor mode?

;;; Code:

(require' whitespace)

;; Modify whitespace defaults so it doesn't delete empty lines at the
;; beginning and the end

;; (setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-style '(face tabs lines-tail trailing))

;; (custom-set-faces
;;  '(my-tab-face            ((((class color)) (:background "grey10"))) t)
;;  '(my-trailing-space-face ((((class color)) (:background "gray10"))) t)
;;  '(my-long-line-face ((((class color)) (:background "gray10"))) t))

;; (add-hook 'font-lock-mode-hook
;;           (function
;;            (lambda ()
;;              (setq font-lock-keywords
;;                    (append font-lock-keywords
;;                            '(("\t+" (0 'my-tab-face t))
;;                              ("^.\\{81,\\}$" (0 'my-long-line-face t))
;;                              ("[ \t]+$"      (0 'my-trailing-space-face t))))))))

(defvar-local whitespace-cleanup-p t
  "When true, do `whitespace-cleanup' when the current buffer is saved.")

(defun toggle-whitespace-cleanup ()
  "Turn the `whitespace-cleanup-safe' hook on and off."
  (interactive)
  (setq whitespace-cleanup-p (not whitespace-cleanup-p))
  (message "whitespace-cleanup %s"
           (if whitespace-cleanup-p "enabled" "disabled")))

(defun whitespace-cleanup-safe ()
  "Run `whitespace-cleanup' only when it makes sense to do so."
  (when (and (not buffer-read-only) whitespace-cleanup-p)
    ;; turn off and on to work around Emacs bug #4069
    (whitespace-turn-on)
    (whitespace-turn-off)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'whitespace-cleanup-safe)
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

(provide 'whitespace-cleanup)

;;; whitespace-cleanup.el ends here
