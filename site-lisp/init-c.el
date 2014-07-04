;; Hungry delete is nice since we use spaces instead of tabs.
(setq c-hungry-delete-key t) 
;;(setq-default c-basic-offset 2)

;; Let emacs insert newlines automatically whenever it can.
;;(setq c-auto-newline 0)
;;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 0)))
;; Set the K&R indentation style when starting C-mode.

(setq c-default-style "k&r"
          c-basic-offset 2)


;; (add-hook 'c-mode-hook
;;           '(lambda ()
;;              (c-set-style "k&r")
;;              (auto-fill-mode 1)
;;              ))

;;(setq-default c-basic-offset 2)
;;(setq tab-width 2)

;; (setq-default c-basic-offset 2
;;                   tab-width 2
;;                   indent-tabs-mode t)

(provide 'init-c)
