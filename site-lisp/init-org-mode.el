(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org-mode" (expand-file-name
                                             "site-lisp" dotfiles-dir))))
;; Load up Org Mode and Babel
(require 'org-install)
(require 'ob-tangle)
(require 'org-element)


(org-babel-do-load-languages
    'org-babel-load-languages '((python . t) (R . t)))

;; (defun my-org-confirm-babel-evaluate (lang body)
;;   (not (string= lang "python")))  ; don't ask for python
;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Don't ask for confirmation when running code
(setq org-confirm-babel-evaluate nil)

;; Latex for previous versions
;; (require 'org-latex)
;; (setq org-export-latex-listings 'minted)
;; (add-to-list 'org-export-latex-packages-alist '("" "minted"))

;; Include the latex-exporter
(require 'ox-latex)
;; Add minted to the defaults packages to include when exporting.
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
;; (setq org-latex-listings 'minted)

;; fontify
(setq org-src-fontify-natively t)

;; Shift-arrow bindings on timestamps don't work for some reason, workaround
(setq org-time-stamp-rounding-minutes '(0 1))
(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "S-<up>") 'org-timestamp-up)
             (local-set-key (kbd "S-<down>") 'org-timestamp-down)
             (local-set-key (kbd "S-<left>") 'org-timestamp-up-day)
             (local-set-key (kbd "S-<right>") 'org-timestamp-down-day)))

;; disable linum-mode in org-mode, due to slowing down
(add-hook 'org-mode-hook '(lambda () (linum-mode 0)))

(provide 'init-org-mode)
