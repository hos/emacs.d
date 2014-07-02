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

(require 'org-latex)
(setq org-export-latex-listings 'minted)
;; (add-to-list 'org-export-latex-packages-alist '("" "minted"))

(setq org-src-fontify-natively t)

(provide 'init-org-mode)
