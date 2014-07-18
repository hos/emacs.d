;;Active AucTEX by putting this (autocompletion emacs)
;; (load "auctex.el" nil t t)

(defun my-run-latex ()
  (interactive)
  (TeX-save-document (TeX-master-file))
  (TeX-command "LaTeX" 'TeX-master-file -1))

(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                    (local-set-key (kbd "C-c C-a") 'my-run-latex)
                     )))
;; A handy macro for dealing with the listings package in LaTeX.
;; It puts the current word inside \lstinline|..|
;;(fset 'mark-lstinline "\C-[b\\lstinline|\C-[f|")
;;(add-hook 'latex-mode-hook (lambda () (local-set-key (kbd "C-c m") 'mark-lstinline)))

(provide 'init-latex)
