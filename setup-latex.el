
;;Active AucTEX by putting this (autocompletion emacs)
(load "auctex.el" nil t t)


;; A handy macro for dealing with the listings package in LaTeX.
;; It puts the current word inside \lstinline|..|
;;(fset 'mark-lstinline "\C-[b\\lstinline|\C-[f|")
;;(add-hook 'latex-mode-hook (lambda () (local-set-key (kbd "C-c m") 'mark-lstinline)))

(provide 'setup-latex)
