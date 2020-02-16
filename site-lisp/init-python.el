;;(autoload 'python-mode "python-mode.el" "Python mode." t)
;;(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

(setq python-python-command "/usr/bin/python2")
(add-to-list 'auto-mode-alist '("\\.ufl\\'" . python-mode))

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))


(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)
             (format-all-mode)
             ;; (py-yapf-enable-on-save)
             ))

(provide 'init-python)
