;;(autoload 'python-mode "python-mode.el" "Python mode." t)
;;(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

(setq python-python-command "/usr/bin/python2")
(add-to-list 'auto-mode-alist '("\\.ufl\\'" . python-mode))

(provide 'init-python)
