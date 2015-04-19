;; Associate .m files with octave (or matlab)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(setq octave-mode-hook
      ( lambda () (progn (setq octave-comment-char ?%)
                         (setq comment-start "% "))))
;; (modify-syntax-entry ?% "<"  octave-mode-syntax-table)

(provide 'init-octave)
