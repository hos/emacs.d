;; Enhancements to editing capabilities of emacs

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun whack-whitespace ()
  (interactive "P")
  (let ((regexp "[ \t\n]+"))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

;; (defun whack-whitespace (arg)
;;  "Delete all white space from point to the next word.  With prefix ARG
;;    delete across newlines as well.  The only danger in this is that you
;;    don't have to actually be at the end of a word to make it work.  It
;;    skips over to the next whitespace and then whacks it all to the next
;;    word."
;;   (interactive "P")
;;   (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
;;     (re-search-forward regexp nil t)
;;     (replace-match "" nil nil)))

;; Move to match if on (){}[] when pressing %, otherwise insert %.
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	((looking-at "\\s\{") (forward-list 1) (backward-char 1))
	((looking-at "\\s\}") (forward-char 1) (backward-list 1))
	((looking-at "\\s[") (forward-list 1) (backward-char 1))
	((looking-at "\\s]") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))



(provide 'enhancements)
