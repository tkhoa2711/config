;;; init-formatting.el

;; tabs are evil, never use it!
(setq-default indent-tabs-mode nil)

;; http://www.emacswiki.org/emacs/NoTabs
;; when open a file, default indentation is space, only use tabs if
;; that is what the file is primarily using
(defun infer-indentation-style ()
  "Use the tab/space indentation style as same as the source file.
Otherwise, use current `indent-tabs-mode' by default."
  (let ((space-count (how-many-region (point-min) (point-max) "^ "))
        (tab-count (how-many-region (point-min) (point-max) "^\t ")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (< space-count tab-count) (setq indent-tabs-mode t))))

(defun how-many-region (begin end regexp &optional interactive)
  "Count the number of non-trivial matches for REGEXP in the region.
Non-interactive arguments are BEGIN, END and REGEXP."
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point-max)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(provide 'init-formatting)

;;; init-formatting.el ends here
