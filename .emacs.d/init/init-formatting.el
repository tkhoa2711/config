;;; init-formatting.el

(set-language-environment "utf-8")

(setq-default
 indent-tabs-mode nil ; no tab please!
 cursor-in-non-selected-window nil
 require-final-newline 'ask
 )

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

;; bind infer-indentation-style to your mode hook

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setf show-trailing-whitespace (not show-trailing-whitespace)))

;; ----------------------------------------------------------------------------

(provide 'init-formatting)

;;; init-formatting.el ends here
