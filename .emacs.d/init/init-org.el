;;; init-org.el


;; enable syntax highlighting when exporting to HTML
(setq org-src-fontify-natively t)

(setq org-hide-emphasis-markers t)

(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)

;; automatically update the entry to DONE
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; taking notes easily with org-capture
(global-set-key "\C-c c" 'org-capture)

;; highlight these keywords in orgmode
(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|NOTE\\):" 1 font-lock-warning-face t)))))

(provide 'init-org)

;;; init-org.el ends here
