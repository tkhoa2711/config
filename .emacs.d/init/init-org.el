;;; init-org.el


;; enable syntax highlighting when exporting to HTML
(setq org-src-fontify-natively t)

(setq org-hide-emphasis-markers t)

;; taking notes easily with org-capture
(global-set-key "\C-c c" 'org-capture)

;; highlight these keywords in orgmode
(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|NOTE\\):" 1 font-lock-warning-face t)))))

(provide 'init-org)

;;; init-org.el ends here
