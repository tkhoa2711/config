;;; init-org.el


;; enable syntax highlighting when exporting to HTML
(setq org-src-fontify-natively t)

(setq org-hide-emphasis-markers t)

;; taking notes easily with org-capture
(global-set-key "\C-c c" 'org-capture)

(provide 'init-org)

;;; init-org.el ends here
