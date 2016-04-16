;;; init-filesystem.el

;; NOTE to view the path without copying, use `M-: buffer-file-name'
(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(provide 'init-filesystem)

;;; init-filesystem.el ends here
