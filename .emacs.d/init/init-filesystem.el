;;; init-filesystem.el

(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(require 'init-filesystem)

;;; init-filesystem.el ends here
