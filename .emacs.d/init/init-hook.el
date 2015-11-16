;;; init-hook.el

;; make script executable if it is stared with #/ symbol
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(provide 'init-hook)

;;; init-hook.el ends here
