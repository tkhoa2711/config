;;; init-env.el

(use-package exec-path-from-shell
  :defer nil
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(provide 'init-env)

;;; init-env.el ends here
