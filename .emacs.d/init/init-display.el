;;; init-display.el

;; disable the bell
(setq ring-bell-function 'ignore)

;; use more appealing color theme for emacs on terminal
(unless (display-graphic-p)
  (use-package zenburn-theme
    :init
    (use-package color-theme)
    :config
    (load-theme 'zenburn t)))


(provide 'init-display)

;;; init-display.el ends here
