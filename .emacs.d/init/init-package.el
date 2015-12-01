;;; init-package.el

;; ============================================================================
;; setup MELPA repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; ----------------------------------------------------------------------------

(provide 'init-package)

;;; init-package.el ends here
