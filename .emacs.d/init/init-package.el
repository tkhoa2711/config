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

;; ============================================================================
;; bootstrap `use-package'
;; TODO this would fail if we're behind the corporate firewall which requires authentication
;; more than often, after being authenticated, first request will fail for unknown reason
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; load `use-package'
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; ----------------------------------------------------------------------------

(provide 'init-package)

;;; init-package.el ends here
