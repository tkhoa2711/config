;;; init.el

(eval-when-compile
  (require 'cl))

; TODO add recursive dir to load-path

(cl-labels ((add-path (p)
                      (let ((default-directory (concat user-emacs-directory p)))
                        (normal-top-level-add-to-load-path '("."))
                        (normal-top-level-add-subdirs-to-load-path))))
  (add-path "init")  ; init files go here
  (add-path "elisp") ; my personal elisp code
  (add-path "lisp")  ; my personal lisp code
  )

(require 'init-package)
(require 'init-util)
(require 'init-env)
(require 'init-display)
(require 'init-face)
(require 'init-formatting)
(require 'init-hook)
(require 'init-keybinding)
(require 'init-mode)
(require 'init-org)
(require 'init-patch)

;;; init.el ends here
