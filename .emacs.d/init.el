;;; init.el

(defvar emacs-root) ; defined in .emacs

(eval-when-compile
  (require 'cl))

; TODO add recursive dir to load-path

(cl-labels ((add-path (p)
		   (let ((default-directory (concat (file-name-as-directory emacs-root) p)))
		     (normal-top-level-add-to-load-path '("."))
		     (normal-top-level-add-subdirs-to-load-path))))
  (add-path "init")  ; init files go here
  (add-path "elisp") ; my personal elisp code
  (add-path "lisp")  ; my persoanl lisp code
  )

(require 'init-package)
(require 'init-keybinding)
(require 'init-mode)

;;; init.el ends here
