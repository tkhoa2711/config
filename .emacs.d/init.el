;;; init.el

(defvar emacs-root)

(require 'cl)

(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p))))
	(add-path "init")  ; init files go here
	(add-path "elisp") ; my personal elisp code
        )

(require 'init-keybinding)

;;; init.el ends here
