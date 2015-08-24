;;; init.el

(defvar emacs-root) ; defined in .emacs

(require 'cl)

(labels ((add-path (p)
          (add-to-list 'load-path
                       (concat (file-name-as-directory emacs-root) p))))
  (add-path "init")  ; init files go here
  (add-path "elisp") ; my personal elisp code
  )

(require 'init-keybinding)
(require 'init-mode)

;;; init.el ends here
