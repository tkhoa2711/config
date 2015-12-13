;;; init-patch.el

;; fix the `x-popup-dialog' problem in Mac which causes Emacs to freeze or crash
(defmacro prevent-dialog (f)
  `(defadvice ,f (around prevent-dialog activate)
     "Prevent ,f from activating a dialog"
     (let ((use-dialog-box nil))
       ad-do-it)))

;(prevent-dialog yes-or-no-p)
;(prevent-dialog y-or-n-p)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))


(provide 'init-patch)

;;; init-patch.el ends here
