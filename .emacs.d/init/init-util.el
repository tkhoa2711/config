;;; init-util.el - Common utilities

(defmacro with-system (sys-type &rest body)
  (declare (indent defun))
  `(when (eq system-type ',sys-type)
     ,@body))

(provide 'init-util)

;;; init-util.el ends here
