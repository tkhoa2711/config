;;; init-face.el

;; regex pattern matching:
;; \\( and \\)        : grouping
;; \\sw               : a word character
;; \\s_               : a symbol character
;; \\(\\sw\\|\\s_\\)+ : a variable name
;; [ \t\n]*           : whitespace including newlines

;; highlight function's name
;; http://stackoverflow.com/questions/18289329/how-to-highlight-all-the-functions-name-in-emacs-lisp-mode
(defface font-lock-func-face
  '((nil (:foreground "#7F0055" :weight bold))
    (t (:bold t :italic t)))
  "Font Lock mode face used for function calls."
  :group 'font-lock-highlighting-face)

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\s-*\\(\\_<\\(?:\\sw\\|\\s_\\)+\\)\\_>"
    1 'font-lock-func-face)))

;; ----------------------------------------------------------------------------

(provide 'init-face)

;;; init-face.el ends here
