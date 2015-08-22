;;; init-mode.el

;; ============================================================================
;; major modes

;; org-mode ---------------------------
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; nasm-mode --------------------------
(add-to-list 'load-path "~/.emacs.d/lisp")
(autoload 'nasm-mode "nasm-mode" "Major mode for nasm" t)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

;; newlisp-mode -----------------------
(add-to-list 'load-path "~/.emacs.d/elpa/newlisp-mode-20150120.1040")
(autoload 'newlisp-mode "newlisp-mode" "Major mode for newlisp files" t)
(setq auto-mode-alist (cons '("\\.lsp$" . newlisp-mode) auto-mode-alist))

;; company-mode -----------------------
(add-hook 'after-init-hook 'global-company-mode)

; make emacs's "speedbar" recognize newlisp files
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".lsp"))

; another way to use C-x C-e to eval stuffs without jumping to the next function
; (define-key newlisp-mode-map [(control x) (control e)] 'newlisp-evaluate-prev-sexp)

; if you're too lazy to load the newlisp interpreter all the time
(defun start-newlisp ()
  "Starts newlisp interpreter or show it if already running.
  Requires newlisp-mode to be loaded."
  (interactive)
  (newlisp-show-repl))


;; ============================================================================
;; other modes

;; auto0revert-mode -------------------
;; tell emacs to refresh all the buffers from disk automatically
;; if the buffer is modified, it won't be reverted
(global-auto-revert-mode t)

;; ido-mode ---------------------------
;; use ido-mode for switching buffers
(ido-mode 1)

; display choices vertically
(setq ido-separator "\n")

(setq ido-enable-flex-matching t)


;; ----------------------------------------------------------------------------

(provide 'init-mode)

;;; init-mode.el ends here
