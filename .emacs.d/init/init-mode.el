;;; init-mode.el

;; ============================================================================
;; major modes

;; org-mode ---------------------------
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; nasm-mode --------------------------
(autoload 'nasm-mode "nasm-mode" "Major mode for nasm" t)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

;; company-mode -----------------------
(add-hook 'after-init-hook 'global-company-mode)

;; newlisp-mode -----------------------
(autoload 'newlisp-mode "newlisp-mode" "Major mode for newlisp files" t)
(add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))

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

;; markdown-mode ----------------------
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; python-mode ------------------------
;(setq py-install-directory "~/.emacs.d/elpa/python-mode")
;(add-to-list 'load-path py-install-directory)
;(require 'python-mode)
(autoload 'python-mode "python-mode" "Major mode for python files" t)
(add-to-list 'auto-mode-alist '("\\.\\(py\\|ipython\\)$" . python-mode))

; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)


;; ===========================================================================
;; loading SLIME

;(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; setup loadpath for SLIME manually
(eval-when-compile
  (require 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

  ;; specify the lisp program you are using. Default is "lisp"
  (setq inferior-lisp-program "/usr/local/bin/sbcl")

  ;; custom core file to improve loading SLIME speed
  ;; preload packages for sockets, posix, introspect, asdf

  ;; those that take the most time to load.
  ;;
  ;; create the core by executing the following:
  ;; * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
  ;; * (save-lisp-and-die "sbcl.core-for-slime")
  ;; (setq slime-lisp-implementations
  ;;       '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))

  ; (setq slime-net-coding-system 'utf-8-unix)

  ;; set to load contrib packages
  (setq slime-contribs '(slime-fancy))
  (slime-setup '(slime-fancy slime-asdf))
  )

;; ============================================================================
;; other modes

;; auto-revert-mode -------------------
;; tell emacs to refresh all the buffers from disk automatically
;; if the buffer is modified, it won't be reverted
(global-auto-revert-mode t)

;; ido-mode ---------------------------
;; use ido-mode for switching buffers
;; (ido-mode 1)

; display choices vertically
(setq ido-separator "\n")

(setq ido-enable-flex-matching t)

;; column-number-mode -----------------
(setq column-number-mode t)

;; display-time-mode ------------------
(display-time-mode 1)

;; open recent files ------------------
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-o") 'recentf-open-files)


;; ----------------------------------------------------------------------------

(provide 'init-mode)

;;; init-mode.el ends here
