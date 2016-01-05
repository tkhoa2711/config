;;; init-keybinding.el

;; settings for CMD and OPTION key in Mac OS
(with-system darwin
  (eval-when-compile
    (defvar mac-option-key-is-meta)
    (defvar mac-command-key-is-meta)
    (defvar mac-command-modifier)
    (defvar mac-option-modifier))
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'nil))

;; press RET should enter newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
  Move point to the first non-whitespace character on this line.
  If point was already at that position, move point to beginning of line."
  (interactive)                         ; use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)               ; go to first non-whitespace character
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

;; kill from the current position to the beginning of line
;; C-0 C-k is the original way to do this
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)))

;; prevent accidental exit from emacs
(global-set-key (kbd "C-x C-c") '(lambda ()
                                   (interactive)
                                   (if (y-or-n-p-with-timeout "Do you really want to exit?" 3 nil)
                                       (save-buffers-kill-emacs))))

;; entented selection
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

;; use Slime's style of navigation to source of symbol at point and back
;; with M-. and M-,
(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))


;; ============================================================================
;; BUFFER
;; ----------------------------------------------------------------------------

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(defun rename-current-buffer-file ()
  "Rename current buffer/file."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer with name '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
          (message "File '%s' successfully renamede to '%s'" name
                   (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun other-window-kill-buffer ()
  "Kill the buffer in other window, normally a temporary buffer."
  (interactive)
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(global-set-key (kbd "C-x c") 'other-window-kill-buffer)

(defun new-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitle")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(global-set-key "\C-n" 'new-buffer)

(defun prev-window ()
  "Go to previous windows."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-." ) 'other-window)
(global-set-key (kbd "C-," ) 'prev-window)


;; ============================================================================

(provide 'init-keybinding)

;;; init-keybinding ends here
