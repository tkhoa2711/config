;;; init-keybinding.el

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
  Move point to the first non-whitespace character on this line.
  If point was already at that position, move point to beginning of line."
  (interactive)			        ; use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation) 		; go to first non-whitespace character
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

;; kill from the current position to the beginning of line
;; C-0 C-k is the original way to do this
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)))

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

;; ============================================================================

(provide 'init-keybinding)

;;; init-keybinding ends here
