;;;; my personal Emacs config

;; ===========================================================================
;; just my note for some frequently used commands
;
;; ===========
;; == EMACS ==
;; ===========
;
;; BUFFERS, WINDOWS
; C-x k : close/kill the buffer
; C-x 1 : remove split windows
; C-x 2 : split window horizontally
; C-x 3 : split window vertically
;
;; COMMAND, CONTROL
; C-x [z*] : repeat last command
;
;; EDITING
; [C-x u|C-_] : undo
; C-x u C-x u : redo
; C-_ : redo (not portable)
; M-x revert-buffer : undo all changes
; C-w : cut
; M-w : copy
; C-y : paste (yank)
; C-a C-SPC C-n C-w : cut the entire line with newline at the end
; M-[del|d] : delete word backward/forward
; C-k : kill (delete) line from the current cursor
; C-a C-k : kill whole line
; [C|M]-t : transpose two characters/words
;
;; FORMATTING
; M-u : uppercase word
; M-l : lowercase word
; M-c : capitalize word
;
;; NAVIGATION, SEARCH
; C-[f|b] : move forward/backward one character
; M-[f|b] : move forward/backward one word
; C-[s|r] : search forward/backward
; C-g : return to where search started (only if still in search mode)
;
;; ===========
;; == COLOR SCHEME ==
;; ===========
; M-x color-schem-select : select a color schema via a GUI menu
; M-x customize-themes : customize with multiple themes
;
;; ===========
;; == SLIME ==
;; ===========
;
; M-. : jump to definition of function under the cursor
; M-, : jump back
; C-c C-d d : show symbol definition (when on symbol)
; C-c C-d h : open symbol definition (when on symbol) from CL hyperspec in browser
;
;; COMPILATION, MESSAGES
; C-c C-c : compile current form (funtion)
; C-c C-k : compile current buffer source file and load it
; C-c M-k : compile current buffer but don't load it
; M-[n|p] : step forward/backward through annotations post compilation
;
;; DEBUGGING
; , : enter command
;

;; ===========================================================================

(set-language-environment "utf-8")

;; in OS X use 'cmd as 'meta key
; (setq mac-command-modifier 'meta)
; (setq mac-option-modifier 'nil)

(setq-default
 indent-tab-mode nil ; no tab please!
 cursor-in-non-selected-window nil
 require-final-newline 'ask
 )

;; coloring while on window system
(when window-system
  (set-foreground-color "white")
  (set-background-color "black"))

;; hot key for switching between windows
;; C-x o is probably the worst default emacs setup :|
(global-set-key (kbd "C-." ) 'other-window)
(global-set-key (kbd "C-," ) 'prev-window)

(defun prev-window ()
    (interactive)
    (other-window -1))

;; ============
;; == BACKUP ==
;; ============
;; put all backup files in one nice place if possible
;; uncomment these if you wish to keep all in one place
(if (file-directory-p "~/.backup")
    (setq backup-directory-alist '(("." . "~/.backup")))
  (message "Directory does not exist: ~/.backup"))

(setq backup-by-copying t    ; don't delink hardlinks
      delete-old-versions t  ; clean up the backups
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2    ; and some old ones, too
      version-control t      ; use version numbers on backups
      )

(defun toggle-show-trailing-whitespace ()
  (setf show-trailing-whitespace (not show-trailing-whitespace)))

;; tell emacs to refresh all the buffers from disk automatically
;; if the buffer is modified, it won't be reverted
;(global-auto-revert-mode t)

;; ===========================================================================
;; setup MELPA repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; ===========================================================================
;; this need to be set after initializing package.el
;; color-theme
(require 'color-theme)
;(setq color-theme-is-global t)
;(color-theme-initialize)
;(color-theme-vim-colors)

;; sublime-themes
;(require 'emacs-color-themes)
;(load-theme 'mccarthy t)

;; ===========================================================================
;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; ===========================================================================
;; loading SLIME

;(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; setup loadpath for SLIME manually
;(add-to-list 'load-path "~/lisp/slime")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; specify the lisp program you are using. Default is "lisp"
(setq inferior-lisp-program "/usr/local/bin/sbcl")

; (setq slime-net-coding-system 'utf-8-unix)

;; set to load contrib packages
;(setq slime-contribs '(slime-scratch slime-editing-command))   ; basic stuffs
;(setq slime-contribs '(slime-repl))                            ; repl only
;(add-to-list 'slime-contribs 'slime-repl)
;(add-to-list 'slime-contribs 'slime-fancy)
(setq slime-contribs '(slime-fancy))                            ; almost everything (fancy)
(slime-setup '(slime-fancy slime-asdf))

;; Loading nasm mode
(load "~/.emacs.d/lisp/nasm.el")
(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
