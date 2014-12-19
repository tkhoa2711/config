;;;; my personal Emacs config

;; ===========================================================================
;;;; just my note for some frequently used commands

;;; == EMACS ==
;; buffer, windows
; C-x k : close buffer
; C-x 1 : remove split windows
; C-x 2 : split window horizontally
; C-x 3 : split window vertically
;
;; command control
; C-x [z*] : repeat last command
;
;; editing
; C-x u : undo
; C-x u C-x u : redo
; C-_ : redo (not portable)
; M-x revert-buffer : undo all changes
; C-w : cut
; M-w : copy
; C-y : paste (yank)
;
;; navigation, search
; C-[f|b] : move forward/backward one character
; N-[f|b] : move forward/backward one word
; C-[s|r] : search forward/backward
; C-g : return to where search started (only if still in search mode)
;
;;; ----------------------------------
;;; == SLIME ==
; M-. : jump to definition of function under the cursor
; M-, : jump back
; C-c C-d d : show symbol definition (when on symbol)
; C-c C-d h : open symbol definition (when on symbol) from CL hyperspec in browser
;
;; compilation, messages
; C-c C-c : compile current form (funtion)
; C-c C-k : compile current buffer (file)
; M-[n|p] : step forward/backward through annotations post compilation
;
;; debugging
; , : enter command
;

;; ===========================================================================

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

;; == auto-save and backup ==
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

;; ===========================================================================
;;;; Loading SLIME 

;; setup loadpath
(add-to-list 'load-path "~/lisp/slime")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; specify the lisp program you are using. Default is "lisp"
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; set to load contrib packages
;(setq slime-contribs '(slime-scratch slime-editing-command))	; basic stuffs
;(setq slime-contribs '(slime-repl))	; repl only
;(add-to-list 'slime-contribs 'slime-repl)
;(add-to-list 'slime-contribs 'slime-fancy)
(setq slime-contribs '(slime-fancy))	; almost everything (fancy)
(slime-setup '(slime-fancy slime-asdf))
