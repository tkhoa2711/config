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
; C-s c-s : continue search forward
; C-g : return to where search started (only if still in search mode)
;
;; MENU
; <F10> : display menu on terminal
;
;; ==================
;; == COLOR SCHEME ==
;; ==================
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
; C-c C-c : interrupt the current evaluating form
; C-c C-c : compile current form (funtion)
; C-c C-k : compile current buffer source file and load it
; C-c M-k : compile current buffer but don't load it
; M-[n|p] : step forward/backward through annotations post compilation
;
;; DEBUGGING
; , : enter command
;

;; ===========================================================================
;; INIT

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "This is the place where additional Emacs' user-specific files locate beneath.
Note that this shuold end with a directory separator.
And that is the root of my emacs evolution."))

(load (concat user-emacs-directory "init.el"))

;; ===========================================================================
;; BACKUP

;; put all backup and auto-save files in one nice place if possible
;; uncomment these if you wish to keep all in one place
(let ((backup-dir (concat user-emacs-directory ".backup")))
  (unless (file-directory-p backup-dir)
    (message "Directory does not exist: %s. Creating it." backup-dir)
    (mkdir backup-dir))
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,backup-dir t))))

(setq backup-by-copying t    ; don't delink hardlinks
      delete-old-versions t  ; clean up the backups
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2    ; and some old ones, too
      version-control t      ; use version numbers on backups
      )

;; ===========================================================================
;; miscellaneous

;; use ibuffer to list buffers
(defalias 'list-buffers 'ibuffer)

;; use the symlink to open file instead of opening the actual file
(setq vc-follow-symlinks nil)

;; start emacs server (only if it's not already started)
(require 'server)
(unless (server-running-p)
  (server-start))

;; ===========================================================================
;;; .emacs ends here
