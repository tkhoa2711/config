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


;; INIT
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "This is the place where additional Emacs' user-specific files locate beneath.
Note that this shuold end with a directory separator.
And that is the root of my emacs evolution."))

(load (concat user-emacs-directory "init.el"))


;; ===========================================================================

;; coloring while on window system
(when window-system
  (set-foreground-color "white")
  (set-background-color "black"))


;; ============
;; == BACKUP ==
;; ============
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

; (defun save-core (core-fn)
;   "Save the lisp core in a cool way"
;   (progn
;     #+sbcl
;     (let ((fork-result (sb-posix:fork)))
;       (case for-result
;         (-1 (error "fork failed!"))
;         (0 (sb-ext:save-lisp-and-die core-fn :toplevel #'main :executable t))
;         (otherwise (sb-ext:wait)))
;       (format t "standalone core ~a saved." core-fn))
;     #-sbcl
;     (error "not supported on this lisp implementation")
;     (values)))

;; use ibuffer to list buffers
(defalias 'list-buffers 'ibuffer)

;; use the symlink to open file instead of opening the actual file
(setq vc-follow-symlinks nil)


;; ===========================================================================
;; this need to be set after initializing package.el
;; color-theme
(require 'color-theme)
;(setq color-theme-is-global t)
;(color-theme-initialize)
;(color-theme-vim-colors)

;; sublime-themes
; (require 'emacs-color-themes)
; (load-theme 'mccarthy t)
(load-theme 'zenburn t)


;; ===========================================================================
;(setf asdf:*central-registry*
      ;; default directories, usually just the ``current directory''
;      '(*default-pathname-defaults*

	;; additional places where ASDF can search
;	#p"~/source/dev/lisp/"))
;(require :asdf)
;(pushnew "~/source/dev/lisp/" asdf:*central-registry* :test #'equal)


;; ===========================================================================
;; miscellaneous

;; start emacs server (only if it's not already started)
(require 'server)
(unless (server-running-p)
  (server-start))

;; define-word -------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/define-word")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes nil)
 '(custom-safe-themes (quote ("da7fa7211dd96fcf77398451e3f43052558f01b20eb8bee9ac0fd88627e11e22" default)))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
