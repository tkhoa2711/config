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
(require 'cl)

(defvar emacs-root "~/.emacs.d/"
  "The root of my emacs evolution.")

(load "~/.emacs.d/init.el")


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
(defun prev-window ()
    (interactive)
    (other-window -1))

(global-set-key (kbd "C-." ) 'other-window)
(global-set-key (kbd "C-," ) 'prev-window)

;; ============
;; == BACKUP ==
;; ============
;; put all backup files in one nice place if possible
;; uncomment these if you wish to keep all in one place
(let ((backup-dir (concat (file-name-as-directory emacs-root) ".backup")))
  (unless (file-directory-p backup-dir)
    (message "Directory does not exist: %s. Creating it." backup-dir)
    (mkdir backup-dir))
  (setq backup-directory-alist '(("." . "~/.backup"))))

(setq backup-by-copying t    ; don't delink hardlinks
      delete-old-versions t  ; clean up the backups
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2    ; and some old ones, too
      version-control t      ; use version numbers on backups
      )

(defun toggle-show-trailing-whitespace ()
  (setf show-trailing-whitespace (not show-trailing-whitespace)))

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
; (require 'emacs-color-themes)
; (load-theme 'mccarthy t)
(load-theme 'zenburn t)


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

;; custom core file to improve loading SLIME speed
;; preload packages for sockets, posix, introspect, asdf

;; those that take the most time to load.
;;
;; create the core by executing the following:
;; * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
;; * (save-lisp-and-die "sbcl.core-for-slime")
; (setq slime-lisp-implementations
;       '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))

; (setq slime-net-coding-system 'utf-8-unix)

;; set to load contrib packages
;(setq slime-contribs '(slime-scratch slime-editing-command))   ; basic stuffs
;(setq slime-contribs '(slime-repl))                            ; repl only
;(add-to-list 'slime-contribs 'slime-repl)
;(add-to-list 'slime-contribs 'slime-fancy)
(setq slime-contribs '(slime-fancy))                            ; almost everything (fancy)
(slime-setup '(slime-fancy slime-asdf))

;(setf asdf:*central-registry*
      ;; default directories, usually just the ``current directory''
;      '(*default-pathname-defaults*

	;; additional places where ASDF can search
;	#p"~/source/dev/lisp/"))
;(require :asdf)
;(pushnew "~/source/dev/lisp/" asdf:*central-registry* :test #'equal)


;; ===========================================================================
;; miscellaneous

;; emacs server
(server-start)

;; define-word -------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/define-word")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("da7fa7211dd96fcf77398451e3f43052558f01b20eb8bee9ac0fd88627e11e22" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
