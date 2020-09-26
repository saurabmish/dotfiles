;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Backup, Auto-save, and Session Management ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialization

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

; Baseline

(let ((backup-dir "~/.cache/emacs/backups/")
      (auto-saves-dir "~/.cache/emacs/auto-saves/")
      (desktop-dir "~/.cache/emacs/desktop/")
      (recent-files-dir "~/.cache/emacs/recent/"))
  (dolist (dir (list backup-dir auto-saves-dir desktop-dir recent-files-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)
    )
  )
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir
        recentf-save-file (expand-file-name "recentf" recent-files-dir)
  )
)

; Backup

(setq
  backup-by-copying   t                                 ; don't delink hardlinks
  delete-old-versions t                                 ; clean up the backups
  version-control     t                                 ; use version numbers on backups
  kept-new-versions   7                                 ; keep some new versions
  kept-old-versions   4                                 ; and some old ones, too
)

; Session

(setq
  desktop-dirname             "~/.cache/emacs/desktop/"           ; location of desktop files
  desktop-base-file-name      "emacs.desktop"                     ; name of desktop file (hidden by default)
  desktop-base-lock-name      "emacs-desktop.lock"                ; name of lock file when desktop file is being used
  desktop-path                (list desktop-dirname)              ; used to load desktop file if desktop-dirname is not default
  desktop-save                t                                   ; save desktop session while exiting
  desktop-files-not-to-save   "^$"                                ; reload tramp paths
  desktop-auto-save-timeout   30                                  ; auto-save desktop file every 30 seconds
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Graphical User Interface ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Elements

(when window-system
  (setq default-frame-alist
    '((top . 80) (left . 300) (width . 85) (height . 45)))        ; window size and position
  (set-face-attribute 'default nil :font "Menlo" :height 160)     ; Menlo font of size 16
  (tool-bar-mode -1)                                              ; don't show icons for "File, Edit, ..."
  (scroll-bar-mode -1)                                            ; don't show scroll bar
  (menu-bar-mode -1)                                              ; don't show "File, Edit, ..."
  (tooltip-mode -1)                                               ; don't show information at mouse pointer
  (fringe-mode '(0 . 0))                                          ; no borders on either side of frame
  (add-to-list 'default-frame-alist '(ns-appearance . dark))      ; makes titlebar font visible in dark background
)

; Offset successive frames

(defun make-offset-frame (&optional x y)
  "..."
  (let* ((params  (frame-parameters))
         (ctop    (or (cdr (assoc 'top params))   0))
         (cleft   (or (cdr (assoc 'left params))  0)))
    (setq x  (or x  30)
          y  (or y  30))
    (make-frame (append `((top . ,(+ ctop y)) (left . ,(+ cleft x)))
                        default-frame-alist))))

(defun my-make-frame-command (&optional x y)
  "..."
  (interactive)
  (make-offset-frame x y))

(global-set-key (kbd "C-x 5 2") 'my-make-frame-command)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Data and Text Editing ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Global

(global-display-line-numbers-mode t)                    ; show line numbers in all buffers
(set-default-coding-systems 'utf-8)                     ; UTF-8 file encoding
(defalias 'yes-or-no-p 'y-or-n-p)                       ; ask fr confirmation with 'y' / 'n' insead of 'yes' / 'no'
(global-auto-revert-mode t)                             ; refresh current buffer if file changes through a different source
(desktop-save-mode t)                                   ; save and restore files from previous session
(recentf-mode t)                                        ; keep track of recent files

; Better defaults

(setq-default
  inhibit-startup-screen    t
  initial-scratch-message   ""                          ; no message on scratch buffer
  initial-major-mode        (quote text-mode)           ; start in text mode
  show-paren-mode           t                           ; highlight matching parenthesis
  column-number-mode        t                           ; show column at cursor position
  delete-selection-mode     t                           ; highlighted characters will be replaced on keypress
  delete-trailing-lines     t                           ; delete all trailing whitespace
  select-enable-clipboard   t                           ; enable system clipboard
  sentence-end-double-space nil                         ; sentences end with single space after period
  tab-width                 4                           ; TAB is 4 characters
  tab-stop-list             (number-sequence 4 200 4)   ; every TAB is 4 spaces
  indent-tabs-mode          nil                         ; indentation will always be done using spaces
  confirm-kill-emacs        (quote y-or-n-p)            ; always confirm on exit
  help-window-select        t                           ; help window will become active if opened
  word-wrap                 t                           ; wrap words to next line if they dont fit in the window
  ring-bell-function        (quote ignore)              ; disable audio bell
)

; Tabs and whitespace

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))
  )
)

(add-hook 'before-save-hook 'untabify-except-makefiles) ; convert all tabs to spaces on save



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Settings made within emacs ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(custom-enabled-themes '(wombat))
  '(custom-theme-directory "~/.config/emacs/themes"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
