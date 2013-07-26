;c-h m display information about current mode
;;c-x g find file recursivelly. c-u prefix - asks for diretory
;;c-x f find file in curr dir. C-u prefix - find file in history

(global-set-key "\C-z" nil)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(add-to-list 'load-path "/home/pavel/.emacs.d/helm")
(add-to-list 'load-path "/home/pavel/.elisp")
(require 'cl)
(require 'dired+)
(require 'dired-details)
(require 'helm-config)
(require 'helm-projectile)
(require 'buffer-move)
(require 'visible-mark)
(require 'auto-mark)
;;(require 'windows-path)
;;(windows-path-activate)

;;(require 'workgroups2)

(dired-details-install)

(defun cygstart-in-dired ()
  "Uses the cygstart command to open the file at point."
  (interactive)
  (shell-command (concat (concat "cygstart '" (dired-file-name-at-point)) "'")))
  (add-hook 'dired-mode-hook '(lambda () (local-set-key (kbd "RET") 'cygstart-in-dired))))

(defun open-in-external ()
  "Open the current file in desktop.
Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (cond
   ((string-equal system-type "cygwin") (shell-command (concat "cygstart " 
(shell-quote-argument dired-filename-at-point))))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux") (shell-command "xdg-open ."))
   ))

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

(defun close-and-kill-next-pane ()
   "Close the other pane and kill the buffer in it also."
   (interactive)
   (other-window 1)
   (kill-buffer (buffer-name))
   (other-window -1)
   ;;(delete-window)
 )

 (defun close-and-kill-this-pane ()
   "Close this pane and kill the buffer in it also."
   (interactive)
   (kill-buffer (buffer-name))
   ;;(delete-window)
 )

(defun other-window-previous-buffer ()
"Switch to the previous buffer in sibling window"
   (interactive)
   (other-window 1)
   (previous-buffer)
   (other-window -1))

(defun other-window-next-buffer ()
  (interactive)
  (other-window 1)
  (next-buffer)
  (other-window -1))

(defun back-window ()
  (interactive)
  (other-window -1))

(defun ido-dired-other-window ()
(interactive)
(other-window -1)
(ido-dired))

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(defmacro bind (key fn)
  "shourcut for global-set-key"
  `(global-set-key (kbd ,key)
                   ;;handle unquoted function names and lambdas
                   ,(if (listp fn)
                        fn
                      `',fn)))

(defmacro cmd (name &rest body)
  "declare an interactive command without all the boilerplate"
  `(defun ,name ()
     ,(if (stringp (car body)) (car body))
     (interactive)
     ,@(if (stringp (car body)) (cdr `,body) body)))

(cmd isearch-other-window
     (save-selected-window
       (other-window 1)
       (isearch-forward)))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(desktop-save-mode t)     ; save all opened files (or disable it)

;;(setq wg-prefix-key (kbd "C-z")
;;      wg-restore-associated-buffers nil ; restore all buffers opened in this WG?
;;      wg-use-default-session-file nil   ; turn off for "emacs --daemon" 
;;      wg-default-session-file "~/.elisp/wg-session"
;;      wg-use-faces nil
;;      wg-morph-on nil)                  ; animation off

;;(workgroups-mode 1)     ; Aticvate workgroups
(global-auto-mark-mode 1)
(projectile-global-mode)
(winner-mode 1)

(setq dired-dwim-target t)
;; allow dired to be able to delete or copy a whole dir.
;; “always” means no asking. “top” means ask once. Any other symbol means ask each and every time for a dir and subdir.
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(defun my-ido-dired (&optional prefix)
    (interactive "P")
    (if prefix (other-window -1) (ido-dired))
      (ido-dired))
;;(define-key (current-global-map) (kbd "O") 'cygstart-in-dired)
(define-key (current-global-map) (kbd "C-,") 'previous-buffer)
(define-key (current-global-map) (kbd "C-.") 'next-buffer)
(define-key (current-global-map) (kbd "C-M-,") 'other-window-previous-buffer)
(define-key (current-global-map) (kbd "C-M-.") 'other-window-next-buffer)
(define-key (current-global-map) (kbd "C-M-1") 'delete-other-windows)

(define-key (current-global-map) (kbd "M-[") 'other-window)
(define-key (current-global-map) (kbd "M-]") 'back-window)

;;(define-key (current-global-map) (kbd "C-s-r") 'wg-reload-session)
(define-key helm-map (kbd "C-o") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-d") 'helm-execute-persistent-action)


(define-key dired-mode-map (kbd "C-o") 'diredp-insert-subdirs)
(define-key dired-mode-map (kbd "C-o") 'dired-display-file)   
(define-key dired-mode-map (kbd "M-i") 'rgrep)
(define-key dired-mode-map (kbd "M-p") nil)

(define-key (current-global-map) (kbd "C-M-g") 'helm-find)
(define-key (current-global-map) (kbd "C-M-f") 'helm-find-files)
(define-key (current-global-map) (kbd "C-M-b") 'helm-buffers-list)
(define-key (current-global-map) (kbd "C-M-5") 'toggle-frame-split)

(define-key (current-global-map) (kbd "C-M-;") 'helm-bookmarks)
(define-key (current-global-map) (kbd "C-M-d") 'my-ido-dired)

(define-key (current-global-map) (kbd "M-k") 'close-and-kill-this-pane)
(define-key (current-global-map) (kbd "C-M-k") 'close-and-kill-next-pane)
(define-key (current-global-map) (kbd "M-s") 'save-buffer)
(define-key (current-global-map) (kbd "M-o") 'helm-occur)
(global-set-key (kbd "M-i") 'rgrep)
(global-set-key (kbd "M-x") 'helm-M-x)

;; ;;; Windows related operations
(define-key global-map (kbd "C-M-3") 'split-window-horizontally)
(define-key global-map (kbd "C-M-2") 'split-window-vertically)
(define-key global-map (kbd "C-M-0") 'delete-window)

;;(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;;(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;;(global-set-key (kbd "S-C-<down>") 'shrink-window)
;;(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(define-key global-map (kbd "s-I"   ) 'buf-move-up)
(define-key global-map (kbd "s-K" ) 'buf-move-down)
(define-key global-map (kbd "s-L" ) 'buf-move-right)
(define-key global-map (kbd "s-J") 'buf-move-left)

;;(define-key global-map (kbd "s-o") 'occur)

(bind "C-M-S" isearch-other-window)

(define-key (current-global-map) (kbd "s-i") 'helm-imenu)
