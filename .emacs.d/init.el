;;C-h m Display information about current mode
;;C-x g Find file recursivelly. C-u prefix - asks for directory
;;C-x f Find file in curr dir. C-u prefix - find file in history

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(add-to-list 'load-path "/home/pavel/.emacs.d/helm")
(add-to-list 'load-path "/home/pavel/.elisp")

(require 'helm-config)
(require 'helm-projectile)
(require 'buffer-move)
(require 'visible-mark)
(require 'auto-mark)
(require 'workgroups2)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(desktop-save-mode t)     ; save all opened files (or disable it)
(setq wg-prefix-key (kbd "C-z")
      wg-restore-associated-buffers nil ; restore all buffers opened in this WG?
      wg-use-default-session-file nil   ; turn off for "emacs --daemon"
      wg-default-session-file "~/.elisp/wg-session"
      wg-use-faces nil
      wg-morph-on nil)                  ; animation off

(workgroups-mode 1)     ; Aticvate workgroups
(global-auto-mark-mode 1)
(projectile-global-mode)
(winner-mode 1)

(setq dired-dwim-target t)
;; allow dired to be able to delete or copy a whole dir.
;; “always” means no asking. “top” means ask once. Any other symbol means ask each and every time for a dir and subdir.
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

(global-set-key "\C-z" nil)

(defun back-window ()
  (interactive)
  (other-window -1))

;;;; Удобство перелкючения между буферами. C-, - предыдущий, C-. - следующий
(define-key (current-global-map) (kbd "C-,") 'previous-buffer)
(define-key (current-global-map) (kbd "C-.") 'next-buffer)

(define-key (current-global-map) (kbd "M-p") 'other-window)
(define-key (current-global-map) (kbd "M-n") 'back-window)

(define-key (current-global-map) (kbd "C-s-r") 'wg-reload-session)

(define-key (current-global-map) (kbd "C-x C-g") 'helm-find)
(define-key (current-global-map) (kbd "C-x C-f") 'helm-find-files)
(define-key (current-global-map) (kbd "C-x C-b") 'helm-buffers-list)
(define-key (current-global-map) (kbd "C-x C-;") 'helm-bookmarks)
(define-key (current-global-map) (kbd "s-i") 'helm-imenu)

(global-set-key (kbd "M-x") 'helm-M-x)

;; ;;; Windows related operations
(define-key global-map (kbd "C-x |") 'split-window-horizontally)
(define-key global-map (kbd "C-x _") 'split-window-vertically)
(define-key global-map (kbd "C-{") 'shrink-window-horizontally)
(define-key global-map (kbd "C-}") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-^") 'enlarge-nwindow)
(define-key global-map (kbd "C-_") 'shrink-window)

;; ;; Navgating: Windmove uses C-<up> etc.
(define-key global-map (kbd "s-I"   ) 'buf-move-up)
(define-key global-map (kbd "s-K" ) 'buf-move-down)
(define-key global-map (kbd "s-L" ) 'buf-move-right)
(define-key global-map (kbd "s-J") 'buf-move-left)

(define-key global-map (kbd "s-o") 'occur)

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

;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead
(global-set-key (kbd "C-x 5") 'toggle-frame-split)


(cmd isearch-other-window
     (save-selected-window
       (other-window 1)
       (isearch-forward)))

(bind "C-M-S" isearch-other-window)

