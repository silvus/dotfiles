;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; User
;; -----------------------------------------------------------------------------
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")


;; Theme
;; -----------------------------------------------------------------------------
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 12 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 12))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-oceanic-next)
;; (setq doom-theme 'doom-material-dark)
;; (setq doom-theme 'doom-henna)
(setq doom-theme 'doom-molokai)

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.1)))))

(custom-theme-set-faces!
'doom-molokai
'(org-level-8 :inherit outline-3 :height 1.0)
'(org-level-7 :inherit outline-3 :height 1.0)
'(org-level-6 :inherit outline-3 :height 1.1)
'(org-level-5 :inherit outline-3 :height 1.2)
'(org-level-4 :inherit outline-3 :height 1.3)
'(org-level-3 :inherit outline-3 :height 1.4)
'(org-level-2 :inherit outline-2 :height 1.5)
'(org-level-1 :inherit outline-1 :height 1.6)
'(org-document-title  :height 1.8 :bold t :underline nil))

(setq org-modern-table-vertical 1)
(setq org-modern-table t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Places lines between the current line and the screen edge
(setq scroll-margin 25)

;; Load org mode config (wrapped in after! within the file)
(load (expand-file-name "package_org.el" (file-name-directory load-file-name)))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; CUA mode for standard cut/copy/paste keybindings
(after! cua-base
  (cua-mode 1))

;; Custom keybindings
(map! "C-s" #'save-buffer)

;; Page Up to scroll normally, but when the buffer cannot scroll further (e.g. already near the top),
;; it should move the cursor to the beginning of the buffer
(setq scroll-error-top-bottom t)

(defun delete-line-no-kill ()
  "Delete the current line without affecting the kill-ring."
  (interactive)
  (delete-region (line-beginning-position)
                 (min (1+ (line-end-position)) (point-max))))

;; Evil-specific keybindings
(after! evil
  (map! :nvi "C-d" #'delete-line-no-kill))

;; Evil comment keybinding
(after! evil-nerd-commenter
  (map! :nvi "C-/" #'evilnc-comment-or-uncomment-lines))

;; Evil multiple cursors
(after! evil-mc
  (map! :nvi "C-S-l" #'evil-mc-make-cursor-move-next-line))

;; Window management keybindings
(map! :nvi ;; normal, visual, insert
      "M-<left>"  #'windmove-left
      "M-<right>" #'windmove-right
      "M-<up>"    #'windmove-up
      "M-<down>"  #'windmove-down)

(map! :nvi "M-w" #'delete-window)

;; Vertical split with Alt+T
(map! :nvi "M-t" #'split-window-right)

;; Resize splits with Alt+Shift+Arrow
(map! :nvi "M-S-<left>"  (lambda () (interactive) (shrink-window-horizontally 5)))
(map! :nvi "M-S-<right>" (lambda () (interactive) (enlarge-window-horizontally 5)))
(map! :nvi "M-S-<up>"    (lambda () (interactive) (enlarge-window 5)))
(map! :nvi "M-S-<down>"  (lambda () (interactive) (shrink-window 5)))

(defun swap-windows (dir)
  "Swap current window with window in direction DIR."
  (let ((other (windmove-find-other-window dir)))
    (when other
      (let ((this-buffer (window-buffer))
            (other-buffer (window-buffer other)))
        (set-window-buffer other this-buffer)
        (set-window-buffer (selected-window) other-buffer)))))

(map! :nvi "M-C-<left>"  (lambda () (interactive) (swap-windows 'left)))
(map! :nvi "M-C-<right>" (lambda () (interactive) (swap-windows 'right)))
(map! :nvi "M-C-<up>"    (lambda () (interactive) (swap-windows 'up)))
(map! :nvi "M-C-<down>"  (lambda () (interactive) (swap-windows 'down)))
