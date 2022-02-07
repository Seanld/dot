;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sean Wilkerson"
      user-mail-address "seanldmcdonald@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)



(defun scroll-up-several-lines ()
  (interactive)
  (scroll-up-line 5))

(defun scroll-down-several-lines ()
  (interactive)
  (scroll-down-line 5))

(defun lp-get-yank ()
  "Prompts user from minibuffer to get LessPass password
parameters, pass them to CLI, and yank the result.
Requires `lesspassgo' CLI.

`lesspassgo' was chosen over normal `lesspass' because `lesspass'
is written in JS, and requires NPM packages which are an insecure pain."
  (interactive)
  (let ((site (read-from-minibuffer "Site: "))
        (login (read-from-minibuffer "Login: "))
        (options (read-from-minibuffer "Options: "))
        (password (read-passwd "Master: " t)))
    (kill-new
     (substring
      (shell-command-to-string
       (format "lesspassgo --site %s --login %s --password %s %s" site login password options))
      0 -1)))
  (message "Password copied!"))


;;;;;;;;;;;;;;
;; KEYBINDS ;;
;;;;;;;;;;;;;;

(map! :leader
      "y" #'yas-insert-snippet)
;; (map! :leader
;;       "x C" #'capitalize-region)
;; (map! :leader
;;       "x f" #'replace-string)
(map! :leader
      "y" #'replace-regexp)
(map! :leader
      "w /" #'+evil/window-vsplit-and-follow)
(map! :leader
      "w -" #'+evil/window-split-and-follow)
(map! :leader
      "#" #'calc)
(map! :leader
      "l" #'lp-get-yank)
(map! :leader
      "f m" #'make-directory)
(map! :leader
      "w <up>" #'evil-window-up)
(map! :leader
      "w <down>" #'evil-window-down)
(map! :leader
      "w <left>" #'evil-window-left)
(map! :leader
      "w <right>" #'evil-window-right)

(map! "<f12>" #'kill-this-buffer)
(map! "<C-f12>" #'kill-buffer-and-window)
(map! "<f11>" #'delete-window)
(map! "<M-S-down>" #'scroll-up-several-lines)
(map! "<M-S-up>" #'scroll-down-several-lines)


;;;;;;;;;;;;;;;;
;; ORG CONFIG ;;
;;;;;;;;;;;;;;;;

(setq org-todo-keywords
  '((sequence "TODO" "ACTIVE" "POSTPONED" "|" "DONE" "CANCELLED")))
(setq org-enforce-todo-dependencies t)



;;;;;;;;;;;;;;
;; C CONFIG ;;
;;;;;;;;;;;;;;

(setq c-style-indent-amount 4)

(setq-default c-basic-offset c-style-indent-amount)

;; Will check to see if everything preceding the cursor is whitespace. If so,
;; then unindent to the previous indentation level.
(defun unindent-or-backspace (ARG &optional KILLP)
  (interactive)
  (let ((current-column (current-column)))
    (if (eq 0 current-column)
        (evil-delete-backward-char-and-join 1)
      (let* ((all-spaces (equal (string-limit
                                 (thing-at-point 'line t)
                                 (1- current-column))
                                (make-string (1- current-column) 32)))
             (column-remainder (% current-column c-style-indent-amount))
             (delete-amount (if (eq 0 column-remainder)
                                c-style-indent-amount
                              column-remainder)))
        (if all-spaces
            (delete-char (- delete-amount))
          (delete-char -1))))))

(setq c-backspace-function 'unindent-or-backspace)

(add-hook 'c-mode-hook (lambda ()
                         (electric-pair-mode)))



;;;;;;;;;;;;;;;;
;; ZPL CUSTOM ;;
;;;;;;;;;;;;;;;;

;; ZPL is a primitive programming language for Zebra printers
;; for programmatic drawing.

(defvar zpl-highlights nil "something")

(setq zpl-highlights
      '(("\\^XA\\|\\^XZ\\|\\^FD\\|\\^FS" . 'font-lock-keyword-face)
        ((rx (one-or-more digit)) . (2 'font-lock-constant-face))))

(define-derived-mode zpl-mode text-mode "ZPL"
  "Major mode for editing ZPL code."
  (setq font-lock-defaults '(zpl-highlights)))



;;;;;;;;;;;;;;;;;;;;;;;
;; RESTCLIENT CUSTOM ;;
;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'company-backends 'company-restclient)

(add-hook 'restclient-mode-hook (lambda ()
                                  (display-line-numbers-mode)))

















;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
