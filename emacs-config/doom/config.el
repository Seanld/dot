;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sean Wilkerson"
      user-mail-address "me@seanld.xyz")

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
(setq doom-theme 'doom-dracula-custom)
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
(setq doom-font (font-spec :family "Iosevka Custom Medium" :size 16))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/docs/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)



;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun quick-align-spaced (start end)
  "Auto aligns columns of regions laid-out like Linux's fstab."
  (interactive "r")
    (align-regexp start end "\\( +\\)" 1 2 t))

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

(defun magit-status-side ()
  "Open Magit's status buffer in the other window, rather than
the current one (like in Spacemacs)."
  (interactive)
  (let ((window-count (count-windows)))
    (if (file-directory-p ".git")
        (progn
          (if (= window-count 1)
              (split-window-right))
          (other-window 1)
          (magit-status))
      (message "Not a Git repository!"))))

(defun make-last-and-close ()
  "Run makefile, and close the output buffer (it's in the way)."
  (interactive)
  (+make/run-last)
  (+popup/close-all))



;;;;;;;;;;;;;;
;; KEYBINDS ;;
;;;;;;;;;;;;;;

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(map! "<f12>" #'kill-this-buffer
      "<C-f12>" #'kill-buffer-and-window
      "<f11>" #'delete-window
      "<C-M-down>" #'scroll-up-several-lines
      "<C-M-up>" #'scroll-down-several-lines

      :leader
      "y" #'yas-insert-snippet
      "S" #'replace-string
      "R" #'replace-regexp

      "w /" #'+evil/window-vsplit-and-follow
      "w -" #'+evil/window-split-and-follow
      "w <up>" #'evil-window-up
      "w <down>" #'evil-window-down
      "w <left>" #'evil-window-left
      "w <right>" #'evil-window-right
      "w <S-up>" #'+evil/window-move-up
      "w <S-down>" #'+evil/window-move-down
      "w <S-left>" #'+evil/window-move-left
      "w <S-right>" #'+evil/window-move-right
      "w P" #'+popup/close-all

      ;; Replace man page lookup command with
      ;; woman, because it's just better.
      "h W" #'woman

      "c M" #'+make/run
      "c m" 'make-last-and-close

      "l" #'lp-get-yank
      "|" #'quick-align-spaced
      "f m" #'make-directory
      "s E" #'iedit-mode
      "@" #'pop-global-mark

      ;; These are other bigger applications that aren't simple
      ;; small functions (RSS reader, email, browser, etc)
      (:prefix ("A" . "applications")
       "e" #'elpher-go
       "E" #'elpher
       "#" #'calc)

      (:prefix ("c F" . "Flycheck")
       "[" #'flycheck-previous-error
       "]" #'flycheck-next-error))



;;;;;;;;;;;;;;;;
;; ORG CONFIG ;;
;;;;;;;;;;;;;;;;

(custom-set-faces!
  ;; Titles
  '(org-headline-default :inherit org-agenda-structure)
  '(org-document-title   :inherit outline-1 :height 170)
  '(org-level-1          :inherit outline-2 :height 160)
  '(org-level-2          :inherit outline-3 :height 150)
  '(org-level-3          :inherit outline-4 :height 140)
  '(org-level-4          :inherit outline-5 :height 130)
  '(org-level-5          :inherit outline-6 :height 120)
  '(org-level-6          :inherit outline-7 :height 110)
  '(org-level-7          :inherit outline-8 :height 100)
  '(org-level-8          :inherit outline-4 :height 90)

  ;; Other
  '(org-inline-src-block :inherit org-inline-src-block :family "Iosevka Custom")
  '(org-meta-line :inherit font-lock-comment-face))

(defun org-face-mode-variable ()
  "Set Org mode to use variable-width font, and adjust some of the
font settings to look better with variable-width (like sizing)."
  (interactive)
  (custom-set-faces!
    ;; Titles
    '(org-headline-default :inherit org-agenda-structure)
    '(org-document-title   :inherit outline-1 :height 230)
    '(org-level-1          :inherit outline-2 :height 200)
    '(org-level-2          :inherit outline-3 :height 170)
    '(org-level-3          :inherit outline-4 :height 140)
    '(org-level-4          :inherit outline-5 :height 140)
    '(org-level-5          :inherit outline-6 :height 140)
    '(org-level-6          :inherit outline-7 :height 140)
    '(org-level-7          :inherit outline-8 :height 140)
    '(org-level-8          :inherit outline-4 :height 140)
    ;; Other
    '(org-meta-line :inherit font-lock-comment-face))
  (setq buffer-face-mode-face '(:family "Libre Baskerville"))
  (buffer-face-mode))

(defun org-sparse-indirect ()
  (interactive)
  (clone-indirect-buffer "*org sparse result*" t)
  (org-sparse-tree))

(defun org-insert-timestamp-time ()
  (interactive)
  (org-insert-time-stamp (current-time) t))

(after! org
  (map! :localleader
        :map org-mode-map
        "," #'org-ctrl-c-ctrl-c
        ";" #'org-next-link
        ":" #'org-previous-link
        "O" #'org-delete-property
        "S" #'org-insert-timestamp-time
        (:prefix ("B" . "babel")
         "h" #'org-babel-insert-header-arg)
        (:prefix ("s" . "tree/subtree")
         "s" #'org-sparse-indirect))

  (setq org-publish-project-alist
        '(("houston-wiki"
           :base-directory "~/repos/houston-wiki/org"
           :base-extension "org"
           :publishing-directory "~/repos/houston-wiki"
           :publishing-function org-md-publish-to-md)))

  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-cycle-separator-lines -1)
  (setq org-return-follows-link t)
  (setq org-superstar-headline-bullets-list '("◉" "◈" "▶"))
  (setq org-adapt-indentation nil)
  (setq org-export-with-toc nil)
  (setq org-todo-keywords
    '((sequence "TODO" "PSPN" "ACTV" "CFRM" "|" "DONE" "CNCL")))
  (setq org-todo-keyword-faces
    '(("TODO" :foreground "grey" :height 0.85)
      ("PSPN" :foreground "orange" :slant italic :height 0.85)
      ("ACTV" :foreground "yellow" :height 0.85)
      ("CFRM" :foreground "light sky blue" :height 0.85)
      ("DONE" :foreground "chartreuse" :height 0.85)
      ("CNCL" :foreground "grey" :slant italic :height 0.85)))
  (setq org-enforce-todo-dependencies t)
  (add-hook 'org-mode-hook (lambda ()
                             (if (= (count-windows) 1)
                                 (progn
                                   (olivetti-mode 1)
                                   (olivetti-set-width 100)))
                             (org-indent-mode 1)
                             (highlight-indent-guides-mode -1)
                             (display-line-numbers-mode -1))))



;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;

(add-hook 'markdown-mode-hook (lambda ()
                                (olivetti-mode 1)
                                (olivetti-set-width 100)))

(custom-set-faces!
  ;; Titles
  '(markdown-header-delimiter-face :inherit org-agenda-structure)
  '(markdown-header-face-1 :inherit org-level-1)
  '(markdown-header-face-2 :inherit org-level-2)
  '(markdown-header-face-3 :inherit org-level-3)
  '(markdown-header-face-4 :inherit org-level-4)
  '(markdown-header-face-5 :inherit org-level-5)
  '(markdown-header-face-6 :inherit org-level-6))

;; I hate the message it shows every time you open an .md file, so
;; disable it, and then manually enable when it's actually needed.
(setq markdown-enable-math nil)



;;;;;;;;;;;;;;
;; C CONFIG ;;
;;;;;;;;;;;;;;

(add-hook 'c-mode-hook (lambda ()
                         (setq lsp-clients-clangd-args '("--header-insertion=never"))
                         (setq lsp-clients-clangd-library-directories '("/home/seanld/repos/os/libc"))
                         (setq c-style-indent-amount 4)))

;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ;; Will check to see if everything preceding the cursor is whitespace. If so,
;; ;; then unindent to the previous indentation level.
;; (defun unindent-or-backspace (ARG &optional KILLP)
;;   (interactive)
;;   (let ((current-column (current-column)))
;;     (if (eq 0 current-column)
;;         (evil-delete-backward-char-and-join 1)
;;       (let* ((all-spaces (equal (string-limit
;;                                  (thing-at-point 'line t)
;;                                  (1- current-column))
;;                                 (make-string (1- current-column) 32)))
;;              (column-remainder (% current-column c-style-indent-amount))
;;              (delete-amount (if (eq 0 column-remainder)
;;                                 c-style-indent-amount
;;                               column-remainder)))
;;         (if all-spaces
;;             (delete-char (- delete-amount))
;;           (delete-char -1))))))

;; (setq c-backspace-function 'unindent-or-backspace)

;; (add-hook 'c-mode-hook (lambda ()
;;                          (smartparens-mode -1)
;;                          (electric-pair-mode)))



;;;;;;;;;;;;;;;;;;
;; SHELL CONFIG ;;
;;;;;;;;;;;;;;;;;;

(add-hook 'sh-mode-hook (lambda ()
                          (flycheck-mode -1)))



;;;;;;;;;;;;;;;;;;;;;
;; ASSEMBLY CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;

; `nasm-mode' is much better for NASM than the generic assembly mode.
(add-hook 'asm-mode-hook (lambda ()
                           (nasm-mode)))



;;;;;;;;;;;;;;;;
;; LUA CONFIG ;;
;;;;;;;;;;;;;;;;

(add-hook 'lua-mode-hook (lambda ()
  (setq lua-indent-level 4)))



;;;;;;;;;;;;;;;;
;; ZPL CONFIG ;;
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
;; JAVASCRIPT CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq lsp-eslint-enable nil)

(setq js2-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; For some reason, `js-mode' wants to handle ligatures for arrow symbols
;; on its own, when that's not its job. Disable.
(eval-after-load 'js '(progn
                        (setq js--prettify-symbols-alist nil)))



;;;;;;;;;;;;;;;;
;; CSV CONFIG ;;
;;;;;;;;;;;;;;;;

(map! :localleader
      :map csv-mode-map
      "a" #'csv-align-fields
      "A" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "d" #'csv-toggle-descending)



;;;;;;;;;;;;;;;;
;; WEB CONFIG ;;
;;;;;;;;;;;;;;;;

;; TODO make `web-mode' more effective at auto-complete, without
;; inserting extraneous symbols, and add handy `:localleader' keys.
(add-hook 'web-mode-hook (lambda ()
                           (smartparens-mode -1)
                           (setq company-backends '(company-web-html))))

;; This is the indentation value used by `mhtml-mode' as well.
(setq sgml-basic-offset 4)



;;;;;;;;;;;;;;;;;;;;;;;
;; RESTCLIENT CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'company-backends 'company-restclient)

(add-hook 'restclient-mode-hook (lambda ()
                                  (display-line-numbers-mode)))



;;;;;;;;;;;;;;;;;
;; CALC CONFIG ;;
;;;;;;;;;;;;;;;;;

(setq math-additional-units '(
  (PiB "1024 * TB" "Pebibyte")
  (TiB "1024 * GB" "Tebibyte")
  (GiB "1024 * MB" "Gibibyte")
  (MiB "1024 * KB" "Mebibyte")
  (KiB "1024 * byte" "Kibibyte")
  (PB "1024 * TB" "Petabyte")
  (TB "1024 * GB" "Terabyte")
  (GB "1024 * MB" "Gigabyte")
  (MB "1024 * KB" "Megabyte")
  (KB "1024 * byte" "Kilobyte")
  (B "byte" "8 bit byte")
  (byte "8 * bit" "8 bit byte")
  (bit nil "Binary digit")
))

(setq calc-window-height 8)



;;;;;;;;;;;;;;;;;;
;; DIRED CONFIG ;;
;;;;;;;;;;;;;;;;;;

(after! dired
  ;; Override the default commands for opening files with ENTER in Dired.
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; Open images in `sxiv'
              (setq dired-guess-shell-alist-user
                    '(("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" "sxiv"))))))



;;;;;;;;;;;;;;;;;;;
;; ELPHER CONFIG ;;
;;;;;;;;;;;;;;;;;;;

(setq elpher-ipv4-always t)
(setq elpher-default-url-type "gemini")



;;;;;;;;;;;;;;;;;;;;;
;; MODELINE CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;

(setq doom-modeline-height 10)
(setq doom-modeline-env-enable-python nil)
(setq doom-modeline-icon nil)



;;;;;;;;;;;;;;;;;
;; MISC CONFIG ;;
;;;;;;;;;;;;;;;;;

;; Expand region bindings, similar to those in
;; Spacemacs (this is an invaluable feature to me).
(map! :leader "v" #'er/expand-region)
(setq expand-region-contract-fast-key "c")
(setq expand-region-reset-fast-key "q")

;; Don't say the annoying "LSP connected" message every time
;; I open a file. It's so janky.
(setq lsp--show-message nil)

(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(defhydra doom-window-resize-hydra (:hint nil)
  ("<left>" evil-window-decrease-width)
  ("<right>" evil-window-increase-height)
  ("<down>" evil-window-decrease-height)
  ("<up>" evil-window-increase-height)
  ("q" nil))

(map! (:prefix "w" :desc "Hydra resize" :n "SPC" #'doom-window-resize-hydra/body))

(defun my-caddyfile-hook ()
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t))

(add-hook! 'caddyfile-mode-hook #'my-caddyfile-hook)

(use-package! company-nginx
  :ensure t
  :config (add-hook! 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))

;; Make woman mode use maximum width of window. By default, it's arbitrarily limited to 65
;; characters to simulate a terminal for some reason.
(setq woman-fill-frame 't)


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
