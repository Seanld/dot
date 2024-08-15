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
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))
(setq doom-font (font-spec :family "Iosevka Custom Medium" :size 16))
;; (setq doom-font (font-spec :family "BlexMono Nerd Font" :size 16 :width 'condensed :height (lambda (starting-height)
;;                                                                                              (message starting-height)
;;                                                                                              (* starting-height 2))))

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

(defun pop-global-mark-centered ()
  "Same as `pop-global-mark' but centers the line in the window as well."
  (interactive)
  (pop-global-mark)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun micro-beginning-of-line ()
  "Imitates beginning-of-line functionality from Micro."
  (interactive)
  (let ((col (evil-column)))
    (if (= col 0)
        (back-to-indentation)
      (beginning-of-line))))



;;;;;;;;;;;;;;
;; KEYBINDS ;;
;;;;;;;;;;;;;;

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(defun insert-unicode (char-code)
  (interactive)
  (let* ((point-before (point))
         (next-point (+ point-before 1)))
    (push-mark)
    (goto-char next-point)
    (insert-char char-code)
    (goto-char next-point)))

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
      "@" #'pop-global-mark-centered
      "z" #'evil-toggle-fold
      "g j" #'vc-refresh-state
      "c R" #'lsp-workspace-restart

      ;; ;; Frequently-used Unicode symbols.
      ;; (:prefix ("i S" . "symbols")
      ;;   :desc "Pi"
      ;;   "p" (insert-unicode #x03c0)
      ;;   :desc "Square root"
      ;;   "q" (insert-unicode #x221a)
      ;;   :desc "Degrees"
      ;;   "d" (insert-unicode #x00b0))

      ;; These are other bigger applications that aren't simple
      ;; small functions (RSS reader, email, browser, etc)
      (:prefix ("A" . "applications")
        "e" #'elpher-go
        "E" #'elpher
        "#" #'calc
        "F" #'full-calc
        "P" #'list-packages
        "r" #'elfeed) ;; 'r' because Elfeed is an RSS reader.

      (:prefix ("c F" . "flycheck")
        "[" #'flycheck-previous-error
        "]" #'flycheck-next-error))

(map! :after evil
      :gni "<home>" #'micro-beginning-of-line)



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

(defun org-variable-face-mode ()
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
    '(org-meta-line :inherit font-lock-comment-face)
    ;; Monospaced (these look bad when variable-width)
    '(org-document-info-keyword :family "Iosevka Custom Medium")
    '(org-block-begin-line      :family "Iosevka Custom Medium")
    '(org-block                 :family "Iosevka Custom Medium")
    '(org-table                 :family "Iosevka Custom Medium")
    '(org-drawer                :family "Iosevka Custom Medium")
    '(org-special-keyword       :family "Iosevka Custom Medium")
    '(org-property-value        :family "Iosevka Custom Medium"))
  (setq buffer-face-mode-face '(:family "Libre Baskerville"))
  (buffer-face-mode))

(defun org-sparse-indirect ()
  (interactive)
  (clone-indirect-buffer "*org sparse result*" t)
  (org-sparse-tree))

(defun org-insert-timestamp-time ()
  (interactive)
  (org-insert-time-stamp (current-time) t))

;; Used for markup modes (Markdown, Org) that I want centered on the
;; screen to make it easier to read, and prevent lines from being too long.
;; (defun markup-centerize ()
;;   (olivetti-mode 1)
;;   (olivetti-set-width 100))
(defun markup-centerize ())

(after! org
  (map! :localleader
        :map org-mode-map
        "," #'org-ctrl-c-ctrl-c
        ";" #'org-next-link
        ":" #'org-previous-link
        "O" #'org-delete-property
        "S" #'org-insert-timestamp-time
        "<" #'org-previous-visible-heading
        ">" #'org-next-visible-heading
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
                                 (markup-centerize))
                             (org-indent-mode 1)
                             (highlight-indent-guides-mode -1)
                             (display-line-numbers-mode -1))))



;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;

(add-hook 'markdown-mode-hook (lambda ()
                                (markup-centerize)))

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



;;;;;;;;;;;;;;;;;;
;; TYPST CONFIG ;;
;;;;;;;;;;;;;;;;;;

;; These are enabled because the dev says they
;; have them enabled on the README.
(setq typst-ts-mode-watch-options "--open")
(setq typst-ts-mode-enable-raw-blocks-highlight t)
(setq typst-ts-mode-highlight-raw-blocks-at-startup t)

(map! :map typst-ts-mode-map
      :localleader
      "," #'typst-ts-mode-compile
      "." #'typst-ts-mode-compile-and-preview)

(setq typst-ts-mode-indent-offset 2)



;;;;;;;;;;;;;;;;;
;; YAML CONFIG ;;
;;;;;;;;;;;;;;;;;

(add-hook 'yaml-mode-hook (lambda ()
                            (face-remap-add-relative 'font-lock-variable-name-face
                                                     `(:foreground ,(face-attribute font-lock-type-face :foreground)))))



;;;;;;;;;;;;;;;;;;;
;; PYTHON CONFIG ;;
;;;;;;;;;;;;;;;;;;;

(setq lsp-modeline-diagnostics-enable nil
      lsp-progress-function #'ignore)



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

(setq sgml-basic-offset 2
      js-indent-level 2
      js2-basic-offset 2
      lsp-eslint-enable nil)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; For some reason, `js-mode' wants to handle ligatures for arrow symbols
;; on its own, when that's not its job. Disable.
(eval-after-load 'js '(progn
                        (setq js--prettify-symbols-alist nil)
                        (setq standard-indent 2)))

(eval-after-load 'rjsx-mode '(progn
                               (setq sgml-basic-offset 2)))

(setq typescript-indent-level 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-markup-indent-offset 2)



;;;;;;;;;;;;;;;;
;; NIM CONFIG ;;
;;;;;;;;;;;;;;;;

;; (after!
;;   (add-hook 'nim-mode (lambda ()
;;                         (flycheck-mode nil)
;;                         (lsp nil))))

(setq nim-use-flycheck-nimsuggest nil
      nimsuggest-path ""
      nim-compile-default-command '("c" "-r" "--hint[Processing]:off" "--excessiveStackTrace:on"))

(use-package! nim-mode
  :ensure t
  :hook
  (nim-mode . lsp))

(add-hook! 'nim-mode-hook #'lsp)



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
                           ;; (smartparens-mode -1)
                           ;; Disable dtrt-indent-mode silently (it outputs a message normally).
                           (let ((inhibit-message t))
                             (dtrt-indent-mode -1))
                           (setq company-backends '(company-web-html))))

;; This is the indentation value used by `mhtml-mode' as well.
(setq sgml-basic-offset 4)
(setq web-mode-script-padding 4)
(setq web-mode-style-padding 4)
(setq web-mode-part-padding 4)



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
  (PB "1000 * TB" "Petabyte")
  (TB "1000 * GB" "Terabyte")
  (GB "1000 * MB" "Gigabyte")
  (MB "1000 * KB" "Megabyte")
  (KB "1000 * byte" "Kilobyte")
  (B "byte" "8 bit byte")
  (byte "8 * bit" "8 bit byte")
  (bit nil "Binary digit")
  (tau "2 * pi" "Tau")
  ;; (W "V * A")
))

(setq calc-window-height 12)

(setq calc-show-banner nil)



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

;; Use `w' key to open a file in the other window.
(map! :map dired-mode-map
      "w" #'dired-jump-other-window)



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

(setq flycheck-xml-xmlstarlet-executable "xsd")

(add-hook! 'prog-mode-hook
           (lambda ()
             (setq prettify-symbols-alist
                   '(("lambda" . ?λ)
                     ("sqrt"   . ?√)))
             (prettify-symbols-mode 1)))

;; Expand region bindings, similar to those in
;; Spacemacs (this is way better than the default).
(map! :leader "j" #'er/expand-region)
(setq expand-region-contract-fast-key "k")
(setq expand-region-reset-fast-key "q")

;; These are primarily done for lsp-mode performance reasons.
(setq gc-cons-threshold 80000000)
(setq lsp-idle-delay 0.15)
(setq lsp-ui-doc-delay 0.3)

;; ;; Make the indent lines more blocky (and possibly more performant).
;; (setq highlight-indent-guides-method 'column)
;; (setq highlight-indent-guides-delay 0.2)

;; Dotted line for indent guides.
(setq highlight-indent-guides-character 9482)

;; Don't say the annoying "LSP connected" message every time
;; I open a file. It's so janky.
(setq lsp--show-message nil)

;; Disable LSP lenses by default (they just clutter the code).
(setq lsp-lens-enable nil)

;; LSP action modeline symbol is missing, so replace it.
(setq lsp-modeline-code-action-fallback-icon "ACTIONS")

(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(defhydra hydra-window-resize (:hint nil)
  "Resize window"
  ("<left>" evil-window-decrease-width "Decrease width")
  ("<right>" evil-window-increase-width "Increase width")
  ("<down>" evil-window-decrease-height "Decrease height")
  ("<up>" evil-window-increase-height "Increase height")
  ("q" nil))

(map! (:prefix "SPC w" :desc "Resize window" :n "SPC" #'hydra-window-resize/body))

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

;; Add `company-spell' to list of company backends, to replace `company-ispell'.
(add-to-list 'company-backends 'company-spell t)

;; Make scratch buffers start in `org-mode' (much nicer to take quick notes in).
(setq doom-scratch-initial-major-mode 'org-mode)

(setq hl-todo-keyword-faces '(("TODO" warning bold)
                              ("FIXME" error bold)
                              ("BUG" error bold)
                              ("REVIEW" font-lock-keyword-face bold)
                              ("HACK" font-lock-constant-face bold)
                              ("DEPRECATED" font-lock-doc-face bold)
                              ("NOTE" success bold)
                              ("SOLVED" success bold)))

;; The default of 1 second is too sluggish to me.
(setq flycheck-idle-change-delay 0.6)

;; Set threshold above which so-long-mode is invoked and corners are cut
;; to improve performance.
(setq so-long-threshold 1000)

;; Make `exec-path' match shell $PATH.
(let ((path-from-shell (replace-regexp-in-string
                        "[ \t\n]*$" "" (shell-command-to-string
                                        "$SHELL --login -c 'echo $PATH'"
                                        ))))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))


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
