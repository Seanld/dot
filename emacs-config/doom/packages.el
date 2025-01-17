;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)


(package! focus) ;; Handy for non-code writing.
(package! tree-edit)
(package! evil-tree-edit)
(package! nasm-mode)
(package! flymake-nasm)
(package! company-irony)
(package! elpher)
(package! cycle-at-point)
(package! web-mode)
(package! company-web)
(package! json-navigator) ;; Makes JSON files less cluttered when reading.
(package! typescript-mode) ;; Basic TS syntax highlighting.
(package! mermaid-mode) ;; Plaintext to graphs and flowcharts!
(package! js2-mode)
(package! rjsx-mode)
(package! olivetti)
(package! paradox)
(package! common-lisp-snippets)
(package! graphql-mode)
(package! basic-mode)
(package! caddyfile-mode)
(package! nginx-mode)
(package! company-nginx)
;; (package! gemini-mode)
;; (package! ox-gemini)
(package! dockerfile-mode)
(package! calc-at-point) ;; Do mathematical operations on numbers in-place in buffers, via Calc.
(package! literate-calc-mode) ;; Inline calculation results (can be handy in markup doc buffers).
(package! hnreader)
(package! paredit)
(package! evil-paredit)
(package! d2-mode)
(package! systemd)
(package! justl)
(package! just-mode)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

(package! cook-mode
  :recipe (:host github :repo "cooklang/cook-mode"))

(package! vlang-mode
  :recipe (:host github :repo "Naheel-Azawy/vlang-mode"))

;; More generic than `company-ispell' as it can take other backends (aspell).
(package! company-spell
  :recipe (:host github :repo "enzuru/company-spell"))


;; Typst is a modern alternative to LaTeX.
;; --------------
;; This needs to be run once on a new setup to install the grammar:
;;
;; (add-to-list 'treesit-language-source-alist
;;              '(typst "https://github.com/uben0/tree-sitter-typst"))
;; (treesit-install-language-grammar 'typst)
(package! typst-ts-mode
  :recipe (:host sourcehut :repo "meow_king/typst-ts-mode"))



;; ;; Pin to fix issue with package update.
;; (package! transient :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440")
;; (package! with-editor :pin "391e76a256aeec6b9e4cbd733088f30c677d965b")
;; (package! vertico :pin "af1c893f3891902162e32f73f062213436636567")

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
