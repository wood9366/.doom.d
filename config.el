;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Li Yang"
      user-mail-address "wood9366@gmail.com")

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
;; (setq doom-font (font-spec :family "Source Code Variable" :size 12 :weight 'semi-light))

(defun ly/set-font()
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; english font
        (set-face-attribute 'default nil :font (format "%s-%d:weight=%s" "Source Code Variable" 13 'semi-light)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))) ;; 14 16 20 22 28
    ))

(defun ly/set-frame-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (ly/set-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'ly/set-frame-font)
  (ly/set-font))

;; 12345678
;; abcdefgh
;; 你好你好你好

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(after! git-gutter-fringe
  (if (fboundp 'fringe-mode) (fringe-mode nil)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; csharp
(after! omnisharp
  (setq omnisharp-server-executable-path "/usr/local/bin/omnisharp"))
;; (use-package! lsp-mode
;;   :config
;;   (setq lsp-csharp-server-path "/usr/local/bin/omnisharp"))

(map! :localleader
      :map omnisharp-mode-map
      (:prefix "r"
       "."  #'omnisharp-run-code-action-refactoring))

;; org reveal
(after! org-re-reveal
  (setq org-re-reveal-root (concat "file://" (expand-file-name "~/packs/reveal.js"))
        org-re-reveal-revealjs-version "4"))

;; lua
(add-to-list 'auto-mode-alist
             '("lua\\.txt\\'" . lua-mode))

;; (use-package! lsp-mode
;;   :config
;;   (setq lsp-clients-emmy-lua-jar-path (f-expand "~/packs/EmmyLua-LS-all.jar")))

;; perl
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist
             '("Construct\\'" . perl-mode))

(after! cperl-mode
  (setq
   cperl-close-paren-offset -4
   cperl-continued-statement-offset 4
   cperl-indent-level 4
   cperl-indent-parens-as-block t
   cperl-tabs-always-indent t))

(add-hook! lua-mode
  (add-hook! 'xref-backend-functions :local #'etags--xref-backend))
(use-package rime
  :custom
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-user-data-dir "~/Library/Rime")
  (rime-show-candidate 'posframe)
  (default-input-method "rime"))
