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
                            (font-spec :family "Source Han Sans SC" :size 16)))
                            ;; (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))
        ) ;; 14 16 20 22 28
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

(map! :map 'Info-mode-map
      :n "gn" #'Info-scroll-up
      :n "gp" #'Info-scroll-down)

(defun tidy-xml ()
  "Tidies the HTML content in the buffer using `tidy'"
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "tidy -i -w -xml -q"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Tidy Error Buffer*"
   ;; show error buffer?
   t))

;; csharp
;; (after! omnisharp
;;   (setq omnisharp-server-executable-path "/usr/local/bin/omnisharp"))
(use-package! csharp-mode
  :mode "\\.cs\\'")

;; (add-hook! csharp-mode
;;   (setq-local projectile-generic-command "fd . -0 -H --color=never --type file --type symlink --follow --exclude .git -e cs"))

(defadvice! projectile-files-via-ext-command-around (fn root command)
  :around '(projectile-files-via-ext-command)
  (when command
    (let ((cmd command))
      (let ((unity-ver-file (cond ((locate-file "ProjectVersion.txt"
                                                `(,(concat (file-name-as-directory root)
                                                           "ProjectSettings"))))
                                  ((locate-file "ProjectVersion.txt"
                                                `(,(concat (file-name-as-directory root)
                                                           (file-name-as-directory "unity")
                                                           "ProjectSettings")))))))
        (if unity-ver-file
            (setq cmd (concat cmd " -e sh -e cs -e txt -e md -e txt -e json -e xml -e bytes -e lua -e shader -e cginc"))))
      (message "ext command run at root %s with command %s" root cmd)
      (funcall fn root cmd))))

(add-to-list 'auto-mode-alist '("cginc\\'" . shader-mode))
(add-to-list 'auto-mode-alist '("glslinc\\'" . shader-mode))

;; (use-package! tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

  ;; (require 'tree-sitter-indent)
  ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-indent-mode))
;; (use-package! csharp-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

;; (set-docsets! 'csharp-mode "unity3d")

;; (use-package! dash-docs
;;   :config
;;   (setq dash-docs-docsets-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))

(use-package! lsp-mode
  :config
  (setq lsp-csharp-server-path "/usr/local/bin/omnisharp")
  (loop for it in '("[/\\\\]Library\\'"
                    "[/\\\\]Temp\\'"
                    "[/\\\\]Builds\\'"
                    "[/\\\\]DownloadAssets\\'"
                    "[/\\\\]Logs\\'"
                    "[/\\\\]obj\\'"
                    "[/\\\\]_Android\\'"
                    "[/\\\\]_iOS\\'")
        do (add-to-list 'lsp-file-watch-ignored-directories it))

  (loop for it in '("[/\\\\].+\\.csproj\\'"
                    "[/\\\\].+\\.sln\\'"
                    "[/\\\\].+\\.meta\\'")
        do (add-to-list 'lsp-file-watch-ignored-files it))
  (setq lsp-file-watch-threshold 2000))

(defun wood9366/project-try-projectile (dir)
  (let ((probe (locate-dominating-file dir ".projectile")))
    (when probe (cons 'transient probe))))

(add-hook 'project-find-functions #'wood9366/project-try-projectile 'append)

(add-hook 'c++-mode-hook
          (lambda()
            (setq-local flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck))))

(when (featurep! :tools lsp +eglot)
  (use-package! eglot
    :init
    (add-to-list 'eglot-stay-out-of 'eldoc)))

;; (use-package! eglot
;;   :config
;;   (add-to-list "eglot-server-programs '(csharp-mode . ("/usr/local/bin/omnisharp" "-lsp"))))

(map! :localleader
      :map omnisharp-mode-map
      (:prefix "r"
       "."  #'omnisharp-run-code-action-refactoring))

;; org reveal
(after! org-re-reveal
  (setq org-re-reveal-root (concat "file://" (expand-file-name "~/packs/reveal.js"))
        org-re-reveal-revealjs-version "4"))

;; lua
(add-to-list 'auto-mode-alist '("lua\\.txt\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("lua\\.bytes\\'" . lua-mode))

(after! lua-mode
  (setq lua-indent-level 4))

(add-hook! lua-mode
  (add-hook! 'xref-backend-functions :local #'etags--xref-backend))

;; (when (featurep! :tools lsp)
;;   (setq lsp-lua-files-associations '(("*.lua" . "lua"))))
;; (after! lsp-mode
;;   (setq lsp-lua-files-associations '(("*.lua.bytes" . "lua")
;;                                      ("*.lua.txt" . "lua")
;;                                      ("*.lua" . "lua"))))

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

(use-package rime
  :custom
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-user-data-dir "~/Library/Rime")
  (rime-show-candidate 'posframe)
  (default-input-method "rime"))


; mail
(after! mu4e
  (setq smtpmail-smtp-service 25
        mu4e-use-fancy-chars nil)

  (set-email-account! "1hy"
                      '((user-mail-address            . "liyang@hy.com")
                        (smtpmail-smtp-user           . "liyang@hy.com")
                        (smtpmail-default-smtp-server . "192.168.2.238")
                        (smtpmail-smtp-server         . "192.168.2.238")
                        (smtpmail-stream-type         . plain)
                        (mu4e-sent-folder             . "/1hy/Sent")
                        (mu4e-drafts-folder           . "/1hy/Drafts")
                        (mu4e-trash-folder            . "/1hy/Trash")
                        (mu4e-refile-folder           . "/1hy/All")
                        (mu4e-compose-signature       . "---\nLi Yang"))
                      t)

  (set-email-account! "2hy-nas"
                      '((user-mail-address            . "liyang@huanyudigital.net")
                        (smtpmail-smtp-user           . "liyang@huanyudigital.net")
                        (smtpmail-default-smtp-server . "192.168.2.240")
                        (smtpmail-smtp-server         . "192.168.2.240")
                        (smtpmail-stream-type         . plain)
                        (mu4e-sent-folder             . "/2hy-nas/Sent")
                        (mu4e-drafts-folder           . "/2hy-nas/Drafts")
                        (mu4e-trash-folder            . "/2hy-nas/Trash")
                        (mu4e-refile-folder           . "/2hy-nas/All")
                        (mu4e-compose-signature       . "---\nLi Yang"))
                      t))

(defun markdown-preview-filter (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(defun markdown-preview-start ()
  "start preview markdown in browser"
  (interactive)
  (if (eq major-mode 'markdown-mode)
      (progn
        (unless (httpd-running-p)
          (httpd-start))
        (impatient-mode)
        (imp-set-user-filter #'markdown-preview-filter)
        (browse-url (format "http://localhost:8080/imp/live/%s/" (buffer-name))))
    (message "current buffer isn't markdown mode")))

(defun markdown-preview-stop()
  "stop preview markdown in browser"
  (interactive)
  (if (eq major-mode 'markdown-mode)
      (progn
        (if (httpd-running-p)
            (httpd-stop))
        (impatient-mode 0)
        (imp-remove-user-filter))))
