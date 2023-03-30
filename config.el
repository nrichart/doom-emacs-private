;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "lisp/akantu-c")
(c-add-style "akantu" akantu-c-style)

(load! "lisp/akantu-input")
(load! "lisp/poly-yaml-jinja2")

(setq ;;tab-width 2
 tab-always-indent t
 indent-tabs-mode nil
 fill-column 80

 user-full-name "Nicolas Richart"
 user-mail-address "nicolas.richart@gmail.ch"

 epa-file-encrypt-to user-mail-address

 projectile-project-search-path '("~/dev")

 ;; ensure fill-paragraph takes doxygen @ markers as start of new
 ;; paragraphs properly
 paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"

 ;; Line numbers are pretty slow all around. The performance boost of
 ;; disabling them outweighs the utility of always keeping them on.
 ;; display-line-numbers-type nil

 ;; On-demand code completion. I don't often need it.
 company-idle-delay nil


 lsp-enable-indentation nil
 ;; lsp-enable-on-type-formatting nil
 lsp-enable-symbol-highlighting nil
 lsp-enable-file-watchers nil

 ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
 ;; disable it by default.
 lsp-ui-sideline-enable t
 lsp-ui-peek-enable t
 lsp-ui-peek-always-show nil

 ;;lsp-ui-flycheck-enable t
 ;;lsp-ui-flycheck-live-reporting t
 lsp-pylsp-configuration-sources ["blake"]
 lsp-pylsp-plugins-flake8-enabled t
 lsp-pylsp-plugins-pycodestyle-enabled nil

 lsp-ui-flycheck-enable t
 lsp-ui-flycheck-live-reporting t

 lsp-ui-doc-enable nil

 lsp-file-watch-ignored (quote
                         ("[/\\\\]\\.git$"
                          "[/\\\\]\\.hg$"
                          "[/\\\\]\\.bzr$"
                          "[/\\\\]_darcs$"
                          "[/\\\\]\\.svn$"
                          "[/\\\\]_FOSSIL_$"
                          "[/\\\\]\\.idea$"
                          "[/\\\\]\\.ensime_cache$"
                          "[/\\\\]\\.eunit$"
                          "[/\\\\]node_modules$"
                          "[/\\\\]\\.fslckout$"
                          "[/\\\\]\\.tox$"
                          "[/\\\\]\\.stack-work$"
                          "[/\\\\]\\.bloop$"
                          "[/\\\\]\\.metals$"
                          "[/\\\\]target$"
                          "[/\\\\]\\.deps$"
                          "[/\\\\]autom4te.cache$"
                          "[/\\\\]\\.reference$"
                          "[/\\\\]build.*$"
                          "[/\\\\]test$"
                          "[/\\\\]extra-packages$"
                          "[/\\\\]examples$"))

 lsp-tex-server 'digestif
 lsp-disabled-clients '(ccls)

 ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
 ;; They're generally unhelpful and only add confusing visual clutter.
 mode-line-default-help-echo nil
 show-help-function nil

 global-prettify-symbols-mode nil
 prettify-symbols-mode -1

 c-default-style (quote
                  ((c++-mode . "akantu")
                   (java-mode . "java")
                   (awk-mode . "awk")
                   (other . "doom")))

 ;; clang-format-executable "clang-format-14"

 todotxt-file "/home/richart/Clouds/Syncthing/todo/todo.txt"
 org-roam-directory "/home/richart/Clouds/Syncthing/roam/"

 magit-git-executable "git"

 hl-todo-keyword-faces (quote
                        (("TODO" warning bold)
                         ("FIXME" error bold)
                         ("HACK" font-lock-constant-face bold)
                         ("REVIEW" font-lock-keyword-face bold)
                         ("NOTE" success bold)
                         ("DEPRECATED" font-lock-doc-face bold)
                         ("\\todo" warning bold)
                         ("\\warning" warning bold)
                         ("\\deprecated" font-lock-doc-face bold)))

 safe-local-variable-values (quote
                             ((c-file-style . akantu)
                              (flycheck-checker . c/c++-clang-tidy)
                              (eval set-background-color "#000015")
                              (projectile-enable-caching . t)
                              (projectile-project-name . "Akantu[master]")))

 magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
 magit-git-executable "git"

 compilation-scroll-output 'first-error

 vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"
 frame-background-mode "dark"
 )

(setq tramp-remote-path (quote
                         (tramp-own-remote-path
                          tramp-default-remote-path)))

;; https://docs.doomemacs.org/latest/modules/lang/cc/
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"
                                "--malloc-trim"
                                "--pch-storage=disk")
      lsp-clients-clangd-executable "/home/richart/dev/perso/bin/clangd"
      lsp-clangd-binary-path "/home/richart/dev/perso/bin/")

;; (when (featurep! +lsp)
;;   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
;;                    :major-modes '(c-mode c++-mode)
;;                    :remote? t
;;                    :server-id 'clangd-remote))

(after! lsp-clangd (set-lsp-priority! 'clangd 1))

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t))
        ccls-executable "/home/richart/dev/perso/bin/ccls")
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

(after! eglot
  :config
  (add-hook 'f90-mode-hook 'eglot-ensure)
  (set-eglot-client! 'python-mode '("pylsp"))
  (set-eglot-client! 'cc-mode '("clangd-16" "-j=2" "--clang-tidy"))
  (setq exec-path (append exec-path '(
                                      (concat (getenv "HOME") "/.local/bin/") ;; pyls
                                      (concat (getenv "HOME") "/.luarocks/bin/") ;; tex
                                      )))
  )

(add-to-list 'auto-mode-alist '("\\.F90\\'" . f90-mode))

(map! "<f9>"     #'projectile-compile-project
      "C-c ;"    #'comment-region
      "M-g"      #'goto-line
      "<f7>"     #'tototxt

      (:when (featurep! :editor format)
        "<f5>"     #'+format/buffer
        )

      ;;; tr_eemacs
      (:when (featurep! :ui treemacs)
        "<f8>"   #'+treemacs/toggle
        "<C-f8>" #'+treemacs/find-file)

      ;;; ivy
      (:when (featurep! :completion ivy)
        :map ivy-minibuffer-map
        "TAB"    #'ivy-partial
        [tab]    #'ivy-partial)

      ;;; vc
      (:when (featurep! :emacs vc)
        "C-x g"  #'magit-status)
      )

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-dracula)

(load! "lisp/gud-enhancement")

(after! magit
  (setq magit-diff-refine-hunk 'all))

(add-hook! magit-mode
  (transient-append-suffix 'magit-push "-u"
    '(1 "=s" "Skip gitlab pipeline" "--push-option=ci.skip"))
  (transient-append-suffix 'magit-push "=s"
    '(1 "=c" "Create mr" "--push-option=merge_request.create"))  ;; create a merge request
  (transient-append-suffix 'magit-push "=c"
    '(1 "=V" "Set CI variable" "--push-option=ci.variable="))  ;; Will prompt, can only set one extra variable
  (transient-append-suffix 'magit-push "=V"
    '(1 "=O" "Set push option" "--push-option="))  ;; Will prompt, can only set one extra push option
  )

(add-hook! 'LaTeX-mode-hook
  (add-to-list 'TeX-view-program-list '("Evince" "evince --page-index=%(outpage) %o"))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  )
