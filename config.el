;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq tab-width 2
      tab-always-indent t
      indent-tabs-mode nil
      fill-column 80

      user-full-name "Nicolas Richart"
      user-mail-address "nicolas.richart@gmail.ch"
      
      epa-file-encrypt-to user-mail-address

      ;; ensure fill-paragraph takes doxygen @ markers as start of new
      ;; paragraphs properly
      paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      ;; display-line-numbers-type nil

      ;; On-demand code completion. I don't often need it.
      company-idle-delay nil

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil

      lsp-ui-peek-always-show t
      lsp-ui-flycheck-live-reporting nil
      lsp-ui-flycheck-enable nil

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

      ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
      ;; They're generally unhelpful and only add confusing visual clutter.
      mode-line-default-help-echo nil
      show-help-function nil
      )
(add-to-list 'auto-mode-alist '("\\.F90\\'" . f90-mode))

(load! "lisp/akantu-c")
(c-add-style "akantu" akantu-c-style)
(load! "lisp/akantu-input")

(map! "<f9>"     #'projectile-compile-project
      "<f5>"     #'clang-format-buffer
      "C-c ;"    #'comment-region
      "M-g"      #'goto-line

      ;;; treemacs
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

;; (defun mydoom-c-mode-setup ()
;;   (interactive)
;;   (lsp)
;;   )

;; (when (featurep! :lang cc +lsp)
;;   (progn
;;     (add-hook! 'c-mode-hook 'mydoom-c-mode-setup)
;;     (add-hook! 'cc-mode-hook 'mydoom-c-mode-setup)
;;     (add-hook! 'c++-mode-hook 'mydoom-c-mode-setup)))

(after! smartparens
  (smartparens-global-mode -1))

(setq doom-theme 'doom-one)

(load! "lisp/gud-enhancement")
