;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq-default tab-width 2
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

(c-add-style "llvm.org"
             '((fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (statement-cont . ++)
                                   (member-init-intro . ++)
                                   ))
               ))

(defconst akantu-c-style
  '("llvm.org")
  "Akantu C Programming Style.")

(c-add-style "akantu" akantu-c-style)

(map! "<f9>"     #'projectile-compile-project
      "<f5>"     #'clang-format

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


(when 'irony
  (progn
    (add-hook! 'c-mode-hook 'irony-mode)
    (add-hook! 'c++-mode-hook 'irony-mode)))

(when 'flycheck-irony
  (progn
    (add-hook! 'c-mode-hook 'flycheck-irony-setup)
    (add-hook! 'c++-mode-hook 'flycheck-irony-setup)))

(after! smartparens
  (smartparens-global-mode -1))

(setq doom-theme 'doom-dracula)
