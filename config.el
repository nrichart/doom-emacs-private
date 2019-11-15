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
      )

(load! "lisp/akantu-c")
(c-add-style "akantu" akantu-c-style)

(map! "<f9>"     #'projectile-compile-project
      "<f5>"     #'clang-format-buffer

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


(defun mydoom-c-mode-setup ()
  (interactive)
  (irony-mode)
  (flycheck-irony-setup)
  (irony-cdb-autosetup-compile-options)
  )

(when (featurep! :lang cc)
  (progn
    (add-hook! 'c-mode-hook 'mydoom-c-mode-setup)
    (add-hook! 'cc-mode-hook 'mydoom-c-mode-setup)
    (add-hook! 'c++-mode-hook 'mydoom-c-mode-setup)))

(after! smartparens
  (smartparens-global-mode -1))

(setq doom-theme 'doom-dracula)
