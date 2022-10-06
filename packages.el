;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

(package! gitconfig-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitconfig-mode.el")))
(package! gitignore-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitignore-mode.el")))


(package! polymode)
(package! groovy-mode)

(package! clang-format)

(package! gitlab-ci-mode)
(package! gitlab-ci-mode-flycheck)

(package! visual-regexp)

(package! cmake-font-lock)

;(package! platformio-mode)

;(package! icicles)

(package! gmsh-mode)
(package! scad-mode)
(package! scad-preview)

(package! magit-section)
(package! magit-lfs)
(package! magit-todos)
(package! magit-delta)

(package! todotxt)

;(package! flymd)

(package! ztree)

(unpin! magit forge)

(package! slack)
