;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; (package! gitconfig-mode
;;   :recipe (:host github :repo "magit/git-modes"
;;            :files ("gitconfig-mode.el")))
;; (package! gitignore-mode
;;   :recipe (:host github :repo "magit/git-modes"
;;            :files ("gitignore-mode.el")))

;; (package! forge
;;   :recipe (:host github :repo "magit/forge" :branch "main"))

;; (package! transient
;;   :recipe (:host github :repo "magit/transient" :branch "main"))
;; (package! with-editor
;;   :recipe (:host github :repo "magit/with-editor" :branch "main"))

(package! bearbolt
  :recipe (:host github :repo "joaotavora/beardbolt"))

(package! gmsh-mode)

(package! polymode)
(package! groovy-mode)

(package! clang-format)

(package! gitlab-ci-mode)
(package! gitlab-ci-mode-flycheck)

(package! visual-regexp)

(package! cmake-font-lock)

;;;(package! platformio-mode)

;;;(package! icicles)

(package! gmsh-mode)
(package! scad-mode)
                                        ;(package! scad-preview)

(package! magit-section)
(package! magit-lfs)
(package! magit-todos)
(package! magit-delta)

(package! todotxt)

;;;(package! flymd)

(package! ztree)


(package! gptel)
(package! ellama)

(package! iedit)

(package! puppet-mode)

;; From https://github.com/iyefrat/doom-emacs/commit/bd944dc318efe2dfb00c1107ca6d70797dad1331
;; Due to https://github.com/doomemacs/doomemacs/issues/7191
;; (package! code-review :recipe (:files ("graphql" "code-review*.el"))
;;     :pin "26f426e99221a1f9356aabf874513e9105b68140")
;;     ; HACK closql c3b34a6 breaks code-review wandersoncferreira/code-review#245,
;;     ; and the current forge commit (but forge does have an upstream fix),
;;     ; pinned as a temporary measure to prevent user breakages
;; (package! closql :pin "0a7226331ff1f96142199915c0ac7940bac4afdd")

(package! catppuccin-theme)

;;(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

;;(package! copilot
;;  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! x86-lookup)
(package! nasm-mode)
