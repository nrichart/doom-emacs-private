;;; akantu-c.el --- Coding convention for akantu

;;; Commentary:

;;; Code:
(require 'cc-mode)

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

(provide 'akantu-c)
;;; akantu-c.el ends here
