;;; akantu-c.el --- Coding convention for akantu

;;; Commentary:

;;; Code:
(require 'cc-mode)

(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
             '("gnu"
               (fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)
                                   (statement-cont . llvm-lineup-statement)))))

(defconst akantu-c-style
  '("llvm.org")
  "Akantu C Programming Style.")

(c-add-style "akantu" akantu-c-style)

(provide 'akantu-c)
;;; akantu-c.el ends here
