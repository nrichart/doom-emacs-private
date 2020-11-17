;;; ~/.doom.d/lisp/poly-yaml-jinja2.el -*- lexical-binding: t; -*-
;;; Commentary:

;; Edit YAML files for containing embedded Jinja2 templating.

;;; Code:

(require 'polymode)


(defcustom pm-inner/jinja2
  (pm-inner-chunkmode :mode #'jinja2-mode
                      :head-matcher "{[%{#][+-]?"
                      :tail-matcher "[+-]?[%}#]}"
                      :head-mode 'body
                      :head-adjust-face nil)
  "Jinja2 chunk."
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-ansible-mode "poly-ansible")
(define-polymode poly-yaml-jinja2-mode
  :hostmode 'pm-host/yaml
  :innermodes '(pm-inner/jinja2))

;;;###autoload
(add-to-list 'auto-mode-alist
             '(".*\\.ya?ml\.j2\\'" . poly-yaml-jinja2-mode))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("/\\(?:group\\|host\\)_vars/" . poly-yaml-jinja2-mode))


;(defun jinja2-ansible-functions-keywords (args)
;  "Advice to provide additional keywords for Jinja2 filters defined by Ansible.
;ARGS is provided by the advised function, `jinja2-functions-keywords'."
;  (append args poly-ansible-jinja2-filters))

;(advice-add 'jinja2-functions-keywords :filter-return
;            #'jinja2-ansible-functions-keywords)

(provide 'poly-yaml-jinja2-mode)

;;; poly-yaml-jinja2.el ends here
