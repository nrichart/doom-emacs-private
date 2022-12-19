;;; lisp/fortran-openacc.el -*- lexical-binding: t; -*-

;;;;
;;;; Enable Highlighing of OpenACC and OpenMP directives in Fortan
;;;;
(defface openacc-face
  '((t
     :foreground "#ff43a4"
     ;; :background "yellow"
     ;; :bold       t
     ))
  "Face for !$ACC comments"
  )

(defvar openacc-face 'openacc-face)

(defface openmp-face
  '((t
     :foreground "#43ff84"
     ;; :background "yellow"
     ;; :bold       t
     ))
  "Face for !$OMP comments"
  )

(defvar openmp-face 'openmp-face)

;; Regex describing all known OpenACC directive names
(defvar acc-commands-regex
  ;; Use regexp-opt to build the regex and replace-regexp-in-string
  ;; to allow any sequence of blank characters as separator
  (replace-regexp-in-string " " "[ \t]+"
                            (regexp-opt '( "cache"
                                           "data"
                                           "declare"
                                           "host_data"
                                           "init"
                                           "kernels"     ;; will also catch "kernels loop"
                                           "loop"
                                           "parallel"    ;; will also catch "parallel loop"
                                           "routine"
                                           "set"
                                           "shutdown"
                                           "update"
                                           "wait"
                                           "end parallel"
                                           "end kernels"
                                           "end data"
                                           "end host_data"
                                           "end atomic"
                                           "atomic read"
                                           "atomic write"
                                           "atomic update"
                                           "atomic capture"
                                           "enter data"
                                           "exit data"
                                           ))
                            ))

;; Regex describing all known OpenMP directive names (up to version 4.5)
;; Remark: Combined directives are not present since they are matched by they first kerword
;;         (e.g. "parallel" will also match "parallel do")
(defvar omp-commands-regex
  (replace-regexp-in-string " " "[ \t]+"
                            (regexp-opt '(
                                          "atomic"
                                          "barrier"
                                          "cancel"
                                          "cancellation point"
                                          "critical"
                                          "declare reduction"
                                          "declare simd"
                                          "declare target"
                                          "distribute"
                                          "do"
                                          "end atomic"
                                          "end critical"
                                          "end distribute"
                                          "end do"
                                          "end master"
                                          "end ordered"
                                          "end parallel"
                                          "end sections"
                                          "end simd"
                                          "end single"
                                          "end target"
                                          "end task"
                                          "end taskgroup"
                                          "end taskloop"
                                          "end teams"
                                          "end workshare"
                                          "flush"
                                          "master"
                                          "ordered"
                                          "parallel"
                                          "section"
                                          "sections"
                                          "simd"
                                          "single"
                                          "target"
                                          "task"
                                          "taskgroup"
                                          "taskloop"
                                          "taskwait"
                                          "taskyield"
                                          "teams"
                                          "threadprivate"
                                          "workshare"
                                          ))
                            ))


;; For OpenACC, uncomment one of the two lines below to choose between a simple regexp
;; that only matches the $ACC prefix or a more advanced version that also matches the directive name.
;; (defvar acc-regex "$acc[\t ].*")
(defvar acc-regex (concat "$acc[ \t]+\\(" acc-commands-regex  "\\b\\|&\\).*" ))

;; Same for OpenMP
;;(defvar omp-regex "$omp[\t ].*")
(defvar omp-regex (concat "$omp[ \t]+\\(" omp-commands-regex  "\\b\\|&\\).*" ))

;; Regex describing F77 and F90 comments containing an OpenACC or OpenMP directive
;;
;; Reminder: For F90 comments, the \\( ... \\)) indicates which part should be
;; highlighted. Then the argument 1 should be passed instead of 0 to font-lock-add-keywords
;;
(defvar acc-regex-f90 (concat "^[ \t]*\\(!" acc-regex "\\)" ) )
(defvar acc-regex-f77 (concat "^[cC]"       acc-regex ) )
(defvar omp-regex-f90 (concat "^[ \t]*\\(!" omp-regex "\\)" ) )
(defvar omp-regex-f77 (concat "^[cC]"       omp-regex ) )

(font-lock-add-keywords 'f90-mode
                        ( list
                          ( list acc-regex-f90 1 'openacc-face t)
                          ( list omp-regex-f90 1 'openmp-face  t)
                          ))

(font-lock-add-keywords 'fortran-mode
                        (list
                         ( list acc-regex-f77 0 'openacc-face t)
                         ( list acc-regex-f90 1 'openacc-face t)
                         ( list omp-regex-f77 0 'openmp-face  t)
                         ( list omp-regex-f90 1 'openmp-face  t)
                         ))

(provide 'fortran-openacc)
