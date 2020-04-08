;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'geiser-eval)

;; Simple wrappers for geiser functions.

(defun geiser-kawa-eval--to-res (sexp)
  (let* ((question
          (format "(geiser:eval (interaction-environment) %S)"
                  (format "%S" sexp)))
         (answer (geiser-eval--send/wait question)))

    (if (assoc 'error answer)
        (signal 'peculiar-error
                (list (string-trim
                       (car (split-string (geiser-eval--retort-output
                                           answer)
                                          "\t")))))
      ;; from: ((result "expr-tree") (output . ...))
      ;; to: "expr-tree"
      (cadr (car answer)))))

(provide 'geiser-kawa-eval-util)
