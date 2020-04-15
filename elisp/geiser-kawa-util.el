;;; geiser-kawa-util.el --- utility functions for `geiser-kawa' -*- lexical-binding:t -*-

;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'geiser-syntax)
(require 'geiser-eval)

;; Utility functions used by other parts of `geiser-kawa'.

(defun geiser-kawa-util--eval-to-res (sexp)
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

(defun geiser-kawa-util--retort-result (ret)
  ;; This skips the reading `geiser-eval--retort-result'
  ;; does, but doesn't have the variable binding depth
  ;; limit that `geiser-eval--retort-result' has.
  ;; We use this when we need to read strings longer
  ;; than what `geiser-eval--retort-result' allows.
  ;; Drawback is that `ret' must be valid elisp.
  (car (read-from-string (cadr (assoc 'result ret)))))

(defun geiser-kawa-util--repl-point-after-prompt ()
  (save-excursion
    (and (string-prefix-p
          (geiser-repl-buffer-name 'kawa)
          (buffer-name))
         (re-search-backward geiser-kawa--prompt-regexp nil t)
         (re-search-forward geiser-kawa--prompt-regexp nil t))))

(defun geiser-kawa-util--point-is-at-toplevel-p ()
  (equal (point)
         (save-excursion
           (geiser-syntax--pop-to-top)
           (point))))

(provide 'geiser-kawa-util)

;;; geiser-kawa-util.el ends here
