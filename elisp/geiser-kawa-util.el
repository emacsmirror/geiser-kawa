;;; geiser-kawa-util.el --- utility functions for `geiser-kawa' -*- lexical-binding:t -*-

;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;; Commentary:
;; Some general utility functions used by the `geiser-kawa' package.

(require 'geiser-syntax)
(require 'geiser-eval)

;; Utility functions used by other parts of `geiser-kawa'.

;;; Code:

(defun geiser-kawa-util--eval-to-res (sexp)
  "Alternative to geiser-eval--send/eval with custom behavior.
If a Throwable has been raised while running in Kawa an error is
signalled.
Argument SEXP is a sexp to evaluate in Kawa."
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
  "Function that skips the reading `geiser-eval--retort-result' does.
Differently from `geiser-eval--retort-result', this function doesn't
have a variable binding depth limit.  We use this when we need to read
strings longer than what `geiser-eval--retort-result' allows.
Drawback is that `RET' must be valid elisp."
  (car (read-from-string (cadr (assoc 'result ret)))))

(defun geiser-kawa-util--repl-point-after-prompt ()
  "If in a Kawa REPL buffer, get point after prompt."
  (save-excursion
    (and (string-prefix-p
          (geiser-repl-buffer-name 'kawa)
          (buffer-name))
         (re-search-backward geiser-kawa--prompt-regexp nil t)
         (re-search-forward geiser-kawa--prompt-regexp nil t))))

(defun geiser-kawa-util--point-is-at-toplevel-p ()
  "Return non-nil if point is at toplevel (not inside a sexp)."
  (equal (point)
         (save-excursion
           (geiser-syntax--pop-to-top)
           (point))))

(provide 'geiser-kawa-util)

;;; geiser-kawa-util.el ends here
