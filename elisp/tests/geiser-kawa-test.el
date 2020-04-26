;;; test-geiser-kawa.el --- tests for `geiser-kawa's elisp side -*- lexical-binding:t -*-

;; Copyright (C) 2019, 2020 spellcard199 <spellcard199@protonmail.com>

;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;; Tests for testing elisp side of `geiser-kawa`.

(require 'buttercup)

(require 'geiser)
(require 'geiser-mode)
(require 'geiser-kawa)
(require 'gnus-util)

;;; Code:

(defun geiser-kawa-test--switch-reset-work-buffer()
  "Reset *test-geiser-kawa*.
New tests expect a clean buffer to run."
  (switch-to-buffer "*geiser-kawa-test*")
  (delete-region (point-min) (point-max))
  (geiser-impl--set-buffer-implementation 'kawa))

(describe
 "run-kawa"

 (before-all

  (print "[test-geiser-kawa.el] Running `mvnw package'...")

  (geiser-kawa-deps-mvnw-package geiser-kawa-dir)

  (while compilation-in-progress
    (sleep-for 0 250))

  (print "[test-geiser-kawa.el] `mvnw package' done.")

  (setq geiser-kawa-use-included-kawa t)
  (geiser-kawa-test--switch-reset-work-buffer)
  (run-kawa)
  (geiser-mode))

 (before-each
  (geiser-kawa-test--switch-reset-work-buffer))

 (it (concat "can find " geiser-kawa-deps-jar-path)
     (expect
      (file-exists-p geiser-kawa-deps-jar-path)))

 (it "can `run-kawa'"
     (expect
      (process-live-p (get-buffer-process
                       (get-buffer "* Kawa REPL *")))))

 (it "can `geiser-eval-buffer'"
     (expect
      (progn
        (insert "(display 'foobar)")
        (geiser-eval-buffer))
      :to-equal '((result "") (output . "foobar"))))

 (it "can `geiser:autodoc'"
     ;; TODO: How to test directly
     ;; `geiser-autodoc--autodoc-at-point'?
     ;; Always returns `nil' when run inside the tests.
     (expect
      (caar
       (geiser-eval--send/result
        (prin1-to-string
         '(geiser:eval (interaction-environment)
                       "(geiser:autodoc '(display))"))))
      :to-equal "display"))

 (it "can `macroexpand'"
     (expect
      (progn
        (insert "(when #t 'foo 'bar)")
        (goto-char (point-max))
        (geiser-expand-last-sexp)
        (geiser-debug--with-buffer
         (buffer-substring-no-properties (point-min) (point-max))))
      :to-equal "(if #t (begin (quote foo) (quote bar)))"))

 (it "can `geiser:completions'"
     (expect (geiser-completion--complete "dis" nil))
     :to-equal '(display disassemble))

 (it "can `geiser-kawa-devutil-complete--get-data'"
     (expect
      (cadr (assoc "completion-type"
                   (geiser-kawa-devutil-complete--get-data
                    "(java.lang.String:)" 18))))
     :to-equal "METHODS")

 (it "can `geiser-kawa-devutil-exprtree--for-expression'"
     (expect
      (string-suffix-p "SimpleSymbol)))"
                       (geiser-kawa-devutil-exprtree--for
                        "(display 'foobar)")))
     :to-equal t)

 (it "can `geiser-kawa-devutil-complete--exprtree'"
     (expect
      (string-prefix-p "(Module"
                       (geiser-kawa-devutil-complete--exprtree
                        "(java.lang.String:)" 18)))
     :to-equal t)

 )

(provide 'geiser-kawa-test)

;;; geiser-kawa-test.el ends here
