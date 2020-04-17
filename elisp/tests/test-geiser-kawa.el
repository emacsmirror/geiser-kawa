;;; test-geiser-kawa.el --- tests for `geiser-kawa's elisp side -*- lexical-binding:t -*-

;; Copyright (C) 2019, 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'geiser)
(require 'geiser-mode)
(require 'geiser-kawa)
(require 'gnus-util)

(defun switch-to-and-reset-scratch-buffer()
  (switch-to-buffer "*scratch*")
  (delete-region (point-min) (point-max))
  (geiser-impl--set-buffer-implementation 'kawa))

(describe
 "run-kawa"

 (before-all

  (print "[test-geiser-kawa.el] Running `mvnw package'...")

  (let ((mvnw-buf (geiser-kawa-deps-mvnw-package geiser-kawa-dir)))
    (while compilation-in-progress
      (sleep-for 0 250)))

  (print "[test-geiser-kawa.el] `mvnw package' done.")

  (setq geiser-kawa-use-included-kawa t)
  (switch-to-and-reset-scratch-buffer)
  (run-kawa)
  (geiser-mode))

 (before-each
  (switch-to-and-reset-scratch-buffer))

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

;;; test-geiser-kawa.el ends here
