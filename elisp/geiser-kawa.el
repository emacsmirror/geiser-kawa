;;; geiser-kawa.el --- geiser support for Kawa scheme -*- lexical-binding:t -*-

;; Copyright (C) 2018 Mathieu Lirzin <mthl@gnu.org>
;; Copyright (C) 2019, 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Author: spellcard199 <spellcard199@protonmail.com>
;; Maintainer: spellcard199 <spellcard199@protonmail.com>
;; Keywords: languages, kawa, scheme, geiser
;; Homepage: https://gitlab.com/spellcard199/geiser-kawa
;; Package-Requires: ((emacs "26.1") (geiser "0.11.2"))
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; geiser-kawa extends the `geiser' package to support the Kawa
;; scheme implementation.


(require 'geiser-base)
(require 'geiser-custom)
(require 'geiser-syntax)
(require 'geiser-log)
(require 'geiser-connection)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser)

(require 'compile)
(require 'info-look)
(require 'cl-lib)

(require 'geiser-kawa-globals)
(require 'geiser-kawa-deps)
(require 'geiser-kawa-devutil-complete)
(require 'geiser-kawa-devutil-exprtree)
(require 'geiser-kawa-arglist)
(require 'geiser-kawa-ext-help)


;;; Code:
;;; REPL support:

(defun geiser-kawa--geiser-procedure (proc &rest args)
  "Geiser's marshall-procedure for `geiser-kawa'.
Argument PROC passed by Geiser.
Optional argument ARGS passed by Geiser."

  (cl-case proc
    ((eval compile)
     (format
      "(geiser:eval (interaction-environment) %S)"
      (cadr args)))

    ((load-file compile-file)
     (format "(geiser:load-file %s)" (car args)))

    ((no-values) "(geiser:no-values)")

    (t
     (let ((form (mapconcat 'identity args " ")))
       (format "(geiser:%s %s)" proc form)))))

;; TODO
;; (defun geiser-kawa--find-module (&optional module))

;; Doesn't work:
;; (defun geiser-kawa--symbol-begin (module)
;;  (save-excursion (skip-syntax-backward "^|#") (point)))
;; TODO: see if it needs improvements.
(defun geiser-kawa--symbol-begin (module)
  "Needed for completion.
Copied from geiser-chibi.el, geiser-guile.el, which are identical to
each other.
Argument MODULE argument passed by Geiser."
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))

(defun geiser-kawa--import-command (module)
  "Return command used to import MODULEs."
  (format "(import %s)" module))

(defun geiser-kawa--exit-command ()
  "Command to send to exit from Kawa REPL."
  "(exit 0)")


;;; REPL startup


;;; Error display

;; TODO
(defun geiser-kawa--enter-debugger ()
  "TODO")

(defun geiser-kawa--display-error (module key msg)
  "Needed to show output (besides result).
Modified from geiser-guile.el.
Argument MODULE is passed by Geiser.
Argument KEY is passed by Geiser.
Argument MSG is passed by Geiser."
  (when (stringp msg)
    (save-excursion (insert msg))
    (geiser-edit--buttonize-files))
  (and (not key) (not (zerop (length msg))) msg))


;;; Implementation definition:

(define-geiser-implementation kawa
  (unsupported-procedures '(find-file
                            symbol-location
                            module-location
                            symbol-documentation
                            module-exports
                            callers
                            callees
                            generic-methods))
  (binary geiser-kawa--binary)
  (arglist geiser-kawa-arglist)
  (version-command geiser-kawa--version-command)
  (repl-startup geiser-kawa--repl-startup)
  (prompt-regexp geiser-kawa--prompt-regexp)
  (debugger-prompt-regexp nil)
  (marshall-procedure geiser-kawa--geiser-procedure)
  ;; TODO
  ;; (find-module geiser-kawa--find-module nil)
  (exit-command geiser-kawa--exit-command)
  (import-command geiser-kawa--import-command)
  (find-symbol-begin geiser-kawa--symbol-begin)
  (display-error geiser-kawa--display-error)
  (case-sensitive nil)
  (external-help geiser-kawa--manual-look-up))

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'kawa t)
(geiser-impl--add-to-alist 'regexp "\\.sld$" 'kawa t)

;; Check for kawa-geiser jar each time `run-kawa' is called.
(geiser-kawa-deps--run-kawa--advice-add)

(provide 'geiser-kawa)

;;; geiser-kawa.el ends here
