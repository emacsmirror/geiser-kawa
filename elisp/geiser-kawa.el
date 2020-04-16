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
(require 'cl)

(require 'geiser-kawa-deps)
(require 'geiser-kawa-devutil-complete)
(require 'geiser-kawa-devutil-exprtree)
(require 'geiser-kawa-arglist)
(require 'geiser-kawa-ext-help)

;;; Code:


;; Adaptations for making this package separate from geiser

;; Adapted from geiser.el
;;;###autoload
(defconst geiser-kawa-elisp-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory containing geiser-kawa's Elisp files.")

;; Adapted from geiser.el
;;;###autoload
(defconst geiser-kawa-dir
  (if (string-suffix-p "elisp/" geiser-kawa-elisp-dir)
      (expand-file-name "../" geiser-kawa-elisp-dir)
    geiser-kawa-elisp-dir)
  "Directory where geiser-kawa is located.")

;; Adapted from geiser.el
(custom-add-load 'geiser-kawa (symbol-name 'geiser-kawa))
(custom-add-load 'geiser      (symbol-name 'geiser-kawa))

;; Moved from geiser.el
;;;###autoload
(autoload 'run-kawa "geiser-kawa" "Start a Geiser Kawa Scheme REPL." t)

;;;###autoload
(autoload 'switch-to-kawa "geiser-kawa"
  "Start a Geiser Kawa Scheme REPL, or switch to a running one." t)

;; `geiser-active-implementations' is defined in `geiser-impl.el'
(add-to-list 'geiser-active-implementations 'kawa)

;; End of adaptations for making this package separate from geiser


;;; Customization:

(defgroup geiser-kawa nil
  "Customization for Geiser's Kawa Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom
    geiser-kawa-binary "kawa"
  "Name to use to call the Kawa Scheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-kawa)

(geiser-custom--defcustom
    geiser-kawa-manual-path
    (when (executable-find geiser-kawa-binary)
      (expand-file-name
       "../doc/kawa-manual.epub"
       (file-name-directory
        (executable-find geiser-kawa-binary))))
  "Path of kawa manual. Supported formats are `.epub' (using
`eww-mode') and `.info' (using `info.el')."
  :type 'string
  :group 'geiser-kawa)

(defcustom geiser-kawa-deps-jar-path
  (geiser-kawa-deps--jar-path geiser-kawa-dir)
  "Path to the kawa-geiser fat jar."
  :type 'string
  :group 'geiser-kawa)

(defcustom geiser-kawa-use-included-kawa
  nil
  "Use the Kawa included with `geiser-kawa' instead of the `kawa' binary.

Instead of downloading kawa yourself, you can use the Kawa version
included in `geiser-kawa'."
  :type 'boolean
  :group 'geiser-kawa)


;;; REPL support:

(defconst geiser-kawa--prompt-regexp
  "#|kawa:[0-9]+|# ")

(defun geiser-kawa--geiser-procedure (proc &rest args)
  "Geiser's marshall-procedure for `geiser-kawa'.
Argument PROC passed by Geiser.
Optional argument ARGS passed by Geiser."

  (case proc
    ((eval compile)
     (let* ((form (mapconcat 'identity args " ")) ;;unused
            (send-this
             (format
              "(geiser:eval (interaction-environment) %S)"
              (cadr args))))
       send-this))

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

(defun geiser-kawa--version-command (binary)
  "Return command to get kawa version.
Argument BINARY argument passed by Geiser."
  (let* ((program (if geiser-kawa-use-included-kawa
                      "java"
                    "kawa"))
         (args  (if geiser-kawa-use-included-kawa
                    (list (geiser-kawa-arglist--make-classpath-arg
                           geiser-kawa-deps-jar-path)
                          "kawa.repl"
                          "--version")
                  (list "--version")))
         (output (apply #'process-lines
                        (cons program args)))
         (progname-plus-version (car output)))
    ;; `progname-plus-version' is something like:
    ;; "Kawa 3.1.1"
    (cadr (split-string progname-plus-version " "))))

(defun geiser-kawa--repl-startup (remote)
  "Geiser's repl-startup.
Argument REMOTE passed by Geiser."
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)))


;;; Error display

;; TODO
(defun geiser-kawa--enter-debugger ()
  "TODO.")

(defun geiser-kawa--display-error (module key msg)
  "Needed to show output (besides result).
Modified from geiser-guile.el.
Argument MODULE passed by Geiser.
Argument KEY passed by Geiser.
Argument MSG passed by Geiser."
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
