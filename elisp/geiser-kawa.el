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
;; Package-Requires: ((emacs "26.1") (geiser "20191025.650"))
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

(require 'geiser-kawa-complete-java)

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
  "geiser-kawa's directory.")

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


;; Download, compile and package "kawa-geiser" and its recursive
;; dependencies into a fat jar.
(defun geiser-kawa-mvn-package-java-deps()
  (interactive)
  (let ((default-directory geiser-kawa-dir))
    (compile "mvn package")))

;; Using `mvn package' from the pom.xml's directory should produce a
;; jar containing all the java dependencies.
(defcustom geiser-kawa-kawa-geiser-jar-path
  (expand-file-name
   "./target/kawa-geiser-wrapper-0.1-SNAPSHOT-jar-with-dependencies.jar"
   geiser-kawa-dir)
  "Path to the kawa-geiser fat jar."
  :type 'string
  :group 'geiser-kawa)


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

(defcustom geiser-kawa-use-kawa-version-included-in-kawa-geiser
  nil
  "Instead of downloading kawa yourself, you can use the Kawa version
 included in geiser-kawa, which is the head of Kawa's master branch."
  :type 'boolean
  :group 'geiser-kawa)


;;; REPL support:

(defun geiser-kawa--binary ()
  ". If `geiser-kawa-binary' is a list, take the first and ignore
 `geiser-kawa-use-kawa-version-included-in-kawa-geiser'."
  (if geiser-kawa-use-kawa-version-included-in-kawa-geiser
      "java"
    (if (listp geiser-kawa-binary)
        (car geiser-kawa-binary)
      geiser-kawa-binary)))

(defun geiser-kawa--make-classpath ()
  (let ((jars
         (append
          (if (and
               (not geiser-kawa-use-kawa-version-included-in-kawa-geiser)
               (executable-find geiser-kawa-binary))
              (let ((lib-dir (expand-file-name
                              "../lib/"
                              (file-name-directory
                               (executable-find geiser-kawa-binary)))))
                (if (file-directory-p lib-dir)
                    (list
                     (concat lib-dir "kawa.jar")
                     (concat lib-dir "servlet.jar")
                     (concat lib-dir "domterm.jar")
                     (concat lib-dir "jline.jar"))
                  nil))
            nil)
          (list geiser-kawa-kawa-geiser-jar-path))))
    (mapconcat #'identity jars ":")))

(defvar geiser-kawa--arglist
  `(;; jline "invisibly" echoes user input and prints ansi chars that
    ;; makes harder detecting end of output and finding the correct
    ;; prompt regexp.
    "console:use-jline=no"
    "-e"
    "(require <kawageiser.Geiser>)"
    "--"))

(defun geiser-kawa--parameters ()
  "Return a list with all parameters needed to start Kawa Scheme."
  (append
   (list (format "-Djava.class.path=%s" (geiser-kawa--make-classpath)))
   (if geiser-kawa-use-kawa-version-included-in-kawa-geiser
       (list "kawa.repl"))
   geiser-kawa--arglist))

(defconst geiser-kawa--prompt-regexp
  "#|kawa:[0-9]+|# ")

(defun geiser-kawa--geiser-procedure (proc &rest args)

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
  ;; Needed for completion. Copied from geiser-chibi.el,
  ;; geiser-guile.el, which are identical to each other.
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))

(defun geiser-kawa--import-command (module)
  (format "(import %s)" module))

(defun geiser-kawa--exit-command ()
  "(exit 0)")


;;; REPL startup

(defun geiser-kawa--version-command (binary)
  (let ((prog+vers (car (process-lines binary "--version"))))
    (cadr (split-string prog+vers " "))))

(defun geiser-kawa--repl-startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)))


;;; Error display

;; TODO
(defun geiser-kawa--enter-debugger ())

(defun geiser-kawa--display-error (module key msg)
  ;; Needed to show output (besides result). Modified from
  ;; geiser-guile.el.
  (when (stringp msg)
    (save-excursion (insert msg))
    (geiser-edit--buttonize-files))
  (and (not key) (not (zerop (length msg))) msg))


;;; Manual lookup

;;;; Support for manual in .epub format

;; FIXME: port old scheme logic to java
(cl-defun geiser-kawa--manual-epub-unzip-to-tmpdir
    (&optional (epub-path geiser-kawa--manual))
  "Unzip the .epub file with kawa/java, since:
- kawa is already a dependency
- kawa/java is more portable that using emacs' arc-mode, which relies
  on external executables installed"
  (with-temp-buffer
    (with--geiser-implementation
        'kawa
      (geiser-eval--send/result
       (format
        "(geiser:eval (interaction-environment) %S)"
        (format "(geiser:manual-epub-unzip-to-tmp-dir %S)"
                epub-path))))))

(defvar geiser-kawa--manual-epub-cached-overall-index
  nil
  "Since `eww-open-file' is slow we use it just the first time.
Then we cache the result in this variable so that future lookups in
the manual are more responsive.")

(cl-defun geiser-kawa--manual-epub-search
    (needle &optional (epub-path geiser-kawa-manual-path))
  ;; Validate args
  (assert (stringp needle) nil (type-of needle))
  (assert (stringp epub-path) nil (type-of epub-path))
  (assert (string-suffix-p ".epub" epub-path) nil epub-path)
  (assert (file-exists-p epub-path) nil epub-path)

  (with-current-buffer (get-buffer-create
                        " *geiser-kawa-epub-manual*")
    (eww-mode)
    (if geiser-kawa--manual-epub-cached-overall-index
        (progn
          (read-only-mode -1)
          (delete-region (point-min) (point-max))
          (insert geiser-kawa--manual-epub-cached-overall-index))
      (let* ((unzipped-epub-dir
              ;; Ask kawa to unzip epub: more portable than unzipping
              ;; with emacs' `arc-mode'.
              (geiser-kawa--manual-epub-unzip-to-tmpdir epub-path))
             (overall-index-file
              (format "%s/OEBPS/Overall-Index.xhtml" unzipped-epub-dir))
             (epub-man-buffer
              (get-buffer-create "*geiser-kawa-epub-manual*")))
        (when (not unzipped-epub-dir)
          (error "Can't open manual: Kawa did not unzip the epub when asked."))
        (eww-open-file overall-index-file)
        ;; Store overall index page in a variable to be used as cache.
        (setq geiser-kawa--manual-epub-cached-overall-index (buffer-string))))

    ;; At this point the Overall Index page should be opened.
    (goto-char (point-min))
    (if (search-forward (concat "\n" needle ": ") nil t) ;; Search
        (progn
          (backward-char 3) ;; Move point over link
          (eww-browse-url (car (eww-links-at-point))) ;; Follow link
          (recenter-top-bottom 'top))
      (message (format "No match for `%s' found in Kawa's epub manual." needle)))))

;;;; Support for manual in .info format
(cl-defun geiser-kawa--manual-info-search
    (needle &optional (info-path geiser-kawa-manual-path))

  ;; Validate args
  (assert (stringp needle) nil (type-of needle))
  (assert (stringp info-path) nil (type-of info-path))
  (assert (string-suffix-p ".info" info-path) nil info-path)
  (assert (file-exists-p info-path) nil info-path)

  (with-current-buffer (get-buffer-create "*geiser-kawa-info-manual*")
    (info info-path (current-buffer))
    (Info-goto-node "Overall Index")
    (if (search-forward (concat "\n* " needle) nil t)
        (progn
          (Info-follow-nearest-node)
          (recenter-top-bottom 'top))
      (progn
        (quit-window)
        (message (format "No match for `%s' found in Kawa's info manual."
                         needle))))))

;;;; Dispatch to epub or info manual function based on
;;;; `geiser-kawa-manual-path's file extension.
(defun geiser-kawa--manual-look-up (id mod)
  "Use epub or info manual depending on `geiser-kawa-manual-path'.

Argument ID is the symbol to look for in the manual.
Argument MOD is passed by geiser, but it's not used here."
  (assert (file-exists-p geiser-kawa-manual-path)
          nil (format
               (concat
                "Kawa's manual file specified by "
                "`geiser-kawa-manual-path' does not exist: \"%s\"")
               geiser-kawa-manual-path))
  (cond
   ((string-suffix-p ".epub" geiser-kawa-manual-path)
    (geiser-kawa--manual-epub-search (symbol-name id)
                                     geiser-kawa-manual-path))
   ((string-suffix-p ".info" geiser-kawa-manual-path)
    (geiser-kawa--manual-info-search (symbol-name id)
                                     geiser-kawa-manual-path))
   (t (error "Supported formats for `geiser-kawa-manual-path' are only `.epub' and `.info'"))))


;;; Implementation definition:

(eval
 ;; (temporary?) Workaround for Cask issue. Wrapping
 ;; `define-geiser-implementation' with `eval' avoids issue
 ;; https://github.com/cask/cask/issues/472 in projects that depend on
 ;; geiser-kawa.
 '(define-geiser-implementation kawa
    (unsupported-procedures '(macroexpand
                              find-file
                              symbol-location
                              module-location
                              symbol-documentation
                              module-exports
                              callers
                              callees
                              generic-methods))
    (binary geiser-kawa--binary)
    (arglist geiser-kawa--parameters)
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
    (external-help geiser-kawa--manual-look-up)))

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'kawa t)
(geiser-impl--add-to-alist 'regexp "\\.sld$" 'kawa t)

(provide 'geiser-kawa)

;;; geiser-kawa.el ends here
