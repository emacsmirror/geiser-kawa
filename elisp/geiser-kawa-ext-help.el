;;; geiser-kawa-ext-help.el --- Support for the "external-help" geiser feature -*- lexical-binding:t -*-

;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;; Commentary:
;; Functions for providing the "external-help" Geiser feature.
;; Currently, the external help for Kawa is the kawa manual in either
;; its .info or .epub format.  For the feature to work
;; `geiser-kawa-manual-path' must point to where the .info or .epub
;; Kawa manual is located.
;; Depends on global variables:
;; `geiser-kawa-binary'

(require 'cl-lib)
(require 'geiser-custom)
(require 'geiser-impl)
(require 'geiser-eval)
(require 'eww)
(require 'info)
(require 'geiser-kawa-globals)

;;; Code:

;; Support for manual in .epub format

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

(cl-defun geiser-kawa-manual--epub-unzip-to-tmpdir
    (&optional (epub-path geiser-kawa-manual-path))
  "Unzip the .epub file using kawa/java.

Rationale for using java instead of emacs:
- Kawa is already a dependency.
- Kawa/java is more portable that using emacs' `arc-mode',
  which relies on external executables being installed."
  (with-temp-buffer
    (geiser-impl--set-buffer-implementation 'kawa)
    (geiser-eval--send/result
     (format
      "(geiser:eval (interaction-environment) %S)"
      (format "(geiser:manual-epub-unzip-to-tmp-dir %S)"
              epub-path)))))

(defvar geiser-kawa-manual--epub-cached-overall-index
  nil
  "Since `eww-open-file' is slow we use it just the first time.
Then we cache the result in this variable so that future lookups in
the manual are more responsive.")

(cl-defun geiser-kawa-manual--epub-search
    (needle &optional (epub-path geiser-kawa-manual-path))
  ;; Validate args
  (cl-assert (stringp needle) nil (type-of needle))
  (cl-assert (stringp epub-path) nil (type-of epub-path))
  (cl-assert (string-suffix-p ".epub" epub-path) nil epub-path)
  (cl-assert (file-exists-p epub-path) nil epub-path)

  (with-current-buffer (get-buffer-create
                        " *geiser-kawa-epub-manual*")
    (eww-mode)
    (if geiser-kawa-manual--epub-cached-overall-index
        (progn
          (read-only-mode -1)
          (delete-region (point-min) (point-max))
          (insert geiser-kawa-manual--epub-cached-overall-index))
      (let* ((unzipped-epub-dir
              ;; Ask kawa to unzip epub: more portable than unzipping
              ;; with emacs' `arc-mode'.
              (geiser-kawa-manual--epub-unzip-to-tmpdir epub-path))
             (overall-index-file
              (format "%s/OEBPS/Overall-Index.xhtml" unzipped-epub-dir)))
        (unless unzipped-epub-dir
          (error "Can't open manual: Kawa did not unzip the epub when asked"))
        (eww-open-file overall-index-file)
        ;; Store overall index page in a variable to be used as cache.
        (setq geiser-kawa-manual--epub-cached-overall-index (buffer-string))))
    ;; At this point the "Overall Index" page should be opened.
    (goto-char (point-min))
    (if (search-forward (concat "\n" needle ": ") nil t) ;; Search
        (progn
          (backward-char 3) ;; Move point over link
          (eww-browse-url (car (eww-links-at-point))) ;; Follow link
          (recenter-top-bottom 'top))
      (message (format "No match for `%s' found in Kawa's epub manual." needle)))))


;; Support for manual in .info format

(cl-defun geiser-kawa-manual--info-search
    (needle &optional (info-path geiser-kawa-manual-path))
  ;; Validate args
  (cl-assert (stringp needle) nil (type-of needle))
  (cl-assert (stringp info-path) nil (type-of info-path))
  (cl-assert (string-suffix-p ".info" info-path) nil info-path)
  (cl-assert (file-exists-p info-path) nil info-path)

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
(defun geiser-kawa-manual--look-up (id mod)
  "Use epub or info manual depending on `geiser-kawa-manual-path'.

Argument ID is the symbol to look for in the manual.
Argument MOD is passed by geiser, but it's not used here yet."
  (cl-assert (file-exists-p geiser-kawa-manual-path)
             nil (format
                  (concat
                   "Kawa's manual file specified by "
                   "`geiser-kawa-manual-path' does not exist: \"%s\"")
                  geiser-kawa-manual-path))
  (cond
   ((string-suffix-p ".epub" geiser-kawa-manual-path)
    (geiser-kawa-manual--epub-search (symbol-name id)
                                     geiser-kawa-manual-path))
   ((string-suffix-p ".info" geiser-kawa-manual-path)
    (geiser-kawa-manual--info-search (symbol-name id)
                                     geiser-kawa-manual-path))
   (t (error "Supported formats for `geiser-kawa-manual-path' are only
   `.epub' and `.info'"))))

(provide 'geiser-kawa-ext-help)

;;; geiser-kawa-ext-help.el ends here
