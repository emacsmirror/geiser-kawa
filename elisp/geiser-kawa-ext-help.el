;;; geiser-kawa-ext-help.el --- Support for the "external-help" geiser feature -*- lexical-binding:t -*-

;;; Commentary:
;; Functions for providing the "external-help" Geiser feature.
;; Currently, the external help for Kawa is the kawa manual in either
;; its .info or .epub format.  For the feature to work
;; `geiser-kawa-manual-path' must point to where the .info or .epub
;; Kawa manual is located.

;;; Code:

;; Support for manual in .epub format

(cl-defun geiser-kawa-manual--epub-unzip-to-tmpdir
    (&optional (epub-path geiser-kawa-manual-path))
  "Unzip the .epub file using kawa/java.

Rationale for using java instead of emacs:
- kawa is already a dependency
- kawa/java is more portable that using emacs' `arc-mode', which relies
  on external executables being installed"
  (with-temp-buffer
    (with--geiser-implementation
        'kawa
      (geiser-eval--send/result
       (format
        "(geiser:eval (interaction-environment) %S)"
        (format "(geiser:manual-epub-unzip-to-tmp-dir %S)"
                epub-path))))))

(defvar geiser-kawa-manual--epub-cached-overall-index
  nil
  "Since `eww-open-file' is slow we use it just the first time.
Then we cache the result in this variable so that future lookups in
the manual are more responsive.")

(cl-defun geiser-kawa-manual--epub-search
    (needle &optional (epub-path geiser-kawa-manual-path))
  ;; Validate args
  (assert (stringp needle) nil (type-of needle))
  (assert (stringp epub-path) nil (type-of epub-path))
  (assert (string-suffix-p ".epub" epub-path) nil epub-path)
  (assert (file-exists-p epub-path) nil epub-path)

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
              (format "%s/OEBPS/Overall-Index.xhtml" unzipped-epub-dir))
             (epub-man-buffer
              (get-buffer-create "*geiser-kawa-epub-manual*")))
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
(defun geiser-kawa-manual--look-up (id mod)
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
    (geiser-kawa-manual--epub-search (symbol-name id)
                                     geiser-kawa-manual-path))
   ((string-suffix-p ".info" geiser-kawa-manual-path)
    (geiser-kawa-manual--info-search (symbol-name id)
                                     geiser-kawa-manual-path))
   (t (error "Supported formats for `geiser-kawa-manual-path' are only
   `.epub' and `.info'"))))

(provide 'geiser-kawa-ext-help)

;;; geiser-kawa-ext-help.el ends here
