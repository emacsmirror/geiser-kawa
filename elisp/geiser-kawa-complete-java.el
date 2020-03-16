;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'subr-x)

(defun geiser-kawa--repl--point-after-prompt ()
  (save-excursion
    (and (string-prefix-p
          (geiser-repl-buffer-name 'kawa)
          (buffer-name))
         (re-search-backward geiser-kawa--prompt-regexp nil t)
         (re-search-forward geiser-kawa--prompt-regexp nil t))))

(defun geiser-kawa--point-at-toplevel-p ()
  (equal (point)
         (save-excursion
           (geiser-syntax--pop-to-top)
           (point))))

(defun geiser-kawa-complete-java--get-data (code-str cursor-index)
  "`code' is a string containing the code. It must be syntatically
  scheme, including balanced parentheses.
`cursor' is an integer representing where the cursor is in that code."
  (let* ((geiser-question
          ;; this formatting hell is caused by the fact geiser:eval
          ;; takes a string instead of a form.
          (format "(geiser:eval (interaction-environment) %S)"
                  (format "%S"
                          `(geiser:complete-java
                            ,code-str
                            ,cursor-index
                            (gnu.expr.Language:getDefaultLanguage)
                            (interaction-environment)))))
         (geiser-answer (geiser-eval--send/wait
                         geiser-question)))

    (if (assoc 'error geiser-answer)
        (signal 'peculiar-error
                (list (string-trim
                       (car (split-string (geiser-eval--retort-output
                                           geiser-answer)
                                          "\t")))))
      (geiser-eval--retort-result geiser-answer))))

(defun geiser-kawa-complete-java--user-choice--field-or-method
    (fm-compl-data)
  ;; fm stands for field or method.
  (let ((compl-for-class
         (cadr (assoc "compl-for-class" fm-compl-data)))
        (modifiers
         (cadr (assoc "modifiers" fm-compl-data)))
        (field-or-method
         (cadr (assoc "field-or-method-or-package" fm-compl-data)))
        (names
         (cadr (assoc "names" fm-compl-data)))
        (before-cursor
         (cadr (assoc "before-cursor" fm-compl-data)))
        ;; unused
        (after-cursor
         (cadr (assoc "after-cursor" fm-compl-data))))

    (completing-read
     (concat "(" (string-join modifiers " ") " " field-or-method ") "
             compl-for-class "."
             ;; "- Exp  : " compl-for-expr "\n"
             ;; ": "
             )
     names
     nil
     nil
     before-cursor)))

(defun geiser-kawa-complete-java--user-choice--package
    (package-compl-data)
  (let ((field-or-method-or-package
         (cadr (assoc "field-or-method-or-package" package-compl-data)))
        (package-name
         (cadr (assoc "package-name" package-compl-data)))
        (names
         (cadr (assoc "names" package-compl-data)))
        (before-cursor
         (cadr (assoc "before-cursor" package-compl-data)))
        ;; unused
        (after-cursor
         (cadr (assoc "after-cursor" package-compl-data))))
    (completing-read
     (concat "(" field-or-method-or-package ") "
             (if (string-equal "" package-name)
                 "(root.)"
               (concat package-name ".")))
     (mapcar (lambda (name)
               (string-remove-prefix
                "." (string-remove-prefix package-name name)))
             names)
     nil
     nil
     (string-remove-prefix
      "." (string-remove-prefix package-name before-cursor))
     )))

(defun geiser-kawa-complete-java--user-choice-dispatch
    (compl-data)
  (let ((compl-for (cadr (assoc "field-or-method-or-package"
                                compl-data))))
    (cond ((equal compl-for "FIELD")
           (geiser-kawa-complete-java--user-choice--field-or-method
            compl-data))
          ((equal compl-for "METHOD")
           (geiser-kawa-complete-java--user-choice--field-or-method
            compl-data))
          ((equal compl-for "PACKAGE")
           (geiser-kawa-complete-java--user-choice--package
            compl-data))
	  ((equal compl-for nil)
	   (message "No completions found.")
	   "")
          (t (error (format "[Unexpected condition] compl-for: %s"
                            (prin1-to-string compl-for)))))))

(defun geiser-kawa-complete-java--code-point-from-toplevel ()
  (let* (reg-beg
         reg-end
         code-str
         cursor-index)
    (if (geiser-kawa--point-at-toplevel-p)
        (progn
          (setq reg-beg (line-beginning-position))
          (setq reg-end (line-end-position))
          (setq cursor-index (current-column)))
      (progn
        (save-excursion
          (setq reg-beg (progn (geiser-syntax--pop-to-top)
                               (point)))
          (setq reg-end (condition-case data
                            (progn (forward-sexp)
                                   (point))
                          (scan-error data))))
        (when (and (listp reg-end)
                   (equal (car reg-end) 'scan-error))
          ;; For now, it's easier not to fix unbalanced parenses
          (signal (car reg-end) (cdr reg-end)))
        (setq cursor-index (- (point) reg-beg))))
    (setq code-str (buffer-substring-no-properties
                    reg-beg reg-end))
    (list
     `("reg-beg"      . ,reg-beg)
     `("reg-end"      . ,reg-end)
     `("code-str"     . ,code-str)
     `("cursor-index" . ,cursor-index))))

(defun geiser-kawa-complete-fmp-at-point ()
  (interactive)
  "Complete java field or method or package (fmp) at point"

  (let* ((code-and-point-data
          (geiser-kawa-complete-java--code-point-from-toplevel))
         (code-str     (cdr (assoc "code-str"
                                   code-and-point-data)))
         (cursor-index (cdr (assoc "cursor-index"
                                   code-and-point-data)))
         (compl-data (geiser-kawa-complete-java--get-data
                      code-str cursor-index))
         (user-choice (geiser-kawa-complete-java--user-choice-dispatch
                       compl-data)))
    (when (thing-at-point 'word)
      (if (looking-back ":" (- (point) 2))
          (kill-word 1)
        (kill-word -1)))
    (insert user-choice)
    ;; (when (not (equal (word-at-point) user-choice))
    ;;   (kill-word 1)
    ))

(provide 'geiser-kawa-complete-java)

;;; geiser-kawa-complete-java.el ends here
