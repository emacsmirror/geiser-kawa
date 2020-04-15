;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'subr-x)
(require 'geiser-kawa-devutil-exprtree)
(require 'geiser-kawa-util)

(defvar geiser-kawa-devutil-complete-add-missing-parentheses
  nil
  "If true, when completing with kawa-devutil we don't check
for missing parentheses and just let kawa-devutil append them
at the end.")

(defun geiser-kawa-devutil-complete--get-data (code-str cursor-index)
  "`code' is a string containing the code. It must be syntatically
  scheme, including balanced parentheses.
`cursor' is an integer representing where the cursor is in that code."
  (let* ((geiser-question
          ;; this formatting hell is caused by the fact geiser:eval
          ;; takes a string instead of a form.
          (format "(geiser:eval (interaction-environment) %S)"
                  (format "%S"
                          `(geiser:kawa-devutil-complete
                            ,code-str
                            ,cursor-index))))
         (geiser-answer (geiser-eval--send/wait
                         geiser-question)))

    (if (assoc 'error geiser-answer)
        (signal 'peculiar-error
                (list (string-trim
                       (car (split-string (geiser-eval--retort-output
                                           geiser-answer)
                                          "\t")))))

      (geiser-kawa-util--retort-result geiser-answer))))

(defun geiser-kawa-devutil-complete--user-choice-classmembers
    (classmember-data)

  (let* ((completion-type
          (cadr (assoc "completion-type" classmember-data)))
         (before-cursor
          (cadr (assoc "before-cursor" classmember-data)))
         (after-cursor ;; unused
          (cadr (assoc "after-cursor" classmember-data)))
         (owner-class
          (cadr (assoc "owner-class" classmember-data)))
         (modifiers
          (cadr (assoc "modifiers" classmember-data)))
         (names
          (cadr (assoc "names" classmember-data)))

         (prompt
          (concat "("
                  (string-join modifiers " ") " " completion-type
                  ") "
                  owner-class ".")))

    (completing-read prompt names
                     nil nil
                     before-cursor)))

(defun geiser-kawa-devutil-complete--user-choice-symbols-plus-packagemembers
    (syms-plus-pkgmembers-data)
  (let* ((completion-type
          (cadr (assoc "completion-type" syms-plus-pkgmembers-data)))
         (before-cursor
          (cadr (assoc "before-cursor" syms-plus-pkgmembers-data)))
         (symbol-names
          (cadr (assoc "symbol-names" syms-plus-pkgmembers-data)))
         (pkgmembers-data
          (cadr (assoc "package-members" syms-plus-pkgmembers-data)))
         (owner-package
          (cadr (assoc "owner-package" pkgmembers-data)))
         (child-package-names
          (cadr (assoc "child-package-names" pkgmembers-data)))
         (child-class-names
          (cadr (assoc "child-class-names" pkgmembers-data)))

         (prompt (concat "(" completion-type
                         " (" owner-package ".) "
                         "(" before-cursor ")"
                         ") "))
         (candidates
          (append symbol-names
                  (mapcar (lambda (n) (concat n "."))
                          child-package-names)
                  child-class-names))
         (initial-input
          (string-remove-prefix
           (if (and owner-package
                    (not (string-equal owner-package "")))
               (concat owner-package ".")
             "")
           before-cursor))
         (choice
          (completing-read prompt candidates
                           nil nil initial-input)))
    choice))

(defun geiser-kawa-devutil-complete--user-choice-dispatch
    (compl-data)
  (let ((completion-type
         (cadr (assoc "completion-type" compl-data))))
    (cond ((or (equal completion-type "METHODS")
               (equal completion-type "FIELDS"))
           (geiser-kawa-devutil-complete--user-choice-classmembers compl-data))
          ((equal completion-type "SYMBOLS_PLUS_PACKAGEMEMBERS")
           (geiser-kawa-devutil-complete--user-choice-symbols-plus-packagemembers
            compl-data))
          ((equal completion-type nil)
	   (message "No completions found.")
	   "")
          (t (error (format "[Unexpected `completion-type' value] completion-type: %s"
                            (prin1-to-string completion-type))))
          )))

(defun geiser-kawa-devutil-complete--code-point-from-toplevel ()
  (let* (reg-beg
         reg-end
         code-str
         cursor-index)
    (if (geiser-kawa-util--point-is-at-toplevel-p)
        (let ;; At toplevel
            ((repl-point-after-prompt
              (geiser-kawa-util--repl-point-after-prompt)))
          (if repl-point-after-prompt
              (progn    ;; Inside the REPL
                (setq reg-beg repl-point-after-prompt)
                (setq reg-end (point-max))
                (setq cursor-index (- (point)
                                      repl-point-after-prompt)))
            (progn    ;; Not inside the REPL
              (setq reg-beg (line-beginning-position))
              (setq reg-end (line-end-position))
              (setq cursor-index (current-column)))))
      (progn ;; Not at toplevel
        (save-excursion
          (setq reg-beg (progn (geiser-syntax--pop-to-top)
                               (point)))
          (setq reg-end (condition-case data
                            (progn (forward-sexp)
                                   (point))
                          (scan-error data))))
        (when (and (listp reg-end)
                   (equal (car reg-end) 'scan-error))
          (if geiser-kawa-devutil-complete-add-missing-parentheses
              ;; kawa-devutil appends missing parentheses at the end,
              ;; so in many cases this would work, but I'm not sure
              ;; it's a good thing not to let the developer know
              ;; about it.
              (setq reg-end (point))
            (signal (car reg-end) (cdr reg-end))))
        (setq cursor-index (- (point) reg-beg))))
    (setq code-str (buffer-substring-no-properties
                    reg-beg reg-end))
    (setq moo code-str)
    (list
     `("reg-beg"      . ,reg-beg)
     `("reg-end"      . ,reg-end)
     `("code-str"     . ,code-str)
     `("cursor-index" . ,cursor-index))))

(defun geiser-kawa-devutil-complete-at-point ()
  (interactive)
  "Complete at point using kawa-devutil's completion."

  (let* ((code-and-point-data
          (geiser-kawa-devutil-complete--code-point-from-toplevel))
         (code-str     (cdr (assoc "code-str"
                                   code-and-point-data)))
         (cursor-index (cdr (assoc "cursor-index"
                                   code-and-point-data)))
         (compl-data (geiser-kawa-devutil-complete--get-data
                      code-str cursor-index))
         (user-choice (geiser-kawa-devutil-complete--user-choice-dispatch
                       compl-data)))
    (when (thing-at-point 'word)
      (if (looking-back ":" (- (point) 2))
          (kill-word 1)
        (kill-word -1)))
    (insert user-choice)
    ;; (when (not (equal (word-at-point) user-choice))
    ;;   (kill-word 1)
    ))

;;;; Functions to get the Expression tree that is made to try and get
;;;; java completions. Useful when debugging why java completion fails.

(defun geiser-kawa-devutil-complete--exprtree (code-str cursor-index)
  (geiser-kawa-util--eval-to-res
   `(geiser:kawa-devutil-complete-expr-tree
     ,code-str
     ,cursor-index)))

(defun geiser-kawa-devutil-complete-expree-at-point ()
  (interactive)
  (let* ((code-and-point-data
          (geiser-kawa-devutil-complete--code-point-from-toplevel))
         (code-str     (cdr (assoc "code-str"
                                   code-and-point-data)))
         (cursor-index (cdr (assoc "cursor-index"
                                   code-and-point-data)))
         (expr-tree (geiser-kawa-devutil-complete--exprtree
                     code-str cursor-index)))
    (geiser-kawa-devutil-exprtree--view expr-tree)))

(provide 'geiser-kawa-devutil-complete)

;;; geiser-kawa-devutil-complete.el ends here
