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

(defun geiser-kawa-complete-java--user-choice
    (compl-for-class modifiers
     field-or-method completions
     before-cursor)
  "`for-class' is the class that owns the field or methods in
`completions'.
`field-or-method' should be either 'field or 'method, but it's not
checked.
`completions' is a list of names (strings) that the user can pick
from."
  (completing-read
   (concat "(" (string-join modifiers " ") " " field-or-method ") "
           compl-for-class "."
           ;; "- Exp  : " compl-for-expr "\n"
           ;; ": "
           )
   completions
   nil
   nil
   before-cursor))

(defun geiser-kawa-complete-java--user-choice-data
    (compl-data)
  (let ((compl-for-class
         (cadr (assoc "compl-for-class" compl-data)))
        (modifiers
         (cadr (assoc "modifiers" compl-data)))
        (field-or-method
         (cadr (assoc "field-or-method" compl-data)))
        (completions
         (cadr (assoc "completions" compl-data)))
        ;; unused
        (before-cursor
         (cadr (assoc "before-cursor" compl-data)))
        ;; unused
        (after-cursor
         (cadr (assoc "after-cursor" compl-data))))
    (geiser-kawa-complete-java--user-choice
     compl-for-class modifiers
     field-or-method completions
     before-cursor)))

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

(defun geiser-kawa-complete-java-fom-at-point ()
  (interactive)
  "Complete java field or method at point"

  (let* ((code-and-point-data
         (geiser-kawa-complete-java--code-point-from-toplevel))
         (code-str     (cdr (assoc "code-str"
                                   code-and-point-data)))
         (cursor-index (cdr (assoc "cursor-index"
                                   code-and-point-data)))
         (compl-data (geiser-kawa-complete-java--get-data
                      code-str cursor-index))
         (user-choice (geiser-kawa-complete-java--user-choice-data
                       compl-data)))
    (when (word-at-point)
      (if (looking-back ":" (- (point) 2))
          (kill-word 1)
          (kill-word -1)))
    (insert user-choice)
    ;; (when (not (equal (word-at-point) user-choice))
    ;;   (kill-word 1)
    ))

(provide 'geiser-kawa-complete-java)
