;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'geiser-kawa-util)

(defvar geiser-kawa-devutil-exprtree-buffer "*kawa exprtree*"
  "Buffer where Expression tree is showed")

(defun geiser-kawa-devutil-exprtree--view (expr-tree)
  (with-current-buffer (get-buffer-create
                        geiser-kawa-devutil-exprtree-buffer)
    (View-quit)
    (delete-region (point-min) (point-max))
    (insert expr-tree)
    (goto-char (point-min)))

  (view-buffer-other-window
   geiser-kawa-devutil-exprtree-buffer))

(defun geiser-kawa-devutil-exprtree--for-expression (code-str)
  (geiser-kawa-util--eval-to-res
   `(geiser:kawa-devutil-expr-tree-formatted ,code-str)))

(defun geiser-kawa-devutil-exprtree-sexp ()
  (interactive)
  "If region is active send region, otherwise send last expression."
  (let* ((code-str
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
            (save-excursion
              (let ((sexp-beg (progn (backward-sexp) (point)))
                    (sexp-end (progn (forward-sexp) (point))))
                (buffer-substring-no-properties sexp-beg sexp-end)))))
         (expr-tree (geiser-kawa-devutil-exprtree--for-expression
                     code-str)))
    (geiser-kawa-devutil-exprtree--view expr-tree)))

(provide 'geiser-kawa-devutil-exprtree)

;;; geiser-kawa-devutil-exprtree.el ends here
