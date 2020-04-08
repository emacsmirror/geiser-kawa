;; Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'geiser-kawa-eval-util)

(defvar geiser-kawa-exprtree-buffer "*kawa exprtree*"
  "Buffer where Expression tree is showed")

(defun geiser-kawa-exprtree--view (expr-tree)
  (with-current-buffer (get-buffer-create
                        geiser-kawa-exprtree-buffer)
    (View-quit)
    (delete-region (point-min) (point-max))
    (insert expr-tree)
    (goto-char (point-min)))

  (view-buffer-other-window
   geiser-kawa-exprtree-buffer))

(defun geiser-kawa-exprtree--for-expression (code-str)
  (geiser-kawa-eval--to-res
   `(geiser:expr-tree-formatted ,code-str)))

(defun geiser-kawa-exprtree-sexp ()
  (interactive)
  "If region is active send region, otherwise send last expression."
  (let* ((code-str
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (save-excursion
              (let ((sexp-beg (progn (backward-sexp) (point)))
                    (sexp-end (progn (forward-sexp) (point))))
                (buffer-substring-no-properties sexp-beg sexp-end)))))
         (expr-tree (geiser-kawa-exprtree--for-expression
                     code-str)))
    (geiser-kawa-exprtree--view expr-tree)))

(provide 'geiser-kawa-exprtree)

;;; geiser-kawa-exprtree.el ends here
