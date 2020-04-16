;; Copyright (C) 2019 spellcard199 <spellcard199@protonmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'geiser-kawa)
(require 'geiser-mode)
(defun run-kawa-after-compilation-finishes (buffer desc)
  ;; Use included Kawa, to avoid issues with unsupported old Kawa versions (< 3.1.1)
  (setq geiser-kawa-use-included-kawa t)
  (run-kawa)
  (switch-to-buffer-other-window "*scratch*")
  (geiser-impl--set-buffer-implementation 'kawa)
  (geiser-mode)
  (remove-hook 'compilation-finish-functions
	       #'run-kawa-after-compilation-finishes))

(add-hook 'compilation-finish-functions
          #'run-kawa-after-compilation-finishes)
(geiser-kawa-deps-mvnw-package)
