(require 'geiser)
(require 'geiser-mode)
(require 'geiser-kawa)

(defun switch-to-and-reset-scratch-buffer()
  (switch-to-buffer "*scratch*")
  (delete-region (point-min) (point-max))
  (geiser-impl--set-buffer-implementation 'kawa))

(describe
 "run-kawa"

 (before-all
  (let ((mvn-buf (geiser-kawa-deps-mvn-package)))
    (while compilation-in-progress
      (sleep-for 0 250)))
  (setq geiser-kawa-use-included-kawa t)
  (switch-to-and-reset-scratch-buffer)
  (run-kawa)
  (geiser-mode))

 (before-each
  (switch-to-and-reset-scratch-buffer))

 (it (concat "can find " geiser-kawa-deps-jar-path)
     (expect
      (file-exists-p geiser-kawa-deps-jar-path)))

 (it "can `run-kawa'"
     (expect
      (get-buffer "* Kawa REPL *")))

 (it "can `geiser-eval-buffer'"
     (expect
      (progn
        (insert "(display 'foobar)")
        (geiser-eval-buffer))
      :to-equal '((result "") (output . "foobar"))))

 )
