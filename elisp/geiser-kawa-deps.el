;;; geiser-kawa-deps.el --- Manage geiser-kawa's java dependencies -*- lexical-binding:t -*-

;;; Commentary:
;; This file contains code related to the download, compilation
;; and packaging of `kawa-geiser', the java dependency (with its
;; recursive dependencies) that `geiser-kawa' depends on.
;; The functions here provide utilities around the command
;; `mvnw package', which uses the pom.xml for the `kawa-geiser'
;; project, included in the `geiser-kawa-dir' directory.

;;; Code:

(require 'cl)

(cl-defun geiser-kawa-deps--jar-path
    (&optional (geiser-kawa-dir geiser-kawa-dir))
  (expand-file-name
   "./target/kawa-geiser-0.1-SNAPSHOT-jar-with-dependencies.jar"
   geiser-kawa-dir))

(defun geiser-kawa-deps-mvnw-package (geiser-kawa-dir)
  "Download, Compile and Package `geiser-kawa's java dependencies.
When called, this function runs `mvnw package' from the path specified
by the variable `GEISER-KAWA-DIR'.
The result is a fat jar that is added to the java classpath of Kawa
at REPL startup."
  ;; Using `mvn package' from the pom.xml's directory should produce a
  ;; jar containing all the java dependencies.
  (interactive)
  (let* ((default-directory geiser-kawa-dir)
         (mvn-buf (compile "./mvnw package")))
    (when mvn-buf
      (let ((save-buf (current-buffer)))
        (switch-to-buffer-other-window mvn-buf)
        (end-of-buffer)
        (switch-to-buffer-other-window save-buf)))))


;;; Manage the `geiser-kawa-deps--run-kawa--advice' advice for
;;; `run-kawa'.
;; `run-kawa' is adviced at the end of `geiser.kawa.el' by calling
;; `geiser-kawa-deps--run-kawa--advice-add' after `run-kawa' has been
;; defined by `define-geiser-implementation'.
;; `geiser-kawa-deps--run-kawa--advice' prompts the user for running
;; `mvnw package' when:
;; 1. the user uses `run-kawa'
;; 2. the fat .jar file that `geiser-kawa' depends on is not found.

(defun geiser-kawa-deps--run-kawa--advice-add()
  "Add our advice to `run-kawa'."
  (add-function :override
                (symbol-function 'run-kawa)
                #'geiser-kawa-deps--run-kawa--advice))

(defun geiser-kawa-deps--run-kawa--advice-remove()
  "Remove our advice from `run-kawa'."
  (remove-function (symbol-function 'run-kawa)
                   #'geiser-kawa-deps--run-kawa--advice))

(defun geiser-kawa-deps--run-kawa-unadviced()
  "Call `run-kawa' without triggering our advice."
  (geiser-kawa-deps--run-kawa--advice-remove)
  (run-kawa)
  (geiser-kawa-deps--run-kawa--advice-add))

(defun geiser-kawa-deps--run-kawa--add-compil-hook()
  "Run `run-kawa' unadviced the next time a compilation finishes."
  ;; The added hook auto-removes itself after being called once.
  (add-hook 'compilation-finish-functions
            #'geiser-kawa-deps--run-kawa--remove-compil-hook))

(defun geiser-kawa-deps--run-kawa--remove-compil-hook(buf desc)
  "Hook called when compilation finishes.
Runs `run-kawa' without the `geiser-kawa-deps--run-kawa--advice'
advice and removes itself from `compilation-finish-functions',
effectively running `run-kawa' unadviced only for one compilation.
Argument BUF passed by Emacs when compilation finishes.
Argument DESC passed by Emacs when compilation finishes."
  (geiser-kawa-deps--run-kawa-unadviced)
  (remove-hook 'compilation-finish-functions
               #'geiser-kawa-deps--run-kawa--remove-compil-hook))

(defun geiser-kawa-deps--run-kawa--advice(&optional install-if-absent)
  "Actual advicing function for `run-kawa'.

If the `geiser-kawa-deps-jar-path' path:
- exists: just run unadviced `run-kawa'
- does not exist:
    1. ask user for permission to run `mvnw package'
    2. if user answers `yes':
        1. download, compile, package `kawa-geiser'
        2. run `run-kawa' after compilation finishes

Optional argument INSTALL-IF-ABSENT: when non-nil, always prompt and
recompile kawa-geiser, ignoring existing jar."
  (if (file-exists-p geiser-kawa-deps-jar-path)
      (geiser-kawa-deps--run-kawa-unadviced)
    (when (or install-if-absent
              (y-or-n-p
               (concat
                "geiser-kawa depends on additional java libraries. "
                "Do you want to download and compile them now?")))
      (geiser-kawa-deps--run-kawa--add-compil-hook)
      (geiser-kawa-deps-mvnw-package geiser-kawa-dir))))

(provide 'geiser-kawa-deps)

;;; geiser-kawa-deps.el ends here
