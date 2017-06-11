;; geiser-newlisp.el -- NewLisp's implementation of the geiser protocols

;; Copyright (C) 2009-2017 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Fri June 02, 2017 13:23



(require 'geiser-connection)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser-log)
(require 'geiser)

(require 'compile)
(require 'info-look)


;;; Customization

(defgroup geiser-newlisp nil
  "Customization for Geiser NewLisp."
  :group 'geiser)

(defcustom geiser-newlisp-lsp-dir "~"
  "Where the lsp init files are."
  :type 'string
  :group 'geiser-newlisp)

(defcustom geiser-newlisp-load-path nil
  "The load path."
  :type 'string
  :group 'geiser-newlisp)

(geiser-custom--defcustom geiser-newlisp-binary
    (cond ((eq system-type 'windows-nt) "newlisp.exe")
          ((eq system-type 'darwin) "newlisp")
          (t "newlisp"))
  "Name to use to call the NewLisp executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-newlisp)

(geiser-custom--defcustom geiser-newlisp-init-file "~/.newlisp-geiser"
  "Initialization file with user code for the Guile REPL.)
If all you want is to load ~/.init.lsp, set
`geiser-newlisp-load-init-file-p' instead."
  :type 'string
  :group 'geiser-newlisp)

(geiser-custom--defcustom geiser-newlisp-load-init-file-p nil
  "Whether to load ~/.guile when starting Guile.
Note that, due to peculiarities in the way Guile loads its init
file, using `geiser-newlisp-init-file' is not equivalent to setting
this variable to t."
  :type 'boolean
  :group 'geiser-newlisp)


;;; REPL
(defun geiser-newlisp--binary ()
  "Return newlisp program."
  (if (listp geiser-newlisp-binary)
      (car geiser-newlisp-binary)
    geiser-newlisp-binary))

(defun geiser-newlisp--parameters ()
  "Return a list of parameters."
  (let ((init-file (and (stringp geiser-newlisp-init-file)
                        (expand-file-name geiser-newlisp-init-file)))
        (q-flags (and (not geiser-newlisp-load-init-file-p) '("-n"))))
    `(,@(and (listp geiser-newlisp-binary) (cdr geiser-newlisp-binary))
      ,@q-flags ,(expand-file-name "lsp/" geiser-newlisp-lsp-dir)
      ,@geiser-newlisp-load-path
      ,@(and (file-readable-p init-file) init-file))))

(defun geiser-newlisp--version (binary)
  (let* ((str (string-to-vector (car (process-lines binary "-v")))))
    (apply 'string (loop for i from 10 to 15
                         collect (aref str i)))))

(defun geiser-newlisp--exit-command ()
  "Exit command."
  "(exit)")

(define-geiser-implementation newlisp
  (binary geiser-newlisp--binary)
  (arglist geiser-newlisp--parameters)
  (version-command geiser-newlisp--version)
  (minimum-version geiser-newlisp-minimum-version)
  (repl-startup)
  (prompt-regexp)
  (debugger-prompt-regexp)
  (enter-debugger)
  (enter-command)
  (exit-command geiser-newlisp--exit-command)
  (find-symbol-begin)
  (display-error)
  (display-help)
  (keywords))

(geiser-impl--add-to-alist 'regexp "\\.lsp$" 'newlisp t)

(provide 'geiser-newlisp)
