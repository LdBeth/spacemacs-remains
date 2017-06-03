;;; packages.el --- newlisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: LdBeth <andpuke@foxmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `newlisp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `newlisp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `newlisp/pre-init-PACKAGE' and/or
;;   `newlisp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst newlisp-packages
  '(newlisp-mode
    (swank-newlisp :location local))
  "The list of Lisp packages required by the newlisp layer.")

(defun newlisp/init-newlisp-mode ()
  "NewLisp."
  (use-package newlisp-mode
    :defer t))

(defun newlisp/init-swank-newlisp ()
  "Swank for newlisp."
  (use-package swank-newlisp
    :defer t))

;;; packages.el ends here
