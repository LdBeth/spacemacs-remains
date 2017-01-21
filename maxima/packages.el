;;; packages.el --- maxima layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Red World <Red_World@Costume-Party.local>
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
;; added to `maxima-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `maxima/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the fu nctions `maxima/pre-init-PACKAGE' and/or
;;   `maxima/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst maxima-packages
  '(
    (maxima :location local)
    )
  )

(defun maxima/init-maxima ()
  (use-package maxima
    :defer t
    :init
    (progn
      ;; Automatic enable EMaxima-mode
      (add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima)
      ;; Associate files ending in .max with particular Emacs mode
      (setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))
      ;; Enhanced terminal mode
      (autoload 'maxima "maxima" "Maxima interaction" t)
      ;; Maxima-mode
      (autoload 'maxima-mode "maxima" "Maxima mode" t)
      ;; Emaxima
      (autoload 'emaxima-mode "emaxima" "EMaxima" t)
      )
    )
  )

;;; packages.el ends here




