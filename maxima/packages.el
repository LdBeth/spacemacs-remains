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

;;; Code:

(defconst maxima-packages
  '((maxima :location local))
  "Maxima, the Lisp CAS.")

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




