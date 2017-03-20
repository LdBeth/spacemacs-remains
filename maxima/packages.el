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
  '((maxima :location site)
    (emaxima :location site)
    (imaxima :location site)
    (imath :location site)
    )
  "Maxima, the Lisp CAS.")

(defun maxima/init-maxima ()
  "Initialize Enhanced terminal mode."
  (use-package maxima
    :defer t
    :commands
    (maxima maxima-mode)
    :init
    ;; Associate files ending in .max with particular Emacs mode
    (progn
      (setq auto-mode-alist
            (cons '("\\.ma[cx]" . maxima-mode) auto-mode-alist))
      (spacemacs/declare-prefix "aC" "CAS" "Computer Algebra System")
      (spacemacs/set-leader-keys
        "aCm" 'maxima
        "aCe" 'emaxima
        "aCi" 'imaxima))))

(defun maxima/init-emaxima ()
  "Initialize emaxima."
  (use-package emaxima
    :defer t
    :commands emaxima-mode
    :init
    ;; Automatic enable EMaxima-mode
    (add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima)))

(defun maxima/init-imaxima ()
  "Initialize imaxima, frontend for maxima with image support."
  (use-package imaxima
    :defer t
    :commands imaxima
    :config
    (setq imaxima-use-maxima-mode-flag t)))

(defun maxima/init-imath ()
  "Initialize imath, imath mode for math formula input."
  (use-package imath
    :defer t
    :commands imath-mode))

;;; packages.el ends here




