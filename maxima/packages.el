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
  '((maxima :location site))
  "Maxima, the Lisp CAS.")

(defun maxima/init-maxima ()
  (use-package maxima
    :defer t
    :load-path "/usr/local/share/maxima/5.38.1/emacs"
    :commands
    (maxima ;; Enhanced terminal mode
     maxima-mode ;; Maxima-mode
     )
    :init
    (progn
      ;; Automatic enable EMaxima-mode
      (add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima)
      ;; Associate files ending in .max with particular Emacs mode
      (setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))
      ;; Emaxima
      (autoload 'emaxima-mode "emaxima" "EMaxima" t))))

;;; packages.el ends here




