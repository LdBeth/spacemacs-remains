;;; packages.el --- utility layer packages file for Spacemacs.
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

(setq utility-packages '((aria2
                          :location (recipe
                                     :fetcher github
                                     :repo "LdBeth/aria2.el"))
                         eww
                          ))

(defun utility/init-aria2 ()
  "Initialize aria2"
  (use-package aria2
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "an" "utility")
      (spacemacs/set-leader-keys "ana" 'aria2-downloads-list))
    :config
    (progn
      (defun aria2//is-aria-process-p (f &rest ARG)
        "Returns t if PID belongs to aria."
        (let ((pid (car ARG)))
          (eq pid (string-to-number
                   (shell-command-to-string
                    (format "pgrep -u %s aria2c" (user-real-login-name)))))))
      (advice-add 'aria2--is-aria-process-p
                  :around #'aria2//is-aria-process-p)
      (setq aria2-add-evil-quirks t)
      (setq aria2-download-directory (expand-file-name "~/Downloads/")))))

(defun utility/init-eww ()
  "Initialize eww"
  (use-package eww
    :defer t
    :init
    (spacemacs/set-leader-keys "ane" 'eww)
    :config
    (add-to-list 'evil-emacs-state-modes 'eww-mode)))

;;; packages.el ends here
