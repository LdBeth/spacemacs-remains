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

(setq utility-packages '(aria2
                          ))

(defun utility/init-aria2 ()
  (use-package aria2
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "ax" "utility")
      (spacemacs/set-leader-keys "axa" 'aria2-downloads-list)
      (add-to-list 'evil-emacs-state-modes 'aria2-mode)
      (add-to-list 'evil-emacs-state-modes 'aria2-dialog-mode))))

;;; packages.el ends here
