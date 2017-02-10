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
    (setq aria2-add-evil-quirks t)
    (setq aria2-download-directory (expand-file-name "~/Downloads/"))))

(defun utility/init-eww ()
  "Initialize eww"
  (use-package eww
    :defer t
    :init
    (spacemacs/set-leader-keys "ane" 'eww)
    :config
    (evilified-state-evilify eww-mode eww-mode-map
      (kbd "r") 'eww-reload
      (kbd "h") 'eww-back-url
      (kbd "h") 'eww-forward-url)))

;;; packages.el ends here
