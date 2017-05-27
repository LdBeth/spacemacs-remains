;;; packages.el --- all-the-icons layer packages file for Spacemacs.
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

(defconst all-the-icons-packages
  '(all-the-icons
    neotree
    all-the-icons-dired
    ;;(spaceline-all-the-icons :location local)
    )
  "The list of Lisp packages required by the all-the-icons layer.")

(defun all-the-icons/init-all-the-icons ()
  "Initialize all-the-incons."
  (use-package all-the-icons
    :defer t))

(defun all-the-icons/post-init-neotree ()
  (setq neo-theme 'icons))

(defun all-the-icons/init-all-the-icons-dired ()
  "Initialize all-the-incons for dired."
  (use-package all-the-icons-dired
    :defer t
    :init
    (progn
      ;; TODO It seems there are some bugs.
      (defun spacemacs/delay-all-the-icons-dired-mode ()
        "Work around for ranger."
        (run-at-time 0.01 nil 'all-the-icons-dired-mode))
      (add-hook 'dired-mode-hook
                'spacemacs/delay-all-the-icons-dired-mode))
    :config
    (spacemacs|diminish all-the-icons-dired-mode)))

;;; packages.el ends here
