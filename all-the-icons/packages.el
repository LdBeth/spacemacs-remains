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
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
      (add-hook 'ranger-mode 'dired-readin))
    :config
    (spacemacs|diminish all-the-icons-dired-mode)))

;; (defun all-the-icons/init-spaceline-all-the-icons ()
;;   "Initialize an alternate spaceline."
;;   (use-package spaceline-all-the-icons
;;     :after spaceline)
;;   (use-package spaceline :after powerline
;;     :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati))))))

;;; packages.el ends here
