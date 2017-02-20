;;; packages.el --- mew layer packages file for Spacemacs.
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
;; added to `mew-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `mew/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `mew/pre-init-PACKAGE' and/or
;;   `mew/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst mew-packages
  '(mew)
  "Mew, Messaging in the Emacs World.")

(defun mew/init-mew ()
  "Initialize mew"
  (use-package mew
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "aM" 'mew)
      (if (boundp 'read-mail-command)
          (setq read-mail-command 'mew))
      (if (fboundp 'define-mail-user-agent)
          (define-mail-user-agent
            'mew-user-agent
            'mew-user-agent-compose
            'mew-draft-send-message
            'mew-draft-kill
            'mew-send-hook)))
    :config
    (progn
      ;; Currently Idon't want to use evilified state in mew.
      (add-to-list 'evil-emacs-state-modes 'mew-summary-mode))))

;;; packages.el ends here
