;;; packages.el --- social layer packages file for Spacemacs.
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

(defconst social-packages
  '(
    (hexo :location (recipe
                     :fetcher github
                     :repo "LdBeth/hexo.el"))
    gitter
    ;; mew
    )
  "The Social Layer, including mail, blog, chat, and RSS reader.")

(defun social/init-hexo ()
  (use-package hexo
    :defer t
    :init
    (defun hexo-my-blog ()
      (interactive)
      (hexo "~/blog/"))
    (spacemacs/set-leader-keys "ab" 'hexo-my-blog)
    :config
    (evilified-state-evilify hexo-mode hexo-mode-map)))

(defun social/init-gitter ()
  "Initialize gitter"
  (use-package gitter
    :defer t
    :init
    (spacemacs/set-leader-keys "aig" 'gitter)))

;; (defun social/init-mew ()
;;   "Initialize mew"
;;   (use-package mew
;;     :defer t
;;     :init
;;     (progn
;;       (spacemacs/set-leader-keys "aw" 'mew)
;;       (if (boundp 'read-mail-command)
;;           (setq read-mail-command 'mew))
;;       (if (fboundp 'define-mail-user-agent)
;;           (define-mail-user-agent
;;             'mew-user-agent
;;             'mew-user-agent-compose
;;             'mew-draft-send-message
;;             'mew-draft-kill
;;             'mew-send-hook)))
;;     :config
;;     (progn
;;       ;; Currently I don't want to use evilified state in mew.
;;       (add-to-list 'evil-emacs-state-modes 'mew-summary-mode))))

;;; packages.el ends here
