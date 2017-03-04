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
    (notmuch :location site)
    (ace-link-notmuch :location local)
    helm-notmuch
    nm
    )
  "The Social Layer, including mail reader, blog, chat, and RSS reader.")

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

(defun social/init-notmuch ()
  "Initialize Notmuch"
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (progn
      (spacemacs/set-leader-keys
        "ann" 'notmuch
        "ans" 'helm-notmuch
        "anm" 'nm)
      (push "/usr/local/share/emacs/site-lisp/notmuch" load-path))
    :config
    (progn
      (add-to-list 'evil-emacs-state-modes 'notmuch-mode)
      (require 'ace-link-notmuch)
      (define-key notmuch-hello-mode-map (kbd "f") 'notmuch-jump-search)
      (define-key notmuch-hello-mode-map (kbd "j") 'widget-forward)
      (define-key notmuch-hello-mode-map (kbd "k") 'widget-backward)

      (evilified-state-evilify-map notmuch-search-mode-map
        :mode notmuch-search-mode
        :bindings
        (kbd "f") 'notmuch-jump-search
        (kbd "j") 'notmuch-search-next-thread
        (kbd "k") 'notmuch-search-previous-thread)

      ;; (define-key notmuch-search-mode-map
      ;;   (kbd "j") 'notmuch-search-next-thread)
      ;; (define-key notmuch-search-mode-map
      ;;   (kbd "k") 'notmuch-search-previous-thread)
      ;; (define-key notmuch-search-mode-map
      ;;   (kbd "f") 'notmuch-jump-search)

      (add-hook 'notmuch-hello-refresh-hook
                (lambda ()
                  (if (and (eq (point) (point-min))
                           (search-forward "Saved searches:" nil t))
                      (progn
                        (forward-line)
                        (widget-forward 1))
                    (if (eq (widget-type (widget-at)) 'editable-field)
                        (beginning-of-line)))))
      )))

(defun social/init-ace-link-notmuch ()
  (use-package ace-link-notmuch
    :defer t))

(defun social/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t))

(defun social/init-nm ()
  "Initialize NERVERMORE"
  (use-package nm
    :defer t
    :config
    (progn
      (add-to-list 'evil-emacs-state-modes 'nm-mode)
      (define-key nm-mode-map (kbd "j") 'forward-line)
      (define-key nm-mode-map (kbd "k") 'previous-line)
      )))

;;; packages.el ends here
