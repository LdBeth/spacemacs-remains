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
    ;; notmuch
    (notmuch :location site)
    (ace-link-notmuch :location local)
    helm-notmuch
    wanderlust
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

;; (defun social/init-gitter ()
;;   "Initialize gitter"
;;   (use-package gitter
;;     :defer t
;;     :init
;;     (spacemacs/set-leader-keys "aig" 'gitter)))

(defun social/init-notmuch ()
  "Initialize Notmuch"
  (use-package notmuch
    :defer t
    :load-path "/usr/local/share/emacs/site-lisp/notmuch"
    :commands notmuch
    :init
    (progn
      (spacemacs/set-leader-keys
        "ann" 'notmuch
        "ans" 'helm-notmuch))
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

(defun social/init-wanderlust ()
  "Initialize WanderLust."
  (use-package wl
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "anw" 'wl
        "anm" 'compose-mail)
      (if (boundp 'mail-user-agent)
          (setq mail-user-agent 'wl-user-agent))
      (if (fboundp 'define-mail-user-agent)
          (define-mail-user-agent
            'wl-user-agent
            'wl-user-agent-compose
            'wl-draft-send
            'wl-draft-kill
            'mail-send-hook))
      (spacemacs/declare-prefix-for-mode 'wl-draft-mode "mm" "mime-edit")
      (with-eval-after-load 'mime-edit
        (spacemacs/set-leader-keys-for-major-mode 'wl-draft-mode
          dotspacemacs-major-mode-leader-key 'wl-draft-send-and-exit
          "k" 'wl-draft-kill
          "s" 'wl-draft-save
          "z" 'wl-draft-save-and-exit
          "m" mime-edit-mode-entity-map
          )))
    :config
    (progn
      (add-hook 'wl-folder-mode-hook 'evil-emacs-state);; Unknown Reason
      (dolist (mode '(wl-message-mode
                      wl-summary-mode
                      wl-folder-mode
                      wl-draft-mode
                      mime-view-mode))
        (add-to-list 'evil-emacs-state-modes mode)))))

;;; packages.el ends here
