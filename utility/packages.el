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
(defconst utility-packages
  '(notmuch
    ;; (notmuch :location site)
    (ace-link-notmuch :location local)
    helm-notmuch
    helm-fuzzy-find
    wanderlust
    bbdb
    bbdb-
    helm-bbdb
    (aria2
     :location
     (recipe
      :fetcher github
      :repo "LdBeth/aria2.el"))
    (eww :location built-in)
    w3m
    shimbun
    (namazu :location local)
    evalator)
  "The Utility Layer, including some useful network tools.")

;; (defun utility/init-hexo ()
;;   (use-package hexo
;;     :defer t
;;     :init
;;     (defun hexo-my-blog ()
;;       (interactive)
;;       (hexo "~/blog/"))
;;     (spacemacs/set-leader-keys "ab" 'hexo-my-blog)
;;     :config
;;     (evilified-state-evilify hexo-mode hexo-mode-map)))

;; (defun utility/init-gitter ()
;;   "Initialize gitter"
;;   (use-package gitter
;;     :defer t
;;     :init
;;     (spacemacs/set-leader-keys "aig" 'gitter)))

(defun utility/init-notmuch ()
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
                        (beginning-of-line))))))))

(defun utility/init-ace-link-notmuch ()
  (use-package ace-link-notmuch
    :defer t))

(defun utility/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t))

(defun utility/init-helm-fuzzy-find ()
  (use-package helm-fuzzy-find
    :defer t))

(defun utility/init-wanderlust ()
  "Initialize WanderLust."
  (use-package wl
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "anw" 'wl
        "anm" 'compose-mail)
      (setq read-mail-command 'wl
            mail-user-agent 'wl-user-agent
            org-mime-library 'semi)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook)
      (spacemacs/declare-prefix-for-mode 'wl-draft-mode "mime" "mime-edit")
      (with-eval-after-load 'mime-edit
        (spacemacs/set-leader-keys-for-major-mode 'wl-draft-mode
          dotspacemacs-major-mode-leader-key 'wl-draft-send-and-exit
          "k" 'wl-draft-kill
          "s" 'wl-draft-save
          "z" 'wl-draft-save-and-exit
          "m" mime-edit-mode-entity-map
          "c" 'bbdb-:start-completion)))
    :config
    (progn
      (add-hook 'wl-folder-mode-hook 'evil-emacs-state);; Unknown Reason
      (dolist (mode '(wl-message-mode
                      wl-summary-mode
                      wl-folder-mode
                      wl-draft-mode
                      mime-view-mode))
        (add-to-list 'evil-emacs-state-modes mode)))))

(defun utility/init-bbdb ()
  "Initialize bbdb."
  (use-package bbdb
    :defer t
    :init
    (setq bbdb-file (expand-file-name "bbdb" dotspacemacs-directory)
          bbdb-auto-notes-rules '(("X-Face" (".+" x-face 0 'replace))
                                  ("Face" (".+" face 0 'replace))))))

(defun utility/init-bbdb- ()
  "Initialize BBDB-."
  (use-package bbdb-
    :defer t))

(defun utility/init-helm-bbdb ()
  "Initialize bbdb."
  (use-package helm-bbdb
    :defer t
    :config
    (spacemacs/set-leader-keys
      "and" 'helm-bbdb
      "an-" 'bbdb-:open)))

(defun utility/init-aria2 ()
  "Initialize aria2."
  (use-package aria2
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "an" "network")
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
  "initialize eww"
  (use-package eww
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "ane" 'eww)
      (spacemacs/set-leader-keys "anb" 'eww-list-bookmarks)
      )
    :config
    (progn
      (setq eww-bookmarks-directory
            (if (file-exists-p dotspacemacs-directory)
                dotspacemacs-directory
              spacemacs-cache-directory))
      (define-key eww-mode-map (kbd "r") 'eww-reload)
      (define-key eww-mode-map (kbd "b") 'eww-back-url)
      (define-key eww-mode-map (kbd "f") 'eww-forward-url)
      (define-key eww-mode-map (kbd "a") 'eww-add-bookmark)
      (define-key eww-mode-map (kbd "s") 'eww-view-source)
      (define-key eww-mode-map (kbd "l") nil)
      (define-key eww-mode-map (kbd "v") nil)
      (define-key eww-mode-map (kbd "h") nil)
      (define-key eww-mode-map (kbd "g") nil)
      (define-key eww-mode-map (kbd "?") nil)
      (evil-make-overriding-map eww-mode-map 'normal)
      ;; force update evil keymaps after eww-mode loaded
      (add-hook 'eww-mode-hook #'evil-normalize-keymaps)
      (spacemacs/set-leader-keys-for-major-mode 'eww-mode
        "e" 'eww
        "n" 'eww-buffer-show-next
        "p" 'eww-buffer-show-previous)
      (evilified-state-evilify eww-history-mode eww-history-mode-map)
      (evilified-state-evilify eww-bookmark-mode eww-bookmark-mode-map))))

(defun utility/pre-init-eww ()
  (with-eval-after-load 'url
    (evilified-state-evilify url-cookie-mode url-cookie-mode-map)))

(defun utility/init-w3m ()
  "Initialize w3m."
  (use-package w3m
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "an3" 'w3m))
    :config
    (setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8
          w3m-default-display-inline-images t
          w3m-use-cookies t
          w3m-namazu-default-index nil)))

(defun utility/init-shimbun ()
  "Initialize shimbun."
  (use-package shimbun
    :defer t))

(defun utility/init-namazu ()
  "Initialize namazu."
  (use-package namazu
    :defer t
    :commands namazu))

(defun utility/init-evalator ()
  "Initialize a REPL."
  (use-package evalator
    :defer t
    :init (spacemacs/register-repl 'evalator 'evalator "evalator")))

;;; packages.el ends here
