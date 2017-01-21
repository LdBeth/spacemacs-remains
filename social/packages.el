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
  '(blog-admin
    gitter
    newsticker)
  "The list of Lisp packages required by the social layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun social/init-blog-admin ()
  "Initialize blog-admin"
  (use-package blog-admin
    :defer t
    :init
    ;; Keybinding
    (spacemacs/set-leader-keys "ab" 'blog-admin-start)
    :config
    (progn
      ;; Open post after create new post
      (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
      ;; Hexo
      (setq blog-admin-backend-path "~/blog")
      (setq blog-admin-backend-type 'hexo)
      ;; create new post in drafts by default
      (setq blog-admin-backend-new-post-in-drafts t)
      ;; create same-name directory with new post
      (setq blog-admin-backend-new-post-with-same-name-dir t)
      ;; default assumes _config.yml
      (setq blog-admin-backend-hexo-config-file "_config.yml"))
    )
  )

(defun social/init-gitter ()
  "Initialize gitter"
  (use-package gitter
    :defer t
    :init
    (spacemacs/set-leader-keys "aig" 'gitter)
    :config
    (setq gitter-token "f4d803fefe6e1a44035dbe3553fd929a34677564")
    )
  )

(defun social/init-newsticker ()
  "Initialize newsticker"
  (use-package newsticker
    :defer t
    :commands
    newsticker-show-news
    :init
    (progn
      (setq newsticker-retrieval-interval 3600)
      (defun newsticker/show-news ()
        (interactive)
        (require 'newsticker)
        (cl-letf (((symbol-function 'newsticker-start) #'ignore))
          (newsticker-show-news)))
      (spacemacs/set-leader-keys "af" 'newsticker/show-news)
      ;; (unless (newsticker-running-p) (newsticker-start))
      (setq newsticker-url-list nil)
      (add-to-list
       'newsticker-url-list
       '("网易新闻"
         "http://news.163.com/special/00011K6L/rss_newsattitude.xml"))
      ;; (evil-set-initial-state 'newsticker-treeview-mode 'emacs)
      ;; (evil-set-initial-state 'newsticker-treeview-item-mode 'emacs)
      ;; (evil-set-initial-state 'newsticker-treeview-list-mode 'emacs)
      (evilified-state-evilify newsticker-treeview-mode
        newsticker-treeview-mode-map
        (kbd "n") 'newsticker-treeview-next-item
        (kbd "N") 'newsticker-treeview-next-new-or-immortal-item
        (kbd "'") 'newsticker-treeview-next-page
        (kbd "gj") 'newsticker-treeview-jump
        (kbd "RET") 'newsticker-treeview-browse-url)
      (evilified-state-evilify newsticker-treeview-item-mode
        newsticker-treeview-item-mode-map)
      (evilified-state-evilify newsticker-treeview-list-mode
        newsticker-treeview-list-mode-map))
    :config
    (progn
      (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
      (setq newsticker-retrieval-method 'intern))))

;;; packages.el ends here
