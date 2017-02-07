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
  '((hexo :location (recipe
                     :fetcher github
                     :repo "LdBeth/hexo.el"))
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

(defun social/init-hexo ()
  (use-package hexo
    :defer t
    :init
    (defun hexo-my-blog ()
      (interactive)
      (hexo "~/blog/"))
    (spacemacs/set-leader-keys "ab" 'hexo-my-blog)
    :config
    (evilified-state-evilify hexo-mode hexo-mode-map
      (kbd "\'") 'hexo-command-show-article-info
      (kbd "w") 'hexo-new
      (kbd "r") 'hexo-command-revert-tabulated-list)))

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
    :init
    (progn
      (setq newsticker-retrieval-interval 3600)
      (defun newsticker/show-news ()
        (interactive)
        (require 'newsticker)
        (cl-letf (((symbol-function 'newsticker-start) #'ignore))
          (newsticker-show-news)))
      (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
      (spacemacs/set-leader-keys "af" 'newsticker/show-news)
      ;; (unless (newsticker-running-p) (newsticker-start))
      (setq newsticker-url-list nil)
      (add-to-list
       'newsticker-url-list
       '("月光博客"
         "http://feed.williamlong.info/"))
      (add-to-list
       'newsticker-url-list
       '("ACG" "http://www.acgpiping.net/feed/")))
    (with-eval-after-load 'newsticker
      (setq newsticker-retrieval-method 'intern)
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
        newsticker-treeview-list-mode-map))))

;;; packages.el ends here
