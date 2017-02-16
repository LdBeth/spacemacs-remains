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
    (newsticker :location built-in)
    )
  "The Social Layer, including blog, chat, and RSS reader.")

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
    (spacemacs/set-leader-keys "aig" 'gitter)
    :config
    (setq gitter-token "f4d803fefe6e1a44035dbe3553fd929a34677564")))

(defun social/init-newsticker ()
  "Initialize newsticker"
  (use-package newsticker
    :defer t
    :init
    (progn
      (setq newsticker-retrieval-interval 7200)
      ;; (defun newsticker/show-news ()
      ;;   (interactive)
      ;;   (require 'newsticker)
      ;;   (cl-letf (((symbol-function 'newsticker-start) #'ignore))
      ;;     (newsticker-show-news)))
      (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
      (spacemacs/set-leader-keys "af" 'newsticker-show-news)
      ;; (unless (newsticker-running-p) (newsticker-start))
      (setq newsticker-url-list nil
            newsticker-dir (expand-file-name
                            (concat spacemacs-cache-directory
                                    "newsticker/")))
      (add-to-list
       'newsticker-url-list
       '("月光博客"
         "http://feed.williamlong.info/" nil nil nil))
      (add-to-list
       'newsticker-url-list
       '("ACG" "http://www.acgpiping.net/feed/" nil nil nil)))
    (with-eval-after-load 'newsticker
      (setq newsticker-retrieval-method 'intern)
      (evilified-state-evilify newsticker-treeview-mode
        newsticker-treeview-mode-map
        (kbd "n") 'newsticker-treeview-next-item
        (kbd "N") 'newsticker-treeview-next-new-or-immortal-item
        (kbd "'") 'newsticker-treeview-next-page
        (kbd "gj") 'newsticker-treeview-jump
        (kbd "gn") 'newsticker-treeview-get-news
        (kbd "ga") 'newsticker-get-all-news
        (kbd "RET") 'newsticker-treeview-browse-url)
      (evilified-state-evilify newsticker-treeview-item-mode
        newsticker-treeview-item-mode-map)
      (evilified-state-evilify newsticker-treeview-list-mode
        newsticker-treeview-list-mode-map))))

;;; packages.el ends here
