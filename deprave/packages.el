;;; packages.el --- deprave Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Code:
(defconst deprave-packages
  '((multi-keys
     :location (recipe
                :fetcher github
                :repo "Liu233w/multi-keys.el"
                :branch "feature-keymap"))
    gnus
    elfeed
    erc
    chinese-pyim-basedict
    chinese-pyim
    company
    smex
    slime
    helm-smex
    (eshell :location built-in)
    esh-buf-stack
    pcomplete-extension
    pcmpl-homebrew
    header2)
  "The Improved Spacemacs Layer.")

(defun deprave/init-multi-keys ()
  "Initialize multi-keys"
  (require 'multi-keys)
  (spacemacs|add-toggle multi-keys
    :mode multi-keys-mode
    :documentation
    "Toggle binded commands of combinations of key-strokes."
    :evil-leader "tk")

  (global-multi-keys-mode)
  (spacemacs|diminish multi-keys-mode))

(defun deprave/init-gnus ()
  "Initialize gnus"
  (use-package gnus
    :defer t
    :commands gnus
    :init
    (spacemacs/set-leader-keys "ag" 'gnus)
    :config
    (progn
      ;; No primary server
      (setq gnus-select-method '(nnnil ""))

      ;; Use topics per default
      (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

      ;; Show the article headers in this order.
      (setq gnus-sorted-header-list
            '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
              "^Subject:" "^Date:" "^Gnus"))

      (setq-default
       gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15n  %B (%c) %s%)\n"
       gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
       gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
       gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
       gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
       gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
       gnus-sum-thread-tree-false-root ""
       gnus-sum-thread-tree-indent " "
       gnus-sum-thread-tree-leaf-with-other "├► "
       gnus-sum-thread-tree-root ""
       gnus-sum-thread-tree-single-leaf "╰► "
       gnus-sum-thread-tree-vertical "│"
       gnus-article-browse-delete-temp t
       gnus-treat-strip-trailing-blank-lines 'last
       gnus-keep-backlog 'nil
       gnus-summary-display-arrow nil ; Don't show that annoying arrow:
       gnus-mime-display-multipart-related-as-mixed t ; Show more MIME-stuff:
       gnus-auto-select-first nil ; Don't get the first article automatically:
       smiley-style 'medium
       gnus-keep-backlog '0)

      (require 'browse-url)
      (require 'nnrss)
      (defun spacemacs/browse-nnrss-url (arg)
        "Open RSS Article directy in the browser"
        (interactive "p")
        (let ((url (assq nnrss-url-field
                         (mail-header-extra
                          (gnus-data-header
                           (assq (gnus-summary-article-number)
                                 gnus-newsgroup-data))))))
          (if url
              (progn
                (browse-url (cdr url))
                (gnus-summary-mark-as-read-forward 1))
            (gnus-summary-scroll-up arg))))
      (add-to-list 'nnmail-extra-headers nnrss-url-field)
      (define-key gnus-summary-mode-map
        (kbd "<RET>") 'spacemacs/browse-nnrss-url)

      (dolist (modes '(gnus-group-mode
                       gnus-server-mode
                       gnus-browse-mode
                       gnus-article-mode
                       gnus-summary-mode))
        (add-to-list 'evil-emacs-state-modes modes)))))

(defun deprave/pre-init-elfeed ()
  (spacemacs|use-package-add-hook elfeed
    :post-init
    (setq elfeed-db-directory
          (if (file-exists-p dotspacemacs-directory)
              (concat dotspacemacs-directory "elfeed")
            (concat spacemacs-cache-directory "elfeed")))
    :post-config
    (evil-define-key 'evilified elfeed-show-mode-map "J"
      'elfeed-goodies/split-show-next)
    (evil-define-key 'evilified elfeed-show-mode-map "K"
      'elfeed-goodies/split-show-prev)))

(defun deprave/pre-init-erc ()
  (spacemacs|use-package-add-hook erc
    :post-init
    (spacemacs/set-leader-keys "ais" 'erc-server-select)
    :post-config
    (setq erc-insert-timestamp-function 'erc-insert-timestamp-left
          erc-timestamp-format "%H%M ")))

(defun deprave/init-chinese-pyim-basedict ()
  "Initialize chinese-pyim-basedict."
  (use-package chinese-pyim-basedict
    :defer t))

(defun deprave/post-init-chinese-pyim ()
  (with-eval-after-load 'chinese-pyim
    ;; For some reason cannot use `spacemacs|add-company-hook'.
    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-isearch-mode
                    pyim-probe-dynamic-english
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template))
    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))
    (setq pyim-isearch-enable-pinyin-search t
          pyim-page-tooltip 'pos-tip)))

(defun deprave/init-smex ()
  "Initialize smex."
  (use-package smex
    :defer t
    :init
    (progn
      (setq-default smex-history-length 32
                    smex-save-file (concat spacemacs-cache-directory
                                           ".smex-items")))))

(defun deprave/pre-init-slime ()
  (spacemacs|use-package-add-hook slime
    :post-config
    (setq inferior-lisp-program "ccl")))

(defun deprave/init-helm-smex ()
  (use-package helm-smex
    :init
    (progn
      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda () (spacemacs/set-leader-keys
                             dotspacemacs-emacs-command-key 'helm-M-x)))
      (spacemacs/set-leader-keys ":" #'helm-smex-major-mode-commands)
      (defun helm-smex-or-major-mode-commands (arg)
        "Call `helm-smex' or `helm-smex-major-mode-commands' depends on prefix ARG."
        (interactive "P")
        (if arg
            (call-interactively #'helm-smex-major-mode-commands)
          (call-interactively #'helm-smex)))
      (global-set-key (kbd "M-x") #'helm-smex-or-major-mode-commands))))

(defun deprave/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-ispell
    :modes text-mode)
  (defun spell/toggle-company-ispell ()
    "Toggles company-ispell"
    (interactive)
    (cond
     ((memq 'company-ispell company-backends)
      (setq company-backends
            (delq 'company-ispell company-backends))
      (message "company-ispell disabled."))
     (t
      (add-to-list 'company-backends 'company-ispell)
      (message "company-ispell enabled."))))
  (spacemacs/set-leader-keys "Si" 'spell/toggle-company-ispell))

(defun deprave/pre-init-eshell ()
  (spacemacs|use-package-add-hook eshell
    :post-config
    (progn
      (setup-eshell-buf-stack)
      (add-hook 'eshell-mode-hook
                (lambda ()
                  (progn
                    (local-set-key (kbd "<RET>") 'config/return)
                    (local-set-key (kbd "M-q") 'eshell-push-command))))
      (mapc (lambda (x) (push x eshell-visual-commands))
            '("vim" "mutt" "nethack" "rtorrent" "w3m")))))

(defun deprave/init-esh-buf-stack ()
  (use-package esh-buf-stack
    :defer t
    :after eshell))

(defun deprave/init-pcomplete-extension ()
  (use-package pcomplete-extension
    :defer t
    :after eshell))

(defun deprave/init-pcmpl-homebrew ()
  (use-package pcmpl-homebrew
    :defer t
    :after eshell))

(defun deprave/init-header2 ()
  (use-package header2
    :defer t))
