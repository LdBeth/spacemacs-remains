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
  '(
    (aria2
     :location (recipe
                :fetcher github
                :repo "LdBeth/aria2.el"))
    (eww :location built-in)
    wc-mode
    )
  "The Utility Layer, including some useful tools.")

(defun utility/init-aria2 ()
  "Initialize aria2"
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

(defun utility/init-wc-mode ()
  "Initialize `wc-mode'."
  (use-package wc-mode
    :defer t
    :config
    (setq wc-modeline-format "[%tw:%w/%gw]")))

;;; packages.el ends here
