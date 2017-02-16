;;; packages.el --- emms layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: LdBeth <andpuke@foxmial.com>
;; URL: https://github.com/LdBeth/emms
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
;; added to `emms-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `emms/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `emms/pre-init-PACKAGE' and/or
;;   `emms/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst emms-packages
  '(
    (emms :location (recipe
                     :fetcher github
                     :repo "alejandroerickson/emms"
                     :files ("lisp/*.el"
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path" "Makefile")
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path/src" "src/*")
                             )
                     )
          )
    emms-state
    helm-emms
    podcaster
    )
  "The Emacs Media Layer.")

(setq emms-excluded-packages '())

(defun emms/init-emms ()
  "Initialize my package"
  (use-package emms
    :defer t
    :init
    (progn
      (global-set-key [(f8)] 'emms-smart-browse)
      (spacemacs/declare-prefix "am" "music")
      (spacemacs/set-leader-keys
        "ams" 'emms-streams
        "amb" 'emms-browser
        "amc" 'podcaster
        "amp" 'emms-playlist-mode-go
        "amo" 'emms-show
        "a SPC" 'emms-play-pause-dwim
        "a ." 'emms-next
        "a ," 'emms-previous
        "a RET" 'emms-smart-browse
        )
      (setq emms-directory (concat dotspacemacs-directory "emms"))
      (add-hook 'emms-browser-show-display-hook 'evil-initialize)
      (add-hook 'emms-stream-hook 'evil-initialize)
      )
    :config
    (progn
      ;; (require 'emms-setup)
      (emms-all)
      (emms-mode-line 0)
      (emms-playing-time 0)
      (emms-default-players)
      (setq emms-player-list '(emms-player-mplayer))
      (setq emms-source-file-default-directory "/Applications/osu!.app/Contents/Resources/drive_c/osu!/Songs/")
      (setq emms-playlist-buffer-name "*iTunes*")
      (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
      (define-key emms-browser-mode-map (kbd "D") 'emms-browser-move-files-to-trash)
      (define-key emms-browser-mode-map (kbd "t") 'emms-browser-toggle-subitems)
      (require 'emms-info-libtag)
      (setq emms-info-functions '(emms-info-libtag))

      (evilified-state-evilify-map emms-stream-mode-map
        :mode emms-stream-mode
        )
      (evilified-state-evilify-map emms-mark-mode-map
        :mode emms-mark-mode
        :bindings
        "t" 'emms-mark-toggle
        "u" 'emms-mark-unmark-forward
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode-disable
        )
      (evilified-state-evilify-map emms-playlist-mode-map
        :mode emms-playlist-mode
        :bindings
        "l" 'emms-next
        "h" 'emms-previous
        "H" 'emms-playlist-mode-first
        "L" 'emms-playlist-mode-last
        "W" 'emms-playlist-save
        ;; P also works for emms-pause but it's kind of a stupid binding.
        ;; can't use SPC, so we'll make do with TAB
        (kbd "TAB") 'emms-pause
        "," 'emms-seek-minute-backward
        "." 'emms-seek-minute-forward
        "u" 'emms-playlist-mode-undo
        "p" 'emms-playlist-mode-yank
        "P" 'emms-playlist-mode-yank-pop
        "O" 'emms-playlist-mode-insert-newline
        ;; having trouble with this because it is
        ;; sometimes calling 'emms-playlist-mode-current-kill
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode
        )
      (evilified-state-evilify-map emms-browser-mode-map
        :mode emms-browser-mode
        :bindings
        ;; since this is normally SPC
        "t" 'emms-browser-toggle-subitems
        ;; makes more sense than C-j
        (kbd "<S-return>") 'emms-browser-add-tracks-and-play
        )
      )
    )
  )

(defun emms/init-emms-state ()
  (use-package emms-state
    ;; for some reason if this is deferred you can't bring up the smart browser.
    :config
    (emms-state-mode 0)
    ))

(defun emms/init-helm-emms ()
  (use-package helm-emms
    :defer t))

(defun emms/init-podcaster ()
  (use-package podcaster
    :defer t))

;;; packages.el ends here
