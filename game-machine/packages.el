;;; packages.el --- game-machine layer packages file for Spacemacs.
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

(defconst game-machine-packages
  '(roguel-ike
    (helm-games :location local
                :toggle (configuration-layer/package-usedp 'helm))
    (tetris :location built-in)
    )
  "Configure Games.")

(defun game-machine/init-roguel-ike ()
  (use-package roguel-ike
    :defer t
    :init
    (push '("Roguel-ike" . (roguel-ike)) helm-games-list)
    ))

(defun game-machine/init-helm-games ()
  (use-package helm-games
    :commands helm-games
    :init
    (progn
      (spacemacs/declare-prefix "aG" "games")
      (spacemacs/set-leader-keys "aG" 'helm-games))))

(defun game-machine/init-tetris ()
  (use-package tetris
    :defer t
    :init
    (progn
      (push
       '("Tetris" . (tetris :quit spacemacs/tetris-quit-game
                            :reset tetris-start-game))
       helm-games-list)
      (setq tetris-score-file (concat spacemacs-games-cache-directory
                                      "tetris-scores.txt")))
    :config
    (progn
      (evilified-state-evilify tetris-mode tetris-mode-map
        "h" 'tetris-move-left
        "i" 'tetris-rotate-prev
        "j" 'tetris-move-bottom
        "k" 'tetris-rotate-next
        "l" 'tetris-move-right
        "q" 'spacemacs/tetris-quit-game))))

;;; packages.el ends here
