(require 'slime)
(require 'slime-repl)

(slime-setup '(slime-repl))
(setq slime-protocol-version 'ignore)

(defun swank-newlisp-init (port-filename coding-system)
  (format "%S\n" `(swank:start-server ,port-filename)))

(defun slime-newlisp ()
  (interactive)
  (let* ((slime-protocol-version 'ignore)
         (file (expand-file-name "shell/swank.lsp" dotspacemacs-directory))
         (slime-lisp-implementations
          `((newlisp ("newlisp" "-n" ,file)
                     :init swank-newlisp-init
                     :coding-system utf-8-unix))))
    (slime 'newlisp)))
