;;; Org
(setq org-latex-compiler "xelatex"
;;; helm-ag
      helm-ag-base-command "pt -e --nocolor --nogroup"
;;; Mutt
      auto-mode-alist (cons '("/mutt" . mail-mode) auto-mode-alist)
;;; Magithub
      magithub-api-timeout 15)
;;; LaTeX
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;;; Conkeror.el
(autoload 'conkeror-minor-mode "conkeror-minor-mode")
(add-hook 'js-mode-hook (lambda ()
                          (when (string= ".conkerorrc" (buffer-name))
                            (conkeror-minor-mode 1))))
