;; Org
(setq org-latex-compiler "xelatex")
;; helm-ag
(setq helm-ag-base-command "pt -e --nocolor --nogroup")
;; LaTeX
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; Mutt
(setq auto-mode-alist (cons '("/mutt" . mail-mode) auto-mode-alist))
;; Conkeror.el
(autoload 'conkeror-minor-mode "conkeror-minor-mode")
(add-hook 'js-mode-hook (lambda ()
                          (when (string= ".conkerorrc" (buffer-name))
                            (conkeror-minor-mode 1))))
