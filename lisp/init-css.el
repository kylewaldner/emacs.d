;;; init-css.el --- CSS/Less/SASS/SCSS support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Colourise CSS colour literals
(when (straight-use-package 'rainbow-mode)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))


;;; Embedding in html
(straight-use-package 'mmm-mode)
(after-load 'mmm-vars
  (mmm-add-group
   'html-css
   '((css-cdata
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
      :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
      :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t]*\n?"
      :back "[ \t]*</style>"
      :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css-inline
      :submode css-mode
      :face mmm-code-submode-face
      :front "style=\""
      :back "\"")))
  (dolist (mode (list 'html-mode 'nxml-mode))
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))




;;; SASS and SCSS
(straight-use-package 'sass-mode)
(unless (fboundp 'scss-mode)
  ;; Prefer the scss-mode built into Emacs
  (straight-use-package 'scss-mode))
(setq-default scss-compile-at-save nil)



;;; LESS
(unless (fboundp 'less-css-mode)
  ;; Prefer the scss-mode built into Emacs
  (straight-use-package 'less-css-mode))
(when (straight-use-package 'skewer-less)
  (add-hook 'less-css-mode-hook 'skewer-less-mode))



;; Skewer CSS
(when (straight-use-package 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode))


;;; Use eldoc for syntax hints
(straight-use-package 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)


;;; css indent
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

(provide 'init-css)
;;; init-css.el ends here
