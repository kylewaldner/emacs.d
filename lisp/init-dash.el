;;; init-dash.el --- Integrate with the Mac app "Dash" -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Support for the http://kapeli.com/dash documentation browser

(when *is-a-mac*
  (when (straight-use-package 'dash-at-point)
    (global-set-key (kbd "C-c D") 'dash-at-point)))

(provide 'init-dash)
;;; init-dash.el ends here
