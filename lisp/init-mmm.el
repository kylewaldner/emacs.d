;;; init-mmm.el --- Multiple Major Modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
;; Install mmm-mode via straight.el
(straight-use-package 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

(provide 'init-mmm)
;;; init-mmm.el ends here
