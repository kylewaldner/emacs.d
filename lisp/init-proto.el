;;; init-proto.el --- Support for protobuf -*- lexical-binding: t -*-
;;; Commentary:
;;; works with proto
;;; Code:


(straight-use-package
 '(protobuf-mode
   :type git
   :host github
   :repo "protocolbuffers/protobuf"
   :files ("editors/protobuf-mode.el")))

(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


(provide 'init-proto)
;;; init-proto.el ends here
