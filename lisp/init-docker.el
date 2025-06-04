;;; init-docker.el --- Work with Docker and its tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (straight-use-package 'docker)
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))
(straight-use-package 'dockerfile-mode)
(straight-use-package 'docker-compose-mode)

(provide 'init-docker)
;;; init-docker.el ends here
