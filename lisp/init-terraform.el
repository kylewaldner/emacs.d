;;; init-terraform.el --- Work with Terraform configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Terraform

(when (straight-use-package 'terraform-mode)
  (when (straight-use-package 'company-terraform)
    (after-load 'terraform-mode
      (company-terraform-init)

      ;; I find formatters based on "reformatter" to be more reliable
      ;; so I redefine `terraform-format-on-save-mode' here.
      (when (straight-use-package 'reformatter)
        (reformatter-define terraform-format
          :program "terraform" :args '("fmt" "-"))))))

(provide 'init-terraform)
;;; init-terraform.el ends here
