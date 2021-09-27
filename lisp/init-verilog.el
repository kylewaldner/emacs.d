;;; init-verilog.el --- Support for Verilog -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'verilog-mode)

(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?va?h?\\'" . verilog-mode))

(setq verilog-indent-level             3
      verilog-indent-level-module      3
      verilog-indent-level-declaration 3
      verilog-indent-level-behavioral  3
      verilog-indent-level-directive   1
      verilog-case-indent              2
      verilog-auto-newline             t
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        t
      verilog-auto-endcomments         t
      verilog-minimum-comment-distance 40
      verilog-indent-begin-after-if    t
      verilog-auto-lineup              'declarations
      verilog-linter                   "verilator –lint-only")


(provide 'init-verilog)
;;; init-verilog.el ends here
