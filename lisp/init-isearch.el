;;; init-isearch.el --- isearch settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Show number of matches while searching
(when (straight-use-package 'anzu)
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; Activate occur easily inside isearch
(after-load 'isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

  (when (fboundp 'isearch-occur)
    ;; to match ivy conventions
    (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)))

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)


(defun sanityinc/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'sanityinc/isearch-exit-other-end)

;; Enhanced isearch that populates search bar with selected text
(defun isearch-forward-with-region ()
  "Start isearch forward, populating search bar with selected text if region is active."
  (interactive)
  (let ((search-text (when (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end)))))
    (if search-text
        (progn
          ;; Deactivate the region
          (deactivate-mark)
          ;; Start isearch with the selected text
          (isearch-forward)
          (isearch-yank-string search-text))
      ;; No region active, start normal isearch
      (isearch-forward))))

;; Bind the enhanced isearch function to C-s
(global-set-key (kbd "C-s") 'isearch-forward-with-region)

(provide 'init-isearch)
;;; init-isearch.el ends here
