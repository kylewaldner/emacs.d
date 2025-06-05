;;; init-chat.el --- Text cleanup utilities -*- lexical-binding: t -*-
;;; Commentary:
;; Utilities for cleaning up text, particularly useful for chat/AI interactions
;;; Code:

(defun demoji ()
  "Delete all emojis in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((emoji-ranges '(
                          ;; Emoticons
                          (#x1F600 . #x1F64F)
                          ;; Miscellaneous Symbols and Pictographs
                          (#x1F300 . #x1F5FF)
                          ;; Transport and Map Symbols
                          (#x1F680 . #x1F6FF)
                          ;; Regional Indicator Symbols
                          (#x1F1E0 . #x1F1FF)
                          ;; Miscellaneous Symbols
                          (#x2600 . #x26FF)
                          ;; Dingbats
                          (#x2700 . #x27BF)
                          ;; Supplemental Symbols and Pictographs
                          (#x1F900 . #x1F9FF)
                          ;; Symbols and Pictographs Extended-A
                          (#x1FA70 . #x1FAFF)
                          ;; Additional common emoji ranges
                          (#x1F004 . #x1F004)  ; Mahjong tile
                          (#x1F0CF . #x1F0CF)  ; Playing card
                          (#x1F18E . #x1F18E)  ; AB button
                          (#x1F191 . #x1F19A)  ; Squared symbols
                          (#x1F201 . #x1F202)  ; Squared symbols
                          (#x1F21A . #x1F21A)  ; Squared CJK
                          (#x1F22F . #x1F22F)  ; Squared finger
                          (#x1F232 . #x1F23A)  ; Squared symbols
                          (#x1F250 . #x1F251)  ; Circled symbols
                          ;; Arrows and other symbols often used as emoji
                          (#x2194 . #x2199)    ; Arrows
                          (#x21A9 . #x21AA)    ; Curved arrows
                          (#x231A . #x231B)    ; Watch symbols
                          (#x2328 . #x2328)    ; Keyboard
                          (#x23CF . #x23CF)    ; Eject symbol
                          (#x23E9 . #x23F3)    ; Media symbols
                          (#x23F8 . #x23FA)    ; Media symbols
                          (#x24C2 . #x24C2)    ; Circled M
                          (#x25AA . #x25AB)    ; Squares
                          (#x25B6 . #x25B6)    ; Play button
                          (#x25C0 . #x25C0)    ; Reverse play
                          (#x25FB . #x25FE)    ; Squares
                          (#x2660 . #x2663)    ; Card suits
                          (#x2665 . #x2666)    ; Card suits
                          (#x2668 . #x2668)    ; Hot springs
                          (#x267B . #x267B)    ; Recycling
                          (#x267E . #x267F)    ; Infinity and wheelchair
                          (#x2692 . #x2697)    ; Tools and symbols
                          (#x2699 . #x269C)    ; Tools and symbols
                          (#x26A0 . #x26A1)    ; Warning signs
                          (#x26AA . #x26AB)    ; Circles
                          (#x26B0 . #x26B1)    ; Coffin and funeral
                          (#x26BD . #x26BE)    ; Sports
                          (#x26C4 . #x26C5)    ; Weather
                          (#x26C8 . #x26C8)    ; Lightning
                          (#x26CE . #x26CF)    ; Ophiuchus and pick
                          (#x26D1 . #x26D1)    ; Helmet
                          (#x26D3 . #x26D4)    ; Chains and no entry
                          (#x26E9 . #x26EA)    ; Buildings
                          (#x26F0 . #x26F5)    ; Mountain to sailboat
                          (#x26F7 . #x26FA)    ; Skier to tent
                          (#x26FD . #x26FD)    ; Fuel pump
                          ))
          (count 0))
      ;; Build a regex pattern for all emoji ranges
      (dolist (range emoji-ranges)
        (let ((start (car range))
              (end (cdr range)))
          (goto-char (point-min))
          (while (re-search-forward (format "[%c-%c]" start end) nil t)
            (delete-char -1)
            (setq count (1+ count)))))

      ;; Also remove some common text-based emoji patterns
      (goto-char (point-min))
      (while (re-search-forward "[:;=8]-?[)D(P|/\\]\\|[)D(P|/\\]-?[:;=8]\\|<[3/]\\|\\\\o/" nil t)
        (replace-match "")
        (setq count (1+ count)))

      (message "Removed %d emoji(s)" count))))

(defun decomment ()
  "Delete comments that start with a capital letter.
Handles different comment styles based on major mode."
  (interactive)
  (save-excursion
    (let ((comment-patterns
           (cond
            ;; Python, shell scripts, Ruby, Perl, etc.
            ((memq major-mode '(python-mode python-ts-mode ruby-mode perl-mode sh-mode bash-mode))
             '("^[ \t]*#[ \t]+[A-Z].*$"))
            ;; JavaScript, TypeScript, C, C++, Java, etc.
            ((memq major-mode '(js-mode js2-mode js-ts-mode typescript-mode typescript-ts-mode
                               c-mode c++-mode java-mode csharp-mode go-mode rust-mode))
             '("^[ \t]*//[ \t]+[A-Z].*$"))
            ;; Lisp dialects (Emacs Lisp, Common Lisp, Scheme, Clojure)
            ((memq major-mode '(emacs-lisp-mode lisp-mode scheme-mode clojure-mode))
             '("^[ \t]*;+[ \t]+[A-Z].*$"))
            ;; SQL
            ((eq major-mode 'sql-mode)
             '("^[ \t]*--[ \t]+[A-Z].*$"))
            ;; HTML, XML
            ((memq major-mode '(html-mode xml-mode web-mode))
             '("^[ \t]*<!--[ \t]+[A-Z].*-->[ \t]*$"))
            ;; CSS, SCSS
            ((memq major-mode '(css-mode scss-mode))
             '("^[ \t]*/\\*[ \t]+[A-Z].*\\*/[ \t]*$"))
            ;; Haskell
            ((eq major-mode 'haskell-mode)
             '("^[ \t]*--[ \t]+[A-Z].*$"))
            ;; Lua
            ((eq major-mode 'lua-mode)
             '("^[ \t]*--[ \t]+[A-Z].*$"))
            ;; Default: try common patterns
            (t
             '("^[ \t]*#[ \t]+[A-Z].*$"
               "^[ \t]*//[ \t]+[A-Z].*$"
               "^[ \t]*;+[ \t]+[A-Z].*$"))))
          (count 0))

      (dolist (pattern comment-patterns)
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (beginning-of-line)
          (kill-whole-line)
          (setq count (1+ count))))

      (message "Removed %d comment line(s)" count))))

(defun demoji-region (start end)
  "Delete all emojis in the specified region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (demoji))))

(defun decomment-region (start end)
  "Delete comments that start with a capital letter in the specified region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (decomment))))

(provide 'init-chat)
;;; init-chat.el ends here
