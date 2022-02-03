;; URLS used
;; - http://xahlee.info/emacs/emacs/emacs_move_by_paragraph.html
;; - https://develop.spacemacs.org/doc/DOCUMENTATION.html#binding-keys
;; - http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html



(defun xah-delete-backward-char-or-bracket-text ()
  "Delete backward 1 character, but if it's a \"quote\" or bracket ()[]{}【】「」 etc, delete bracket and the inner text, push the deleted text to `kill-ring'.

What char is considered bracket or quote is determined by current syntax table.

If `universal-argument' is called first, do not delete inner text.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if current-prefix-arg
          (xah-delete-backward-bracket-pair)
        (xah-delete-backward-bracket-text)))
     ((looking-back "\\s(" 1)
      (progn
        (backward-char)
        (forward-sexp)
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
          (progn
            (backward-char )
            (xah-delete-forward-bracket-pairs (not current-prefix-arg)))
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     (t
      (delete-char -1)))))

(defun xah-delete-backward-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor, including the inner text.

This command assumes the left of point is a right bracket, and there's a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

(defun xah-delete-backward-bracket-pair ()
  "Delete the matching brackets/quotes to the left of cursor.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the left of point is a right bracket, and there's a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (let (( $p0 (point)) $p1)
    (forward-sexp -1)
    (setq $p1 (point))
    (goto-char $p0)
    (delete-char -1)
    (goto-char $p1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- $p0 2))))

(defun xah-delete-forward-bracket-pairs ( &optional @delete-inner-text-p)
  "Delete the matching brackets/quotes to the right of cursor.
If *delete-inner-text-p is true, also delete the inner text.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the char to the right of point is a left bracket or quote, and have a matching one after.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if @delete-inner-text-p
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let (($pt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char $pt)
      (delete-char 1))))

;; Setting DEL to delete everything within parentheses for quick deletion

(define-key emacs-lisp-mode-map (kbd "DEL") 'xah-delete-backward-char-or-bracket-text)


(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://xahlee.info/emacs/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))


(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://xahlee.info/emacs/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        ($i 1))
    (while (<= $i n)
      (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq $i n)))
      (setq $i (1+ $i)))))

;; Page up and Page down set to moving by xah-blocks
(global-set-key (kbd "<prior>") 'xah-backward-block)
(global-set-key (kbd "<next>") 'xah-forward-block)

(global-set-key (kbd "C-s") 'isearch-forward)

(global-set-key (kbd "<home>") 'cycle-spacing)
