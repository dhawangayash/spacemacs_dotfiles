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

(define-key emacs-lisp-mode-map (kbd "<delete>") 'xah-delete-backward-char-or-bracket-text)


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

;; URL: http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html
(defun xah-cycle-hyphen-lowline-space ( &optional @begin @end )
  "Cycle hyphen/lowline/space chars in selection or inside quote/bracket or line, in that order.
When this command is called, pressing t will repeat it. Press other key to exit.
The region to work on is by this order:
 1. if there is a selection, use that.
 2. If cursor is string quote or any type of bracket, and is within current line, work on that region.
 3. else, work on current line.
URL `http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html'
Version 2019-02-12 2021-08-09"
  (interactive)
  ;; this function sets a property 'state. Possible values are 0 to length of $charArray.
  (let ($p1 $p2)
    (if (and @begin @end)
        (setq $p1 @begin $p2 @end)
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (if (nth 3 (syntax-ppss))
            (save-excursion
              (skip-chars-backward "^\"")
              (setq $p1 (point))
              (skip-chars-forward "^\"")
              (setq $p2 (point)))
          (let (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
            (skip-chars-backward $skipChars (line-beginning-position))
            (setq $p1 (point))
            (skip-chars-forward $skipChars (line-end-position))
            (setq $p2 (point))
            (set-mark $p1)))))
    (let ( $charArray $length $regionWasActive-p $nowState $changeTo)
      (setq $charArray ["-" "_" " "])
      (setq $length (length $charArray))
      (setq $regionWasActive-p (region-active-p))
      (setq $nowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0 ))
      (setq $changeTo (elt $charArray $nowState))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (goto-char (point-min))
          (while (re-search-forward (elt $charArray (% (+ $nowState 2) $length)) (point-max) "move")
            (replace-match $changeTo t t))))
      (when (or (string-equal $changeTo " ") $regionWasActive-p)
        (goto-char $p2)
        (set-mark $p1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $length))))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "t") 'xah-cycle-hyphen-lowline-space ) $kmap)))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))


;; Page up and Page down set to moving by xah-blocks
(global-set-key (kbd "<prior>") 'xah-backward-block)
(global-set-key (kbd "<next>") 'xah-forward-block)
(global-set-key (kbd "C-9") 'xah-cycle-hyphen-lowline-space)
(global-set-key (kbd "C-8") 'xah-space-to-newline)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "<home>") 'cycle-spacing)

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.
URL `http://xahlee.ino/emacs/emacs/emacs_shrink_whitepace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
    (skip-chars-backward "\n")
    (setq $p3 (point))
    (skip-chars-forward "\n")
    (setq $p4 (point))
    (delete-region $p3 $p4)))

(defun xah-fly-delete-spaces()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version: 2019-06-13"
  (interactive)
  (let ($p1 $p2)
    (skip-chars-forward " \t　")
    (setq $p2 (point))
    (skip-chars-backward " \t　")
    (setq $p1 (point))
    (delete-region $p1 $p2)))

(defun xah-shrink-whitespaces()
  "Remove whitespaces around cursor.

Shrink neighboring space, then newline, then space again, leaving one space or newline at each step, TILL no more whitespaces are there.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version 2014-10-21 2021-11-26 2021-11-30"
  (interactive)
  (let* (($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after))
         ($space-neighbor-p (or
                             (eq $charBefore 32) (eq $charBefore 9)
                             (eq $charBefore 32) (eq $charBefore 9))))
         (skip-chars-backward " \n\t　")
         (setq $p1 (point))
         (goto-char $p0)
         (skip-chars-forward " \n\t　")
         (setq $p2 (point))
         (goto-char $p1)
         (while (search-forward "\n" $p2 t)
           (setq $eol-count (1+ $eol-count)))
         (goto-char $p0)
         (cond
          ((eq $eol-count 0)
           (if (> (- $p2 $p1) 1)
               (progn
                 (delete-horizontal-space) (insert " "))
             (progn (delete-horizontal-space))))
          ((eq $eol-count 1)
           (if $space-neighbor-p
               (xah-fly-delete-spaces)
             (progn (xah-delete-blank-lines) (insert " "))))
          ((eq $eol-count 2)
           (if $space-neighbor-p
               (xah-fly-delete-spaces)
             (progn
               (xah-delete-blank-lines)
               (insert "\n"))))
          ((> $eol-count 2)
           (if $space-neighbor-p
               (xah-fly-delete-spaces)
             (progn
               (goto-char $p2)
               (search-backward "\n")
               (delete-region $p1 (point))
               (insert "\n"))))
          (t (progn
               (message "nothing done. logic error 40873. shouldn't reach here."))))))

(global-set-key (kbd "<kp-0>") 'xah-shrink-whitespaces)

(defun xah-delete-current-text-block ()
  "Delete the current text block plus blank lines, or selection, and copy to `kill-ring'.

URL `http://xahlee.info/emacs/emacs/emacs_delete_block.html'
Version 2017-07-09 2021-08-14"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil 1)
            (setq $p1 (goto-char (match-end 0)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n+" nil 1)
        (setq $p2 (point))))
    (kill-region $p1 $p2)))

(spacemacs/set-leader-keys (kbd "dd") 'xah-delete-current-text-block)

(define-key evil-motion-state-map (kbd "<up>") 'beginning-of-defun)
(define-key evil-motion-state-map (kbd "<down>") 'end-of-defun)
(global-set-key (kbd "<kp-decimal>") 'evil-jump-item)

;; evil-jump-item (found in evil-motion-state-map)
(spacemacs/set-leader-keys (kbd "jj") 'end-of-defun)
(spacemacs/set-leader-keys (kbd "kk") 'beginning-of-defun)
(spacemacs/set-leader-keys (kbd "hh") 'evil-lisp-state-prev-opening-paren)


(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-13"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (equal last-command this-command ))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
            (progn
              (skip-chars-backward "\n\t ")
              (forward-char ))
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2017-05-30"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" ))
    (end-of-line)))

(global-set-key (kbd "<left>") 'xah-beginning-of-line-or-block)
(global-set-key (kbd "<right>") 'xah-end-of-line-or-block)
