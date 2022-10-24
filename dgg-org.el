  (defun zp/org-fold (&optional keep-position)
    (let ((indirectp (not (buffer-file-name)))
          (org-startup-folded 'overview))
      ;; Fold drawers
      (org-set-startup-visibility)
      ;; Fold trees
      (org-overview)
      (unless keep-position
        (goto-char (point-min)))
      (recenter)
      (save-excursion
        (goto-char (point-min))
        (org-show-entry)
        (when (org-at-heading-p)
          (org-show-children)))))

  (defun zp/org-show-all (arg)
    (interactive "p")
    (let ((pos-before (point))
          (indirect (not (buffer-file-name))))
      (setq-local zp/org-narrow-previous-position pos-before)
      ;; Do not widen buffer if in indirect buffer
      (unless indirect
        (widen)
        (org-display-inline-images))
      ;; Unfold everything
      (org-show-all)
      (unless (eq arg 4)
        (goto-char (point-min)))
      (recenter-top-bottom)
      (when arg
        (message "Showing everything.")
        (run-hooks 'zp/org-after-view-change-hook))))

  ;; org-narrow movements

  (defun zp/org-narrow-to-subtree ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (org-narrow-to-subtree)
    (zp/org-fold nil)
    (when (called-interactively-p 'any)
      (message "Narrowing to tree at point.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defun zp/org-widen ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (let ((pos-before (point)))
      (setq-local zp/org-narrow-previous-position pos-before))
    (widen)
    (when (called-interactively-p 'any)
      (message "Removing narrowing.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defvar zp/presentation-mode nil)

  (defun zp/org-narrow-forwards ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (widen)
    (org-forward-heading-same-level 1)
    (org-narrow-to-subtree)
    (unless zp/presentation-mode
      (zp/org-fold nil))
    (when (called-interactively-p 'any)
      (message "Narrowing to next tree.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defun zp/org-narrow-backwards ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (widen)
    (org-backward-heading-same-level 1)
    (org-narrow-to-subtree)
    (unless zp/presentation-mode
      (zp/org-fold nil))
    (when (called-interactively-p 'any)
      (message "Narrowing to previous tree.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defun zp/org-narrow-up-heading (&optional arg keep-position)
    "Move to the upper subtree, and narrow the buffer to it."
    (interactive "p")
    (unless (buffer-narrowed-p)
      (user-error "No narrowing"))
    (let ((pos-before (point)))
      (setq-local zp/org-narrow-previous-position pos-before)
      (widen)
      (org-reveal)
      (outline-up-heading 1)
      (org-narrow-to-subtree)
      (when (or (eq arg 4)
                keep-position)
        (goto-char pos-before)
        (recenter-top-bottom))
      (zp/org-fold (or (eq arg 4)
                       keep-position))
      (when arg
        (message "Narrowing to tree above.")
        (run-hooks 'zp/org-after-view-change-hook))))

  (defun zp/org-narrow-up-heading-dwim (arg)
    "Narrow to the upper subtree, and narrow the buffer to it.
If the buffer is already narrowed to level-1 heading, overview
the entire buffer."
    (interactive "p")
    (if (save-excursion
          ;; Narrowed to a level-1 heading?
          (goto-char (point-min))
          (and (buffer-narrowed-p)
               (equal (org-outline-level) 1)))
        (zp/org-overview arg)
      (zp/org-narrow-up-heading arg)))

  (defun zp/org-narrow-previous-heading (arg)
    "Move to the previously narrowed tree, and narrow the buffer to it."
    (interactive "p")
    (if (bound-and-true-p zp/org-narrow-previous-position)
        (let ((pos-before zp/org-narrow-previous-position))
          (goto-char zp/org-narrow-previous-position)
          (org-reveal)
          (org-cycle)
          (org-narrow-to-subtree)
          (setq zp/org-narrow-previous-position nil)
          (message "Narrowing to previously narrowed tree."))
      (message "Couldn’t find a previous position.")))
