(defun dgg/scratch-region ()
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (insert "+")
    (goto-char (region-beginning))
    (insert "+")))


(defun dgg/bolt-region ()
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (insert "*")
    (goto-char (region-beginning))
    (insert "*")))

;; Youtube URL from emacs Arnold
(defun my/copy-idlink-to-clipboard()
  "Copy an ID link with the headline to killring, if no ID is
there then create a new unique ID. This function works only in
org-mode or org-agenda buffers. The purpose of this function is
to easily construct id:-links to org-mode items. If its assigned
to a key it saves you marking the text and copying to the
killring."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmphead (nth 4 (org-heading-components)))
    (setq mytmpid (funcall 'org-id-get-create))
    (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
    (kill-new mytmplink)
    (message "Copied %s to killring (clipboard)" mytmplink)
    ))





