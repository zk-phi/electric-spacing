(require 'cl-lib)

(defgroup electric-spacing nil
  "A minor mode that inserts spaces automatically."
  :group 'emacs)

(defvar electric-spacing-regexp-pairs
  '(("\\cA\\|\\cC\\|\\ck\\|\\cK\\|\\cH" . "[0-9A-Za-z]")
    ("[0-9A-Za-z]" . "\\cA\\|\\cC\\|\\ck\\|\\cK\\|\\cH"))
  "List of pairs of the form (REGEXP1 . REGEXP2). Each REGEXPs
  must not contain any groups.")

(defun electric-spacing-maybe-insert-space ()
  (cl-some (lambda (pair)
             (when (and (looking-at (car pair))
                        (looking-back (cdr pair)))
               (insert " ")
               t))
           electric-spacing-regexp-pairs))

(defun electric-spacing-update (beg end &rest _)
  (save-excursion
    (goto-char beg)
    (when (electric-spacing-maybe-insert-space)
      (setq end (1+ end)))
    (goto-char end)
    (electric-spacing-maybe-insert-space)
    (save-restriction
      (narrow-to-region beg end)
      (dolist (rx electric-spacing-regexp-pairs)
        (setq rx (concat "\\(" (car rx) "\\)\\(" (cdr rx) "\\)"))
        (goto-char beg)
        (while (search-forward-regexp rx nil t)
          (goto-char (match-beginning 2))
          (insert " "))))))

(define-minor-mode electric-spacing-mode
  "A minor mode that inserts spaces automatically."
  :init-value nil
  :global nil
  (if (not electric-spacing-mode)
      (remove-hook 'after-change-functions 'electric-spacing-update t)
    (electric-spacing-update 1 (1+ (buffer-size)))
    (add-hook 'after-change-functions 'electric-spacing-update nil t)))

(provide 'electric-spacing)
