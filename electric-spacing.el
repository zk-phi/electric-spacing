;;; electric-spacing.el --- insert spaces automatically between user-defined patterns

;; Copyright (C) 2014- zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://zk-phi.gitub.io/
;; Version: 1.0.1
;; Package-Requires: ((cl-lib "0.5"))

;;; Commentary:

;; Require this script
;;
;;   (require 'electric-spacing)
;;
;; and you can enable electric-spacing-mode with
;;
;;   (electric-spacing-mode 1)
;;
;; or "M-x electric-spacing-mode". Enabling the mode with
;; major-mode-hooks may be useful.
;;
;;   (add-hook 'org-mode-hook 'electric-spacing-mode)

;; You can define a pattern where whitespaces should be inserted, by
;; adding a regexp-pair to "electric-spacing-regexp-pairs". For
;; example, following setting inserts spaces between Japanese and
;; English characters like: "foo ほげ bar ふが"
;;
;;   (add-to-list 'electric-spacing-regexp-pairs
;;                '("\\cA\\|\\cC\\|\\ck\\|\\cK\\|\\cH" . "[0-9A-Za-z]"))
;;   (add-to-list 'electric-spacing-regexp-pairs
;;                '("[0-9A-Za-z]" . "\\cA\\|\\cC\\|\\ck\\|\\cK\\|\\cH"))
;;
;; If you want to define a buffer-local pattern, make variable
;; "electric-spacing-regexp-pairs" buffer-local before registering the
;; pattern.
;;
;;   (make-local-variable 'electric-spacing-regexp-pairs)

;; For more informations, see "Readme".

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 add an option `electric-spacing-inhibit-commands'

;;; Code:

(require 'cl-lib)

(defgroup electric-spacing nil
  "A minor mode that inserts spaces automatically."
  :group 'emacs)

(defcustom electric-spacing-regexp-pairs nil
  "List of pairs of the form (REGEXP1 . REGEXP2). Each REGEXPs
  must not contain any groups."
  :group 'electric-spacing)

(defcustom electric-spacing-inhibit-commands
  '(yank clipboard-yank org-yank phi-rectangle-yank t)
  "Commands which should not invoke electric spacing."
  :group 'electric-spacing)

(defun electric-spacing-maybe-insert-space ()
  (cl-some (lambda (pair)
             (when (and (looking-back (car pair))
                        (looking-at (cdr pair)))
               (insert " ")
               t))
           electric-spacing-regexp-pairs))

(defun electric-spacing-update (beg end len)
  (unless (or undo-in-progress
              (memq this-command electric-spacing-inhibit-commands)
              (and (= (- end beg) 0) (eql len 1)))
    (save-match-data
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
              (insert " "))))))))

;;;###autoload
(define-minor-mode electric-spacing-mode
  "A minor mode that inserts spaces automatically."
  :init-value nil
  :global nil
  (if (not electric-spacing-mode)
      (remove-hook 'after-change-functions 'electric-spacing-update t)
    (add-hook 'after-change-functions 'electric-spacing-update nil t)))

(provide 'electric-spacing)

;;; electric-spacing.el ends here
