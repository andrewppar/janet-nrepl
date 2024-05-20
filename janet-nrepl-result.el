;;; janet-nrepl-result.el --- Handle Results From Janet NetRepl -*- lexical-binding: t; -*-

;; Author: andrewppar
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.2") (eros "0.1.0"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Display results from janet's nrepl

;;; Code:

;;;;;;;;;;;;;;;;;
;;; Eros Overlay
(require 'eros)

(defun eros-overlay-result (value)
  "Create eros overlay for VALUE."
  (eros--make-result-overlay (format "%s" value)
    :where (save-excursion (end-of-defun) (point))
    :duration eros-eval-result-duration))

(defvar *janet-nrepl/result-buffer* "*janet-nrepl-results*"
  "Can be reset to a buffer if one is created.")


(defmacro janet-nrepl/with-clean-result-buffer (&rest body)
  "Execute BODY in *janet-nrepl/result-buffer*."
  `(save-window-excursion
     (let ((buffer (switch-to-buffer *janet-nrepl/result-buffer*)))
       (setq *janet-nrepl/result-buffer* buffer)
       (setq inihibit-read-only nil)
       (kill-region (point-min) (point-max))
       (progn ,@body)
       (setq inihibit-read-only t)
       (setq *janet-nrepl/result-buffer* "*janet-nrepl/result-buffer*"))))



(provide 'janet-nrepl-result)
;;; janet-nrepl-result.el ends here
