;;; janet-nrepl-response.el --- Response Objects from janet netrepl -*- lexical-binding: t; -*-

;; Author: andrewppar
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A Janet NREPL Client For Emacs

;;; Code:
(require 'janet-nrepl-message)

(defun janet-nrepl-response/make (repl-name)
  "Create a response structure for REPL-NAME."
  (list
   :stdout ""
   :stderr ""
   :message ""
   :partial-response nil
   :repl-name repl-name
   :repl-prompt nil))

(defun jnr/update (response value location add-fn)
  "Update RESPONSE with VALUE at LOCATION using ADD-FN."
  (let ((old-value (plist-get response location))
	(new-value value))
    (plist-put response location (funcall add-fn old-value new-value))))

(defun jnr/concat (response value location)
  "Concat VALUE to existing value in LOCATION of RESPONSE."
  (jnr/update response value location #'concat))

(defun jnr/add-stdout (response stdout)
  "Add STDOUT to :stdout of RESPONSE."
  (jnr/concat response stdout :stdout))

(defun jnr/add-stderr (response stderr)
  "Add STDERR to :stderr of RESPONSE."
  (jnr/concat response stderr :stderr))

(defun jnr/add-message (response msg)
  "Set MSG to :message of RESPONSE."
  (jnr/update response msg :message #'cons))

(defun jnr/set (response value location)
  "Set LOCATION of RESPONSE to VALUE."
  (jnr/update response value location (lambda (ignore x) x)))

(defun jnr/add-partial-response (response partial-response)
  "Set PARTIAL-RESPONSE to :partial-response of RESPONSE."
  (jnr/set response partial-response :partial-response))

(defun jnr/add-repl-prompt (response repl-prompt)
  "Set REPL-PROMPT to :repl-prompt of RESPONSE."
  (jnr/set response repl-prompt :repl-prompt))

(defun janet-nrepl-response/add-msg (response msg buffer)
  "Update RESPONSE with MSG possibly inserting into BUFFER."
  (cl-destructuring-bind (&key repl-name partial-response &allow-other-keys)
      response
    (if (and (not partial-response) (= (string-bytes msg) 4))
	(jnr/add-partial-response response msg)
      (let* ((prefix (or (plist-get response :partial-response) ""))
	     (new-messages (janet-nrepl-message/parse (concat prefix msg))))
	(dolist (new-message new-messages)
	  (cond ((string-prefix-p "\xFF" new-message)
		 (let ((content (substring new-message 1)))
		   (save-window-excursion
		     (switch-to-buffer buffer)
		     (goto-char (point-max))
		     (insert content))
		   (jnr/add-stdout response content)))
		((string-prefix-p "\xFE" new-message)
		 (jnr/add-stdout response (substring new-message 1)))
		((string-prefix-p repl-name new-message)
		 (jnr/add-repl-prompt response new-message))
		(t
		 (jnr/add-message response new-message))))
	response))))

(provide 'janet-nrepl-response)
;;; janet-nrepl-response.el ends here
