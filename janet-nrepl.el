;;; janet-nrepl.el --- A Janet netrepl -*- lexical-binding: t; -*-

;; Author: andrewppar
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.2") (eros "0.1.0"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A Janet NREPL Client For Emacs

;;; Code:

;;;;;;;;;;;;;;;;;
;;; Eros Overlay
(require 'eros)

(defun eros-overlay-result (value)
  "Create eros overlay for VALUE."
  (eros--make-result-overlay (format "%s" value)
    :where (save-excursion (end-of-defun) (point))
    :duration eros-eval-result-duration))

;;;;;;;;;;
;;; Client

(require 'janet-nrepl-message)

(defvar *janet-nrepl/process* nil)
(defvar *janet-nrepl/repl-name* nil)
(defvar *janet-nrepl/latest-response* nil)
(defvar *janet-nrepl/last-message* "")
(defvar *janet-nrepl/response-history* nil)

(defun janet-nrepl--history-length ()
  "Return the length of nrepls current response history."
  (length *janet-nrepl/response-history*))

(defun janet-nrepl/send-string (process msg &optional without-prefix?)
  "Send MSG to janet netrepl via PROCESS."
  (thread-last msg
               string-as-unibyte
               janet-nrepl-message/pack
               (process-send-string process)))

(defun janet-repl/filter (process msg)
  "Filter for janet nrepl for PROCESS with MSG."
  (when msg
    (setq *janet-nrepl/last-message* msg)
    (if *janet-nrepl/repl-name*
	(thread-last
	  process
	  process-buffer
	  (janet-nrepl-response/add-msg *janet-nrepl/latest-response* msg))
      (let* ((repl-msg (car (janet-nrepl-message/parse msg)))
	     (repl-items (split-string repl-msg ":"))
	     (repl-name (car repl-items))
	     (iteration (cadr repl-items))
	     (response (janet-nrepl-response/make repl-name)))
	(jnr/add-repl-prompt response repl-msg)
	(setq *janet-nrepl/repl-name* repl-name
	      *janet-nrepl/latest-response* response)))))

(defun janet-nrepl/connect (host port name)
  "Connect to a running janet netrepl on HOST and PORT with NAME."
  (let* ((buffer (get-buffer-create "*janet-repl*"))
         (stream (open-network-stream name buffer host port)))
    (set-process-filter stream 'janet-repl/filter)
    (set-process-buffer stream buffer)
    (setq *janet-nrepl/process* stream)
    (setq *janet-nrepl/latest-response* nil)
    (janet-nrepl/send-string
     stream
     (format "\xFF{:auto-flush true :name \"%s\"}" name))
    (while (not *janet-nrepl/latest-response*)
      (sleep-for 0.1))
    (save-window-excursion
      (switch-to-buffer buffer)
      (goto-char (point-max))
      (insert (plist-get *janet-nrepl/latest-response* :repl-prompt)))
    (setq *janet-nrepl/latest-response* nil)))

(defun janet-nrepl/send (msg)
  "Send MSG to connected janet nrepl."
  (let ((process *janet-nrepl/process*))
    (janet-nrepl/send-string process msg)
    (setq *janet-nrepl/latest-response* (janet-nrepl-response/make *janet-nrepl/repl-name*))
    (while (not (plist-get *janet-nrepl/latest-response* :repl-prompt))
      (sleep-for 0.1))
    (save-window-excursion
      (switch-to-buffer (process-buffer process))
      (goto-char (point-max))
      (insert (plist-get *janet-nrepl/latest-response* :repl-prompt)))
    (let ((result (plist-get *janet-nrepl/latest-response* :stdout)))
      (setq *janet-nrepl/latest-response* nil)
      result)))

(defun janet-nrepl/close-connection ()
  "Close the active janet nrepl connection."
  (interactive)
  (when *janet-nrepl/process*
    (setq *janet-nrepl/repl-name* nil
          *janet-nrepl/response-history* nil
          *janet-nrepl/latest-response* nil)
    (let ((buffer (process-buffer *janet-nrepl/process*)))
      (delete-process *janet-nrepl/process*)
      (kill-buffer buffer))
    (setq *janet-nrepl/process* nil)
    (message "Connection to janet process closed")))

(defun janet-eval-defun-at-point ()
  "Eval sexpression at point."
  (interactive)
  (let (start end)
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (setq start (point)))
    (thread-first (buffer-substring start end)
		  janet-nrepl/send
		  string-as-multibyte
		  ansi-color-apply
		  string-trim
		  eros-overlay-result)))

(defun janet-send-defun-at-point ()
  "Eval sexpression at point."
  (interactive)
  (let (start end)
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (setq start (point)))
    (let* ((form (buffer-substring start end))
           (result (string-as-multibyte (janet-nrepl/send form))))
      (eros-overlay-result result))))

(defun janet-nrepl/prompt-connect (host port name)
  "Prompt user for HOST, PORT, and NAME to connect to a janet netrepl."
  (interactive
   (list
    (read-string "host: " "localhost")
    (read-string "port: " "9365")
    (read-string "name: " "gabriel")))
  (janet-nrepl/connect host port name))

(provide 'janet-nrepl)
;;; janet-nrepl.el ends here
