;;; janet-nrepl.el --- A Janet netrepl -*- lexical-binding: t; -*-

;; Author: andrewppar
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A Janet NREPL Client For Emacs

;;; Code:

;;;;;;;;;;
;;; Client

(require 'janet-nrepl-message)
(require 'janet-nrepl-response)
(require 'janet-nrepl-result)

(defvar *janet-nrepl/process* nil)
(defvar *janet-nrepl/repl-name* nil)
(defvar *janet-nrepl/latest-response* nil)
(defvar *janet-nrepl/last-message* "")
(defvar *janet-nrepl/response-history* nil)
(defvar *janet-nrepl/janet-builtins* nil)

(defun janet-nrepl--history-length ()
  "Return the length of nrepls current response history."
  (length *janet-nrepl/response-history*))

(defun janet-nrepl/send-string (process msg &optional without-prefix?)
  "Send MSG to janet netrepl via PROCESS."
  (thread-last (encode-coding-string msg 'utf-8-unix)
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

(defun janet-nrepl/get-builtins ()
  "Transform janet's builtin function docs to elisp."
  (let* ((doc-string (thread-last
		       "(do
                         (var result \"(\")
                         (eachk function-sym root-env
                          (set result
                           (string/format \"%s %s\" result function-sym)))
                         (string/format \"%s)\" result))"
		       janet-nrepl/send
		       strip-ansi-chars
		       (string-replace "\\n" "\n")
		       (string-replace "\\" ""))))

    (setq *janet-nrepl/janet-builtins*
	  (thread-last
	    (substring doc-string 1)
	    read-from-string
	    car
	    (mapcar (lambda (fn) (format "%s" fn)))))))

(defun janet-nrepl/get-doc (symbol-string)
  "Get the documentation for SYMBOL-STRING."
  (thread-last
    symbol-string
    (format "(doc %s)")
    janet-nrepl/send
    strip-ansi-chars
    (replace-regexp-in-string "^nil" "")))

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
    (setq *janet-nrepl/latest-response* nil)
    (janet-nrepl/get-builtins)
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

(defun strip-ansi-chars (str)
  "Remove ANSI characters from STR."
  (let ((clean-str (ansi-color-apply str)))
    (set-text-properties 0 (length clean-str) nil clean-str)
    clean-str))

(defun janet-nrepl/prompt-connect (host port name)
  "Prompt user for HOST, PORT, and NAME to connect to a janet netrepl."
  (interactive
   (list
    (read-string "host: " "localhost")
    (read-string "port: " "9365")
    (read-string "name: " "gabriel")))
  (janet-nrepl/connect host port name))

(defun janet-eval-defun-to-comment ()
  "Eval sexpression at point to comment."
  (interactive)
  (let (start end)
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (setq start (point)))
    (let ((result (thread-first
		    (buffer-substring start end)
		    janet-nrepl/send
		    string-as-multibyte
		    strip-ansi-chars
		    string-trim)))
      (save-excursion
	(goto-char end)
	(insert
	 (string-join
	  (mapcar
	   (lambda (line) (format "# %s" line))
	   (split-string result "\n"))
	  "\n"))))))

(defun janet-nrepl/docs-at-point ()
  "Get documentation for item at point."
  (interactive)
  (let ((word (current-word))
	(window (split-window)))
    (select-window window)
    (switch-to-buffer *janet-nrepl/result-buffer*)
    (janet-nrepl/with-clean-result-buffer
     (insert (janet-nrepl/get-doc word)))))


(provide 'janet-nrepl)
;;; janet-nrepl.el ends here
