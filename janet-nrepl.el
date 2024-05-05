;;; janet-nrepl.el --- A Janet netrepl -*- lexical-binding: t; -*-

;; Author: andrewppar
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A Janet NREPL Client For Emacs

;;; Code:

;;;;;;;;;;;;
;;; Messages

;;; Byte Encoding and Decoding
(defvar *0x100* 256)
(defvar *0x10000* (* 256 256))
(defvar *0x1000000* (* 256 256 256))

(defun janet-nrepl/->bytes (number)
  "Write NUMBER as little endian bytes."
  (unless (integerp number)
    (user-error "Expected an integer"))
  (let (ret)
    (while (/= number 0)
      (setq ret (cons (mod number #x100) ret))
      (setq number (/ number #x100)))
    (reverse (or ret (list 0)))))

(defun janet-nrepl/padding (number)
  "Generate 4 bytes of padding representing NUMBER for a janet netrepl message.

  The format for this is specified in the janet netrepl documentation."
  (let ((bytes (janet-nrepl/->bytes number)))
    (if (> (length bytes) 4)
	(error "Janet netrepl is too long to encode")
      (while (< (length bytes) 4)
	(setq bytes (reverse (cons 0 (reverse bytes)))))
      (let ((b0 (car bytes))
	    (b1 (cadr bytes))
	    (b2 (caddr bytes))
	    (b3 (cadddr bytes)))
	(format "%c%c%c%c" b0 b1 b2 b3)))))

;;;;;;;;;;
;;; Client

(defvar *janet-nrepl/process* nil)
(defvar *janet-nrepl/repl-name* nil)
(defvar *janet-nrepl/latest-response* nil)
(defvar *janet-nrepl/response-history* nil)
(defvar *janet-nrepl/filter-ran?* nil)

(defun janet-nrepl--history-length ()
  "Return the length of nrepls current response history."
  (length *janet-nrepl/response-history*))

(defun janet-nrepl--prepare-message (msg &optional without-prefix?)
  "Prepare MSG to be sent to a janet nrepl server.

  WITHOUT-PREFIX? controls if \xFF is prepended to the message."
  (unless without-prefix?
    (setq msg (format "%s" msg)))
  (let* ((padding (janet-nrepl/padding (length msg))))
    ;;(unless (string-suffix-p "\n" msg)
    ;;  (setq msg (format "%s\n" msg)))
    (format "%s%s" padding msg)))

(defun janet-nrepl/send-string (process msg &optional without-prefix?)
  "Send MSG to janet netrepl via PROCESS."
  (let* ((to-send (janet-nrepl--prepare-message msg without-prefix?)))
    (process-send-string process to-send)))

(defun janet-nrepl--parse-response-chunk (response)
  "Get the next chunk off of RESPONSE."
  (let* ((b0 (aref response 0))
  	 (b1 (aref response 1))
  	 (b2 (aref response 2))
  	 (b3 (aref response 3))
  	 (message-length (+ b0
  			    (* b1 *0x100*)
  			    (* b2 *0x10000*)
  			    (* b3 *0x1000000*)))
	 (predicted-end (+ message-length 4))
	 (response-length (length response))
	 (more? (> response-length predicted-end))
	 (end-idx (min response-length predicted-end))
	 (result (list :chunk (substring response 4 end-idx))))
    (if more?
	(plist-put result :next (substring response end-idx))
      (plist-put result :next nil))))

(defun janet-nrepl--clean-string (string)
  "Remove leading \xFF and ansi-colorize STRING."
  (let ((result (string-trim (ansi-color-apply string))))
    (when (or (string-prefix-p "\xFF" result)
	      (string-prefix-p "\xFE" result))
      (setq result (substring result 1)))))

(defun janet-nrepl/parse-server-response (response)
  "Parse RESPONSE into a pair of success status and eval result."
  (cl-destructuring-bind (&key chunk next)
      (janet-nrepl--parse-response-chunk response)
    (let ((result (list chunk))
	  (todo next))
      (while todo
	(cl-destructuring-bind (&key chunk next)
	    (janet-nrepl--parse-response-chunk todo)
	  (push chunk result)
	  (setq todo (when (and next (not (equal next ""))) next))))
      (let* ((repl-prompt (string-split (car result) ":"))
	     (prompt (car repl-prompt))
	     (iteration (cadr repl-prompt))
	     (eval-result (cadr result))
	     (stdout (caddr result)))
	(list :prompt prompt
	      :iteration iteration
	      :result (janet-nrepl--clean-string eval-result)
	      :stdout (janet-nrepl--clean-string stdout))))))

(defun janet-repl/filter (process msg)
  "Filter for janet nrepl for PROCESS with MSG."
  (setq *janet-nrepl/filter-ran?* t)
  (when msg
    (let ((buffer (process-buffer process))
	  (response (janet-nrepl/parse-server-response msg)))
      (cl-destructuring-bind (&key prompt iteration result stdout)
	  response
	(if *janet-nrepl/repl-name*
	    (progn
	      (setq *janet-nrepl/latest-response* result)
	      (push response *janet-nrepl/response-history*))
	  (setq *janet-nrepl/repl-name* prompt))
	(save-window-excursion
  	  (switch-to-buffer buffer)
  	  (goto-char (point-max))
	  (if stdout
	      (insert (format "%s\n" stdout))
	    (insert "\n"))
	  (insert (format "[%s] %s > " iteration prompt)))))))
;;	(if *janet-nrepl/repl-name*
;;	    (progn
;;
;;
;;	(let ((message (substring msg 9)))
;;	  (setq the-message msg)
;;	  (save-window-excursion
;;  	    (switch-to-buffer buffer)
;;  	    (kill-region (point-min) (point-max))
;;  	    (goto-char (point-max))
;;  	    (insert (format "%s\n" message)))
;;	  (setq *janet-nrepl/repl-name* message))))))

(defun janet-nrepl/connect (host port)
  "Connect to a running janet netrepl on HOST and PORT."
  (let* ((buffer (get-buffer-create "*janet-repl*"))
	 (stream (open-network-stream "gabriel" buffer host port)))
    (set-process-filter stream 'janet-repl/filter)
    (set-process-buffer stream buffer)
    (setq *janet-nrepl/process* stream)
    (janet-nrepl/send-string stream "\xFF{:auto-flush true :name \"arch-gabriel\"}")))

(defun janet-nrepl/send (msg)
  "Send MSG to connected janet nrepl."
  (let ((process (get-process "gabriel"))
	(old-history-length (janet-nrepl--history-length)))
    (setq *janet-nrepl/filter-ran?* nil)
    (janet-nrepl/send-string process msg)
    (while (= old-history-length (janet-nrepl--history-length))
      (sleep-for 0.1))
    *janet-nrepl/latest-response*))

(defun janet-nrepl/close-connection ()
  "Close the active janet nrepl connection."
  (interactive)
  (when *janet-nrepl/process*
    (setq *janet-nrepl/repl-name* nil
	  *janet-nrepl/response-history* nil
	  *janet-nrepl/latest-response* nil)
    (kill-buffer (process-buffer *janet-nrepl/process*))
    (delete-process *janet-nrepl/process*)
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
    (let* ((form (buffer-substring start end))
	   (result (janet-nrepl/send form)))
      (eros-overlay-result result))))

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
	   (result (janet-nrepl/send form t)))
      (eros-overlay-result result))))

(defun janet-nrepl/prompt-connect (host port)
  "Prompt user for HOST and PORT to connect to a janet netrepl."
  (interactive
   (list
    (read-string "host: " "localhost")
    (read-string "port: " "9365")))
  (janet-nrepl/connect host port))

(provide 'janet-nrepl)
;;; janet-nrepl.el ends here
