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

;;;;;;;;;;;;
;;; Messages

;;; Byte Encoding and Decoding
(require 'bindat)

(defvar *0x100* 256)
(defvar *0x10000* (* 256 256))
(defvar *0x1000000* (* 256 256 256))

(defconst *janet-nrepl/header-spec*
  '((b0 u8)
    (b1 u8)
    (b2 u8)
    (b3 u8)))

(defun janet-nrepl--message-spec (size)
  "Create the bindat message spec for a message of SIZE."
  (thread-last
    *janet-nrepl/header-spec*
    reverse
    (cons `(message str ,size))
    reverse))

(defun janet-nrepl/->bytes (number)
  "Write NUMBER as little endian bytes."
  (unless (integerp number)
    (user-error "Expected an integer"))
  (let (ret)
    (while (/= number 0)
      (setq ret (cons (mod number #x100) ret))
      (setq number (/ number #x100)))
    (reverse (or ret (list 0)))))

(defun janet-nrepl--response-size-from-struct (struct)
  "Calculate the size of a message from STRUCT."
  (+ (bindat-get-field struct 'b0)
       (* (bindat-get-field struct 'b1) *0x100*)
       (* (bindat-get-field struct 'b2) *0x10000*)
       (* (bindat-get-field struct 'b3) *0x1000000*)))

(defun janet-nrepl/response-size (msg)
  "Get the length in bytes of the data of receieved MSG."
  (let ((header (bindat-unpack *janet-nrepl/header-spec* msg)))
    (janet-nrepl--response-size-from-struct header)))

(defun janet-nrepl--remove-leading-control-chars (msg)
  "Remove leading \xFF and \xFE from MSG."
  (let ((result msg))
    (while (or (string-prefix-p "\xFF" result)
               (string-prefix-p "\xFF" result))
      (setq result (substring result 1)))
    result))


(defun janet-nrepl/parse-message-chunk (msg)
  "Parse the response MSG from the server."
  (let* ((size (janet-nrepl/response-size msg))
         (spec (janet-nrepl--message-spec size)))
    (bindat-unpack spec msg)))

(defun janet-nrepl/parse-message (msg)
  "Parse a response of MSG from the server."
  (setq msg (janet-nrepl--remove-leading-control-chars msg))
  (setq msg (string-as-unibyte msg))
  (let* ((current-response (janet-nrepl/parse-message-chunk msg))
         (current-idx 0)
         (responses (list current-response)))
    (while current-response
      (let ((last-response-size (janet-nrepl--response-size-from-struct
                                 current-response)))
        (setq current-idx (+ current-idx last-response-size 4))
        (let* ((next-msg-string (substring msg current-idx)))
          (if (equal "" next-msg-string)
              (setq current-response nil)
            (let ((response (janet-nrepl/parse-message-chunk next-msg-string)))
              (setq current-response response)
              (push response responses))))))
    responses))

(defun janet-nrepl/message-size (msg)
  "Get the length in bytes of the data of MSG to be sent."
  (string-bytes (string-as-unibyte msg)))

(defun janet-nrepl/pack-message (msg)
  "Pack MSG to be sent to server."
  (let* ((size  (janet-nrepl/message-size msg))
         (bytes (janet-nrepl/->bytes size))
         (header `((b3 . ,(cadddr bytes))
                   (b2 . ,(caddr bytes))
                   (b1 . ,(cadr bytes))
                   (b0 . ,(car bytes))))
         (type-spec (janet-nrepl--message-spec size))
         (spec (reverse (cons `(message . ,msg) header))))
    (bindat-pack type-spec spec)))

;;;;;;;;;;
;;; Client

(defvar *janet-nrepl/process* nil)
(defvar *janet-nrepl/repl-name* nil)
(defvar *janet-nrepl/latest-response* nil)
(defvar *janet-nrepl/last-message* "")
(defvar *janet-nrepl/response-history* nil)
(defvar *janet-nrepl/filter-ran?* nil)

(defun janet-nrepl--history-length ()
  "Return the length of nrepls current response history."
  (length *janet-nrepl/response-history*))

(defun janet-nrepl/send-string (process msg &optional without-prefix?)
  "Send MSG to janet netrepl via PROCESS."
  (thread-last msg
               string-as-unibyte
               janet-nrepl/pack-message
               (process-send-string process)))

(defun janet-nrepl--clean-string (string)
  "Remove leading \xFF and ansi-colorize STRING."
  (let ((result (string-trim (ansi-color-apply string))))
    (when (or (string-prefix-p "\xFF" result)
              (string-prefix-p "\xFE" result))
      (setq result (substring result 1)))))

(defun janet-nrepl/parse-server-response (response)
  "Parse RESPONSE into a pair of success status and eval result."
  (unless (equal response "^A^@^@^@")
    (let ((parsed-response (janet-nrepl/parse-message response)))
      (let* ((repl-prompt (string-split (alist-get 'message (car parsed-response)) ":"))
             (prompt (car repl-prompt))
             (iteration (cadr repl-prompt))
             (eval-result (alist-get 'message (cadr parsed-response)))
             (stdout (alist-get 'message (caddr parsed-response))))
        (list :prompt prompt
              :iteration iteration
              :result (janet-nrepl--clean-string eval-result)
              :stdout (janet-nrepl--clean-string stdout))))))

(defun janet-repl/filter (process msg)
  "Filter for janet nrepl for PROCESS with MSG."
  (setq *janet-nrepl/filter-ran?* t)
  (when msg
    (setq *janet-nrepl/last-message* msg)
    (let ((buffer (process-buffer process)))
      (let ((response (janet-nrepl/parse-server-response msg)))
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
             (insert (format "[%s] %s > " iteration prompt))))))))

(defun janet-nrepl/connect (host port name)
  "Connect to a running janet netrepl on HOST and PORT with NAME."
  (let* ((buffer (get-buffer-create "*janet-repl*"))
         (stream (open-network-stream name buffer host port)))
    (set-process-filter stream 'janet-repl/filter)
    (set-process-buffer stream buffer)
    (setq *janet-nrepl/process* stream)
    (janet-nrepl/send-string
     stream
     (format "\xFF{:auto-flush true :name \"%s\"}" name))))

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
    (let* ((form (buffer-substring start end))
           (result (string-as-multibyte (janet-nrepl/send form))))
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
