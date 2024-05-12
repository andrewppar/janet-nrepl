;;; janet-nrepl-message.el --- Message Packing and Unpacking -*- lexical-binding: t; -*-

;; Author: andrewppar
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Parse messages from the server
;; Create messages to send to the server

;;; Code:
(require 'bindat)
(require 'subr-x)

(defvar *0x100* 256)
(defvar *0x10000* (* 256 256))
(defvar *0x1000000* (* 256 256 256))

(defconst *jnm--header-spec*
  '((b0 u8)
    (b1 u8)
    (b2 u8)
    (b3 u8)))

(defun jnm--message-spec (size)
  "Create the bindat message spec for a message of SIZE."
  (thread-last
    *jnm--header-spec*
    reverse
    (cons `(message str ,size))
    reverse))

;; Pack

(defun jnm--response-size-from-header (header)
  "Calculate the size of a message from HEADER."
  (+ (bindat-get-field header 'b0)
       (* (bindat-get-field header 'b1) *0x100*)
       (* (bindat-get-field header 'b2) *0x10000*)
       (* (bindat-get-field header 'b3) *0x1000000*)))

(defun jnm--response-size (msg)
  "Get the length in bytes of the data of receieved MSG."
  (let ((header (bindat-unpack *jnm--header-spec* msg)))
    (jnm--response-size-from-header header)))

(defun jnm--parse-response-chunk (msg)
  "Parse the response MSG from the server."
  (let* ((size (jnm--response-size msg))
         (spec (jnm--message-spec size)))
    (bindat-unpack spec msg)))

(defun janet-nrepl-message/parse (msg)
  "Parse a response of MSG from the server."
  (setq msg (encode-coding-string msg 'utf-8-unix))
  (let* ((current-response (jnm--parse-response-chunk msg))
         (current-idx 0)
         (responses (list (alist-get 'message current-response))))
    (while current-response
      (let ((last-response-size (jnm--response-size-from-header
				 current-response)))
        (setq current-idx (+ current-idx last-response-size 4))
        (let* ((next-msg-string (substring msg current-idx)))
          (if (equal "" next-msg-string)
              (setq current-response nil)
            (let ((response (jnm--parse-response-chunk next-msg-string)))
              (setq current-response response)
              (push (alist-get 'message response) responses))))))
    responses))

;; Packing

(defun jnm--message-size (msg)
  "Get the length in bytes of the data of MSG to be sent."
  (string-bytes (encode-coding-string msg 'utf-8-unix)))

(defun jnm--number->bytes (number)
  "Write NUMBER as little endian bytes."
  (unless (integerp number)
    (user-error "Expected an integer"))
  (let (ret)
    (while (/= number 0)
      (setq ret (cons (mod number #x100) ret))
      (setq number (/ number #x100)))
    (reverse (or ret (list 0)))))

(defun janet-nrepl-message/pack (msg)
  "Pack MSG to be sent to server."
  (let* ((size  (jnm--message-size msg))
         (bytes (jnm--number->bytes size))
         (header `((b3 . ,(cadddr bytes))
                   (b2 . ,(caddr bytes))
                   (b1 . ,(cadr bytes))
                   (b0 . ,(car bytes))))
         (type-spec (jnm--message-spec size))
         (spec (reverse (cons `(message . ,msg) header))))
    (bindat-pack type-spec spec)))

(provide 'janet-nrepl-message)
;;; janet-nrepl-message.el ends here
