;;; spot-generic-query.el --- HTTP request layer for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; HTTP request functions for the spot Spotify client.
;; Provides both synchronous and asynchronous request handling.

;;; Code:

(require 'json)
(require 'url)

(require 'spot-var)

(defun spot-retrieve-url-to-alist-synchronously (url)
  "Return alist representation of JSON response from URL."
  (with-current-buffer (url-retrieve-synchronously url nil nil spot--request-timeout)
    (let ((json (decode-coding-region (+ 1 url-http-end-of-headers)
                                      (point-max) 'utf-8 t)))
      (when (not (string= json ""))
        (json-read-from-string json)))))

(cl-defun spot-request (&key method url q-params parse-json extra-headers data)
  "Handle spot HTTP requests.
METHOD is the request method, URL is the URL, Q-PARAMS is the
query parameters, PARSE-JSON is a boolean for whether to parse
and return the json response as an alist, EXTRA-HEADERS is an
alist of headers, and DATA is request body data as JSON."
  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers extra-headers))
    (if parse-json
        (spot-retrieve-url-to-alist-synchronously
         (concat url q-params))
      (url-retrieve-synchronously
       (concat url q-params) nil nil spot--request-timeout))))

(defun spot-retrieve-url-to-alist-asynchronously (url callback)
  "Async version that calls CALLBACK with alist from URL's JSON response."
  (url-retrieve
   url
   (lambda (_status)
     (let ((json (decode-coding-region (+ 1 url-http-end-of-headers)
                                       (point-max) 'utf-8 t)))
       (funcall callback json)))
   nil t t))

(defun spot--message-request-complete (&rest _args)
  "Display message when request completes."
  (message "spot request complete"))

(cl-defun spot-request-async (&key method url q-params callback extra-headers data)
  "Async version of `spot-request'.  CALLBACK receives the response.
METHOD is the request method, URL is the URL, Q-PARAMS is the
query parameters, EXTRA-HEADERS is an alist of headers, and DATA
is request body data as JSON."
  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers extra-headers))
    (spot-retrieve-url-to-alist-asynchronously
     (concat url q-params)
     (or callback #'spot--message-request-complete))))

(defun spot--currently-playing ()
  "Get the currently playing track."
  (spot-request
   :method "GET"
   :url spot-player-url
   :q-params (spot--base-q-params)
   :parse-json t))

(provide 'spot-generic-query)

;;; spot-generic-query.el ends here
