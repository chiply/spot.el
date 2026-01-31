;;; spot-auth.el --- Authentication for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Spotify OAuth authentication for the spot client.
;; Handles authorization flow and token refresh.

;;; Code:

(require 'json)
(require 'url)

(require 'spot-util)
(require 'spot-var)
(require 'spot-generic-query)

;;;###autoload
(defun spot-authorize ()
  "Obtain access and refresh tokens for user account.
This will open a browser to the Spotify authorization page.
After authorizing, copy the code from the redirect URL."
  (interactive)
  (spot-check-credentials)
  (browse-url (spot--auth-url-full))
  (let ((spot-auth-code (read-string "Enter code from URL: ")))
    (spot-request-async
     :method "POST"
     :url spot-token-url
     :q-params (concat "?grant_type=" "authorization_code"
                       "&redirect_uri=" spot-redirect-uri
                       "&code=" spot-auth-code)
     :callback (lambda (response)
                 (let ((json (json-read-from-string response)))
                   (setq
                    spot-access-token (alist-get-chain '(access_token) json)
                    spot-refresh-token (alist-get-chain '(refresh_token) json)))
                 (message "Refreshed spot access token and refresh token"))
     :extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
                      ("Content-Length" . "0")
                      ("Authorization" . ,(concat "Basic " (spot--b64-id-secret)))))))

;;;###autoload
(defun spot-refresh ()
  "Refresh the access token using the stored refresh token."
  (interactive)
  (spot-check-credentials)
  (spot-request-async
   :method "POST"
   :url spot-token-url
   :q-params (concat "?grant_type=" "refresh_token"
                     "&refresh_token=" spot-refresh-token)
   :callback (lambda (response)
               (setq
                spot-access-token
                (alist-get-chain '(access_token) (json-read-from-string response)))
               (message "Refreshed spot access token"))
   :extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
                    ("Content-Length" . "0")
                    ("Authorization" . ,(concat "Basic " (spot--b64-id-secret))))))

(provide 'spot-auth)

;;; spot-auth.el ends here
