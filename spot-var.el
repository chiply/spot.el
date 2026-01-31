;;; spot-var.el --- Variables for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Variable definitions for the spot Spotify client.
;; This file contains all configuration variables, URLs, and state variables.

;;; Code:

(defgroup spot nil
  "Spotify client for Emacs."
  :group 'multimedia
  :prefix "spot-")

(defvar spot--playlist-selected nil
  "Currently selected playlist for operations.")

(defvar spot--selected-playlist-id nil
  "ID of the currently selected playlist.")

(defcustom spot-client-id (getenv "SPOTIFY_CLIENT_ID")
  "Spotify application client ID.
Obtain from https://developer.spotify.com/dashboard"
  :type 'string
  :group 'spot)

(defcustom spot-client-secret (getenv "SPOTIFY_CLIENT_SECRET")
  "Spotify application client secret.
Obtain from https://developer.spotify.com/dashboard"
  :type 'string
  :group 'spot)

(defun spot-check-credentials ()
  "Check if Spotify credentials are configured."
  (interactive)
  (unless (and spot-client-id spot-client-secret)
    (user-error "Spotify credentials not configured.  Set SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET environment variables")))

(defun spot--id-secret ()
  "Return concatenated client ID and secret."
  (concat spot-client-id ":" spot-client-secret))

(defun spot--b64-id-secret ()
  "Return base64 encoded client ID and secret."
  (base64-encode-string (spot--id-secret) t))

(defvar spot-redirect-uri (url-hexify-string "https://spotify.com")
  "Redirect URI for Spotify OAuth.")

(defun spot--auth-url-full ()
  "Return full authorization URL."
  (url-encode-url
   (concat
    "https://accounts.spotify.com/en/authorize"
    "?response_type=code&client_id=" spot-client-id
    "&redirect_uri=" spot-redirect-uri
    "&scope=" (concat "streaming "
                      "user-read-birthdate "
                      "user-read-email "
                      "user-read-private "
                      "user-read-playback-state "
                      "user-library-modify "
                      "user-library-read "
                      "user-modify-playback-state "
                      "user-follow-read "
                      "playlist-modify-public "
                      "playlist-modify-private "
                      "user-read-recently-played")
    "&show_dialog=" "true")))

(defvar spot-token-url "https://accounts.spotify.com/api/token"
  "URL for obtaining Spotify tokens.")

(defvar spot-search-url "https://api.spotify.com/v1/search"
  "URL for Spotify search API.")

(defvar spot-access-token nil
  "Current Spotify access token.")

(defvar spot-refresh-token nil
  "Current Spotify refresh token.")

(defcustom spot-wait-time 1.0
  "Wait time in seconds between player actions and status updates.
There are synchronicity issues when displaying currently-playing
after doing player action (i.e. next, previous), even when using
spot-message-currently-playing as a callback function."
  :type 'number
  :group 'spot)

(defvar spot-player-url "https://api.spotify.com/v1/me/player"
  "URL for Spotify player API.")

(defvar spot-player-play-url (concat spot-player-url "/play")
  "URL for Spotify player play endpoint.")

(defvar spot-currently-playing-url (concat spot-player-url "/currently-playing")
  "URL for Spotify currently playing endpoint.")

(defvar spot-categories-url "https://api.spotify.com/v1/browse/categories"
  "URL for Spotify browse categories.")

(defvar spot-browse-url "https://api.spotify.com/v1/browse"
  "URL for Spotify browse API.")

(defvar spot-playlist-url "https://api.spotify.com/v1/users/spotify/playlists"
  "URL for Spotify playlists.")

(defvar spot-new-releases-url "https://api.spotify.com/v1/browse/new-releases"
  "URL for Spotify new releases.")

(defvar spot-albums-url "https://api.spotify.com/v1/albums"
  "URL for Spotify albums API.")

(defvar spot-artist-url "https://api.spotify.com/v1/artists"
  "URL for Spotify artists API.")

(defvar spot-recommendations-url "https://api.spotify.com/v1/recommendations"
  "URL for Spotify recommendations API.")

(defvar spot-me-url "https://api.spotify.com/v1/me"
  "URL for Spotify user API.")

(defvar spot-users-url "https://api.spotify.com/v1/users"
  "URL for Spotify users API.")

(defvar spot-following-url "https://api.spotify.com/v1/me/following"
  "URL for Spotify following API.")

(defvar spot-tracks-url "https://api.spotify.com/v1/tracks/"
  "URL for Spotify tracks API.")

(defvar spot-recently-played-url "https://api.spotify.com/v1/me/player/recently-played"
  "URL for Spotify recently played API.")

(defcustom spot--request-timeout 10
  "Timeout in seconds for Spotify API requests."
  :type 'integer
  :group 'spot)

(defun spot--base-q-params ()
  "Return base query parameters with access token."
  (concat "?access_token=" spot-access-token))

(provide 'spot-var)

;;; spot-var.el ends here
