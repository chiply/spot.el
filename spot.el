;;; spot.el --- Spotify client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ht "2.3") (dash "2.19.1") (consult "1.0") (marginalia "1.0") (embark "0.23"))
;; Keywords: multimedia, spotify

;;; Commentary:

;; Spot is a Spotify client for Emacs that integrates with the modern
;; completion ecosystem (consult, marginalia, embark).
;;
;; Features:
;; - Search for tracks, albums, artists, playlists, shows, episodes, audiobooks
;; - Rich annotations via marginalia
;; - Context actions via embark
;; - Mode line display of currently playing track
;; - Playback control (play, pause, next, previous)
;;
;; Getting Started:
;; 1. Create a Spotify application at https://developer.spotify.com/dashboard
;; 2. Set environment variables:
;;    export SPOTIFY_CLIENT_ID="your-client-id"
;;    export SPOTIFY_CLIENT_SECRET="your-client-secret"
;; 3. Run `M-x spot-authorize` to authenticate
;; 4. Use `M-x spot-consult-search` to search Spotify
;;
;; Optional: For consult-omni integration, load spot-consult-omni.el separately:
;;   (require 'spot-consult-omni)

;;; Code:

;; Core requires
(require 'spot-var)
(require 'spot-util)
(require 'spot-auth)
(require 'spot-generic-query)
(require 'spot-search)
(require 'spot-consult)
(require 'spot-marginalia)
(require 'spot-embark)
(require 'spot-generic-action)
(require 'spot-mode-line)

;; Optional: consult-omni integration
;; Users should load this separately if they have consult-omni installed:
;; (require 'spot-consult-omni)

(provide 'spot)

;;; spot.el ends here
