;;; spot-consult.el --- Consult integration for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Consult-based completion interface for the spot Spotify client.
;; Provides multi-source search with async candidate fetching.

;;; Code:

(require 'consult)

(require 'spot-util)
(require 'spot-search)
(require 'spot-var)
(require 'spot-generic-query)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; completion functions
(defun spot--consult-completion-function-consult-album (query)
  "Return album candidates for QUERY."
  (spot--search-cached-and-locked query spot--mutex spot--cache)
  spot--candidates-album)

(defun spot--consult-completion-function-consult-artist (query)
  "Return artist candidates for QUERY."
  (spot--search-cached-and-locked query spot--mutex spot--cache)
  spot--candidates-artist)

(defun spot--consult-completion-function-consult-playlist (query)
  "Return playlist candidates for QUERY."
  (spot--search-cached-and-locked query spot--mutex spot--cache)
  spot--candidates-playlist)

(defun spot--consult-completion-function-consult-track (query)
  "Return track candidates for QUERY."
  (spot--search-cached-and-locked query spot--mutex spot--cache)
  spot--candidates-track)

(defun spot--consult-completion-function-consult-show (query)
  "Return show candidates for QUERY."
  (spot--search-cached-and-locked query spot--mutex spot--cache)
  spot--candidates-show)

(defun spot--consult-completion-function-consult-episode (query)
  "Return episode candidates for QUERY."
  (spot--search-cached-and-locked query spot--mutex spot--cache)
  spot--candidates-episode)

(defun spot--consult-completion-function-consult-audiobook (query)
  "Return audiobook candidates for QUERY."
  (spot--search-cached-and-locked query spot--mutex spot--cache)
  spot--candidates-audiobook)

;; histories
(defvar spot--history-sourceAlbum nil
  "History for album source.")

(defvar spot--history-sourceArtist nil
  "History for artist source.")

(defvar spot--history-sourcePlaylist nil
  "History for playlist source.")

(defvar spot--history-sourceTrack nil
  "History for track source.")

(defvar spot--history-sourceShow nil
  "History for show source.")

(defvar spot--history-sourceEpisode nil
  "History for episode source.")

(defvar spot--history-sourceAudiobook nil
  "History for audiobook source.")

;; sources
(defvar spot--consult-source-album
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-album
             :min-input 1)
    :name "spot--consult-source-album"
    :narrow ?a
    :category album
    :history spot--history-sourceAlbum)
  "Consult source for albums.")

(defvar spot--consult-source-artist
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-artist
             :min-input 1)
    :name "spot--consult-source-artist"
    :narrow ?A
    :category artist
    :history spot--history-sourceArtist)
  "Consult source for artists.")

(defvar spot--consult-source-playlist
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-playlist
             :min-input 1)
    :name "spot--consult-source-playlist"
    :narrow ?p
    :category playlist
    :history spot--history-sourcePlaylist)
  "Consult source for playlists.")

(defvar spot--consult-source-track
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-track
             :min-input 1)
    :name "spot--consult-source-track"
    :narrow ?t
    :category track
    :history spot--history-sourceTrack)
  "Consult source for tracks.")

(defvar spot--consult-source-show
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-show
             :min-input 1)
    :name "spot--consult-source-show"
    :narrow ?s
    :category show
    :history spot--history-sourceShow)
  "Consult source for shows.")

(defvar spot--consult-source-episode
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-episode
             :min-input 1)
    :name "spot--consult-source-episode"
    :narrow ?e
    :category episode
    :history spot--history-sourceEpisode)
  "Consult source for episodes.")

(defvar spot--consult-source-audiobook
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-audiobook
             :min-input 1)
    :name "spot--consult-source-audiobook"
    :narrow ?b
    :category audiobook
    :history spot--history-sourceAudiobook)
  "Consult source for audiobooks.")

(defvar spot-search-sources
  '(spot--consult-source-album spot--consult-source-artist
    spot--consult-source-playlist spot--consult-source-track
    spot--consult-source-show spot--consult-source-episode
    spot--consult-source-audiobook)
  "List of consult sources for Spotify search.")

;; multi
(defvar spot--consult-search-search-history nil
  "History for spot consult search.")

;;;###autoload
(defun spot-consult-search (&optional initial)
  "Search Spotify with consult multi-source.
INITIAL is the initial input string."
  (interactive)
  (consult--multi
   spot-search-sources
   :history '(:input spot--consult-search-search-history)
   :initial initial))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current user playlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spot--consult-completion-function-consult-current-user-playlists (_query)
  "Return current user's playlists (ignores QUERY)."
  (spot--propertize-items
   (ht-get*
    (spot--alist-to-ht
     (spot-request
      :method "GET"
      :url (format "%s/playlists" spot-me-url)
      :q-params (spot--base-q-params)
      :parse-json t))
    'items)))

(defvar spot--history-sourceCurrentUserPlaylists nil
  "History for current user playlists source.")

(defvar spot--consult-source-current-user-playlists
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-consult-current-user-playlists
             :min-input 0)
    :name "spot--consult-source-current-user-playlists"
    :narrow ?b
    :category current-user-playlists
    :history spot--history-sourceCurrentUserPlaylists)
  "Consult source for current user's playlists.")

(defvar spot--consult-search-current-user-playlists-history nil
  "History for current user playlists search.")

;;;###autoload
(defun spot-consult-search-current-user-playlists ()
  "List current user's playlists.
This doesn't actually query the backend to filter playlists,
rather it lists all playlists.  To get instant filtering in
Emacs, you can hit SPC and comma to filter the output."
  (interactive)
  (consult--multi
   '(spot--consult-source-current-user-playlists)
   :history '(:input spot--consult-search-current-user-playlists-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; playlist tracks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spot--consult-completion-function-playlist-tracks (_query)
  "Return tracks from selected playlist (ignores QUERY)."
  (spot--propertize-items
   (-map
    (lambda (x) (ht-get* x 'track))
    (ht-get*
     (spot--alist-to-ht
      (spot-request
       :method "GET"
       :url (format "%s/%s/tracks" spot-playlist-url spot--selected-playlist-id)
       :q-params (spot--base-q-params)
       :parse-json t))
     'items))))

(defvar spot--history-sourcePlaylistTracks nil
  "History for playlist tracks source.")

(defvar spot--consult-source-playlists-tracks
  `(:async ,(consult--dynamic-collection
             #'spot--consult-completion-function-playlist-tracks
             :min-input 0)
    :name "spot--consult-source-playlists-tracks"
    :narrow ?b
    :category playlist-tracks
    :history spot--history-sourcePlaylistTracks)
  "Consult source for playlist tracks.")

(defvar spot--consult-search-playlist-tracks-history nil
  "History for playlist tracks search.")

;;;###autoload
(defun spot-consult-search-playlist-tracks ()
  "List tracks in the selected playlist.
This doesn't actually query the backend to filter tracks,
rather it lists all tracks.  To get instant filtering in Emacs,
you can hit SPC and comma to filter the output."
  (interactive)
  (consult--multi
   '(spot--consult-source-playlists-tracks)
   :history '(:input spot--consult-search-playlist-tracks-history)))

(provide 'spot-consult)

;;; spot-consult.el ends here
