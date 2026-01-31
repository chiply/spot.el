;;; spot-embark.el --- Embark actions for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Embark context actions for the spot Spotify client.
;; Provides actions for playing, browsing, and managing Spotify items.

;;; Code:

(require 'ht)
(require 'embark)

(require 'spot-mode-line)
(require 'spot-consult)
(require 'spot-var)
(require 'spot-generic-query)

(defun spot-action--generic-show-data (item)
  "Show raw data for ITEM in a buffer."
  (let ((buf (get-buffer-create "*spotify-search-result*")))
    (with-current-buffer buf
      (erase-buffer)
      (if (fboundp 'yaml-encode)
          (progn
            (insert (yaml-encode (get-text-property 0 'multi-data item)))
            (when (fboundp 'yaml-mode) (yaml-mode)))
        ;; Fallback to pp (pretty-print)
        (pp (get-text-property 0 'multi-data item) (current-buffer))))
    (switch-to-buffer buf)
    (goto-char (point-min))))

(defun spot-action--list-album-tracks (item)
  "List tracks for album ITEM."
  (let* ((table (get-text-property 0 'multi-data item))
         (album-name (ht-get* table 'name))
         (artist-name (ht-get* (nth 0 (ht-get* table 'artists)) 'name)))
    (spot-consult-search
     (concat
      "album:" album-name
      " "
      "artist:" artist-name " -- --type=track"))))

(defun spot-action--list-artist-tracks (item)
  "List tracks for artist ITEM."
  (let* ((table (get-text-property 0 'multi-data item))
         (artist-name (ht-get* table 'name)))
    (spot-consult-search
     (concat "artist:" artist-name " -- --type=track"))))

(defun spot-action--list-playlist-tracks (item)
  "List tracks in playlist ITEM."
  (let* ((table (get-text-property 0 'multi-data item))
         (playlist-id (ht-get* table 'id)))
    (setq spot--selected-playlist-id playlist-id)
    (spot-consult-search-playlist-tracks)))

(defun spot-action--generic-play-uri (item)
  "Play ITEM on Spotify."
  (let* ((table (get-text-property 0 'multi-data item))
         (type (ht-get table 'type))
         (offset (cond
                  ((string= type "track") `(("uri" . ,(ht-get* table 'uri))))
                  ((string= type "playlist") '(("position" . 0)))
                  ((string= type "album") '(("position" . 0)))
                  ((string= type "artist") nil)))
         (context_uri (cond
                       ((string= type "track") (ht-get* table 'album 'uri))
                       ((string= type "playlist") (ht-get* table 'uri))
                       ((string= type "album") (ht-get* table 'uri))
                       ((string= type "artist") (ht-get* table 'uri))))
         (json (list))
         (_ (when context_uri (push (cons 'context_uri context_uri) json)))
         (_ (when offset (push (cons 'offset offset) json)))
         (json (json-encode json)))
    (spot-request-async
     :method "PUT"
     :url spot-player-play-url
     :q-params (spot--base-q-params)
     :callback (lambda (_) (run-with-timer 2.0 nil 'spot--update-modeline-lighters))
     :data json)))

(defun spot-action--add-track-to-playlist (item)
  "Add track ITEM to a playlist."
  (let* ((playlist (or spot--playlist-selected
                       (progn
                         (setq spot--playlist-selected (car (spot-consult-search-current-user-playlists)))
                         spot--playlist-selected)))
         (playlist-id (ht-get* (get-text-property 0 'multi-data playlist) 'id))
         (track-uri (or
                     ;; regular track
                     (ht-get* (get-text-property 0 'multi-data item) 'uri)
                     ;; from currently playing
                     (ht-get* (get-text-property 0 'multi-data item) 'item 'uri)))
         (json (format "{\"uris\":[\"%s\"]}" track-uri))
         (url (format
               "https://api.spotify.com/v1/playlists/%s/tracks"
               playlist-id)))
    (spot-request-async
     :method "POST"
     :url url
     :q-params (spot--base-q-params)
     :data json)))

;; keymaps
(defvar-keymap spot-embark-artist-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri
  "t" #'spot-action--list-artist-tracks)

(defvar-keymap spot-embark-album-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri
  "t" #'spot-action--list-album-tracks)

(defvar-keymap spot-embark-playlist-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri
  "t" #'spot-action--list-playlist-tracks)

(defvar-keymap spot-embark-track-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri
  "+" #'spot-action--add-track-to-playlist)

(defvar-keymap spot-embark-show-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri)

(defvar-keymap spot-embark-episode-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri)

(defvar-keymap spot-embark-audiobook-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri)

(defvar-keymap spot-embark-current-user-playlists-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri)

(defvar-keymap spot-embark-playlist-tracks-keymap
  :parent embark-general-map
  "s" #'spot-action--generic-show-data
  "P" #'spot-action--generic-play-uri)

;; Register keymaps with embark
(add-to-list 'embark-keymap-alist '(album . spot-embark-album-keymap))
(add-to-list 'embark-keymap-alist '(artist . spot-embark-artist-keymap))
(add-to-list 'embark-keymap-alist '(playlist . spot-embark-playlist-keymap))
(add-to-list 'embark-keymap-alist '(track . spot-embark-track-keymap))
(add-to-list 'embark-keymap-alist '(show . spot-embark-show-keymap))
(add-to-list 'embark-keymap-alist '(episode . spot-embark-episode-keymap))
(add-to-list 'embark-keymap-alist '(audiobook . spot-embark-audiobook-keymap))
(add-to-list 'embark-keymap-alist '(current-user-playlists . spot-embark-current-user-playlists-keymap))
(add-to-list 'embark-keymap-alist '(playlist-tracks . spot-embark-playlist-tracks-keymap))

(provide 'spot-embark)

;;; spot-embark.el ends here
