;;; spot-marginalia.el --- Marginalia annotations for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Marginalia annotation functions for the spot Spotify client.
;; Provides rich annotations for albums, artists, tracks, and other Spotify items.

;;; Code:

(require 'ht)

;; Declare marginalia variable to avoid compile warnings
;; The actual require happens when user loads marginalia
(defvar marginalia-annotator-registry)

(defun spot--annotate-album (album)
  "Annotate ALBUM with metadata."
  (concat
   " <- "
   (mapconcat
    'identity
    `(,(ht-get* (get-text-property 0 'multi-data album) 'name)
      ,(ht-get* (nth 0 (ht-get* (get-text-property 0 'multi-data album) 'artists)) 'name)
      ,(ht-get* (get-text-property 0 'multi-data album) 'release_date)
      ,(number-to-string (ht-get* (get-text-property 0 'multi-data album) 'total_tracks)))
    " || ")))

(defun spot--annotate-artist (artist)
  "Annotate ARTIST with metadata."
  (concat
   " <- "
   (mapconcat
    'identity
    `(,(ht-get* (get-text-property 0 'multi-data artist) 'name)
      ,(number-to-string (ht-get* (get-text-property 0 'multi-data artist) 'popularity))
      ,(number-to-string (ht-get* (get-text-property 0 'multi-data artist) 'followers 'total)))
    " || ")))

(defun spot--round-to-two-decimals (num)
  "Round NUM to two decimal places."
  (/ (round (* num 100)) 100.0))

(defun spot--annotate-track (track)
  "Annotate TRACK with metadata."
  (concat
   " <- "
   (mapconcat
    'identity
    `(,(ht-get* (get-text-property 0 'multi-data track) 'name)
      ,(number-to-string (ht-get* (get-text-property 0 'multi-data track) 'track_number))
      ,(ht-get* (nth 0 (ht-get* (get-text-property 0 'multi-data track) 'artists)) 'name)
      ,(number-to-string (spot--round-to-two-decimals
                          (/
                           (ht-get* (get-text-property 0 'multi-data track) 'duration_ms)
                           60000.0)))
      ,(ht-get* (get-text-property 0 'multi-data track) 'album 'name)
      ,(ht-get* (get-text-property 0 'multi-data track) 'album 'album_type)
      ,(ht-get* (get-text-property 0 'multi-data track) 'album 'release_date))
    " || ")))

(defun spot--annotate-playlist (playlist)
  "Annotate PLAYLIST with metadata."
  (concat
   " <- "
   (mapconcat
    'identity
    `(,(ht-get* (get-text-property 0 'multi-data playlist) 'name)
      ,(number-to-string (ht-get* (get-text-property 0 'multi-data playlist) 'tracks 'total)))
    " || ")))

(defun spot--annotate-show (show)
  "Annotate SHOW with metadata."
  (concat
   " <- "
   (mapconcat
    'identity
    `(,(ht-get* (get-text-property 0 'multi-data show) 'name)
      ,(ht-get* (get-text-property 0 'multi-data show) 'publisher)
      ,(ht-get* (get-text-property 0 'multi-data show) 'media_type)
      ,(number-to-string (ht-get* (get-text-property 0 'multi-data show) 'total_episodes))
      ,(ht-get* (get-text-property 0 'multi-data show) 'description))
    " || ")))

(defun spot--annotate-episode (episode)
  "Annotate EPISODE with metadata."
  (concat
   " <- "
   (mapconcat
    'identity
    `(,(ht-get* (get-text-property 0 'multi-data episode) 'name)
      ,(ht-get* (get-text-property 0 'multi-data episode) 'release_date)
      ,(ht-get* (get-text-property 0 'multi-data episode) 'description)
      ,(number-to-string (spot--round-to-two-decimals
                          (/
                           (ht-get* (get-text-property 0 'multi-data episode) 'duration_ms)
                           60000.0))))
    " || ")))

(defun spot--annotate-audiobook (audiobook)
  "Annotate AUDIOBOOK with metadata."
  (concat
   " <- "
   (mapconcat
    'identity
    `(,(ht-get* (get-text-property 0 'multi-data audiobook) 'name)
      ,(ht-get* (get-text-property 0 'multi-data audiobook) 'publisher)
      ,(ht-get* (nth 0 (ht-get* (get-text-property 0 'multi-data audiobook) 'narrators)) 'name)
      ,(ht-get* (nth 0 (ht-get* (get-text-property 0 'multi-data audiobook) 'authors)) 'name)
      ,(string-replace "\n" " " (ht-get* (get-text-property 0 'multi-data audiobook) 'description)))
    " || ")))

;; Register annotators after marginalia is loaded
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry '(album spot--annotate-album))
  (add-to-list 'marginalia-annotator-registry '(artist spot--annotate-artist))
  (add-to-list 'marginalia-annotator-registry '(playlist spot--annotate-playlist))
  (add-to-list 'marginalia-annotator-registry '(track spot--annotate-track))
  (add-to-list 'marginalia-annotator-registry '(show spot--annotate-show))
  (add-to-list 'marginalia-annotator-registry '(episode spot--annotate-episode))
  (add-to-list 'marginalia-annotator-registry '(audiobook spot--annotate-audiobook)))

(provide 'spot-marginalia)

;;; spot-marginalia.el ends here
