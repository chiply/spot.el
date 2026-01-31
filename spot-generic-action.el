;;; spot-generic-action.el --- Player actions for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Player control actions for the spot Spotify client.
;; Provides play, pause, next, and previous track commands.

;;; Code:

(require 'ht)

(require 'spot-mode-line)
(require 'spot-var)
(require 'spot-generic-query)
(require 'spot-util)

;;;###autoload
(defun spot-add-current-track-to-playlist ()
  "Add the currently playing track to a playlist."
  (interactive)
  (setq spot--playlist-selected nil)
  (let* ((current (spot--alist-to-ht (spot--currently-playing)))
         (track-uri (propertize
                     (ht-get* current 'item 'uri)
                     'category 'track
                     'multi-data current)))
    (if track-uri
        (spot-action--add-track-to-playlist track-uri)
      (message "No track currently playing.")))
  (setq spot--playlist-selected nil))

(defun spot--player-action (action)
  "Perform player ACTION (play, pause, next, previous)."
  (spot-request-async
   :method (cond
            ((member action '("play" "pause")) "PUT")
            ((member action '("next" "previous")) "POST"))
   :url (cond
         ((string= action "play") (format "%s/play" spot-player-url))
         ((string= action "next") (format "%s/next" spot-player-url))
         ((string= action "previous") (format "%s/previous" spot-player-url))
         ((string= action "pause") (format "%s/pause" spot-player-url)))
   :q-params (spot--base-q-params)
   :callback (when (member action '("play" "next" "previous"))
               (lambda (_) (run-with-timer 2.0 nil 'spot--update-modeline-lighters)))
   :extra-headers `(("Content-Length" . "0"))))

;;;###autoload
(defun spot-player-play ()
  "Resume playback."
  (interactive)
  (spot--player-action "play"))

;;;###autoload
(defun spot-player-pause ()
  "Pause playback."
  (interactive)
  (spot--player-action "pause"))

;;;###autoload
(defun spot-player-next ()
  "Skip to next track."
  (interactive)
  (spot--player-action "next"))

;;;###autoload
(defun spot-player-previous ()
  "Skip to previous track."
  (interactive)
  (spot--player-action "previous"))

(provide 'spot-generic-action)

;;; spot-generic-action.el ends here
