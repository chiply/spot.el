;;; spot-mode-line.el --- Mode line display for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Mode line integration for the spot Spotify client.
;; Shows currently playing track information in the mode line.

;;; Code:

(require 'ht)

(require 'spot-generic-query)
(require 'spot-util)

(defvar spot--modeline-track nil
  "Currently playing track name.")

(defvar spot--modeline-artist nil
  "Currently playing artist name.")

(defvar spot--modeline-album nil
  "Currently playing album name.")

(defvar spot--modeline-repeat-state nil
  "Current repeat state.")

(defvar spot--modeline-release-date nil
  "Release date of currently playing track.")

(defvar spot--modeline-shuffle-state nil
  "Current shuffle state.")

(defvar spot--modeline-smart-shuffle nil
  "Current smart shuffle state.")

(defvar spot--timer-started nil
  "Whether the update timer has been started.")

(defvar spot--update-timers '()
  "List of active update timers.")

(defcustom spot--update-interval 30
  "Interval in seconds between mode line updates."
  :type 'integer
  :group 'spot)

(defcustom spot-mode-line-foreground "#1db954"
  "Foreground color for mode line display.
Default is the official Spotify green."
  :type 'color
  :group 'spot)

(defun spot--update-modeline-lighters ()
  "Update mode line lighter variables from currently playing."
  (let* ((current (spot--alist-to-ht (spot--currently-playing))))
    (when current
      (setq spot--modeline-track
            (ht-get* current 'item 'name))
      (setq spot--modeline-artist
            (ht-get* (nth 0 (ht-get* current 'item 'artists)) 'name))
      (setq spot--modeline-album
            (ht-get* current 'item 'album 'name))
      (setq spot--modeline-repeat-state
            (ht-get* current 'repeat_state))
      (setq spot--modeline-release-date
            (ht-get* current 'item 'album 'release_date))
      (setq spot--modeline-shuffle-state
            (ht-get* current 'shuffle_state))
      (setq spot--modeline-smart-shuffle
            (ht-get* current 'smart_shuffle)))
    (when (not current)
      (setq spot--modeline-track nil)
      (setq spot--modeline-artist nil)
      (setq spot--modeline-album nil)
      (setq spot--modeline-repeat-state nil)
      (setq spot--modeline-release-date nil)
      (setq spot--modeline-shuffle-state nil)
      (setq spot--modeline-smart-shuffle nil))))

(defun spot--check-for-modeline-update ()
  "Check if mode line needs updating and schedule next update."
  (spot--update-modeline-lighters)
  (when-let* ((current (spot--alist-to-ht (spot--currently-playing)))
              (progress (ht-get current 'progress_ms))
              (duration (ht-get* current 'item 'duration_ms))
              (remaining (- duration progress))
              (delay (max 0 remaining)))
    (mapc (lambda (timer) (cancel-timer timer)) spot--update-timers)
    ;; Updates at the estimated time when the track changes
    (push (run-with-timer
           (+ (/ delay 1000.0) 1.0) nil
           'spot--update-modeline-lighters)
          spot--update-timers)))

(defun spot--start-update-timer ()
  "Start the mode line update timer."
  (when (not spot--timer-started)
    (run-with-timer
     0 spot--update-interval
     'spot--check-for-modeline-update)
    (setq spot--timer-started t)))

(defun spot-mode-line-string ()
  "Return a string for mode line display."
  (let* ((lighters `(,spot--modeline-track
                     ,spot--modeline-artist
                     ,spot--modeline-album
                     ,spot--modeline-release-date
                     ,(cond
                       ((equal spot--modeline-repeat-state "off") nil)
                       ((equal spot--modeline-repeat-state nil) nil)
                       (t "repeat"))
                     ,(or
                       (cond
                        ((equal :json-false spot--modeline-smart-shuffle) nil)
                        ((equal t spot--modeline-smart-shuffle) "smart shuffle")
                        ((equal nil spot--modeline-smart-shuffle) nil))
                       (cond
                        ((equal :json-false spot--modeline-shuffle-state) nil)
                        ((equal t spot--modeline-shuffle-state) "shuffle")
                        ((equal nil spot--modeline-shuffle-state) nil)))))
         (lighters (remove nil lighters)))
    (propertize
     (if lighters (mapconcat 'identity lighters " * ") "*")
     'face `(:foreground ,spot-mode-line-foreground))))

(provide 'spot-mode-line)

;;; spot-mode-line.el ends here
