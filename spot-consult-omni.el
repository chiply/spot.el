;;; spot-consult-omni.el --- Consult-omni integration for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (consult-omni "0.1"))
;; Keywords: multimedia, spotify

;;; Commentary:
;; Optional consult-omni integration for the spot Spotify client.
;; This file is separate and should be loaded only if consult-omni is available.
;;
;; To use:
;;   (require 'spot-consult-omni)

;;; Code:

(require 'ht)
(require 'consult-omni)

(require 'spot-util)
(require 'spot-var)
(require 'spot-search)
(require 'spot-generic-query)

;;; Completion functions

(cl-defun spot--omni-request-album (query &rest args &key callback &allow-other-keys)
  "Request album results for QUERY.
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (progn (spot--search-cached-and-locked
                 query spot--mutex spot--cache)
                spot--candidates-album)))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-artist (query &rest args &key callback &allow-other-keys)
  "Request artist results for QUERY.
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (progn (spot--search-cached-and-locked
                 query spot--mutex spot--cache)
                spot--candidates-artist)))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-playlist (query &rest args &key callback &allow-other-keys)
  "Request playlist results for QUERY.
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (progn (spot--search-cached-and-locked
                 query spot--mutex spot--cache)
                spot--candidates-playlist)))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-track (query &rest args &key callback &allow-other-keys)
  "Request track results for QUERY.
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (progn (spot--search-cached-and-locked
                 query spot--mutex spot--cache)
                spot--candidates-track)))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-show (query &rest args &key callback &allow-other-keys)
  "Request show results for QUERY.
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (progn (spot--search-cached-and-locked
                 query spot--mutex spot--cache)
                spot--candidates-show)))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-episode (query &rest args &key callback &allow-other-keys)
  "Request episode results for QUERY.
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (progn (spot--search-cached-and-locked
                 query spot--mutex spot--cache)
                spot--candidates-episode)))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-audiobook (query &rest args &key callback &allow-other-keys)
  "Request audiobook results for QUERY.
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (progn (spot--search-cached-and-locked
                 query spot--mutex spot--cache)
                spot--candidates-audiobook)))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-current-user-playlists (_query &rest args &key callback &allow-other-keys)
  "Request current user's playlists (ignores QUERY).
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
         (spot--propertize-items
          (ht-get*
           (spot--alist-to-ht
            (spot-request
             :method "GET"
             :url (format "%s/playlists" spot-me-url)
             :q-params (spot--base-q-params)
             :parse-json t))
           'items))))
    (funcall callback annotated-results)
    annotated-results))

(cl-defun spot--omni-request-playlist-tracks (_query &rest args &key callback &allow-other-keys)
  "Request playlist tracks (ignores QUERY).
CALLBACK is called with results.  ARGS are additional arguments."
  (ignore args)
  (let ((annotated-results
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
            'items)))))
    (funcall callback annotated-results)
    annotated-results))

;;; consult-omni sources

(defun spot--omni-group-function (_sources cand _transform &optional _group-by)
  "Group function for consult-omni.
CAND is the candidate."
  (ht-get (get-text-property 0 'multi-data cand) 'type))

(consult-omni-define-source
 "artist"
 :narrow-char ?a :min-input 1 :category 'artist :require-match nil
 :type 'dynamic :request #'spot--omni-request-artist
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

(consult-omni-define-source
 "album"
 :narrow-char ?A :min-input 1 :category 'album :require-match nil
 :type 'dynamic :request #'spot--omni-request-album
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

(consult-omni-define-source
 "playlist"
 :narrow-char ?p :min-input 1 :category 'playlist :require-match nil
 :type 'dynamic :request #'spot--omni-request-playlist
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

(consult-omni-define-source
 "track"
 :narrow-char ?t :min-input 1 :category 'track :require-match nil
 :type 'dynamic :request #'spot--omni-request-track
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

(consult-omni-define-source
 "show"
 :narrow-char ?s :min-input 1 :category 'show :require-match nil
 :type 'dynamic :request #'spot--omni-request-show
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

(consult-omni-define-source
 "episode"
 :narrow-char ?e :min-input 1 :category 'episode :require-match nil
 :type 'dynamic :request #'spot--omni-request-episode
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

(consult-omni-define-source
 "audiobook"
 :narrow-char ?b :min-input 1 :category 'audiobook :require-match nil
 :type 'dynamic :request #'spot--omni-request-audiobook
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

(consult-omni-define-source
 "current-user-playlists"
 :narrow-char ?b :min-input 1 :category 'current-user-playlists :require-match nil
 :type 'dynamic :request #'spot--omni-request-current-user-playlists
 :group #'spot--omni-group-function
 :interactive consult-omni-intereactive-commands-type
 :enabled t)

;; Multi-source search
(defvar consult-omni-spot-sources
  '("artist" "album" "playlist" "track" "show" "episode" "audiobook")
  "List of consult-omni sources for Spotify search.")

;;;###autoload
(defun consult-omni-spot-search (&optional initial prompt sources no-callback &rest args)
  "Search Spotify with consult-omni.
INITIAL is the initial input, PROMPT is the prompt string,
SOURCES overrides default sources, NO-CALLBACK disables callbacks,
ARGS are additional arguments."
  (interactive "P")
  (let ((sources (or sources consult-omni-spot-sources)))
    (consult-omni-multi initial prompt sources no-callback 1 args)))

(provide 'spot-consult-omni)

;;; spot-consult-omni.el ends here
