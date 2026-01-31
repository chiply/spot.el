;;; spot-util.el --- Utility functions for spot -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/spot.el
;; Version: 0.1.0
;; Keywords: multimedia, spotify

;;; Commentary:
;; Utility functions for the spot Spotify client.
;; Provides alist manipulation, hash table conversion, and filtering utilities.

;;; Code:

(require 'ht)

(defun spot--alist-get-chain (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (spot--alist-get-chain (cdr symbols)
                       (assoc (car symbols) alist))
    (cdr alist)))

(defun spot--alist-to-ht (alist)
  "Convert ALIST (a JSON object) to a hash table, handling nested structures."
  (cond
   ;; Handle cons cells (alists)
   ((and (consp alist) (consp (car alist)))
    (ht-from-alist
     (mapcar (lambda (pair)
               (cons (car pair)
                     (spot--alist-to-ht (cdr pair))))
             alist)))
   ;; Handle arrays
   ((vectorp alist)
    (mapcar #'spot--alist-to-ht alist))
   ;; Return primitive values as-is
   (t alist)))

(defun spot--propertize-items (tables)
  "Propertize TABLES with category and multi-data properties."
  (-map
   (lambda (table)
     (propertize
      (ht-get table 'name)
      'category (intern (ht-get table 'type))
      'multi-data table))
   tables))

(defun spot--type-equals (cand type)
  "Check if candidate CAND has TYPE."
  (string= (ht-get (get-text-property 0 'multi-data cand) 'type) type))

(defun spot--filter (candidates type)
  "Filter CANDIDATES by TYPE."
  (-filter (lambda (cand) (spot--type-equals cand type)) candidates))

(provide 'spot-util)

;;; spot-util.el ends here
