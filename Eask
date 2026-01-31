(package "spot"
         "0.1.0"
         "Spotify client for Emacs")

(website-url "https://github.com/chiply/spot.el")
(keywords "multimedia" "spotify")

(package-file "spot.el")

;; List files explicitly to exclude optional spot-consult-omni.el
(files "spot.el"
       "spot-auth.el"
       "spot-consult.el"
       "spot-embark.el"
       "spot-generic-action.el"
       "spot-generic-query.el"
       "spot-marginalia.el"
       "spot-mode-line.el"
       "spot-search.el"
       "spot-util.el"
       "spot-var.el")

(script "test" "echo \"No tests yet\"")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")
(depends-on "ht" "2.3")
(depends-on "dash" "2.19.1")
(depends-on "consult" "1.0")
(depends-on "marginalia" "1.0")
(depends-on "embark" "0.23")

(development
 (depends-on "package-lint"))
