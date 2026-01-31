# spot.el

A Spotify client for Emacs that integrates with the modern completion ecosystem.

## Features

- **Search**: Search for tracks, albums, artists, playlists, shows, episodes, and audiobooks
- **Rich Annotations**: See metadata (artist, album, duration, popularity) via marginalia
- **Context Actions**: Play, browse, and manage items via embark
- **Mode Line**: Display currently playing track in the mode line
- **Playback Control**: Play, pause, skip next/previous from Emacs

## Requirements

- Emacs 28.1+
- [ht](https://github.com/Wilfred/ht.el) - Hash table library
- [dash](https://github.com/magnars/dash.el) - List manipulation
- [consult](https://github.com/minad/consult) - Completion framework
- [marginalia](https://github.com/minad/marginalia) - Annotations
- [embark](https://github.com/oantolin/embark) - Context actions

### Optional

- [consult-omni](https://github.com/armindarvish/consult-omni) - Alternative search interface (load `spot-consult-omni.el` separately)
- [yaml-mode](https://github.com/yoshiki/yaml-mode) - For viewing raw data (graceful fallback to `pp` if not available)

## Installation

### Prerequisites: Spotify API Credentials

1. Go to [Spotify Developer Dashboard](https://developer.spotify.com/dashboard)
2. Log in with your Spotify account
3. Click "Create app"
4. Fill in the app details:
   - App name: "Emacs Spot Client" (or any name you prefer)
   - Redirect URI: `https://spotify.com`
   - Select "Web API" under APIs used
5. After creating, go to Settings and note your Client ID and Client Secret

### Set Environment Variables

Add to your shell configuration (`~/.bashrc`, `~/.zshrc`, etc.):

```bash
export SPOTIFY_CLIENT_ID="your-client-id"
export SPOTIFY_CLIENT_SECRET="your-client-secret"
```

### With straight.el

```elisp
(straight-use-package
 '(spot :type git :host github :repo "chiply/spot.el"))
```

### With use-package and straight.el

```elisp
(use-package spot
  :straight (:host github :repo "chiply/spot.el")
  :commands (spot-consult-search spot-authorize)
  :config
  ;; Optional: start mode line updates
  (spot--start-update-timer))
```

### Manual Installation

Clone the repository:

```bash
git clone https://github.com/chiply/spot.el.git ~/.emacs.d/site-lisp/spot.el
```

Add to your init file:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/spot.el")
(require 'spot)
```

## Usage

### Authorization

First, authenticate with Spotify:

```
M-x spot-authorize
```

This will:
1. Open your browser to Spotify's authorization page
2. After you authorize, Spotify redirects to a URL containing a code
3. Copy the code from the URL (the part after `?code=`)
4. Paste it into the Emacs prompt

### Searching

```
M-x spot-consult-search
```

Start typing to search across all Spotify content types. Use narrow keys to filter:

| Key | Type |
|-----|------|
| `a` | Albums |
| `A` | Artists |
| `p` | Playlists |
| `t` | Tracks |
| `s` | Shows |
| `e` | Episodes |
| `b` | Audiobooks |

### Embark Actions

With a candidate selected, press your embark key (usually `C-.`) to see available actions:

| Key | Action |
|-----|--------|
| `P` | Play item |
| `s` | Show raw data |
| `t` | List tracks (for albums/artists/playlists) |
| `+` | Add track to playlist |

### Playback Control

- `M-x spot-player-play` - Resume playback
- `M-x spot-player-pause` - Pause playback
- `M-x spot-player-next` - Next track
- `M-x spot-player-previous` - Previous track

### Other Commands

- `M-x spot-consult-search-current-user-playlists` - Browse your playlists
- `M-x spot-add-current-track-to-playlist` - Add currently playing to a playlist
- `M-x spot-refresh` - Refresh access token

### Mode Line

To show currently playing track in your mode line:

```elisp
;; Start the update timer
(spot--start-update-timer)

;; Add to mode line
(setq-default mode-line-format
              (append mode-line-format '((:eval (spot-mode-line-string)))))
```

Customize the display color:

```elisp
(setq spot-mode-line-foreground "#1db954")  ; Spotify green (default)
```

## Configuration

```elisp
(use-package spot
  :straight (:host github :repo "chiply/spot.el")
  :commands (spot-consult-search spot-authorize)
  :custom
  ;; Request timeout in seconds
  (spot--request-timeout 10)
  ;; Mode line update interval in seconds
  (spot--update-interval 30)
  ;; Mode line color
  (spot-mode-line-foreground "#1db954")
  :config
  (spot--start-update-timer))
```

## Optional: consult-omni Integration

If you use [consult-omni](https://github.com/armindarvish/consult-omni), you can load the optional integration:

```elisp
(use-package spot-consult-omni
  :after (spot consult-omni)
  :straight nil
  :load-path "path/to/spot.el"
  :commands (consult-omni-spot-search))
```

## Troubleshooting

### "Spotify credentials not configured"

Ensure environment variables are set and Emacs was started after setting them:

```elisp
(getenv "SPOTIFY_CLIENT_ID")     ; Should return your client ID
(getenv "SPOTIFY_CLIENT_SECRET") ; Should return your client secret
```

### Token expired

Run `M-x spot-refresh` to refresh your access token.

### No playback device

Spotify requires an active playback device. Open Spotify on your phone, computer, or web player first.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

MIT License - see [LICENSE](LICENSE) for details.
