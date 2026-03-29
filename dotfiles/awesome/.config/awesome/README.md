  # AwesomeWM Konfiguration

  Mein modularer, umfangreicher AwesomeWM-Config-Stack.

  ## Features
  - Modularer Shell-Aufbau (windowing, workspaces, launchers, menu, notify, bar) mit Blueprint-
  Struktur
  - Eigener Theme- und Wallpaper-Controller mit Export nach `/.cache/awesome/theme-state.json`
  - Session State: Snapshot und Restore von Bildschirmen, Tags, Clients; Integration mit autorandr;
  Cache unter `/.cache/awesome/session-state.lua`
  - Bar mit Standard-Position unten, Reveal-Effekt, Tag-Tabs, Startmenü, Notification Indicator
  - Notification Center mit History, Aktionen und benutzerfreundlichem Verhalten
  - Launchers: Run-Action, Session-/Power-Overlay (Shutdown, Hibernate etc.)
  - Autostart für copyq, picom nach Tray-Ready-Signal

  ## Voraussetzungen
  - AwesomeWM 4.3+ mit Standard-Lua-Modulen (awful, gears, naughty)
  - Standard-Apps: kitty, emacsclient, firefox, nemo, gnome-calendar (konfigurierbar)
  - Optional: copyq, picom, rofi (für Start-Aktionen), autorandr (für Signals), apply-gtk-theme/
  apply-starship-theme-Skripte

  ## Installation / Start
  1. Repo nach `/.config/awesome` legen oder symlink setzen
  2. `rc.lua` als Einstiegspunkt verwendet `require("system").init()` mit Defaults
  3. Awesome neu laden (Mod4+Ctrl+r) oder neu starten
  4. Autostart wirkt erst nach Tray-Ready-Signal

  ## Konfiguration
  - Optionen in `system/config.

  - Keybindings in input/, editieren Shell-Aktionen unter shell.*
  - Workspaces konfigurierbar via tags, Layouts, Gaps, Maximale Clients
  - Wallpaper unter ui.wallpaper mit Rotation und Quellen
  - Launchers, Notifications, Autostart-Toggles beschrieben

  ## Ordnerstruktur

  - rc.lua: Einstiegspunkt
  - system/: Config-Merging, Autostart, Fehler, Session- & Theme-States
  - ui/: Themes (z.B. luna), Assets, Wallpaper-Controller
  - shell/: Bar, Workspaces, Windowing, Launchers, Menu, Notify
  - input/: Tastatur-Layouts und Keybindings (ohne Logik)
  - templates/: BLUEPRINT und Module-Skeletons zur schnellen Entwicklung

  ## Signale & State

  - Session-Zustand gespeichert in /.cache/awesome/session-state.lua, Restore bei Start und
    autorandr-Signal
  - Theme-Zustand exportiert nach /.cache/awesome/theme-state.json, löst apply-gtk-theme und apply-
    starship-theme aus
  - Wallpaper-Refresh bei autorandr::applied; Bar resync integriert
  - Logs für Autostart: /tmp/awesome-copyq.log, /tmp/awesome-picom.log, Theme-Apply-Logs

  ## Entwicklungstipps

  - Architektur-Blueprint in templates/BLUEPRINT.md dokumentiert
  - Neue Module mit Skeletons im templates/-Ordner starten
  - Modale Keybindings nur aktiv bei geöffnetem Overlay, saubere Deaktivierung beachten


  Passe Namen und Pfade je nach Bedarf an, das ist eine Entwicklungs- und Nutzungsgrundlage.

─ Worked for 2m 37s ─────────────────────────────────────────────────────────────────────────────────
• Hier ein kompletter README-Entwurf, der den aktuellen Stand und die Architektur widerspiegelt:

  # AwesomeWM Dotfiles

  Opinionated, modularer AwesomeWM-Desktop mit klar getrennten Bäumen für Windowing, Workspaces,
  Launcher, Menü, Notifications und Bar.

  ## Features
  - Modularer Shell-Aufbau laut `templates/BLUEPRINT.md`; Input ist entkoppelt und nutzt nur
  öffentliche Branches.
  - Eigener Theme-/Wallpaper-Stack inkl. Export nach `~/.cache/awesome/theme-state.json` und Hooks
  für `apply-gtk-theme`/`apply-starship-theme`.
  - Session-State-Snapshots (Screens/Tags/Clients) mit Restore beim Start und nach `autorandr`;
  Speicherung unter `~/.cache/awesome/session-state.lua`.
  - Bar (standard: unten) mit Reveal, Tabs/Tags, Start-Button, Clock, Notify-Indicator und Systray.
  - Notification-Center mit History, Aktionen und Center-Overlay.
  - Launcher: Run (drun/optional rofi), Session/Power/Logoff-Overlay mit Sleep/Hibernate/Power-Off.
  - Autostart-Hooks für CopyQ und Picom, ausgelöst sobald der Tray bereit ist.

  ## Voraussetzungen
  - AwesomeWM ≥ 4.3 mit Standard-Libs (`awful`, `gears`, `naughty`).
  - Default-Apps (anpassbar): `kitty`, `emacsclient -c -a ''`, `firefox`, `nemo`, `gnome-calendar`.
  - Optional: `copyq`, `picom`, `rofi` (für Start-Action `rofi`), `autorandr`, `apply-gtk-theme`,
  `apply-starship-theme`.

  ## Installation
  1. Repo nach `~/.config/awesome` legen oder per Symlink verknüpfen.
  2. `rc.lua` ist der Einstieg und ruft `require("system").init()` auf.
  3. Awesome neu laden (`Mod4+Ctrl+r`) oder neu starten.

  ## Konfiguration
  - Alle Defaults stehen in `system/config.lua`.
  - Overrides im Einstieg setzen:

  ```lua
  -- rc.lua
  require("system").init({
    apps = { terminal = "alacritty" },
    ui = { theme = "luna" },
    bar = { position = "top", start = { action = "rofi" } },
    windowing = { fullscreen_dim = { enabled = false } },
  })

  - Wichtige Schalter: system.autostart.*, apps.*, input.modkey/Media/Screenshot, ui.wallpaper
    (Quellen, Regeln, Rotation), bar.*, tags.* (fixed/dynamic, Layouts, Gap), windowing.*
    (Titelbalken, Floating, Fokus, Fullscreen-Dimming), launchers.*, menu.*, notify.*.

  ## Struktur

  - rc.lua – Einstiegspunkt.
  - system/ – Config-Merge, Error-Hooks, Autostart, Session-/Theme-State.
  - ui/ – Themes (Standard: luna), Assets, Wallpaper-Controller.
  - shell/ – Windowing, Workspaces, Launcher, Menü, Notify, Bar (jeweils Policy/Runtime/UI/Behavior).  - input/ – Tastaturpfade; ruft nur öffentliche Shell-Branches.
  - templates/ – Architektur-Blueprint und Modul-Skeletons.

  ## Signals & State

  - Session-State: speichert auf ~/.cache/awesome/session-state.lua, reagiert auf autorandr::pre/
    applied und session::*.
  - Theme-State: Export nach ~/.cache/awesome/theme-state.json, löst theme::state_updated/
    theme::apply aus.
  - Wallpaper-Refresh und Bar-Resync nach autorandr::applied.
  - Autostart-Logs: /tmp/awesome-copyq.log, /tmp/awesome-picom.log; Theme-Hooks loggen nach /tmp/
    apply-*.log.

  ## Entwicklung

  - Architektur-Regeln in templates/BLUEPRINT.md beachten: nur öffentliche Branches nutzen, modale
    Inputs an Overlay-Lebenszyklus hängen, Cleanup sicherstellen.
  - Neue Module über templates/*_skeleton.lua anlegen; Layer separat ändern (Input vs. Verhalten
    vs. UI).


  Passe die Beispiele (Apps, Theme, Bar-Position) nach Bedarf an.
