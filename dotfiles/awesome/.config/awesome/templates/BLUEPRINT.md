# AwesomeWM Blueprint

## Ziel

Diese Konfiguration soll modular, debugbar und erweiterbar bleiben.

Wichtigste Regel:
Neue Features sollen lokal ergänzt werden können, ohne Input, UI, Runtime und mehrere Shell-Bäume gleichzeitig anfassen zu müssen.

---

## Architekturprinzip

Die Shell besteht aus fachlichen Teilbäumen:

- `shell.windowing`
- `shell.workspaces`
- `shell.launchers`
- `shell.menu`
- `shell.notify`
- `shell.bar`

Zusätzliche Querschnittsbäume:

- `ui`
- `input`
- `system`

### Grundregel
Jeder Baum besitzt:
- interne Struktur
- optionale Runtime-Helfer
- klaren öffentlichen Export

Andere Bäume dürfen nur den öffentlichen Export konsumieren.

---

## Verantwortung der Hauptmodule

### `input/`
`input/` ist dumm.

Es:
- bindet Tasten
- ruft Signale oder öffentliche APIs auf
- konsumiert nur freigegebene Input-Branches

Es:
- verwaltet keine Overlay-Zustände
- kennt keine internen Leaves fremder Module
- baut keine Fremdlogik nach

### `shell.windowing`
Verantwortlich für:
- Client-Verhalten
- Fokuslogik
- Navigation
- Minimieren / Wiederherstellen
- Titel-/Fenster-bezogenes Verhalten

Öffentlicher Branch für `input/`:
- `shell.windowing.input.clients`
- `shell.windowing.input.screens`

### `shell.workspaces`
Verantwortlich für:
- Tags / Workspaces
- Layoutsteuerung
- Policies rund um Tags
- Workspace-Runtime

Öffentlicher Branch für `input/`:
- `shell.workspaces.input.tags`

### `shell.launchers`
Verantwortlich für:
- Run Launcher
- Session / Power / Logoff Launcher
- Overlay-Lebenszyklus der Launcher

Öffentlicher Branch für `input/`:
- `shell.launchers.input.open`

### `shell.notify`
Verantwortlich für:
- Notifications
- History
- Notify Center
- Center-Zustand
- modale Eingaben des Notify Centers

### `shell.menu`
Verantwortlich für:
- Startmenü
- Clientmenü
- Popup-Zustand des Menüs
- Outside-Close und modale Menüinteraktion

### `shell.bar`
Verantwortlich für:
- sichtbare Bar
- Widgets
- Bar-bezogene Darstellung
- keine Fremdlogik

---

## Export-Regeln

### Öffentliche Branches statt loser Einzelteile
Bevorzugt:

```lua
shell.windowing.input.clients
shell.windowing.input.screens
shell.workspaces.input.tags
shell.launchers.input.open
```

Nicht bevorzugt:
- viele lose Funktionen durch mehrere Ebenen reichen
- direkte Zugriffe auf interne Runtime-Module

### Alte Kompatibilität nur temporär

Wenn eine alte API parallel weiterläuft:
- explizit als temporär markieren
- Consumer schrittweise umstellen
- danach in eigenem Schritt entfernen

---

## Regeln für neue Module

Wenn ein neues Modul entsteht, vorab klären:

1. Wo lebt es fachlich?
2. Wer darf es aufrufen?
3. Welchen Zustand besitzt es?
4. Welche Signale oder Exporte bietet es an?
5. Gehört seine Interaktion in Input, UI oder Runtime?

### Faustregel

- Verhalten lebt im Fachmodul
- Input löst nur aus
- UI zeichnet nur
- Runtime hält Zustand
- Signale verbinden diese Ebenen

---

## Regeln für modale Eingaben

### Niemals dauerhaft global registrieren, wenn zustandsgebunden

Diese Tasten dürfen nicht als permanente globale Root-Keys für ein Overlay missbraucht werden:

- `Escape`
- `Return`
- `BackSpace`
- `Up`
- `Down`
- `Prior`
- `Next`

### Stattdessen

Modale Eingaben gehören an den echten Open/Close-Zustand des jeweiligen Overlays.

#### Beispiel

- Notify Center offen → Notify-Modalkeys aktiv
- Notify Center geschlossen → Notify-Modalkeys aus

- Session Launcher offen → Launcher-Modalkeys aktiv
- Session Launcher geschlossen → Launcher-Modalkeys aus

- Menü offen → Menü-spezifische Interaktion aktiv
- Menü geschlossen → Interaktion aus

---

## Regeln für Popups und Overlays

Jedes Overlay braucht klaren Lebenszyklus:

1. erstellen  
2. öffnen  
3. aktiv sein  
4. Eingabe annehmen  
5. schließen  
6. Cleanup  

### Cleanup muss zuverlässig sein

Beim Schließen müssen immer aufgeräumt werden:

- Keygrabber  
- Mousegrabber  
- root button patches  
- Signalhooks  
- gespeicherte Handles / Runtime-Zustände  

---

## Regeln für Refactors

### Nie gleichzeitig alles umbauen

Ein Refactor läuft in dieser Reihenfolge:

1. neuen Export hinzufügen  
2. Consumer umstellen  
3. testen  
4. alte Schnittstelle entfernen  

Nicht:

1. alte Schnittstelle löschen  
2. alles gleichzeitig anpassen  
3. hoffen  

### Nur einen Layer pro Schritt

Beispiele für getrennte Schritte:

- nur `input/`  
- nur `windowing` Export  
- nur `workspaces` Export  
- nur Popup-Lebenszyklus  
- nur Notify-Modalität  

---

## Debugging-Regeln

Wenn etwas bricht, immer erst die Kette bestimmen:

1. Kommt der Input an?  
2. Ruft der Consumer den richtigen Export auf?  
3. Existiert der Export?  
4. Wird das Signal verarbeitet?  
5. Ist der Zustand korrekt?  
6. Ist nur die UI kaputt oder auch das Verhalten?  

### Typische Fehlerklassen

- Consumer erwartet alten Shape  
- neuer Export existiert, aber Input nutzt noch Altpfad  
- globale statt modale Tasten  
- Cleanup läuft nicht auf allen Schließpfaden  
- UI-Popup verwaltet Zustände, die ins Fachmodul gehören  

---

## Regeln für Codex / externe Hilfe

Wenn externe Hilfe genutzt wird, vorher immer festhalten:

- welche Datei wirklich geändert werden darf  
- welche Architekturregel nicht verletzt werden darf  
- ob gerade Bugfix oder Refactor gemacht wird  
- welche bestehenden Verhaltensweisen erhalten bleiben müssen  

### Besonders wichtig

Nicht erlauben:

- globale Tastenfresser als "Fix"  
- versteckte Kompatibilität ohne Kennzeichnung  
- Umbau mehrerer Layer gleichzeitig  
- Zustandslogik ins falsche Modul verschieben  

---

## Checkliste vor jedem neuen Feature

- Ist das Feature fachlich richtig verortet?  
- Gibt es schon einen passenden öffentlichen Branch?  
- Muss ein neuer Export definiert werden?  
- Ist die Interaktion zustandsgebunden?  
- Braucht das Feature Cleanup?  
- Muss `input/` wirklich wissen, wie es intern funktioniert?  
- Kann ich das in einem Layer-Schritt bauen?  

---

## Checkliste nach jedem Umbau

- funktionieren Keybinds noch?  
- funktionieren Mauspfade noch?  
- funktionieren Menüs/Launcher/Notify noch über alle Öffnungswege?  
- bleiben Tasten nach Schließen frei?  
- gibt es noch Altpfade, die versehentlich benutzt werden?  
- ist der neue Branch wirklich der einzige Consumer-Pfad?  

---

## Langfristiges Ziel

Ein neues Modul soll sich so ergänzen lassen:

1. Modul fachlich einordnen  
2. öffentlichen Export definieren  
3. Consumer an genau einen Branch hängen  
4. testen  
5. fertig  

Wenn dafür mehrere Bäume gleichzeitig intern geöffnet werden müssen, ist die Struktur noch nicht sauber genug.
