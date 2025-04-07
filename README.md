# EFM-Trainer (Shiny App mit GPT-Anbindung)
[Projektbeschreibung anzeigen](Projektbeschreibung_EFM-Trainer.md)

## Vorbereitung
Öffne deine R-Konsole oder RStudio und führe aus:
install.packages(c("shiny", "googlesheets4", "httr", "jsonlite"))


## 🔐 Authentifizierung
Die Authentifizierung bei Google Sheets funktioniert über einen **Google Service Account**, verwaltet über:

[Google Cloud IAM](https://console.cloud.google.com/iam-admin/iam?project=able-hull-366008)

Um auf ein Google Sheet zuzugreifen, muss es **für diesen Service-Account** mit Bearbeitungsrechten freigegeben sein:

```
efm-714@able-hull-366008.iam.gserviceaccount.com
```

Im App-Ordner (z. B. `efm-app/`) befindet sich ein versteckter Ordner `.secrets/` mit der JSON-Key-Datei:

```
able-hull-366008-48e78f3bc110.json
```

In `global.R` befinden sich alle relevanten Authentifizierungsinformationen – inkl. der **Google Sheet ID**.

---

## 📁 Struktur der App

Die Shiny-App besteht aus drei Hauptdateien:

- `ui.R` – UI-Komponenten (auch z. T. in `global.R`)
- `global.R` – Initialisierung, Variablen, Authentifizierung
- `server.R` – Serverlogik

Als **Datenbank** dient ein Google Sheet.

---

## 📝 Protokollierung

Jeder Aufruf der App wird in eine JSON-Datei geschrieben:

```
users_data.json
```

Diese Datei liegt im App-Verzeichnis.

---

## ⚙️ Hauptfunktionen

### 🆕 Neue Aufgabe

Beim Klick auf den Button `Neue Aufgabe` wird folgende Funktion ausgelöst:

```r
observeEvent(input$newTaskButton, {...})
```

Diese ruft intern zwei Funktionen auf:

- `newTask <- eventReactive(input$newTaskButton, {...})`
- `newQuestionText()`

Je nach ausgewähltem Test wird:

- die Aufgabenstellung über `newTaskText()` generiert
- ein passender Test wie `doTtest()` durchgeführt
- das Ergebnis zurückgegeben

### 👁️ Lösung anzeigen

Beim Klick auf `Lösung anzeigen`:

```r
observeEvent(input$showSolutionButton, {...})
```

Falls eine Lösung angegeben wurde, zeigt die Funktion:

```r
showSolutionText <- eventReactive(input$showSolutionButton, {...})
```

die passende Lösung an – z. B. `getTtestSolution()`.

---

## 🤖 GPT-Anbindung

Die App kann auch ChatGPT nutzen über:

```r
ask_chatgpt <- function(prompt) {...}
```

Dabei wird die **OpenAI API** mit `"gpt-3.5-turbo"` verwendet.

---

## 🔢 Sonstige Hilfsfunktionen

- **`helperGetPercentage(value)`**  
  Konvertiert einen numerischen Wert in einen Prozent-String (z. B. `0.1234 → "12.34%"`)

- **`newScaleInfo()`**  
  Aktualisiert die Information zur Skalierung im UI.

---

## 📦 Setup & Start

1. `.secrets`-Ordner mit JSON-Key erstellen
2. `ui.R`, `global.R`, `server.R` ins gleiche Verzeichnis legen
3. Lokale R-Umgebung mit `shiny`, `googlesheets4`, `httr`, `jsonlite` usw. vorbereiten
4. App starten mit:

```r
shiny::runApp("efm-app")
```

---

## 🚫 GitHub Hinweise

In der `.gitignore` sollten stehen:

```gitignore
efm-app/.secrets/
*.json
.Rhistory
.RData
```

---
