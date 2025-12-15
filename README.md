# Cyclistic_analyse
# Cyclistic Bike-Share Case Study: From Casual Rider to Annual Member

## 1. Executive Summary & Status
**Status:** Abgeschlossen (Ask, Prepare, Process, Analyze, Share, Act)

Dieses Data-Analytics-Projekt untersuchte das Fahrverhalten von über 6 Millionen Cyclistic-Nutzern über einen Zeitraum von einem Jahr. Die zentrale Fragestellung war, wie **Gelegenheitsfahrer (Casual Riders)** zu **Jahresmitgliedern (Annual Members)** konvertiert werden können.

Die Analyse bewies klare Musterunterschiede: Casual Riders fahren **signifikant länger**, primär an **Wochenenden** und an **touristischen Standorten**. Die Top-3-Empfehlungen fokussieren daher auf flexible Wochenend-Abonnements und gezieltes Geo-Marketing.

---

## 2. Key Deliverables

| Deliverable | Beschreibung | Link |
| :--- | :--- | :--- |
| **Executive Presentation** | Die 7-Folien-Präsentation mit den Top-3-Empfehlungen. | [Link 'https://docs.google.com/presentation/d/1pmui61S5Jp4RmNS4sD1MZXfCPIc8xLXrA0opm4mqv-I/edit?usp=sharing' ] |
| **R Analysis Script** | Der vollständige R-Code für Bereinigung, Analyse und Visualisierung. | [Link zur Datei 'https://github.com/Bekzod-coder-1/Cyclistic_analyse/blob/main/Skripte_Process.R' in diesem Repo] |
| **Clean Data Export** | Die finale CSV/RDS-Datei der bereinigten Fahrten. | [Link zur Datei 'Cyclistic_analyse/data_processed/all_trips_clean.rds' oder 'summary_user_stats.csv' , 'summary_day_of_week_stats.csv' and 'summary_top_stations.csv'] |

---

## 3. Technical Stack (Tools & Libraries)

* **Programmiersprache:** R
* **Datenverarbeitung:** Tidyverse (dplyr, tidyr, forcats)
* **Visualisierung:** ggplot2
* **Datenquelle:** Cyclistic Historical Trip Data (12 Monate)
* **Präsentation:** Google Slides

---

## 4. Case Study Process

### 🅰️ Ask (Fragestellung)
Wie unterschiedlich nutzen die Gelegenheitsfahrer und Jahresmitglieder
Wie können Gelegenheitsfahrer in Jahresmitglieder umgewandelt werden, um das Umsatzwachstum zu maximieren? Das Nutzungsverhalten beider Gruppen muss analysiert werden, um eine datengestützte Marketingstrategie zu entwickeln.

### 🅱️ Prepare & Process (Datenvorbereitung)
* **Datenvolumen:** ~6.5 Millionen Rohfahrten.
* **Bereinigung:** Daten wurden in R konsolidiert und bereinigt. Fahrten mit einer Dauer von $\le 0$ Minuten oder über 24 Stunden wurden entfernt.
* **Endgröße:** \[791264\] saubere Fahrten.
* **Zusätzliche Felder:** `ride_length` (in Minuten) und `day_of_week` wurden berechnet.

### 🅲 Analyze & Share (Ergebnisse der Analyse)
Die Analyse ergab drei Schlüsselunterschiede im Verhalten, die das Muster Pendler vs. Freizeit belegen:

#### 📊 Finding 1: Median Ride Duration (Dauer)
Casual Riders fahren **signifikant länger** pro Fahrt. Die Analyse des Medians eliminiert Ausreißer und beweist, dass Gelegenheitsfahrer Fahrräder für längere Entdeckungstouren nutzen.
> *\[<img width="2400" height="1500" alt="plot_median_duration" src="https://github.com/user-attachments/assets/cca5f226-815c-49b3-ba89-bb7603b54da1" />
/]*

#### 🗓️ Finding 2: Day of Week Pattern (Zeitpunkt)
Mitglieder nutzen das System hauptsächlich an Wochentagen (Pendlerverhalten), während Gelegenheitsfahrer am **Wochenende** die höchsten Nutzungszahlen aufweisen.
> *\[<img width="3000" height="1800" alt="plot_day_pattern" src="https://github.com/user-attachments/assets/f6bdecb7-9674-4617-b904-30fa793bb249" />
/]*

#### 🗺️ Finding 3: Top Station Locations (Ort)
Die Top 10 Startstationen der Casual Riders liegen konsistent an **touristischen Orten** (Parks, Seen), im Gegensatz zu den Member-Stationen, die sich in Transit- und Geschäftszentren befinden.
> *\[<img width="3600" height="2100" alt="plot_top_stations" src="https://github.com/user-attachments/assets/77405d22-9130-4516-9901-69dff7965cb0" />
/]*

### 5. Act (Top 3 Handlungsempfehlungen)

Basierend auf dem Freizeitprofil der Casual Riders lauten die Empfehlungen:

1.  **Flexible Weekend/Leisure Passes:** Einführung von Abonnements, die auf das Wochenend- und Langstreckennutzungsverhalten zugeschnitten sind.
2.  **Geotargeted Weekend Marketing:** Konzentration der Marketingbudgets an Samstagen und Sonntagen in der Nähe der Top-Casual-Stationen.
3.  **'Exploration' App Features:** Entwicklung von App-Funktionen, die Freizeitmöglichkeiten und Routen zu Sehenswürdigkeiten vorschlagen, um den Mehrwert des Systems für diese Zielgruppe zu steigern.

---
