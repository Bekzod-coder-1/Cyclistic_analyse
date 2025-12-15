#----------------------------------------------------- 
#Schritt 1:Installationen durchführen
#-----------------------------------------------------

install.packages(c("tidyverse", "lubridate", "janitor"))
# Laden die erforderlichen Bibliotheken für die Datenanalyse
library(tidyverse)
library(lubridate)
library(janitor)

#-----------------------------------------------------
# Schritt 2: Daten laden
# ----------------------------------------------------

# Lädt die CSV-Datei für Q1 2019.
# Die Pipe-Funktion (%>%) leitet den Data Frame sofort an die nächste Funktion weiter.
# 'clean_names()' aus janitor standardisiert alle Spaltennamen (z.B. entfernt Leerzeichen, macht alles kleingeschrieben).
df19 <- read_csv("data_original/Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv") %>% clean_names()

# Lädt die CSV-Datei für Q1 2020 und wendet ebenfalls 'clean_names()' an, 
# um Konsistenz bei den Spaltennamen sicherzustellen.
df20 <- read_csv("data_original/Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv") %>% clean_names()

# ----------------------------------------------------
# SCHRITT 3: DUPLIKATE ENTFERNEN
# ----------------------------------------------------

# Entfernt exakt identische, redundante Zeilen aus dem Data Frame df19,
# um doppelte Einträge zu vermeiden.
df19 <- df19 %>% distinct()
df20 <- df20 %>% distinct()

# ----------------------------------------------------
# SCHRITT 4: STRUKTUR UND KONSISTENZ PRÜFEN
# ----------------------------------------------------

# Zeigt alle Spaltennamen (Variablen) im Data Frame df19 an.
names(df19)
names(df20)

# Vergleicht die Spaltennamen beider Data Frames. 
# Zeigt Spaltennamen, die nur in df19, aber nicht in df20 existieren. (Wichtig für das Zusammenführen)
setdiff(names(df19), names(df20))
# Zeigt Spaltennamen, die nur in df20, aber nicht in df19 existieren. (Wichtig für das Zusammenführen)
setdiff(names(df20), names(df19))

# Zeigt die interne Struktur beider Data Frames an: 
# Anzahl der Beobachtungen/Variablen und den Datentyp jeder Spalte (z.B. character, numeric, POSIXct).
str(df19)
str(df20)


# Zeigt alle Objekte (Variablen, Data Frames, etc.) an, die aktuell im R-Arbeitsbereich gespeichert sind.
ls()

#----------------------------------------------------
# KORRIGIERTE 1. ANPASSUNG DER SPALTENNAMEN IN DF19
#----------------------------------------------------

df19_adjusted <- df19 %>% 
  
  # Benenne die alten Spaltennamen in df19 in die neuen, standardisierten Namen um
  rename(
    # Kritische Identifikations- und Zeitspalten
    ride_id = trip_id,             
    started_at = start_time,       
    ended_at = end_time,           
    rideable_type = bikeid,        
    
    # Stationsspalten ANPASSEN!
    start_station_id = from_station_id,   # Wichtig: From wird zu Start
    start_station_name = from_station_name, 
    end_station_id = to_station_id,       # Wichtig: To wird zu End
    end_station_name = to_station_name,   
    
    # Nutzer- und Dauer-Spalten
    member_casual = usertype,          
    trip_duration = tripduration       # 'tripduration' (alt) wird zu 'trip_duration' 
  )    
#----------------------------------------------------
# 2A. WERTETRANFORMATION (USWERTE ANPASSEN)
#----------------------------------------------------

df19_adjusted <- df19_adjusted %>%
  mutate(
    # Führe die Kategorien zusammen: 'Subscriber' -> 'member', 'Customer' -> 'casual'
    member_casual = recode(member_casual, 
                           "Subscriber" = "member", 
                           "Customer" = "casual")
  )
#----------------------------------------------------
# 2B. DATENTYPEN ANPASSEN (ZEITSTEMPEL)
#----------------------------------------------------

# Stelle sicher, dass beide Datensätze korrekte Datentypen haben
df19_adjusted <- df19_adjusted %>%
  mutate(
    started_at = as_datetime(started_at),
    ended_at = as_datetime(ended_at)
  )

df20_clean <- df20 %>% 
  mutate(
    started_at = as_datetime(started_at), 
    ended_at = as_datetime(ended_at)
  )
#----------------------------------------------------
# 3. METRIKEN HINZUFÜGEN
#----------------------------------------------------

# Berechne ride_length und day_of_week für DF19
df19_final <- df19_adjusted %>%
  mutate(
    ride_length = difftime(ended_at, started_at, units = "mins"), # Dauer in Minuten
    day_of_week = wday(started_at, label = TRUE) # Wochentag (z.B. "Mon", "Tue")
  )

# Berechne ride_length und day_of_week für DF20
df20_final <- df20_clean %>%
  mutate(
    ride_length = difftime(ended_at, started_at, units = "mins"),
    day_of_week = wday(started_at, label = TRUE)
  )

#----------------------------------------------------
# 1. BEHEBUNG DER DATENTYP-INKONSISTENZ (ride_id)
#----------------------------------------------------

# Stelle sicher, dass ride_id in df19_final als Text gespeichert ist
df19_final <- df19_final %>%
  mutate(ride_id = as.character(ride_id))

# Stelle sicher, dass ride_id in df20_final als Text gespeichert ist
# (Dies ist oft schon der Fall, schadet aber nicht, es zu bestätigen)
df20_final <- df20_final %>%
  mutate(ride_id = as.character(ride_id))

#----------------------------------------------------
# 1. DATENTYPEN KORRIGIEREN (ride_id und rideable_type)
#----------------------------------------------------

# KORREKTUR für DF19 (Der Fehler liegt hier oft bei älteren Daten)
df19_final <- df19_final %>%
  mutate(
    # ride_id zu Text umwandeln (falls zuvor numerisch war)
    ride_id = as.character(ride_id), 
    # rideable_type (Bike-ID) zu Text umwandeln (Lösung für den aktuellen Fehler!)
    rideable_type = as.character(rideable_type)
  )

# KORREKTUR für DF20 (Zur Sicherheit auch hier als Text definieren)
df20_final <- df20_final %>%
  mutate(
    ride_id = as.character(ride_id),
    rideable_type = as.character(rideable_type)
  )
# FÜHRE ERNEUT ZUSAMMEN
all_trips <- bind_rows(df19_final, df20_final)
# Überprüfung
glimpse(all_trips)

# Bereinigungprozess
# 1. Prüfe auf Fahrtdauer <= 0:
sum(all_trips$ride_length <= 0, na.rm = TRUE)

# 2. Prüfe auf extreme Ausreißer (z.B. > 24 Stunden, also 1440 Minuten(1 Tag))
summary(all_trips$ride_length) 

# Bereinigung der Fahrtdauer: Nur plausible Fahrten behalten
all_trips_clean <- all_trips %>%
  filter(ride_length > 0) %>%     # 1. Entfernt die 210 fehlerhaften Fahrten
  filter(ride_length <= 1440)     # 2. Entfernt Ausreißer (> 24 Stunden)

#----------------------------------------------------
# 2. DUPLIKATE ENTFERNEN
#----------------------------------------------------

# Prüfe zuerst, ob es Duplikate gibt (optional, aber gut für Doku)
sum(duplicated(all_trips_clean))

# Entferne exakt identische Zeilen
all_trips_clean <- all_trips_clean %>%
  distinct()

#----------------------------------------------------
# 3. FEHLWERTE (NA) UND LEERE STRINGS BEHANDELN
#----------------------------------------------------

all_trips_clean <- all_trips_clean %>%
  # 3.1 Konvertiere leere Strings ("") in NA in kritischen Spalten
  # Der across-Befehl wendet die Funktion auf alle genannten Spalten an
  mutate(across(c(start_station_name, end_station_name, member_casual), ~na_if(., ""))) %>% 
  
  # 3.2 Entferne Zeilen, die NAs in den kritischen Spalten haben
  drop_na(
    member_casual, 
    ride_length,
    # Wir nehmen die Stationsnamen mit auf, da sie oft für die Analyse wichtig sind
    start_station_name, 
    end_station_name
  )

#----------------------------------------------------
# 4. KONSISTENZPRÜFUNG: GROSS-/KLEINSCHREIBUNG
#----------------------------------------------------

# Prüfe die aktuellen, eindeutigen Werte in der Zielspalte
unique(all_trips_clean$member_casual)

# Konvertiere alle Werte in Kleinbuchstaben, um Inkonsistenzen zu vermeiden
all_trips_clean <- all_trips_clean %>%
  mutate(
    member_casual = tolower(member_casual),
    rideable_type = tolower(rideable_type) # Zur Sicherheit auch bei den Radtypen
  )

# Abschließender Check
unique(all_trips_clean$member_casual)

# Speichere das bereinigte Data Frame auf die Festplatte deines Projekts
write_rds(all_trips_clean, "data_processed/all_trips_clean.rds")


#----------------------------------------------------
# NEUE BERECHNUNG DER WOCHENTAGE (ENGLISCHE LOCALE ERZWINGEN)
#----------------------------------------------------

all_trips_clean <- all_trips_clean %>%
  mutate(
    # Erzwingt die englischen Abkürzungen (Sun, Mon, etc.)
    # Dies ist stabiler als das Vertrauen auf die Systemeinstellung.
    day_of_week = wday(started_at, label = TRUE, abbr = TRUE, locale = "C")
  )

# Prüfe die Ergebnisse zur Sicherheit:
unique(all_trips_clean$day_of_week)

# Wie viele Zeilen sind nach allen Bereinigungsschritten übrig?
nrow(all_trips_clean)
#überprüfen ob Spalten korrekte Typen haben
glimpse(all_trips_clean)
#Überprüfen, ob die Filterung der Ausreißer funktioniert hat und keine negativen oder unrealistischen Maxima mehr existieren.
summary(all_trips_clean$ride_length)

# 1. Zähle die STARTGRÖSSE (vor der Bereinigung)
start_rows <- nrow(all_trips)
print(paste("Startgröße (vor Bereinigung):", start_rows))

# 2. Zähle die ENDGRÖSSE (nach der Bereinigung)
end_rows <- nrow(all_trips_clean)
print(paste("Endgröße (nach Bereinigung):", end_rows))

# 3. Berechne die GESAMTANZAHL ENTFERNTER ZEILEN
removed_rows <- start_rows - end_rows
print(paste("Gesamtanzahl entfernter Zeilen:", removed_rows))

# 4. Berechne die DATENVERLUSTRATE (in Prozent)
loss_rate <- (removed_rows / start_rows) * 100
print(paste("Datenverlustrate:", round(loss_rate, 2), "%"))

#Alle Schritte gecheckt und Dokumentation durchgeführt

#------------------------------------------------------
#Analyse Process
#------------------------------------------------------

#----------------------------------------------------
# 1. SCHRITT: AGGREGATION DER KENNZAHLEN PRO NUTZERGRUPPE
#----------------------------------------------------

user_stats <- all_trips_clean %>%
  group_by(member_casual) %>%
  summarise(
    Anzahl_Fahrten = n(),# da Die Spaltenname ist auf Deutsch was aber falsch ist
    Durchschnitts_Dauer_Min = mean(ride_length),# da Die Spaltenname ist auf Deutsch was aber falsch ist
    Median_Dauer_Min = median(ride_length),# da Die Spaltenname ist auf Deutsch was aber falsch ist
    Min_Dauer_Min = min(ride_length),# da Die Spaltenname ist auf Deutsch was aber falsch ist
    Max_Dauer_Min = max(ride_length)# da Die Spaltenname ist auf Deutsch was aber falsch ist
  )

# Zeige die Ergebnisse an
print(user_stats)

#----------------------------------------------------
# 2. SCHRITT: ANZAHL UND DAUER PRO WOCHENTAG
#----------------------------------------------------

day_of_week_stats <- all_trips_clean %>%
  
  # Ordne die Wochentage korrekt (von Sonntag bis Samstag)
  mutate(day_of_week = ordered(day_of_week, 
                               levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  
  group_by(member_casual, day_of_week) %>%
  summarise(
    Anzahl = n(),
    Durchschnitts_Dauer = mean(ride_length)
  )

# Zeige die Ergebnisse an
print(day_of_week_stats)

#----------------------------------------------------
# 3. SCHRITT: TOP 10 BELIEBTESTE STARTSTATIONEN FÜR JEDE GRUPPE
#----------------------------------------------------
top_stations <- all_trips_clean %>%
  # 1. Nach Nutzergruppe UND Station gruppieren
  group_by(member_casual, start_station_name) %>%
  
  # 2. Die Anzahl der Fahrten pro Gruppe/Station zusammenfassen
  summarise(
    Anzahl_Fahrten = n(),
    .groups = 'drop' # <-- HIER entgruppern wir die Daten sofort!
  ) %>%
  
  # 3. Jetzt die Daten NUR nach der Nutzergruppe gruppieren, 
  # um die Top 10 pro Gruppe zu finden.
  group_by(member_casual) %>%
  
  # 4. Die Top 10 Zeilen nach der Anzahl der Fahrten (Anzahl_Fahrten) auswählen
  slice_max(order_by = Anzahl_Fahrten, n = 10) %>%
  
  # 5. Optional: Nach Anzahl sortieren (für eine schönere Ansicht)
  arrange(member_casual, desc(Anzahl_Fahrten))

print(top_stations)

# Wochentag-Analyse
day_of_week_stats <- all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    Anzahl = n(),
    Durchschnitts_Dauer = mean(ride_length),
    .groups = 'drop'
  )

print(day_of_week_stats)

top_stations <- all_trips_clean %>%
  group_by(member_casual, start_station_name) %>%
  summarise(
    Anzahl_Fahrten = n(),
    .groups = 'drop' 
  ) %>%
  group_by(member_casual) %>%
  slice_max(order_by = Anzahl_Fahrten, n = 10) %>%
  arrange(member_casual, desc(Anzahl_Fahrten))

print(top_stations)

#----------------------------------------------------
# 4. EXPORT DER ZUSAMMENFASSUNG (Summary File)
#----------------------------------------------------

# Export der Gesamtstatistik
write_csv(user_stats, "Cyclistic_analyse/data_processed/summary_user_stats.csv")

# Export der Wochentag-Analyse
write_csv(day_of_week_stats, "Cyclistic_analyse/data_processed/summary_day_of_week_stats.csv")

#Export der Top Stationen
write_csv(top_stations, "Cyclistic_analyse/data_processed/top_stations")

if (!dir.exists("data_processed")) {
  dir.create("data_processed")
}

# 3. Export der Top-10-Stationen pro Nutzergruppe (Geografische Analyse)
write_csv(top_stations, "Cyclistic_analyse/data_processed/summary_top_stations.csv")

file.remove("Cyclistic_analyse/data_processed/top_stations")

# hier wieder Code aber mit korrekter Form
user_stats <- all_trips_clean %>%
  group_by(member_casual) %>%
  summarise(
    ride_count = n(),  # NEU: ride_count
    mean_duration_min = mean(ride_length), # NEU: mean_duration_min
    median_duration_min = median(ride_length), # NEU: median_duration_min
    min_duration_Min = min(ride_length),
    max_duration_min = max(ride_length),
    .groups = 'drop'
  )

print(user_stats)
# Export wird später durchgeführt

day_of_week_stats <- all_trips_clean %>%
  mutate(day_of_week = ordered(day_of_week, 
                               levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    ride_count = n(), # NEU: ride_count
    mean_duration = mean(ride_length), # NEU: mean_duration
    .groups = 'drop'
  )

print(day_of_week_stats)
# Export wird später durchgeführt

top_stations <- all_trips_clean %>%
  group_by(member_casual, start_station_name) %>%
  summarise(
    ride_count = n(), # NEU: ride_count
    .groups = 'drop' 
  ) %>%
  group_by(member_casual) %>%
  slice_max(order_by = ride_count, n = 10) %>%
  arrange(member_casual, desc(ride_count))

print(top_stations)
# Export wird später durchgeführt

# EXPORT DER KORRIGIERTEN ZUSAMMENFASSUNGEN
write_csv(user_stats, "Cyclistic_analyse/data_processed/summary_user_stats.csv")
write_csv(day_of_week_stats, "Cyclistic_analyse/data_processed/summary_day_of_week_stats.csv")
write_csv(top_stations, "Cyclistic_analyse/data_processed/summary_top_stations.csv")

# Speichert den gesamten, bereinigten Data Frame effizient auf die Festplatte
write_rds(all_trips_clean, "Cyclistic_analyse/data_processed/all_trips_clean.rds")

library(tidyverse)
# ... weitere Bibliotheken

# Lade das bereinigte Data Frame von der Festplatte
all_trips_clean <- read_rds("Cyclistic_analyse/data_processed/all_trips_clean.rds")

#-----------------------------------------------------
#Visualizations for R
#-----------------------------------------------------

library(ggplot2)

users <- read_csv("Cyclistic_analyse/data_processed/summary_user_stats.csv")
#----------------------------------------
# Visualisation 1: Median Ride Duration (Bar Chart)
#----------------------------------------
plot_median_duration <- ggplot(users, 
                               aes(x = member_casual, y = median_duration_min, fill = member_casual)) +
  geom_col() +
  labs(title = "Median Ride Duration: Casual Riders Take Significantly Longer Trips",
       subtitle = "Median Duration in Minutes",
       x = "User Type",
       y = "Median Duration (Minutes)") +
  # Use contrast to highlight Casual Riders
  scale_fill_manual(values = c("casual" = "red", "member" = "gray")) +
  theme_minimal()

print(plot_median_duration)

# Save the plot for your portfolio
ggsave("visualizations/plot_median_duration.png", plot_median_duration, width = 8, height = 5)

#read_csv skript to upload
summary_day_of_week_stats <- read_csv("Cyclistic_analyse/data_processed/summary_day_of_week_stats.csv")
  
#------------------------------------------------------
# Visualisation 2: Ride Count by Day of Week (Column Chart)
#-----------------------------------------------------

plot_day_pattern <- ggplot(summary_day_of_week_stats, 
                           aes(x = day_of_week, y = ride_count, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Usage Pattern: Commuters (Mon-Fri) vs. Leisure (Weekend)",
       subtitle = "Number of Rides Across the Days of the Week",
       x = "Day of Week",
       y = "Number of Rides") +
  # Consistent theme
  theme_minimal()

print(plot_day_pattern)
ggsave("visualizations/plot_day_pattern.png", plot_day_pattern, width = 10, height = 6)

# diese Visualizaiton ist nicht korrekt erstellt!!!
#------------------------------------
# Visualisation 3: Top 10 Stations per User Group (Horizontal Bar Chart)
#--------------------------------------
summary_top_stations <- read_csv("Cyclistic_analyse/data_processed/summary_top_stations.csv")

plot_top_stations <- top_stations %>%
  ggplot(aes(x = reorder(start_station_name, ride_count), y = ride_count, fill = member_casual)) +
  geom_col() +
  coord_flip() + # Makes station names easier to read
  facet_wrap(~member_casual, scales = "free_y") + # Separates groups for clear comparison
  labs(title = "Top 10 Start Stations by User Group",
       subtitle = "Location confirms Commuter vs. Leisure Motives",
       x = "Start Station",
       y = "Number of Rides") +
  theme_minimal()

print(plot_top_stations)
ggsave("visualizations/plot_top_stations.png", plot_top_stations, width = 12, height = 7)

#kontrolle bei Typen
str(all_trips_clean$day_of_week)

#------------------------------------
#neue Visualization für day_of_week weil die Wachentage nicht in der Reihenfolge sind und andere Korrekturen
#------------------------------------

# 1. Entferne das alte Objekt zur Sicherheit
rm(day_of_week_stats)

# 2. Berechne das Data Frame NEU. Es erbt die korrekte Sortierung von all_trips_clean
day_of_week_stats <- all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    ride_count = n(),
    mean_duration = mean(ride_length),
    .groups = 'drop'
  )

# 3. Überprüfen Sie die Sortierung in der Konsole
print(day_of_week_stats)

#----------------------------------------------
# Visualisation 2: new Ride Count by Day of Week (Column Chart)
#----------------------------------------------
plot_day_pattern <- ggplot(day_of_week_stats, 
                           aes(x = day_of_week, y = ride_count, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Usage Pattern: Commuter vs. Leisure",
       subtitle = "Number of Rides Across the Days of the Week",
       x = "Day of Week",
       y = "Number of Rides") +
  theme_minimal()

print(plot_day_pattern)

# Speichern der korrigierten Grafik
ggsave("visualizations/plot_day_pattern.png", plot_day_pattern, width = 10, height = 6)

# Definiere die Top 10 Stationen FÜR JEDE GRUPPE getrennt
top_stations <- all_trips_clean %>%
  group_by(member_casual, start_station_name) %>%
  summarise(ride_count = n(), .groups = 'drop') %>%
  # Filtert NA-Stationen heraus, falls noch vorhanden
  filter(!is.na(start_station_name)) %>% 
  # Sortiere und wähle die Top 10 pro Gruppe
  group_by(member_casual) %>%
  slice_max(order_by = ride_count, n = 10) %>%
  ungroup()

# Überprüfung (Print)
print(top_stations)

# Visualisation 3: Top 10 Stations per User Group (Horizontal Bar Chart)
plot_top_stations <- top_stations %>%
  ggplot(aes(x = reorder(start_station_name, ride_count), y = ride_count, fill = member_casual)) +
  
  # HINWEIS: Um die höchste Station nach oben zu bekommen, müssen wir die Sortierreihenfolge umkehren
  # Wir verwenden hier NICHT das negative Vorzeichen, da coord_flip() dies oft schon impliziert.
  # Wir lassen den Reorder-Befehl und ändern nur die Achse (siehe unten).
  
  geom_col() +
  coord_flip() + 
  
  # Freie Achsenskalen für einen fairen Vergleich der Top-Stationen beider Gruppen
  facet_wrap(~member_casual, scales = "free_y") + 
  
  labs(title = "Top 10 Start Stations by User Group",
       subtitle = "Location confirms Commuter vs. Leisure Motives",
       x = "Start Station",
       y = "Number of Rides") +
  
  # WICHTIG: Sortierung umkehren, damit die Top-Stationen oben stehen
  scale_x_discrete(limits = rev) +
  
  theme_minimal()

print(plot_top_stations)
#nach Korrektur wieder nicht funktioniert 

# Visualisation 3: Top 10 Stations per User Group (Horizontal Bar Chart)
plot_top_stations <- top_stations %>%
  ggplot(aes(x = reorder(start_station_name, -ride_count), y = ride_count, fill = member_casual)) +
  
  # WICHTIG: -ride_count erzwingt die absteigende Sortierung (Höchster Wert zuerst)
  geom_col() +
  coord_flip() + 
  
  # Freie Achsenskalen für einen fairen Vergleich der Top-Stationen beider Gruppen
  facet_wrap(~member_casual, scales = "free_y") + 
  
  labs(title = "Top 10 Start Stations by User Group",
       subtitle = "Location confirms Commuter vs. Leisure Motives",
       x = "Start Station",
       y = "Number of Rides") +
  
  # Entferne die vorherige Korrektur, da wir die Sortierung direkt über reorder steuern:
  # scale_x_discrete(limits = rev) + 
  
  theme_minimal()

print(plot_top_stations)

# 1. Sicherstellen, dass die Datenbasis korrekt ist (Top 10 pro Gruppe)
top_stations <- all_trips_clean %>%
  group_by(member_casual, start_station_name) %>%
  summarise(ride_count = n(), .groups = 'drop') %>%
  filter(!is.na(start_station_name)) %>%
  group_by(member_casual) %>%
  slice_max(order_by = ride_count, n = 10) %>%
  ungroup()

# 2. Sortierung des Stationsnamens (Factors) für den Plot erzwingen:
# Wir definieren die Reihenfolge (Levels) innerhalb jeder Gruppe (facet) neu.
top_stations_ordered <- top_stations %>%
  group_by(member_casual) %>%
  mutate(
    start_station_name = fct_reorder(start_station_name, ride_count) # Sortiert nach ride_count
  ) %>%
  ungroup()

# Überprüfung (Print)
print(top_stations_ordered)

# Visualisation 3: Top 10 Stations per User Group (Horizontal Bar Chart)
plot_top_stations <- top_stations_ordered %>%
  ggplot(aes(x = start_station_name, y = ride_count, fill = member_casual)) +
  
  # Hier verwenden wir KEIN reorder mehr, da die Vorsortierung in Schritt 1 erledigt wurde
  geom_col() +
  coord_flip() + 
  
  # Freie Achsenskalen für einen fairen Vergleich der Top-Stationen beider Gruppen
  facet_wrap(~member_casual, scales = "free_y") + 
  
  labs(title = "Top 10 Start Stations by User Group",
       subtitle = "Location confirms Commuter vs. Leisure Motives",
       x = "Start Station",
       y = "Number of Rides") +
  
  # WICHTIG: Kehrt die Achsenreihenfolge um, sodass der höchste Wert (HQ QR) oben steht
  scale_x_discrete(limits = rev) +
  
  theme_minimal()

print(plot_top_stations)
ggsave("visualizations/plot_top_stations.png", plot_top_stations, width = 12, height = 7)

rm(plot_top_stations)
# WICHTIG: Erstellen Sie ein NEUES Data Frame, um sicherzustellen, dass die Sortierung angewendet wird
top_stations_final_sort <- top_stations %>%
  group_by(member_casual) %>%
  mutate(
    # Sortiert die Stationsnamen in ABSTEIGENDER Reihenfolge (höchster Wert zuerst)
    start_station_name = fct_reorder(start_station_name, -ride_count) 
  ) %>%
  ungroup()

# Überprüfung (Print) - Die höchste Station muss jetzt ganz oben in der Liste stehen
print(top_stations_final_sort)


# Visualisation 3: Top 10 Stations per User Group (Horizontal Bar Chart)
plot_top_stations <- top_stations_final_sort %>%
  ggplot(aes(x = start_station_name, y = ride_count, fill = member_casual)) +
  
  # Kein reorder notwendig, da in Schritt 1 erledigt
  geom_col() +
  coord_flip() + 
  
  # Freie Achsenskalen für einen fairen Vergleich der Top-Stationen beider Gruppen
  facet_wrap(~member_casual, scales = "free_y") + 
  
  labs(title = "Top 10 Start Stations by User Group",
       subtitle = "Location confirms Commuter vs. Leisure Motives",
       x = "Start Station",
       y = "Number of Rides") +
  
  scale_x_discrete(limits = rev) +
  # LASSEN SIE HIER DIE FUNKTION scale_x_discrete(limits = rev) WEG!
  
  theme_minimal()

print(plot_top_stations)

# 1. Sicherstellen, dass die Datenbasis korrekt ist (Top 10 pro Gruppe)
top_stations <- all_trips_clean %>%
  group_by(member_casual, start_station_name) %>%
  summarise(ride_count = n(), .groups = 'drop') %>%
  filter(!is.na(start_station_name)) %>%
  group_by(member_casual) %>%
  slice_max(order_by = ride_count, n = 10) %>%
  ungroup()

# 2. Sortierung des Stationsnamens (Factors) für den Plot erzwingen:
# Wir definieren die Reihenfolge (Levels) innerhalb jeder Gruppe (facet) neu.
top_stations_ordered <- top_stations %>%
  group_by(member_casual) %>%
  mutate(
    start_station_name = fct_reorder(start_station_name, ride_count) # Sortiert nach ride_count
  ) %>%
  ungroup()

# Überprüfung (Print)
print(top_stations_ordered)

# Visualisation 3: Top 10 Stations per User Group (Horizontal Bar Chart)
plot_top_stations <- top_stations_ordered %>%
  ggplot(aes(x = start_station_name, y = ride_count, fill = member_casual)) +
  
  # Hier verwenden wir KEIN reorder mehr, da die Vorsortierung in Schritt 1 erledigt wurde
  geom_col() +
  coord_flip() + 
  
  # Freie Achsenskalen für einen fairen Vergleich der Top-Stationen beider Gruppen
  facet_wrap(~member_casual, scales = "free_y") + 
  
  labs(title = "Top 10 Start Stations by User Group",
       subtitle = "Location confirms Commuter vs. Leisure Motives",
       x = "Start Station",
       y = "Number of Rides") +
  
  # WICHTIG: Kehrt die Achsenreihenfolge um, sodass der höchste Wert (HQ QR) oben steht
  scale_x_discrete(limits = rev) +
  
  theme_minimal()

print(plot_top_stations)
ggsave("visualizations/plot_top_stations.png", plot_top_stations, width = 12, height = 7)
# jetzt funktioniert aber die höchsten Ergebnisse sind unten
