# data_sources.R

# Load required libraries
library(tibble)
library(dplyr)
library(readr)

# Read the shapefile
shapefile_path <- "data/SHP/adr2024.shp"
adr2024 <- st_read(shapefile_path)

# Transform coordinates to WGS84 (lat/lon) for Leaflet compatibility if needed
if (st_crs(adr2024)$epsg != 4326) {
  adr2024 <- st_transform(adr2024, crs = 4326)
}

# Define color mapping for WL2024 categories
wl_colors <- c("A" = "red", "B" = "blue", "C" = "green")

# Load address data from CSV
ref_adresse <- read_csv("data/adr2024.csv")

# Create the WL_FAKTOR column based on WL_2024
ref_adresse <- ref_adresse %>%
  mutate(ADRESS_ID_KURZ = substr(ADRESS_ID, 6, nchar(ADRESS_ID))) %>%
  arrange(STRASSE, ADRESS_ID_KURZ) %>%
  select(-ADRESS_ID_KURZ) %>%
  mutate(WL_FAKTOR = case_when(
    WL_2024 == "A" ~ 0.00,
    WL_2024 == "B" ~ -0.07,
    WL_2024 == "C" ~ -0.10,
    TRUE ~ NA_real_
  ))

# Define Baujahr reference data
ref_baujahr <- tibble::tribble(
  ~Baujahr, ~Factor,
  "bis 1918", 0.00,
  "1919 - 1945", -0.07,
  "1946 - 1977", -0.10,
  "1978 - 1984", -0.05,
  "1985 - 1989", -0.01,
  "1990 - 1995", -0.01,
  "1996 - 2004", 0.06,
  "2005 - 2012", 0.12,
  "2013 - 2018", 0.19,
  "2019 - 2023", 0.24
)

# Define reference_groesse with all ranges included
ref_groesse <- tibble::tribble(
  ~von, ~bis_unter, ~low, ~med, ~hi, ~options,
  25L, 26L, 9.84, 11.86, 13.88, "25 bis unter 26 m²",
  26L, 27L, 9.66, 11.64, 13.61, "26 bis unter 27 m²",
  27L, 28L, 9.48, 11.43, 13.37, "27 bis unter 28 m²",
  28L, 29L, 9.32, 11.23, 13.14, "28 bis unter 29 m²",
  29L, 30L, 9.17, 11.05, 12.93, "29 bis unter 30 m²",
  30L, 31L, 9.03, 10.89, 12.74, "30 bis unter 31 m²",
  31L, 32L, 8.90, 10.73, 12.55, "31 bis unter 32 m²",
  32L, 33L, 8.78, 10.58, 12.38, "32 bis unter 33 m²",
  33L, 34L, 8.67, 10.44, 12.22, "33 bis unter 34 m²",
  34L, 35L, 8.56, 10.31, 12.06, "34 bis unter 35 m²",
  35L, 36L, 8.46, 10.19, 11.92, "35 bis unter 36 m²",
  36L, 37L, 8.36, 10.07, 11.79, "36 bis unter 37 m²",
  37L, 38L, 8.27, 9.96, 11.66, "37 bis unter 38 m²",
  38L, 39L, 8.18, 9.86, 11.54, "38 bis unter 39 m²",
  39L, 40L, 8.10, 9.76, 11.42, "39 bis unter 40 m²",
  40L, 41L, 8.02, 9.67, 11.31, "40 bis unter 41 m²",
  41L, 42L, 7.95, 9.58, 11.21, "41 bis unter 42 m²",
  42L, 43L, 7.88, 9.49, 11.11, "42 bis unter 43 m²",
  43L, 44L, 7.81, 9.41, 11.01, "43 bis unter 44 m²",
  44L, 45L, 7.75, 9.33, 10.92, "44 bis unter 45 m²",
  45L, 46L, 7.69, 9.26, 10.83, "45 bis unter 46 m²",
  46L, 47L, 7.63, 9.19, 10.75, "46 bis unter 47 m²",
  47L, 48L, 7.57, 9.12, 10.67, "47 bis unter 48 m²",
  48L, 49L, 7.52, 9.06, 10.60, "48 bis unter 49 m²",
  49L, 50L, 7.47, 9.00, 10.52, "49 bis unter 50 m²",
  50L, 51L, 7.42, 8.94, 10.45, "50 bis unter 51 m²",
  51L, 52L, 7.37, 8.88, 10.39, "51 bis unter 52 m²",
  52L, 53L, 7.32, 8.82, 10.32, "52 bis unter 53 m²",
  53L, 54L, 7.28, 8.77, 10.26, "53 bis unter 54 m²",
  54L, 55L, 7.24, 8.72, 10.20, "54 bis unter 55 m²",
  55L, 56L, 7.20, 8.67, 10.14, "55 bis unter 56 m²",
  56L, 57L, 7.16, 8.62, 10.09, "56 bis unter 57 m²",
  57L, 59L, 7.10, 8.55, 10.01, "57 bis unter 59 m²",
  59L, 61L, 7.03, 8.47, 9.91, "59 bis unter 61 m²",
  61L, 64L, 6.95, 8.37, 9.79, "61 bis unter 64 m²",
  64L, 67L, 6.86, 8.26, 9.67, "64 bis unter 67 m²",
  67L, 70L, 6.77, 8.16, 9.55, "67 bis unter 70 m²",
  70L, 77L, 6.65, 8.01, 9.38, "70 bis unter 77 m²",
  77L, 81L, 6.53, 7.86, 9.20, "77 bis unter 81 m²",
  81L, 87L, 6.44, 7.76, 9.08, "81 bis unter 87 m²",
  87L, 93L, 6.35, 7.65, 8.94, "87 bis unter 93 m²",
  93L, 100L, 6.25, 7.53, 8.82, "93 bis unter 100 m²",
  100L, 108L, 6.16, 7.42, 8.69, "100 bis unter 108 m²",
  108L, 120L, 6.06, 7.30, 8.54, "108 bis unter 120 m²",
  120L, 132L, 5.96, 7.18, 8.40, "120 bis unter 132 m²",
  132L, 141L, 5.88, 7.09, 8.29, "132 bis unter 141 m²",
  141L, 150L, 5.82, 7.02, 8.21, "141 bis unter 150 m²"
)

# Define renovation options and their conditions
ref_renovation <- tibble::tribble(
  ~Option, ~Description, ~Allowed_Baujahr_Range, ~Value,
  "Keine Sanierung/Renovierung bekannt", "No known renovation", "all", 0.00,
  "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)", "Complete renovation since 2013", "<1990", 0.11,
  "Sanitärbereich (mind. Fliesen, Wanne, WC) erneuert", "Bathroom renovation (tiles, bathtub, WC)", "all", 0.06,
  "Elektroinstallation (zeitgemäß) erneuert", "Updated electrical installation", "all", 0.06,
  "Heizanlage/Warmwasserversorgung erneuert", "Heating system/hot water updated", "all", 0.06,
  "Schallschutz eingebaut", "Soundproofing added", "all", 0.06,
  "Fußböden erneuert", "Floors renovated", "all", 0.06,
  "Fenster-/Rahmenerneuerung", "Window/frame renovation", "all", 0.06,
  "Innen- und Wohnungstüren erneuert", "Internal doors replaced", "all", 0.06,
  "Treppenhaus, Eingangsbereich erneuert", "Staircase/entrance area renovated", "all", 0.06,
  "barrierearme Ausstattung geschaffen", "Barrier-free equipment created", "all", 0.06,
  "Grundriss verbessert", "Floor plan improved", "all", 0.06,
  "Dachsanierung", "Roof renovation", "all", 0.06,
  "Fassadensanierung", "Facade renovation", "all", 0.06
)

# Define options for ref_sanitaer
ref_sanitaer <- c(
  "Keine besondere Sanitärausstattung",
  "zwei oder mehr abgeschlossene Badezimmer in der Wohnung vorhanden",
  "zweites WC/Gäste-WC vorhanden",
  "(separate) Einzeldusche",
  "Fußbodenheizung",
  "Belüftung(sanlage)",
  "separater WC-Raum vorhanden",
  "Handtuchheizkörper",
  "zweites Waschbecken im selben Badezimmer"
)

# Define Ausstattung items with corresponding percentages
ref_ausstattung <- list(
  "Keine besondere Ausstattung" = 0.00,
  "Einbauküche mit mindestens zwei Elektroeinbaugeräten (z. B. Herd/Ofen, Gefrierschrank/-truhe, Kühlschrank, Geschirrspülmaschine) wird vom Vermieter ohne zusätzlichen Mietzuschlag gestellt. (+4%)" = 0.04,
  "Terrasse oder Dachterrasse (+6%)" = 0.06,
  "Aufzug in Gebäuden mit weniger als 5 Stockwerken (+7%)" = 0.07,
  "Überwiegend Parkett-, Dielen- oder Steinfußboden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut (+3%)" = 0.03,
  "Energiebedarfsklasse lt. Energiebedarfsausweis lautet F, G oder H; bzw. der Wert kWh/m2a ist größer oder gleich 200 (-9%)" = -0.09,
  "Teppichboden, PVC- oder Linoleum-Boden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut, welcher seit 2013 nicht modernisiert bzw. saniert wurde (-11%)" = -0.11
)

