#-----
library(shiny)
library(dplyr)
library(readr)
library(tibble)
library(sf)
library(leaflet)
library(shinyjs)
library(markdown)
library(tinytex)
library(shinyjs)
library(kableExtra)


#------ Referenzdaten ------
# Die Referenzdaten tragen jeweils den Namen 'ref_kategorie' (also z.B.
# 'ref_adresse' für die Adressen).

    # Einlesen eines Shapefiles zur Kartendarstellung der jeweiligen Adressen
    shapefile_path <- "data/SHP/adr2024.shp"
    adr2024 <- st_read(shapefile_path)
  # Anpassung der Projektion auf WGS84 (EPSG: 4326)
    if (st_crs(adr2024)$epsg != 4326) {
    adr2024 <- st_transform(adr2024, crs = 4326)
    }
  # Farben für die Wohnlagen
    wl_colors <- c("A" = "red", "B" = "blue", "C" = "green")

  # Laden externer Adressdaten aus CSV-Datei (zur Befüllung des
  # Selectize-Inputs in der Kategorie Adresse/Lage)
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

# Mögliche Angaben für Baujahr mit zugehörigem Zu-/Abschlag
ref_baujahr <- tibble::tribble(
  ~Baujahr, ~Faktor,
  "1918 und älter", 0.00,
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

# Mögliche Angaben (options) für den Größenbereich der Wohnungen
# mit zugehörigen Basiswerten (low, mid, hi)
ref_groesse <- tibble::tribble(
  ~von, ~bis_unter, ~low, ~mid, ~hi, ~options,
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
  37L, 38L, 8.27,  9.96, 11.66, "37 bis unter 38 m²",
  38L, 39L, 8.18,  9.86, 11.54, "38 bis unter 39 m²",
  39L, 40L, 8.10,  9.76, 11.42, "39 bis unter 40 m²",
  40L, 41L, 8.02,  9.67, 11.31, "40 bis unter 41 m²",
  41L, 42L, 7.95,  9.58, 11.21, "41 bis unter 42 m²",
  42L, 43L, 7.88,  9.49, 11.11, "42 bis unter 43 m²",
  43L, 44L, 7.81,  9.41, 11.01, "43 bis unter 44 m²",
  44L, 45L, 7.75,  9.33, 10.92, "44 bis unter 45 m²",
  45L, 46L, 7.69,  9.26, 10.83, "45 bis unter 46 m²",
  46L, 47L, 7.63,  9.19, 10.75, "46 bis unter 47 m²",
  47L, 48L, 7.57,  9.12, 10.67, "47 bis unter 48 m²",
  48L, 49L, 7.52,  9.06, 10.60, "48 bis unter 49 m²",
  49L, 50L, 7.47,  9.00, 10.52, "49 bis unter 50 m²",
  50L, 51L, 7.42,  8.94, 10.45, "50 bis unter 51 m²",
  51L, 52L, 7.37,  8.88, 10.39, "51 bis unter 52 m²",
  52L, 53L, 7.32,  8.82, 10.32, "52 bis unter 53 m²",
  53L, 54L, 7.28,  8.77, 10.26, "53 bis unter 54 m²",
  54L, 55L, 7.24,  8.72, 10.20, "54 bis unter 55 m²",
  55L, 56L, 7.20,  8.67, 10.14, "55 bis unter 56 m²",
  56L, 57L, 7.16,  8.62, 10.09, "56 bis unter 57 m²",
  57L, 59L, 7.10,  8.55, 10.01, "57 bis unter 59 m²",
  59L, 61L, 7.03,  8.47,  9.91, "59 bis unter 61 m²",
  61L, 64L, 6.95,  8.37,  9.79, "61 bis unter 64 m²",
  64L, 67L, 6.86,  8.26,  9.67, "64 bis unter 67 m²",
  67L, 70L, 6.77,  8.16,  9.55, "67 bis unter 70 m²",
  70L, 77L, 6.65,  8.01,  9.38, "70 bis unter 77 m²",
  77L, 81L, 6.53,  7.86,  9.20, "77 bis unter 81 m²",
  81L, 87L, 6.44,  7.76,  9.08, "81 bis unter 87 m²",
  87L, 93L, 6.35,  7.65,  8.94, "87 bis unter 93 m²",
  93L, 100L, 6.25,  7.53, 8.82, "93 bis unter 100 m²",
  100L, 108L, 6.16, 7.42, 8.69, "100 bis unter 108 m²",
  108L, 120L, 6.06, 7.30, 8.54, "108 bis unter 120 m²",
  120L, 132L, 5.96, 7.18, 8.40, "120 bis unter 132 m²",
  132L, 141L, 5.88, 7.09, 8.29, "132 bis unter 141 m²",
  141L, 150L, 5.82, 7.02, 8.21, "141 bis unter 150 m²"
)

# Mögliche Angaben unter "Renovierung"
ref_renovation <- tibble::tribble(
  ~Option, ~Value,
  "Keine Sanierung/Renovierung bekannt", 0.00,
  "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)", 0.11,
  "Sanitärbereich (mind. Fliesen, Wanne, WC) erneuert", 0.06,
  "Elektroinstallation (zeitgemäß) erneuert", 0.06,
  "Heizanlage/Warmwasserversorgung erneuert",  0.06,
  "Schallschutz eingebaut", 0.06,
  "Fußböden erneuert", 0.06,
  "Fenster-/Rahmenerneuerung", 0.06,
  "Innen- und Wohnungstüren erneuert", 0.06,
  "Treppenhaus, Eingangsbereich erneuert", 0.06,
  "barrierearme Ausstattung geschaffen", 0.06,
  "Grundriss verbessert", 0.06,
  "Dachsanierung (energetisch)", 0.06,
  "Fassadensanierung (energetisch)", 0.06
)

# Mögliche Angaben für "Sanitärausstattung"
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

# Mögliche Angaben für "Ausstattung"
ref_ausstattung <- tibble::tibble(
  Option = c(
    paste0("Keine besondere Ausstattung"),
    paste0("Einbauküche mit mindestens drei Elektroeinbaugeräten ",
      "(z. B. Herd/Ofen, Gefrierschrank/-truhe, Kühlschrank, ",
      "Geschirrspülmaschine) wird vom Vermieter ohne zusätzlichen ",
      "Mietzuschlag gestellt. (+4%)"),
    paste0("Terrasse oder Dachterrasse (+6%)"),
    paste0("Aufzug in Gebäuden mit weniger als 5 Stockwerken (+7%)"),
    paste0("Überwiegend Parkett-, Dielen- oder Steinfußboden im ",
      "überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von ",
      "Flur/Bad verbaut (+3%)"),
    paste0("Energiebedarfsklasse lt. Energiebedarfsausweis lautet ",
      "F, G oder H; bzw. der Wert kWh/m2a ist größer oder gleich 200 (-9%)"),
    paste0("Teppichboden, PVC- oder Linoleum-Boden im überwiegenden Teil ",
      "des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut, welcher ",
      "seit 2013 nicht modernisiert bzw. saniert wurde (-11%)")
  ),
  Factor = c(
    0.00,
    0.04,
    0.06,
    0.07,
    0.03,
    -0.09,
    -0.11
  )
)

#------ Hilfsfunktionen------

# Flexible Darstellung von Währungs- und Prozentwerten
# mit/ohne Einheit und opt. Vorzeichen
format_value <- function(value, unit = "", add_plus = FALSE) {
  # Behandlung von NA-Werten
  if (is.na(value)) {
    return("NA")
  }
  # format String in Sprintf: +-Vorzeichen ja oder nein
  fmt <- if (add_plus) "%+.2f" else "%.2f"

  # Runden und lt. fmt formatieren
  formatted_value <- sprintf(fmt, round(value, 2)) # Apply rounding and format

  # Dezimal,- und Tausenderzeichen an deutsche Konventionen anpassen
  formatted_value <- gsub("\\.", ",", formatted_value)
  formatted_value <- gsub("(?<=\\d)(?=(\\d{3})+,)",
                          ".",
                          formatted_value,
                          perl = TRUE)

  # Einheit (z.B. "€/m²") anhängen
  paste0(formatted_value, unit)
}

# Prüfen, ob eine sinnvolle und verwertbare Eingabe vorliegt,
# abhängig vom Ergebnis wird der Hinweistext ausgeblendet und
# der Hintergrund der jeweiligen fluidRow rot (keine sinnvolle Eingabe)
# oder grün (sinnvolle Eingabe) eingefärbt (via styles.css)
# Die Funktion check_if_complete() wird für jede Kategorie einzeln aufgerufen
# und reagiert auf Veränderungen in den jeweiligen Eingabefeldern.
check_if_complete <- function(input, input_id, hint_id, row_id, session) {
  observe({
    # Check if the input is null or contains only empty strings
    if (is.null(input[[input_id]]) || all(input[[input_id]] == "")) {
      shinyjs::show(hint_id) # Show the hint
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('framed-row-complete').addClass('framed-row-incomplete');",
        row_id
      ))
      # Mark the row as incomplete
    } else {
      shinyjs::hide(hint_id) # Hide the hint
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('framed-row-incomplete').addClass('framed-row-complete');",
        row_id
      )) # Mark the row as complete
    }
  })
}






#----- User Interface ----
ui <- fluidPage(
  useShinyjs(), # Enable shinyjs for JavaScript interactions
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("function scrollToTop() {window.scrollTo(0, 0);}"))
  ),
  # Title panel wrapped in the always-visible class for consistent width and
  # visibility
  div(
    class = "always-visible",
    id = "title-container",
    titlePanel("Mietspiegelrechner 2024")
  ),
  # Main app container, 800px width, centered on the page
  div(
    id = "app-container",
    class = "app-container",
    #---- Größenbereichsauswahl ----
    # Die in ref_groesse vorgehaltene Referenzdatei (im folgenden die ersten
    # drei Datenzeilen) speist mit 'options' das Dropdownfeld (selectizeInput)
    # für die Wohnungsgröße und hält liefert auf der Grundlage der getroffenen
    # Auswahl die drei Werte 'low', 'mid' und 'hi' für die untere, mittlere und
    # obere Grenze des jeweiligen Basiswertes. Diese drei Werte werden in der
    # Folge mit den jeweiligen prozentualen Zu-/Abschlägen multipliziert
    # Datenstruktur von ref_groesse:
    # > head(ref_groesse, 3)
    # A tibble: 3 × 6
    #       von   bis_unter   low   mid   hi        options
    #       <int> <int>      <dbl> <dbl> <dbl>      <chr>
    #   1    25    26        9.84  11.9  13.9      25 bis unter 26 m²
    #   2    26    27        9.66  11.6  13.6      26 bis unter 27 m²
    #.  3    27    28        9.48  11.4  13.4      27 bis unter 28 m²
    fluidRow(
      class = "framed-row",
      id = "groesse_row",
      column(
        width = 4,
        selectInput("groesse", "Wohnungsgröße (m²)",
          c("", ref_groesse$options),
          selectize = TRUE
        ),
        div(
          id = "groesse_hint",
          "Bitte wählen Sie hier den gesuchten Größenbereich aus ",
          "(Wohnfläche in m² lt. Mietvertrag), ",
          "z.B. '25 bis unter 26 m²' "
        ),
      ),
      column(
        width = 2, br(),
        htmlOutput("groesse_info")
      ),
      column(
        width = 2, br(),
        div(
          HTML("Untere Grenze"),
          htmlOutput("groesse_ug")
        )
      ),
      column(
        width = 2, br(),
        div(
          HTML("<strong>Ortsüblich</strong>"),
          div(
            htmlOutput("groesse_oue", inline = TRUE), # Allow inline HTML
            style = "font-weight: bold;"             # Bold styling
          )
        )
      ),
      column(
        width = 2, br(),
        div(
          HTML("Obere Grenze"),
          htmlOutput("groesse_og")
        )
      )
    ),
    #---- Adressenauswahl ----
    fluidRow(
      class = "framed-row",
      id = "adresse_row",
      # Left column: Selectize input and hint
      column(
        width = 6,
        selectizeInput(
          inputId = "adresse",
          label = "Adresse",
          choices = NULL,
          multiple = FALSE
        )
      ),
      # Right column: Wohnlage text and leaflet map
      column(
        width = 6,
        fluidRow(
          # First sub-row: Wohnlage text
          column(
            width = 12,
            htmlOutput("adresse_factor") # Output for Wohnlage and factor
          )
        ),
        fluidRow(
          # Second sub-row: Leaflet map
          column(
            width = 12,
            div(
              id = "adresse_hint",
              "Bitte geben Sie die gesuchte Adresse in das ",
              "Suchfeld ein. Diese Adresse bestimmt die Wohnlage ",
              "A (+-0%), B (-7%) oder C (-10%). Auch Teileingaben ",
              "werden erkannt, z.B. führt die Angabe 'inn 76' ",
              "direkt zur Innstraße 76."
            ),
            leafletOutput("adresse_map", height = "200px") # Map
          )
        )
      )
    ),
    #---- Baujahrsauswahl -----
    fluidRow(
      class = "framed-row",
      id = "baujahr_row",
      # Left column: Dropdown for Baujahr
      column(
        width = 6,
        selectInput(
          inputId = "baujahr",
          label = "Baujahr",
          choices = c("", ref_baujahr$Baujahr), # Add empty placeholder
          selected = NULL
        )
      ),
      # Right column: Hint and Baujahr factor output
      column(
        width = 6,
        div(
          id = "baujahr_hint",
          "Bitte wählen Sie das Baujahr des Gebäudes aus (ggf. als Schätzung).",
          " Der Baujahresfaktor wird automatisch berechnet und angezeigt."
        ),
        htmlOutput("baujahr_factor") # Output for Baujahr and factor
      )
    ),
    #---- Renovierungsauswahl -----
    fluidRow(
      class = "framed-row",
      id = "renovierung_row", # Unique ID for styling
      column(
        width = 6,
        # Step 1: Dropdown for high-level renovation selection
        selectInput(
          inputId = "renovierung_main",
          label = "Renovierungsart",
          choices = c(
            "",
            "Keine Sanierung/Renovierung bekannt",
            "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)",
            "Teilrenovierung seit 2013 (mind. 3 Maßnahmen)"
          ),
          selected = ""
        ),
        # Step 2: Checkboxes for Teilrenovierung (hidden by default)
        conditionalPanel(
          condition = paste("input.renovierung_main ==",
                            " 'Teilrenovierung seit 2013 (mind. 3 Maßnahmen)'"),
          checkboxGroupInput(
            inputId = "renovierung_details",
            label = "Welche Maßnahmen wurden durchgeführt?",
            choices = setdiff(
              ref_renovation$Option,
              c("Keine Sanierung/Renovierung bekannt",
                "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)")
            )
          )
        )
      ),
      column(
        width = 6,
        div(
          id = "renovierung_hint",
          "Bitte machen Sie Angaben zum Renovierungszustand des Objekts. ",
          "Berücksichtigen Sie dabei, dass nur ab 2013 durchgeführte ",
          "Renovierungen relevant sind, und dass eine Vollmodernisierung ",
          "(Zuschlag 11%) nur bei Baujahr vor 1990 angegeben werden kann."
        ),
        # Display renovation factor as output
        htmlOutput("renovierung_factor")
      )
    ),
    #---- Sanitärausstattungsauswahl -----
    fluidRow(
      class = "framed-row",
      id = "sanitaer_row", # Unique ID for background color handling
      column(
        width = 6,
        # Primary dropdown for sanitär options
        selectInput(
          inputId = "sanitaer_main",
          label = "Sanitärausstattung",
          choices = c("",
            "Keine besondere Sanitärausstattung",
            "Verbesserte Sanitärausstattung"
          ),
          selected = ""
        ),
        # Secondary checkbox group for detailed options (hidden by default)
        conditionalPanel(
          condition = "input.sanitaer_main == 'Verbesserte Sanitärausstattung'",
          checkboxGroupInput(
            inputId = "sanitaer_details",
            label = "Welche Verbesserungen sind vorhanden?",
            choices = ref_sanitaer[-1]
          )
        )
      ),
      column(
        width = 6,
        # Hint text in the right column
        div(
          id = "sanitaer_hint",
          "Bitte wählen Sie die Sanitärausstattung der Immobilie aus. ",
          "Eine verbesserte Sanitärausstattung führt bei mindestens ",
          "drei Angaben zu einem Zuschlag von +6%. Ohne besondere ",
          "Ausstattung bleibt der Faktor bei 0%."
        ),
        # Output for sanitär factor
        htmlOutput("sanitaer_factor")
      )
    ),
    #---- Ausstattungsauswahl -----
    fluidRow(
      class = "framed-row",
      id = "ausstattung_row", # Unique ID for styling
      column(
        width = 6,
        # Primary dropdown for Ausstattung options
        selectInput(
          inputId = "ausstattung_main",
          label = "Ausstattung",
          choices = c(
            "", # Empty value for initial state
            "Keine besondere Ausstattung",
            "Besonderheiten in der Ausstattung"
          ),
          selected = "" # Start with no selection
        ),
        # Secondary checkbox group for detailed options (hidden by default)
        conditionalPanel(
          condition = paste("input.ausstattung_main == ",
                            "'Besonderheiten in der Ausstattung'"),
          checkboxGroupInput(
            inputId = "ausstattung_details",
            label = "Welche Ausstattungsmerkmale treffen zu?",
            choices = ref_ausstattung %>%
              dplyr::filter(Option != "Keine besondere Ausstattung") %>%
              dplyr::pull(Option) # all options except the first one
          )
        )
      ),
      column(
        width = 6,
        # Hint text for Ausstattung section
        div(
          id = "ausstattung_hint",
          "Bitte wählen Sie die Besonderheiten der Ausstattung aus. ",
          "Jede Auswahl trägt mit einem individuellen Faktor zum ",
          "Ausstattungsfaktor bei."
        ),
        # Display Ausstattung factor as output
        htmlOutput("ausstattung_factor")
      )
    ),
    #---- Zusammenfassung und Spannengrenzen -----
    fluidRow(
      class = "framed-row",
      id = "zusammenfassung_row",
      column(
        width = 6,
        # Slider for fine-tuning groesse
        sliderInput(
          inputId = "slider_groesse",
          label = "Feinanpassung der Wohnungsgröße (in m²):",
          min = 25,
          max = 125,
          value = 70,
          step = 0.1
        )
      ),
      column(
        width = 6,
        # Top sub-row for the sum of factors
        div(
          htmlOutput("sum_factors"), # Display globals$sum$info_text
          style = "font-weight: bold; text-align: center; margin-bottom: 10px;"
        ),
        # Captions for the sub-columns
        fluidRow(
          column(width = 4, div("Untere Grenze",
                                style = "text-align: center;")),
          column(width = 4, div(HTML("<strong>Ortsüblich</strong>"),
                                style = "text-align: center;")),
          column(width = 4, div("Obere Grenze",
                                style = "text-align: center;"))
        ),
        # Bottom sub-row for the three sub-columns
        fluidRow(
          column(
            width = 4,
            div(
              htmlOutput("lower_limit",
                         style = "text-align: center;"),
              br(),
              htmlOutput("lower_limit_final",
                         style = "text-align: center;")
            )
          ),
          column(
            width = 4,
            div(
              htmlOutput("typical_value",
                         style = "text-align: center; font-weight: bold;"),
              br(),
              htmlOutput("typical_value_final",
                         style = "text-align: center; font-weight: bold;")
            )
          ),
          column(
            width = 4,
            div(
              htmlOutput("upper_limit",
                         style = "text-align: center;"),
              br(),
              htmlOutput("upper_limit_final",
                         style = "text-align: center;")
            )
          )
        )
      )
    ),
    fluidRow(
      class = "framed-row",
      id = "report_row", # Unique ID for styling
      column(
        width = 12,
        # Button for downloading the report
        downloadButton(
          outputId = "downloadReport",
          label = "Berechnungsprotokoll herunterladen", # Button label
          class = "btn-primary" # Optional styling
        )
      )
    )
  )
)



#------ server: Berechnungslogik ------
server <- function(input, output, session) {
  #------ Globale Variablen ------
  # Eine geschachtelte Liste globaler Variablen wird angelegt, um die
  # Eingaben und Berechnungen der verschiedenen Kategorien zu speichern.
  # Die Liste wird in globals gespeichert und enthält die folgenden
  # Kategorien: groesse, adresse, baujahr, renovierung, sanitaer, ausstattung
  # Insbesondere werden die Eingaben, Faktoren und Informationen für jede
  # Kategorie gespeichert und so für die Verwendung z.B. im Print-Report
  # bereitgestellt. Die Liste kann en bloc in der YAML head matter des
  # RMarkdown-Reports über den params-Parameter übergeben werden.
  globals <- reactiveValues(
    # Section: Wohnungsgröße
    groesse = list(
      selection = NULL,  # Eingabe/Auswahl (z.B., "50-60 m²")
      factor = 1,        # Resultierender Faktor ( bei der Größe immer 100%)
      info_text = "",    # Dynamischer Text für die Anzeige
      lo = NA_real_,     # Basiswert untere Spannengrenze
      mid = NA_real_,    # Basiswert Ortsübliche Vergleichsmiete
      hi = NA_real_,     # Basiswert obere Spannengrenze
      detail = NA_real_  # Genaue Wohnungsgröße (z.B., 55.5 m²)
    ),
    # Section: Adresse
    adresse = list(
      selection = NULL,  # Eingabe/Auswahl (z.B., "Innstraße 76")
      factor = 0,        # Abschlag 0%, -6% oder -10%, abhängig von Wohnlage
      Lage = NULL,       # Wohnlage (A, B, C)
      info_text = ""     # Dynamischer Text für die Anzeige
    ),
    # Section: Baujahr
    baujahr = list(
      selection = NULL,  # Eingabe/Auswahl (z.B., "1918 und älter")
      factor = 0,        # Zu-/Abschlag in %, abhängig vom Baujahr
      info_text = "Bitte wählen Sie ein Baujahr aus."
    ),
    # Section: Renovierung
    renovierung = list(
      selection = NULL,
      factor = 0,
      info_text = "Bitte wählen Sie Renovierungsmaßnahmen aus."
    ),
    # Section: Sanitär
    sanitaer = list(
      selection = NULL,
      factor = 0,
      info_text = "Bitte wählen Sie die Sanitärausstattung aus."
    ),
    # Section: Ausstattung
    ausstattung = list(
      selection = NULL,
      factor = 0,
      info_text = "Bitte wählen Sie die Besonderheiten der Ausstattung aus."
    ),
    # Section: Aggregation
    sum = list(
      factor = 0,
      breakdown = list(
        groesse = 0,
        adresse = 0,
        baujahr = 0,
        renovierung = 0,
        sanitaer = 0,
        ausstattung = 0
      ),
      info_text = "Gesamtsumme der Faktoren noch nicht berechnet."
    )
  )
  #---- Alias für Funktion
  fv <- format_value # 2 Nachkommastellen, opt. +_Zeichen, opt. Einheit

  #------ Hinweistexte bei fehlender Eingabe -----
  # Überprüfung, ob in einer Kategorie eine verwertbare Eingabe erfolgt ist
  # Falls ja, wird der jeweilige kategorie_hint-Text ausgeblendet und der
  # fluidRow-Background von rot (default) auf grün umgestellt.
  # Die Funktion check_if_complete() kommt aus functions.R und wird für jede
  # Kategorie einzeln aufgerufen.
  check_if_complete(input,
                   "adresse",
                    "adresse_hint",
                    "adresse_row",
                    session)
  check_if_complete(input,
                    "groesse",
                    "groesse_hint",
                    "groesse_row",
                    session)
  check_if_complete(input,
                    "baujahr",
                    "baujahr_hint",
                    "baujahr_row",
                    session)
  check_if_complete(input,
                    "renovierung_main",
                    "renovierung_hint",
                    "renovierung_row",
                    session)
  check_if_complete(input,
                    "sanitaer_main",
                    "sanitaer_hint",
                    "sanitaer_row",
                    session)
  check_if_complete(input,
                    "ausstattung_main",
                    "ausstattung_hint",
                    "ausstattung_row",
                    session)

  #------ Section 'groesse' -----
  # Observe input$groesse and update globals accordingly
  observeEvent(input$groesse, {
    if (!is.null(input$groesse) && input$groesse != "") {
      selected_groesse <- ref_groesse %>% filter(options == input$groesse)
      if (nrow(selected_groesse) > 0) {
        globals$groesse$selection <- selected_groesse$options
        globals$groesse$factor <- selected_groesse$mid
        globals$groesse$info_text <- "Ausgangswert für Zu-/Abschläge:"
        globals$groesse$lo <- selected_groesse$low
        globals$groesse$mid <- selected_groesse$mid
        globals$groesse$hi <- selected_groesse$hi
        updateSliderInput(
          session,
          "slider_groesse",
          min = selected_groesse$von,
          max = selected_groesse$bis_unter - 0.1,
          value = (selected_groesse$von + selected_groesse$bis_unter) / 2
        )
      }
    }
  })
  # Observe the slider input and update the globals accordingly
  observeEvent(input$slider_groesse, {
    globals$groesse$detail <- input$slider_groesse
  })
  # Render all groesse outputs based on globals
  output$groesse_info <- renderText({
    globals$groesse$info_text
  })
  output$groesse_ug <- renderText({
    if (!is.na(globals$groesse$lo)) {
      fv(globals$groesse$lo, " €/m²")
    } else {
      "->"
    }
  })
  output$groesse_oue <- renderText({
    if (!is.na(globals$groesse$mid)) {
      fv(globals$groesse$mid, " €/m²")
    } else {
      "Auswahl fehlt"
    }
  })
  output$groesse_og <- renderText({
    if (!is.na(globals$groesse$hi)) {
      fv(globals$groesse$hi, " €/m²")
    } else {
      "<-"
    }
  })
  #------ Section 'adresse' -----
  # Observe input$adresse and update globals accordingly
  observeEvent(input$adresse, {
    if (!is.null(input$adresse) && input$adresse != "") {
      # Find the selected address details
      selected_adresse <- ref_adresse %>% filter(STRASSE_HS == input$adresse)
      if (nrow(selected_adresse) > 0) {
        globals$adresse$selection <- selected_adresse$STRASSE_HS
        globals$adresse$factor <- selected_adresse$WL_FAKTOR
        globals$adresse$Lage <- selected_adresse$WL_2024
        globals$adresse$info_text <- paste0(
          "Die Adresse liegt in Wohnlage <strong>", selected_adresse$WL_2024,
          "</strong>, Lagenfaktor <strong>",
          format_value(selected_adresse$WL_FAKTOR * 100, " %", add_plus = TRUE),
          "</strong><br>"
        )
        # Filter data for the specified street
        selected_street <- sub(" [0-9]+.*$", "", globals$adresse$selection)
        filtered_data <- adr2024 %>% filter(STRASSE == selected_street)
        req(nrow(filtered_data) > 0)
        # Create and store the Leaflet map in globals
        coords <- st_coordinates(filtered_data)
        lng_min <- min(coords[, 1], na.rm = TRUE)
        lng_max <- max(coords[, 1], na.rm = TRUE)
        lat_min <- min(coords[, 2], na.rm = TRUE)
        lat_max <- max(coords[, 2], na.rm = TRUE)
        globals$adresse$map <- leaflet(data = filtered_data) %>%
          addTiles() %>%
          addCircleMarkers(
            radius = 4,
            color = ~ ifelse(WL_2024 %in% names(wl_colors),
                             wl_colors[WL_2024],
                             "black"),
            stroke = FALSE,
            fillOpacity = 0.8,
            label = ~ paste0(STRASSE_HS, " (", WL_2024, ")"),
            group = "All Addresses"
          ) %>%
          addCircleMarkers(
            data = filtered_data %>% filter(
              STRASSE_HS == globals$adresse$selection
              ),
            radius = 6,
            color = "yellow",
            stroke = TRUE,
            weight = 2,
            fillOpacity = 1,
            label = ~ paste(STRASSE_HS, "(Selected)")
          ) %>%
          fitBounds(
            lng1 = lng_min, lat1 = lat_min,
            lng2 = lng_max, lat2 = lat_max,
            options = list(padding = c(20, 20, 20, 50))
          ) %>%
          addLegend(
            position = "bottomleft",
            colors = c("yellow",
                       wl_colors["A"],
                       wl_colors["B"],
                       wl_colors["C"]),
            labels = c("Ausgewählt",
                       "Lage A",
                       "Lage B",
                       "Lage C"),
            title = "Wohnlagen",
            opacity = 1
          )
      }
    } else {
      globals$adresse$selection <- NULL
      globals$adresse$factor <- 0
      globals$adresse$Lage <- NULL
      globals$adresse$map <- NULL
    }
  })
  updateSelectizeInput(
    session = session,
    inputId = "adresse",
    choices = c("", ref_adresse$STRASSE_HS), # Add empty placeholder
    server = TRUE  # Enable server-side processing
  )
  output$adresse_factor <- renderText({
    globals$adresse$info_text
  })
  # Render the Leaflet map using the globalized map
  output$adresse_map <- renderLeaflet({
    req(globals$adresse$map) # Ensure the map exists
    globals$adresse$map
  })
  #------ Section 'baujahr' -----
  # 'baujahr is handled exactly the same way as 'adresse':
  # Observe input$baujahr and update globals accordingly
  # Use -> Auswahl fehlt <- as default value for missing selections
  observeEvent(input$baujahr, {
    if (!is.null(input$baujahr) && input$baujahr != "") {
      selected_baujahr <- ref_baujahr %>% filter(Baujahr == input$baujahr)
      if (nrow(selected_baujahr) > 0) {
        globals$baujahr$selection <- selected_baujahr$Baujahr
        globals$baujahr$factor <- selected_baujahr$Faktor
        globals$baujahr$info_text <- paste0(
          "Baujahr: <strong>", selected_baujahr$Baujahr,
          "</strong>, Baujahresfaktor <strong>",
          format_value(selected_baujahr$Faktor * 100, " %", add_plus = TRUE),
          "</strong>"
        )
      }
    } else {
      # Reset globals if no selection is made
      globals$baujahr$selection <- NULL
      globals$baujahr$factor <- 0
      globals$baujahr$info_text <- ""
    }
  })
  observeEvent(input$baujahr, {
    if (
      input$baujahr >= "1990"
      ) {
      if (
        input$renovierung_main == paste0(
          "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)"
          )
        ) {
        # If "Vollmodernisierung" is selected, reset to no selection
        updateSelectInput(
          session,
          "renovierung_main",
          selected = "" # Reset auf "", nicht "Keine ..."
        )
        globals$renovierung$factor <- 0
        globals$renovierung$info_text <- ""
      }
      # Remove "Vollmodernisierung" from the dropdown
      updateSelectInput(
        session,
        "renovierung_main",
        choices = c(
          "",
          "Keine Sanierung/Renovierung bekannt",
          "Teilrenovierung seit 2013 (mind. 3 Maßnahmen)"
        )
      )
    } else {
      # Add "Vollmodernisierung" back if Baujahr allows it
      updateSelectInput(
        session,
        "renovierung_main",
        choices = c(
          "",
          "Keine Sanierung/Renovierung bekannt",
          "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)",
          "Teilrenovierung seit 2013 (mind. 3 Maßnahmen)"
        )
      )
    }
  })
  output$baujahr_factor <- renderText({
    globals$baujahr$info_text
  })
  #------ Section 'renovierung' -----
  observeEvent(input$renovierung_main, {
    if (
      input$renovierung_main == "Keine Sanierung/Renovierung bekannt"
      ) {
      # Reset details and set globals
      updateCheckboxGroupInput(session,
                               "renovierung_details",
                               selected = character(0))
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- paste0(
        "Keine Renovierung bekannt: <strong>0%</strong>."
      )
      globals$renovierung$selection <- NULL
    } else if (
      input$renovierung_main == paste0(
        "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)"
      )
      ) {
      if (input$baujahr >= "1990") {
        # Notify user about invalid selection and reset dropdown
        showNotification(
          paste0(
            "Vollmodernisierung ist nur bei Baujahr vor 1990 möglich."
            ),
          type = "error")
        updateSelectInput(session, # Reset to empty
                          "renovierung_main",
                          selected = ""
                          )
        globals$renovierung$factor <- 0
        globals$renovierung$info_text <- "" # Reflect incomplete state
        globals$renovierung$selection <- NULL
      } else {
        # Valid Vollmodernisierung selection
        updateCheckboxGroupInput(session,
                                 "renovierung_details",
                                 selected = character(0)
                                 )
        globals$renovierung$factor <- 0.11
        globals$renovierung$info_text <- paste0(
          "Vollmodernisierung: <strong>+11%</strong>."
        )
        globals$renovierung$selection <- "Vollmodernisierung"
      }
    } else if (
      input$renovierung_main == "Teilrenovierung seit 2013 (mind. 3 Maßnahmen)"
      ) {
      # Teilrenovierung: Reset factor and wait for details
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- paste0(
        "Bitte wählen Sie die durchgeführten Maßnahmen aus."
      )
      globals$renovierung$selection <- NULL
    } else {
      # Handle empty or invalid input
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- ""
      globals$renovierung$selection <- NULL
    }
  })
  observeEvent(input$renovierung_details, {
    if (
      is.null(input$renovierung_details) ||
      length(input$renovierung_details) < 3
      ) {
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- paste0(
        length(input$renovierung_details),
        " von mind. 3 für einen 6%-Zuschlag ",
        "erforderlichen Maßnahmen: <strong>+-0%</strong>"
      )
    } else {
      globals$renovierung$factor <- 0.06
      globals$renovierung$info_text <- paste0(
        "Teilrenovierung seit 2013 (mind. 3 Maßnahmen): <strong>+6%</strong>"
      )
    }
    # Pass the selected renovation details to the globals
    globals$renovierung$selection <- input$renovierung_details
  })
  output$renovierung_factor <- renderText({
    globals$renovierung$info_text
  })
  #------ Section 'sanitaer' -----
  observeEvent(input$sanitaer_main, {
    if (
      is.null(input$sanitaer_main) ||
      input$sanitaer_main == ""
      ) {
      # No selection: Reset globals
      updateCheckboxGroupInput(session,
                               "sanitaer_details",
                               selected = character(0))
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- ""
      globals$sanitaer$selection <- NULL
    } else if (input$sanitaer_main == "Keine besondere Sanitärausstattung") {
      # Reset details and finalize with 0% for "Keine"
      updateCheckboxGroupInput(session,
                               "sanitaer_details",
                               selected = character(0))
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- paste0(
        "Keine besondere Sanitärausstattung: <strong>+-0%</strong>."
      )
      globals$sanitaer$selection <- NULL
    } else if (input$sanitaer_main == "Verbesserte Sanitärausstattung") {
      # Reset factor and wait for details
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- "Bitte wählen Sie die Verbesserungen aus."
      globals$sanitaer$selection <- NULL
    }
  })
  observeEvent(input$sanitaer_details, {
    if (
      is.null(input$sanitaer_details) ||
      length(input$sanitaer_details) < 3
      ) {
      # Fewer than 3 valid selections
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- paste0(
        length(input$sanitaer_details),
        " von mind. 3 für einen 6%-Zuschlag",
        " erforderlichen Verbesserungen: <strong>+-0%</strong>"
        )
    } else {
      # At least 3 selections
      globals$sanitaer$factor <- 0.06
      globals$sanitaer$info_text <- paste0(
        "An mind. drei Positionen verbesserte Sanitärausstattung:",
        " <strong>+6%</strong>"
        )
    }
    # Pass the selected sanitär details to the globals
    globals$sanitaer$selection <- input$sanitaer_details
  })
  output$sanitaer_factor <- renderText({
    globals$sanitaer$info_text
  })
  #------ Section 'ausstattung' -----
  observeEvent(input$ausstattung_main, {
    if (is.null(input$ausstattung_main) || input$ausstattung_main == "") {
      # No selection: Reset globals
      updateCheckboxGroupInput(session,
                               "ausstattung_details",
                               selected = character(0))
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- ""
      globals$ausstattung$selection <- NULL
    } else if (input$ausstattung_main == "Keine besondere Ausstattung") {
      # Reset details and finalize with 0% for "Keine"
      updateCheckboxGroupInput(session,
                               "ausstattung_details",
                               selected = character(0))
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- paste0(
        "Keine besondere Ausstattung: <strong>0%</strong>."
      )
      globals$ausstattung$selection <- NULL
    } else if (input$ausstattung_main == "Besonderheiten in der Ausstattung") {
      # Reset factor and wait for details
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- paste0(
        "Bitte wählen Sie die Ausstattungsmerkmale aus."
      )
      globals$ausstattung$selection <- NULL
    }
  })
  observeEvent(input$ausstattung_details, {
    if (
      is.null(input$ausstattung_details) ||
      length(input$ausstattung_details) == 0
      ) {
      # No selections
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- paste0(
        "Für die Ausstattung wurden keine Besonderheiten ausgewählt."
        )
      globals$ausstattung$selection <- NULL
    } else {
      # Calculate the total factor based on selected options
      selected_factors <- ref_ausstattung %>%
        dplyr::filter(Option %in% input$ausstattung_details) %>%
        dplyr::pull(Factor)
      total_factor <- sum(selected_factors, na.rm = TRUE)
      globals$ausstattung$factor <- total_factor
      globals$ausstattung$info_text <- paste0(
        "Anpassungsfaktor bei einer Auswahl von ",
        length(input$ausstattung_details), " Merkmal(en): <strong>",
        format_value(total_factor * 100, " %</strong>", add_plus = TRUE)
      )
      globals$ausstattung$selection <- input$ausstattung_details
    }
  })
  output$ausstattung_factor <- renderText({
    globals$ausstattung$info_text
  })
  #------ Section 'zusammenfassung' ------
  #------ Section 'zusammenfassung' ------
  # Sum of all factors
  observe({
    sum_factors <- sum(
      globals$adresse$factor,
      globals$baujahr$factor,
      globals$renovierung$factor,
      globals$sanitaer$factor,
      globals$ausstattung$factor,
      na.rm = TRUE
    )
    # Store the aggregated factor in globals
    globals$sum$factor <- sum_factors
    globals$sum$info_text <- paste(
      "Summe der Faktoren:",
      format_value(sum_factors * 100, " %", add_plus = TRUE)
    )
  })
  # Render sum of factors for display
  output$sum_factors <- renderText({
    req(globals$sum$factor)
    globals$sum$info_text
  })
  # Intermediate Values (groesse_result * (1 + sum_factors))
  output$lower_limit <- renderText({
    req(globals$groesse$lo, globals$sum$factor)
    adjusted_lo <- globals$groesse$lo * (1 + globals$sum$factor)
    format_value(adjusted_lo, " €/m²")
  })
  output$typical_value <- renderText({
    req(globals$groesse$mid, globals$sum$factor)
    adjusted_mid <- globals$groesse$mid * (1 + globals$sum$factor)
    format_value(adjusted_mid, " €/m²")
  })
  output$upper_limit <- renderText({
    req(globals$groesse$hi, globals$sum$factor)
    adjusted_hi <- globals$groesse$hi * (1 + globals$sum$factor)
    format_value(adjusted_hi, " €/m²")
  })
  # Final Values (intermediate values * slider_groesse)
  output$lower_limit_final <- renderText({
    req(globals$groesse$lo,
        globals$sum$factor,
        input$slider_groesse)
    final_lo <- globals$groesse$lo *
      (1 + globals$sum$factor) *
      input$slider_groesse
    format_value(final_lo, " €")
  })
  output$typical_value_final <- renderText({
    req(globals$groesse$mid,
        globals$sum$factor,
        input$slider_groesse)
    final_mid <- globals$groesse$mid *
      (1 + globals$sum$factor) *
      input$slider_groesse
    format_value(final_mid, " €")
  })
  output$upper_limit_final <- renderText({
    req(globals$groesse$hi,
        globals$sum$factor,
        input$slider_groesse)
    final_hi <- globals$groesse$hi *
      (1 + globals$sum$factor) *
      input$slider_groesse
    format_value(final_hi, " €")
  })
  #----- Render Report -----
  output$downloadReport <- downloadHandler(
    filename = function() {
      # Ersetze Leerzeichen in der Adresse mit '_' ...
      address <- if (!is.null(globals$adresse$selection)) {
        gsub("\\s+", "_", globals$adresse$selection)
      } else {
        "Unbekannte_Adresse" # Fallback if no address is selected
      }
      paste0("Mietspiegelberechnung_", address, ".pdf")
    },
    content = function(file) {
      rmarkdown::render(
        input = "Report.Rmd",
        output_file = file,
        params = list(globals = reactiveValuesToList(globals)),  # Pass globals
        envir = new.env(parent = globalenv())  # Use an isolated environment
      )
    }
  )
}
shinyApp(ui = ui, server = server)
