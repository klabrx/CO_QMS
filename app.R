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
#library(mapview)
library(shinyjs)

source("data_sources.R")
source("functions.R")

#----- 

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
    # Größenbereiche werden in data_sources.R aus externer Datei geladen und im
    # tibble (47 x 6) 'ref_groesse' gespeichert. Struktur des tibbles:
    # von      : int [1:47] 25 26 27 28 29 30 31 32 33 34 ...
    # bis_unter: int [1:47] 26 27 28 29 30 31 32 33 34 35 ...
    # low      : num [1:47] 9.84 9.66 9.48 9.32 9.17 9.03 8.9 8.78 8.67...
    # med      : num [1:47] 11.9 11.6 11.4 11.2 11.1 ...
    # hi       : num [1:47] 13.9 13.6 13.4 13.1 12.9 ...
    # options  : chr [1:47] "25 bis unter 26 m²" "26 bis unter 27 m²" ...
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
          HTML("<strong>Untere Grenze</strong>"),
          htmlOutput("groesse_ug")
        )
      ),
      column(
        width = 2, br(),
        div(
          HTML("<strong>Ortsüblich</strong>"),
          htmlOutput("groesse_oue")
        )
      ),
      column(
        width = 2, br(),
        div(
          HTML("<strong>Obere Grenze</strong>"),
          htmlOutput("groesse_og")
        )
      )
    ),
    #---- Adressenauswahl ----
    # Adressen werden in data_sources.R aus externer Datei geladen und im
    # tibble (12.466 x 7) 'ref_adresse' gespeichert. Struktur des tibbles:
    #  $ STRASSE   : chr [1:12466] "Abteistraße" "Abteistraße" "Abteistraße" ...
    # $ ADRESS_ID : chr [1:12466] "303000002" "303000004" "303000006" ...
    # $ STRASSE_HS: chr [1:12466] "Abteistraße 2" "Abteistraße 4" ...
    # $ PLZ       : num [1:12466] 94034 94034 94034 94034 94034 ...
    # $ STADTTEIL : chr [1:12466] "Grubweg" "Grubweg" "Grubweg" "Grubweg" ...
    # $ WL_2024   : chr [1:12466] "B" "B" "B" "B" ...
    # $ WL_FAKTOR : num [1:12466] -0.07 -0.07 -0.07 -0.07 -0.07 -0.07 -0.07 ...
    fluidRow(
      class = "framed-row",
      id = "adresse_row",
      # Left column: Selectize input and hint
      column(
        width = 6,
        selectizeInput(
          inputId = "adresse",
          label = "Adresse",
          choices = c("", ref_adresse$STRASSE_HS), # Add an empty string for the placeholder
          multiple = FALSE
        ),
        div(
          id = "adresse_hint",
          "Bitte geben Sie die gesuchte Adresse in das ",
          "Suchfeld ein. Auch Adressbestandteile werden erkannt, z.B. führt ",
          "Sie die Eingabe 'inn 76' direkt zur Innstraße 76."
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
          choices = c("", ref_baujahr$Baujahr), # Add empty string for placeholder
          selected = NULL
        )
      ),
      
      # Right column: Hint and Baujahr factor output
      column(
        width = 6,
        div(
          id = "baujahr_hint",
          "Bitte wählen Sie das Baujahr der Immobilie aus. Der Baujahresfaktor ",
          "wird automatisch berechnet und angezeigt."
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
            "Teilrenovierung"
          ),
          selected = ""
        ),
        # Step 2: Checkboxes for Teilrenovierung (hidden by default)
        conditionalPanel(
          condition = "input.renovierung_main == 'Teilrenovierung'",
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
            label = "Welche Sanitärausstattungen wurden hinzugefügt?",
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
          "drei Maßnahmen zu einem Zuschlag von +6%. Ohne besondere ",
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
          condition = "input.ausstattung_main == 'Besonderheiten in der Ausstattung'",
          checkboxGroupInput(
            inputId = "ausstattung_details",
            label = "Welche Ausstattungsmerkmale treffen zu?",
            choices = ref_ausstattung %>% 
              dplyr::filter(Option != "Keine besondere Ausstattung") %>% 
              dplyr::pull(Option) # Exclude the first option and extract the labels          )
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
    )
    )
  )
)
    
    #---- Zusammenfassung und Spannengrenzen -----

server <- function(input, output, session) {
  updateSelectInput(
    session,
    "renovierung_main",
    selected = ""
  )
  
  
  
#---- aliases for functions
  fv <- format_value
  
  #----- Define global variables ------
  globals <- reactiveValues(
    # Section: Wohnungsgröße
    groesse = list(
      selection = NULL,  # Selected size category (e.g., "50-60 m²")
      factor = 1,        # Default factor for size (100%)
      info_text = NULL,  # Informational text for display
      lo = NA_real_,     # Lower range value
      mid = NA_real_,    # Midpoint value
      hi = NA_real_      # Upper range value
    ),
    
    # Section: Adresse
    adresse = list(
      selection = NULL,  # Selected address
      factor = 0,        # Address-specific factor
      Lage = NULL,       # Wohnlage category (e.g., A, B, C)
      info_text = "Bitte wählen Sie eine Adresse aus."  # Text for display
    ),
    
    # Section: Baujahr
    baujahr = list(
      selection = NULL,  # Selected Baujahr
      factor = 0,        # Baujahr-specific factor
      info_text = "Bitte wählen Sie ein Baujahr aus."  # Text for display
    ),
    
    # Section: Renovierung
    renovierung = list(
      selection = NULL,  # Selected renovierung measures
      factor = 0,        # Renovierung-specific factor (e.g., +6%)
      info_text = "Bitte wählen Sie Renovierungsmaßnahmen aus."  # Text for display
    ),
    
    # Section: Sanitär
    sanitaer = list(
      factor = 0,        # Sanitär-specific factor
      info_text = "Bitte wählen Sie die Sanitärausstattung aus."  # Text for display
    ),
    
    # Section: Ausstattung
    ausstattung = list(
      selection = NULL,  # Selected ausstattung measures
      factor = 0,        # Sum of factors for selected measures
      info_text = "Bitte wählen Sie die Besonderheiten der Ausstattung aus."  # Text for display
    ),
    
    # Section: Aggregation
    sum = list(
      factor = 0,        # Aggregated factor across all inputs
      info_text = "Gesamtsumme der Faktoren noch nicht berechnet."  # Summary info text
    )
  )
  
  

  #---- Hinweistexte bei fehlender Eingabe -----
  # Hints werden ausgeblendet, sobald eine gültige Eingabe erfolgt ist.
  showHintIfEmpty(input, "adresse", "adresse_hint", "adresse_row", session)
  showHintIfEmpty(input, "groesse", "groesse_hint", "groesse_row", session)
  showHintIfEmpty(input, "baujahr", "baujahr_hint", "baujahr_row", session)
  showHintIfEmpty(input, "renovierung_main", "renovierung_hint", "renovierung_row", session)
  showHintIfEmpty(input, "sanitaer_main", "sanitaer_hint", "sanitaer_row",  session)
  showHintIfEmpty(input, "ausstattung_main", "ausstattung_hint", "ausstattung_row", session)
  
#----- Section 'groesse' -----
  # Observe input$groesse and update globals accordingly
  observeEvent(input$groesse, {
    if (!is.null(input$groesse) && input$groesse != "") {
      selected_groesse <- ref_groesse %>% filter(options == input$groesse)
      if (nrow(selected_groesse) > 0) {
        globals$groesse$selection <- selected_groesse$options
        globals$groesse$factor <- selected_groesse$mid
        globals$groesse$info_text <- "<br>Basiswert:"
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
  
  

#----- Section 'adresse' -----
  
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
            color = ~ ifelse(WL_2024 %in% names(wl_colors), wl_colors[WL_2024], "black"),
            stroke = FALSE,
            fillOpacity = 0.8,
            label = ~ paste0(STRASSE_HS, " (", WL_2024, ")"),
            group = "All Addresses"
          ) %>%
          addCircleMarkers(
            data = filtered_data %>% filter(STRASSE_HS == globals$adresse$selection),
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
            colors = c("yellow", wl_colors["A"], wl_colors["B"], wl_colors["C"]),
            labels = c("Ausgewählt", "Lage A", "Lage B", "Lage C"),
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
  
  output$adresse_factor <- renderText({
    globals$adresse$info_text
  })

  # Render the Leaflet map using the globalized map
  output$adresse_map <- renderLeaflet({
    req(globals$adresse$map) # Ensure the map exists
    globals$adresse$map
  })
  
#----- Section 'baujahr' -----
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
    if (input$baujahr >= "1990") {
      if (input$renovierung_main == "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)") {
        # If "Vollmodernisierung" is selected, reset to no selection
        updateSelectInput(
          session,
          "renovierung_main",
          selected = "" # Reset to empty, not "Keine Sanierung/Renovierung bekannt"
        )
        globals$renovierung$factor <- 0
        globals$renovierung$info_text <- "-> Auswahl fehlt <-" # Reflect no selection
      }
      
      # Remove "Vollmodernisierung" from the dropdown
      updateSelectInput(
        session,
        "renovierung_main",
        choices = c(
          "",
          "Keine Sanierung/Renovierung bekannt",
          "Teilrenovierung"
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
          "Teilrenovierung"
        )
      )
    }
  })
  
  
  
  output$baujahr_factor <- renderText({
    globals$baujahr$info_text
  })
  
  
  
#----- Section 'renovierung' -----
  observeEvent(input$renovierung_main, {
    if (input$renovierung_main == "Keine Sanierung/Renovierung bekannt") {
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- "Keine Renovierung bekannt: 0%."
    } else if (input$renovierung_main == "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)") {
      if (input$baujahr >= "1990") {
        # Notify user about invalid selection and reset dropdown
        showNotification("Vollmodernisierung ist nur bei Baujahr vor 1990 möglich.", type = "error")
        updateSelectInput(session, "renovierung_main", selected = "") # Reset to empty
        globals$renovierung$factor <- 0
        globals$renovierung$info_text <- "-> Auswahl fehlt <-" # Reflect incomplete state
      } else {
        globals$renovierung$factor <- 0.11
        globals$renovierung$info_text <- "Vollmodernisierung: +11%."
      }
    } else if (input$renovierung_main == "Teilrenovierung") {
      # Teilrenovierung: Reset factor and wait for details
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- "Bitte wählen Sie die durchgeführten Maßnahmen aus."
    } else {
      # Handle empty or invalid input
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- "-> Auswahl fehlt <-"
    }
  })
  
  
  observeEvent(input$renovierung_details, {
    if (is.null(input$renovierung_details) || length(input$renovierung_details) < 3) {
      globals$renovierung$factor <- 0
      globals$renovierung$info_text <- "Für +6% sind mindestens drei Maßnahmen erforderlich."
    } else {
      globals$renovierung$factor <- 0.06
      globals$renovierung$info_text <- paste(
        "Teilrenovierung: +6% (", length(input$renovierung_details), " Maßnahmen ausgewählt)"
      )
    }
  })
  
  output$renovierung_factor <- renderText({
    globals$renovierung$info_text
  })
  
  
  
# #----- Section 'sanitaer' -----
  observeEvent(input$sanitaer_main, {
    if (is.null(input$sanitaer_main) || input$sanitaer_main == "") {
      # No selection: Reset globals
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- "-> Auswahl fehlt <-"
      #shinyjs::show("sanitaer_hint")
    } else if (input$sanitaer_main == "Keine besondere Sanitärausstattung") {
      # Finalize with 0% for "Keine"
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- "Keine besondere Sanitärausstattung: 0%."
      #shinyjs::hide("sanitaer_hint")
    } else if (input$sanitaer_main == "Verbesserte Sanitärausstattung") {
      # Show detailed options for "Verbesserte"
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- "Bitte wählen Sie die Verbesserungen aus."
      #shinyjs::hide("sanitaer_hint")
    }
  })
  
  observeEvent(input$sanitaer_details, {
    if (is.null(input$sanitaer_details) || length(input$sanitaer_details) < 3) {
      # Fewer than 3 valid selections
      globals$sanitaer$factor <- 0
      globals$sanitaer$info_text <- "Für +6% sind mindestens drei Maßnahmen erforderlich."
    } else {
      # At least 3 selections
      globals$sanitaer$factor <- 0.06
      globals$sanitaer$info_text <- paste(
        "Verbesserte Sanitärausstattung: +6% (", length(input$sanitaer_details), " Maßnahmen ausgewählt)"
      )
    }
  })
  
  
  output$sanitaer_factor <- renderText({
    globals$sanitaer$info_text
  })
    
# #----- Section 'ausstattung' -----
  observeEvent(input$ausstattung_main, {
    if (is.null(input$ausstattung_main) || input$ausstattung_main == "") {
      # No selection: Reset globals
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- "-> Auswahl fehlt <-"
      #shinyjs::show("ausstattung_hint")
    } else if (input$ausstattung_main == "Keine besondere Ausstattung") {
      # Finalize with 0% for "Keine"
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- "Keine besondere Ausstattung: 0%."
      #shinyjs::hide("ausstattung_hint")
    } else if (input$ausstattung_main == "Besonderheiten in der Ausstattung") {
      # Wait for detailed options
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- "Bitte wählen Sie die Ausstattungsmerkmale aus."
      #shinyjs::hide("ausstattung_hint")
    }
  })
  
  observeEvent(input$ausstattung_details, {
    if (is.null(input$ausstattung_details) || length(input$ausstattung_details) == 0) {
      # No selections
      globals$ausstattung$factor <- 0
      globals$ausstattung$info_text <- "Für die Ausstattung wurden keine Besonderheiten ausgewählt."
    } else {
      # Calculate the total factor based on selected options
      selected_factors <- ref_ausstattung %>%
        dplyr::filter(Option %in% input$ausstattung_details) %>%
        dplyr::pull(Factor)
      
      total_factor <- sum(selected_factors, na.rm = TRUE)
      globals$ausstattung$factor <- total_factor
      globals$ausstattung$info_text <- paste0(
        "Ausstattungsfaktor: ", format_value(total_factor * 100, " %", add_plus = TRUE),
        " (", length(input$ausstattung_details), " Merkmale ausgewählt)"
      )
    }
  })
  
  
  output$ausstattung_factor <- renderText({
    globals$ausstattung$info_text
  })
  
  

#   #----- Render Report -----
#   # Render the report as a downloadable HTML file, using the globals to pass the
#   # necessary information to the report. The report is rendered using the
#   # rmarkdown::render function, which takes the input file (report.Rmd) and
#   # the output file (report.html) as arguments. The globals are passed to the
#   # report using the params argument, which is a list of named parameters.
#   # The report will be rendered when the download button is clicked.
#   # 
#   output$downloadReport <- downloadHandler(
#     filename = function() {
#       "report.html"
#     },
#     content = function(file) {
#       rmarkdown::render(
#         input = "report.Rmd",
#         output_file = file,
#         params = list(globals = reactiveValuesToList(globals)),  # Pass globals
#         envir = new.env(parent = globalenv())  # Use an isolated environment
#       )
#     }
#   )
#   
#   

}
shinyApp(ui = ui, server = server)
