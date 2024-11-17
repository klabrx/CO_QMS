
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
source("data_sources.R")
source("functions.R")
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs for JavaScript interactions
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
      column(width = 4,
        selectInput("groesse", "Wohnungsgröße (m²)",
          c("", ref_groesse$options),
          selectize = TRUE
        ),
        div(id = "groesse_hint",
          "Bitte wählen Sie hier den gesuchten Größenbereich aus ",
          "(Wohnfläche in m² lt. Mietvertrag), ",
          "z.B. '25 bis unter 26 m²' "
        ),
      ),
      column(width = 2, br(),
             htmlOutput("groesse_info")
      ),
      column(width = 2, br(),
             div(HTML("<strong>Untere Grenze</strong>"),
                 htmlOutput("groesse_ug")
              )
      ),
      column(width = 2, br(),
             div(HTML("<strong>Ortsüblich</strong>"),
                 htmlOutput("groesse_oue")
              )
      ),
      column(width = 2, br(),
             div(HTML("<strong>Obere Grenze</strong>"),
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
      # linke Spalte, Breite 4 von 12, mit selectizeInput für Adressauswahl
      # und darunter div für Hinweis (wird nach gültiger Eingabe in das Feld
      # ausgeblendet)
      column(
        width = 4,
        selectizeInput("adresse", "Adresse",
                       choices = c("", ref_adresse$STRASSE_HS),
                       multiple = FALSE),
        div(id = "adresse_hint",
            "Bitte geben Sie die gesuchte Adresse in das ",
            "Suchfeld ein. Auch Adressbestandteile werden erkannt, z.B. führt ",
            "Sie die Eingabe 'inn 76' direkt zur Innstraße 76."
            )
      ),
      column(
        width = 8,
        fluidRow(
          column(width = 3, br(), htmlOutput("adresse_info")),
          column(width = 3, br(), htmlOutput("adresse_ug")),
          column(width = 3, br(), htmlOutput("adresse_oue")),
          column(width = 3, br(), htmlOutput("adresse_og"))
        ),
        fluidRow(
          column(
            width = 12,
            leafletOutput("adresse_map", height = "200px")
          )
        )
      )
    ),
    #---- Baujahrsauswahl -----
      fluidRow(
        class = "framed-row",
        column(width = 4, selectInput("baujahr", "Baujahr",
                                      c("", ref_baujahr$Baujahr),
                                      selectize = TRUE),
               div(id = "baujahr_hint", "Bitte geben Sie hier den ",
                   "Baujahresbereich des Gebäudes an, z.B. 'bis 1918' ",
                   "oder '1946 - 1977'."
                   )
        ),
        column(width = 2, br(), htmlOutput("baujahr_info")),
        column(width = 2, br(), htmlOutput("baujahr_ug")),
        column(width = 2, br(), htmlOutput("baujahr_oue")),
        column(width = 2, br(), htmlOutput("baujahr_og"))
      ),
    #---- Renovierungsauswahl -----
      fluidRow(
        class = "framed-row",
        column(width = 4, selectizeInput("renovierung", "Renovierung",
                                         choices = ref_renovation$Option,
                                         multiple = TRUE),
               div(id = "renovierung_hint", "Bitte wählen sie hier die ",
               "stattgefundenen Renovierungsmaßnahmen (bzw. 'Keine ",
               "Sanierung/Renovierung bekannt') aus. Eine Vollsanierung ",
               "führt nur bei Baujahren vor 1990 zu einem Zuschlag (+11%) ",
               "und kann deshalb bei neueren Gebäuden nicht ausgewählt ",
               "werden. Für einen Teilmodernisierungszuschlag von 6% sind ",
               "mindestens drei Verbesserungen erforderlich.")
        ),
        column(width = 2, br(), htmlOutput("renovierung_info")),
        column(width = 2, br(), htmlOutput("renovierung_ug")),
        column(width = 2, br(), htmlOutput("renovierung_oue")),
        column(width = 2, br(), htmlOutput("renovierung_og"))
      ),
    #---- Sanitärausstattungsauswahl -----
      fluidRow(
        class = "framed-row",
        column(width = 4, selectizeInput("sanitaer", "Sanitärausstattung",
                                         choices = ref_sanitaer,
                                         multiple = TRUE),
               div(id = "sanitaer_hint", "Bitte machen Sie hier Angaben zur ",
               "Sanitärausstattung (bzw. 'Keine besondere ",
               "Sanitärausstattung'). Ein Zuschlag für gehobene ",
               "Sanitärausstattung (6%) erfordert mindestens drei ",
               "Zusatzmerkmale.")
        ),
        column(width = 2, br(), htmlOutput("sanitaer_info")),
        column(width = 2, br(), htmlOutput("sanitaer_ug")),
        column(width = 2, br(), htmlOutput("sanitaer_oue")),
        column(width = 2, br(), htmlOutput("sanitaer_og"))
      ),
    #---- Ausstattungsauswahl -----
      fluidRow(
        class = "framed-row",
        column(width = 4, selectizeInput("ausstattung", "Ausstattung",
                                         choices = names(ref_ausstattung),
                                         multiple = TRUE),
               div(id = "ausstattung_hint", "Bitte wählen Sie aus den ",
               "Ausstattungsmerkmalen die zutreffenden aus. Jedes Merkmal ",
               "sorgt für einen Zu- oder Abschlag, diese werden automatisch ",
               "aufsummiert.")
        ),
        column(width = 2, br(), htmlOutput("ausstattung_info")),
        column(width = 2, br(), htmlOutput("ausstattung_ug")),
        column(width = 2, br(), htmlOutput("ausstattung_oue")),
        column(width = 2, br(), htmlOutput("ausstattung_og"))
      ),
    #---- Zusammenfassung und Spannengrenzen -----
      fluidRow(
        class = "framed-row",
        column(
          width = 6,
          strong("Zusammenfassung und Spannengrenzen:"),
          br(),  # Line break for spacing
          sliderInput(
            inputId = "slider_groesse",
            label = "Feinangabe im Größenbereich (m²)",
            min = 25,  # Default lower limit (to be updated dynamically)
            max = 150,  # Default upper limit (to be updated dynamically)
            value = 50,  # Initial value (can be set as desired)
            step = 0.1
          ),
          br(), downloadButton("downloadReport", "Download Report")
        ),
        column(
          width = 2,
          div(
            HTML("Untere Grenze"),
            br(), br(),  # Line break for spacing
            htmlOutput("sum_ug"),
            br(), # Line break for spacing
            htmlOutput("total_ug")  # New output for total UG
          )
        ),
        column(
          width = 2,
          div(
            HTML("<strong>Ortsüblich</strong>"),
            br(), br(),  # Line break for spacing
            htmlOutput("sum_oue"),
            br(),  # Line break for spacing
            htmlOutput("total_oue")  # New output for total OUE
          )
        ),
        column(
          width = 2,
          div(
            HTML("Obere Grenze"),
            br(), br(),  # Line break for spacing
            htmlOutput("sum_og"),
            br(),  # Line break for spacing
            htmlOutput("total_og")  # New output for total OG
          )
        )
      )
  )
  )
server <- function(input, output, session) {
  #---- Hinweistexte bei fehlender Eingabe -----
  # Hints werden ausgeblendet, sobald eine gültige Eingabe erfolgt ist.
  showHintIfEmpty(input, "adresse", "adresse_hint", session)
  showHintIfEmpty(input, "groesse", "groesse_hint", session)
  showHintIfEmpty(input, "baujahr", "baujahr_hint", session)
  showHintIfEmpty(input, "renovierung", "renovierung_hint", session)
  showHintIfEmpty(input, "sanitaer", "sanitaer_hint", session)
  showHintIfEmpty(input, "ausstattung", "ausstattung_hint", session)
 #----
  # Render the Leaflet map based on the selected address
  output$adresse_map <- renderLeaflet({
    req(input$adresse)  # Ensure input is not NULL
    # Extract the street name from the selected address
    selected_street <- sub(" [0-9]+.*$", "", input$adresse)
    # Filter data for the specified street
    filtered_data <- adr2024 %>% filter(STRASSE == selected_street)
    # Ensure there is data to render
    req(nrow(filtered_data) > 0)
    # Extract longitude and latitude from the geometry column
    coords <- st_coordinates(filtered_data)
    # Calculate bounds for fitBounds()
    lng_min <- min(coords[, 1], na.rm = TRUE)
    lng_max <- max(coords[, 1], na.rm = TRUE)
    lat_min <- min(coords[, 2], na.rm = TRUE)
    lat_max <- max(coords[, 2], na.rm = TRUE)
    # Create the leaflet map
    leaflet(data = filtered_data) %>%
      addTiles() %>%  # Add OSM tiles as background
      addCircleMarkers(
        radius = 4,  # Standard marker size for other addresses
        color = ~ifelse(WL_2024 %in% names(wl_colors), wl_colors[WL_2024],
                        "black"),  # Default to black for unexpected values
        stroke = FALSE,  # No border for circles
        fillOpacity = 0.8,  # Set opacity
        label = ~paste0(STRASSE_HS, " (", WL_2024,")"),
        group = "All Addresses"
      ) %>%
      # Highlight the selected address
      addCircleMarkers(
        data = filtered_data %>% filter(STRASSE_HS == input$adresse),
        radius = 6,  # Larger size for highlighted address
        color = "yellow",  # Highlight color
        stroke = TRUE,  # Add border
        weight = 2,
        fillOpacity = 1,
        label = ~paste(STRASSE_HS, "(Selected)")
      ) %>%
      # Fit the map view dynamically to include all points
      fitBounds(
        lng1 = lng_min, lat1 = lat_min,
        lng2 = lng_max, lat2 = lat_max,
        options = list(padding = c(20, 20, 20, 50))
      ) %>%
      # Add a legend to the map
      addLegend(
        position = "bottomleft",  # Position of the legend
        colors = c("yellow", wl_colors["A"], wl_colors["B"], wl_colors["C"]),
        labels = c("Ausgewählt", "Lage A", "Lage B", "Lage C"),
        title = "Wohnlagen",
        opacity = 1
      )
  })
  # Update the slider input limits based on the selected 'groesse'
  observeEvent(input$groesse, {
    selected_groesse <- ref_groesse %>% filter(options == input$groesse)
    if (nrow(selected_groesse) > 0) {
      updateSliderInput(
        session,
        "slider_groesse",
        min = selected_groesse$von,
        max = selected_groesse$bis_unter - 0.1,
        value = (selected_groesse$von + selected_groesse$bis_unter) / 2
      )
    }
  })
  observeEvent(input$baujahr, {
    if (!is.null(input$baujahr) && input$baujahr != "") {
      selected_baujahr <- input$baujahr
      # Check if the selected Baujahr allows Vollmodernisierung
      allow_vollmodernisierung <- selected_baujahr < 1990
      # Update renovation options based on Baujahr
      updateSelectizeInput(
        session,
        "renovierung",
        choices = if (allow_vollmodernisierung) {
          ref_renovation$Option  # All options available
        } else {
          ref_renovation$Option[ref_renovation$Option !=
                                  paste0("Vollmodernisierung seit 2013 (nur ",
                                         "bei Baujahr vor 1990)")]
        },
        selected = input$renovierung  # Preserve  selections where possible
      )
    }
  })
  # Define reactive expressions for sum outputs based on actual calculations
  sum_ug_reactive <- reactive({
    groesse_value <- renderGroesseOutput(input$groesse, "low")
    adresse_value <- renderAdresseOutput(input$adresse, input$groesse, "low")
    baujahr_value <- renderBaujahrOutput(input$baujahr, groesse_value)
    renovierung_value <- renderRenovationGroesseOutput(input$renovierung,
                                                       groesse_value)
    sanitaer_value <- renderSanitaerOutput(input$sanitaer,
                                           groesse_value)
    ausstattung_value <- renderAusstattungOutput(input$ausstattung,
                                                 groesse_value)
    total_sum_ug <- sum(groesse_value,
                        adresse_value,
                        baujahr_value,
                        renovierung_value,
                        sanitaer_value,
                        ausstattung_value,
                        na.rm = TRUE)
    return(total_sum_ug)
  })
  sum_oue_reactive <- reactive({
    groesse_value <- renderGroesseOutput(input$groesse, "med")
    adresse_value <- renderAdresseOutput(input$adresse, input$groesse, "med")
    baujahr_value <- renderBaujahrOutput(input$baujahr, groesse_value)
    renovierung_value <- renderRenovationGroesseOutput(input$renovierung,
                                                       groesse_value)
    sanitaer_value <- renderSanitaerOutput(input$sanitaer,
                                           groesse_value)
    ausstattung_value <- renderAusstattungOutput(input$ausstattung,
                                                 groesse_value)
    total_sum_oue <- sum(groesse_value,
                         adresse_value,
                         baujahr_value,
                         renovierung_value,
                         sanitaer_value,
                         ausstattung_value,
                         na.rm = TRUE)
    return(total_sum_oue)
  })
  sum_og_reactive <- reactive({
    groesse_value <- renderGroesseOutput(input$groesse, "hi")
    adresse_value <- renderAdresseOutput(input$adresse, input$groesse, "hi")
    baujahr_value <- renderBaujahrOutput(input$baujahr, groesse_value)
    renovierung_value <- renderRenovationGroesseOutput(input$renovierung,
                                                       groesse_value)
    sanitaer_value <- renderSanitaerOutput(input$sanitaer,
                                           groesse_value)
    ausstattung_value <- renderAusstattungOutput(input$ausstattung,
                                                 groesse_value)
    total_sum_og <- sum(groesse_value,
                        adresse_value,
                        baujahr_value,
                        renovierung_value,
                        sanitaer_value,
                        ausstattung_value,
                        na.rm = TRUE)
    return(total_sum_og)
  })
  # Render the sum_ug, sum_oue, and sum_og values for display
  # before multiplying by the slider value
  output$sum_ug <- renderText({
    if (!is.null(sum_ug_reactive())) {
      paste(format_output(sum_ug_reactive()), "/m²")
    } else {
      "n/a"
    }
  })
  output$sum_oue <- renderText({
    if (!is.null(sum_oue_reactive())) {
      paste(format_output(sum_oue_reactive()), "/m²")
    } else {
      "n/a"
    }
  })
  output$sum_og <- renderText({
    if (!is.null(sum_og_reactive())) {
      paste(format_output(sum_og_reactive()), "/m²")
    } else {
      "n/a"
    }
  })
  # Calculate total UG based on slider and sum_ug
  output$total_ug <- renderText({
    if (!is.null(input$slider_groesse) && !is.null(sum_ug_reactive())) {
      result <- input$slider_groesse * sum_ug_reactive()
      paste(format_output(result))
    } else {
      "n/a"
    }
  })
  # Calculate total OUE based on slider and sum_oue
  output$total_oue <- renderText({
    if (!is.null(input$slider_groesse) && !is.null(sum_oue_reactive())) {
      result <- input$slider_groesse * sum_oue_reactive()
      paste(format_output(result))
    } else {
      "n/a"
    }
  })
  # Calculate total OG based on slider and sum_og
  output$total_og <- renderText({
    if (!is.null(input$slider_groesse) && !is.null(sum_og_reactive())) {
      result <- input$slider_groesse * sum_og_reactive()
      paste(format_output(result))
    } else {
      "n/a"
    }
  })
  # Additional outputs for other sections
  # (e.g., 'groesse', 'baujahr', 'renovierung', etc.)
  # *_ug, *_oue, *_og are used to display the low, med, and high values
  output$groesse_info <- renderText({
    if (is.null(input$groesse) || input$groesse == "") {
      "Pflichtangabe"
    } else {
      ""
    }
  })
  output$groesse_ug <- renderText({
    format_output(renderGroesseOutput(input$groesse, "low"))
  })
  output$groesse_oue <- renderText({
    HTML(paste0("<strong>",
                format_output(renderGroesseOutput(input$groesse, "med")),
                "</strong>"))
  })
  output$groesse_og <- renderText({
    format_output(renderGroesseOutput(input$groesse, "hi"))
  })
  output$adresse_info <- renderText({
    if (is.null(input$adresse) || input$adresse == "") {
      "Pflichtangabe"
    } else {
      generate_address_info(input$adresse)
    }
  })
  output$adresse_ug <- renderText({
    format_output(renderAdresseOutput(input$adresse, input$groesse, "low"))
  })
  output$adresse_oue <- renderText({
    HTML(paste0("<strong>",
                format_output(renderAdresseOutput(input$adresse,
                                                  input$groesse, "med")),
                "</strong>"))
  })
  output$adresse_og <- renderText({
    format_output(renderAdresseOutput(input$adresse, input$groesse, "hi"))
  })
    output$baujahr_info <- renderText({
    if (is.null(input$baujahr) || input$baujahr == "") {
      "Pflichtangabe"
    } else {
      generate_baujahr_info(input$baujahr)
    }
  })
  output$baujahr_ug <- renderText({
    format_output(renderBaujahrOutput(input$baujahr, sum_ug_reactive()))
  })
  output$baujahr_oue <- renderText({
    format_output(renderBaujahrOutput(input$baujahr, sum_oue_reactive()))
  })
  output$baujahr_og <- renderText({
    format_output(renderBaujahrOutput(input$baujahr, sum_og_reactive()))
  })
  output$renovierung_info <- renderText({
    if (is.null(input$renovierung) || length(input$renovierung) == 0) {
      "Pflichtangabe"
    } else {
      generate_renovation_info(input$renovierung)
    }
  })
  output$renovierung_ug <- renderText({
    format_output(renderRenovationGroesseOutput(input$renovierung,
                                                sum_ug_reactive()))
  })
  output$renovierung_oue <- renderText({
    format_output(renderRenovationGroesseOutput(input$renovierung,
                                                sum_oue_reactive()))
  })
  output$renovierung_og <- renderText({
    format_output(renderRenovationGroesseOutput(input$renovierung,
                                  sum_og_reactive())
    )
  })
  output$sanitaer_info <- renderText({
    if (is.null(input$sanitaer) || length(input$sanitaer) == 0 ) {
      "Pflichtangabe"
    } else {
    generate_sanitaer_info(input$sanitaer)
    }
  })
  output$sanitaer_ug <- renderText({
    format_output(renderSanitaerOutput(input$sanitaer, sum_ug_reactive()))
  })
  output$sanitaer_oue <- renderText({
    format_output(renderSanitaerOutput(input$sanitaer, sum_oue_reactive()))
  })
  output$sanitaer_og <- renderText({format_output(
    renderSanitaerOutput(input$sanitaer,
                         sum_og_reactive())
    )
  })
  output$ausstattung_info <- renderText({
    if (is.null(input$ausstattung) || length(input$ausstattung) == 0) {
      "Pflichtangabe"
    } else {
      generate_ausstattung_info(input$ausstattung)
    }
  })
  output$ausstattung_ug <- renderText({
    format_output(renderAusstattungOutput(input$ausstattung,
                                          sum_ug_reactive())
                  )
  })
  output$ausstattung_oue <- renderText({
    format_output(renderAusstattungOutput(input$ausstattung,
                                          sum_oue_reactive())
                  )
  })
  output$ausstattung_og <- renderText({
    format_output(renderAusstattungOutput(input$ausstattung,
                                          sum_og_reactive())
                  )
  })
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Vergleichsmietenberechnung_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Specify the parameters from your app
      params <- list(
        adresse = input$adresse,
        groesse = input$groesse,
        slider_groesse = input$slider_groesse,
        baujahr = input$baujahr,
        renovierung = input$renovierung
        # Add other parameters as needed
      )
      # Render the Rmd document with parameters
      rmarkdown::render(
        "Report.Rmd",  # Path to your Rmd file
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())  # Isolate env for rendering
      )
    }
  )
}
shinyApp(ui = ui, server = server)
