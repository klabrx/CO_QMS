# app.R

library(shiny)
library(dplyr)
library(readr)
library(tibble)
library(sf)
library(leaflet)
library(shinyjs)
library(markdown)

source("data_sources.R")
source("functions.R")

# Read the shapefile
shapefile_path <- "data/SHP/adr2024.shp"
adr2024 <- st_read(shapefile_path)

# Transform coordinates to WGS84 (lat/lon) for Leaflet compatibility if needed
if (st_crs(adr2024)$epsg != 4326) {
  adr2024 <- st_transform(adr2024, crs = 4326)
}

# Define color mapping for WL2024 categories
wl_colors <- c("A" = "red", "B" = "blue", "C" = "green")

ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs for JavaScript interactions
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Title panel wrapped in the always-visible class for consistent width and visibility
  div(
    class = "always-visible",
    id = "title-container",
    titlePanel("Mietspiegelrechner 2024")
    
  ),
  
  # Consent container with app-container class for consistent width
  div(
    id = "consent-container",
    class = "consent-container framed-row",
    fluidRow(
      column(12, tags$div(
        class = "alert alert-info",
        includeMarkdown("consent.Rmd"),
        br(),  # Line break for spacing
        actionButton("acceptCookies", "Ich stimme zu, weiter zum Mietspiegelrechner.")
      ))
    )
  ),
  
  # Main app container, hidden initially by shinyjs
  div(
    id = "app-container",
    class = "app-container",
    
    
    fluidRow(
      class = "framed-row",
      column(width = 4, selectInput("groesse", "Wohnungsgröße (m²)", c(Pflichtangabe = "", ref_groesse$options), selectize = TRUE)),
      column(width = 2, htmlOutput("groesse_info")),
      column(width = 2, div(HTML("<strong>Untere Grenze</strong>"), htmlOutput("groesse_ug"))),
      column(width = 2, div(HTML("<strong>Ortsüblich</strong>"), htmlOutput("groesse_oue"))),
      column(width = 2, div(HTML("<strong>Obere Grenze</strong>"), htmlOutput("groesse_og")))
    ),
    
    fluidRow(
      class = "framed-row",
      column(
        width = 4,
        selectizeInput("adresse", "Adresse", choices = c(Pflichtangabe = "", ref_adresse$STRASSE_HS), multiple = FALSE)
      ),
      column(
        width = 8,
        fluidRow(
          column(width = 3, htmlOutput("adresse_info")),
          column(width = 3, htmlOutput("adresse_ug")),
          column(width = 3, htmlOutput("adresse_oue")),
          column(width = 3, htmlOutput("adresse_og"))
        ),
        fluidRow(
          column(
            width = 12,
            leafletOutput("adresse_map", height = "200px")
          )
        )
      )
    ),
    
    
      

      
      fluidRow(
        class = "framed-row",
        column(width = 4, selectInput("baujahr", "Baujahr", c(Pflichtangabe = "", ref_baujahr$Baujahr), selectize = TRUE)),
        column(width = 2, htmlOutput("baujahr_info")),
        column(width = 2, htmlOutput("baujahr_ug")),
        column(width = 2, htmlOutput("baujahr_oue")),
        column(width = 2, htmlOutput("baujahr_og"))
      ),
      
      fluidRow(
        class = "framed-row",
        column(width = 4, selectizeInput("renovierung", "Renovierung", choices = ref_renovation$Option, multiple = TRUE)),
        column(width = 2, htmlOutput("renovierung_info")),
        column(width = 2, htmlOutput("renovierung_ug")),
        column(width = 2, htmlOutput("renovierung_oue")),
        column(width = 2, htmlOutput("renovierung_og"))
      ),
      
      fluidRow(
        class = "framed-row",
        column(width = 4, selectizeInput("sanitaer", "Sanitärausstattung", choices = ref_sanitaer, multiple = TRUE)),
        column(width = 2, htmlOutput("sanitaer_info")),
        column(width = 2, htmlOutput("sanitaer_ug")),
        column(width = 2, htmlOutput("sanitaer_oue")),
        column(width = 2, htmlOutput("sanitaer_og"))
      ),
      
      fluidRow(
        class = "framed-row",
        column(width = 4, selectizeInput("ausstattung", "Ausstattung", choices = names(ref_ausstattung), multiple = TRUE)),
        column(width = 2, htmlOutput("ausstattung_info")),
        column(width = 2, htmlOutput("ausstattung_ug")),
        column(width = 2, htmlOutput("ausstattung_oue")),
        column(width = 2, htmlOutput("ausstattung_og"))
      ),
      
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
          )
        ),
        column(
          width = 2,
          div(
            HTML("<strong>Untere Grenze</strong>"),
            htmlOutput("sum_ug"),
            br(),  # Line break for spacing
            htmlOutput("total_ug")  # New output for total UG
          )
        ),
        column(
          width = 2,
          div(
            HTML("<strong>Ortsüblich</strong>"),
            htmlOutput("sum_oue"),
            br(),  # Line break for spacing
            htmlOutput("total_oue")  # New output for total OUE
          )
        ),
        column(
          width = 2,
          div(
            HTML("<strong>Obere Grenze</strong>"),
            htmlOutput("sum_og"),
            br(),  # Line break for spacing
            htmlOutput("total_og")  # New output for total OG
          )
        )
      )
  ),
  # Footer section with width limit
  div(
    class = "footer",
    tags$a(href = "https://www.passau.de/impressum/", target = "_blank", "Impressum"),  # Left side link
    actionButton("backToConsent", "Zurück zu den Benutzerhinweisen"),  # Center button
    tags$a(href = "javascript:history.back()", target = "_blank", "Zurück zur vorherigen Seite")  # Right side link
  )
  )


server <- function(input, output, session) {
  # Initially hide only the app-container, not the consent-container
  shinyjs::hide("app-container")
  
  # Show the main app container and hide consent container upon button click
  observeEvent(input$acceptCookies, {
    shinyjs::hide("consent-container")
    shinyjs::show("app-container")
  })

  # Existing server logic for the app
  
  # Render the Leaflet map based on the selected address
  output$adresse_map <- renderLeaflet({
    req(input$adresse)  # Ensure input is not NULL
    
    # Extract the street name from the selected address
    selected_street <- sub(" [0-9]+.*$", "", input$adresse)
    
    # Filter data for the specified street
    filtered_data <- adr2024 %>% filter(STRASSE == selected_street)
    
    # Create the leaflet map
    leaflet(data = filtered_data) %>%
      addTiles() %>%  # Add OSM tiles as background
      addCircleMarkers(
        radius = 4,                # Standard marker size for other addresses
        color = ~sapply(WL_2024, function(x) wl_colors[x]),  # Map colors using wl_colors
        stroke = FALSE,            # No border for circles
        fillOpacity = 0.8,         # Set opacity
        label = ~paste(STRASSE_HS, WL_2024),  # Label with address and WL2024 category
        group = "All Addresses"
      ) %>%
      # Highlight the selected address
      addCircleMarkers(
        data = filtered_data %>% filter(STRASSE_HS == input$adresse),
        radius = 6,                # Larger size for highlighted address
        color = "yellow",          # Highlight color
        stroke = TRUE,             # Add border
        weight = 2,
        fillOpacity = 1,
        label = ~paste(STRASSE_HS, "(Selected)")
      ) %>%
      # Center the map view on the selected street
      setView(lng = mean(st_coordinates(filtered_data)[,1]), 
              lat = mean(st_coordinates(filtered_data)[,2]), 
              zoom = 15)  # Adjust the zoom level for street focus
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
        value = (selected_groesse$von + selected_groesse$bis_unter) / 2  # Center value
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
          ref_renovation$Option[ref_renovation$Option != "Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)"]
        },
        selected = input$renovierung  # Preserve current selections where possible
      )
    }
  })
  
  
  # Define reactive expressions for sum outputs based on actual calculations
  sum_ug_reactive <- reactive({
    groesse_value <- renderGroesseOutput(input$groesse, "low")
    adresse_value <- renderAdresseOutput(input$adresse, input$groesse, "low")
    baujahr_value <- renderBaujahrOutput(input$baujahr, groesse_value)
    renovierung_value <- renderRenovationGroesseOutput(input$renovierung, groesse_value)
    sanitaer_value <- renderSanitaerOutput(input$sanitaer, groesse_value)
    ausstattung_value <- renderAusstattungOutput(input$ausstattung, groesse_value)
    
    total_sum_ug <- sum(groesse_value, adresse_value, baujahr_value, renovierung_value, sanitaer_value, ausstattung_value, na.rm = TRUE)
    return(total_sum_ug)
  })
  
  sum_oue_reactive <- reactive({
    groesse_value <- renderGroesseOutput(input$groesse, "med")
    adresse_value <- renderAdresseOutput(input$adresse, input$groesse, "med")
    baujahr_value <- renderBaujahrOutput(input$baujahr, groesse_value)
    renovierung_value <- renderRenovationGroesseOutput(input$renovierung, groesse_value)
    sanitaer_value <- renderSanitaerOutput(input$sanitaer, groesse_value)
    ausstattung_value <- renderAusstattungOutput(input$ausstattung, groesse_value)
    
    total_sum_oue <- sum(groesse_value, adresse_value, baujahr_value, renovierung_value, sanitaer_value, ausstattung_value, na.rm = TRUE)
    return(total_sum_oue)
  })
  
  sum_og_reactive <- reactive({
    groesse_value <- renderGroesseOutput(input$groesse, "hi")
    adresse_value <- renderAdresseOutput(input$adresse, input$groesse, "hi")
    baujahr_value <- renderBaujahrOutput(input$baujahr, groesse_value)
    renovierung_value <- renderRenovationGroesseOutput(input$renovierung, groesse_value)
    sanitaer_value <- renderSanitaerOutput(input$sanitaer, groesse_value)
    ausstattung_value <- renderAusstattungOutput(input$ausstattung, groesse_value)
    
    total_sum_og <- sum(groesse_value, adresse_value, baujahr_value, renovierung_value, sanitaer_value, ausstattung_value, na.rm = TRUE)
    return(total_sum_og)
  })
  
  # Render the sum_ug, sum_oue, and sum_og values for display before multiplying by the slider value
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
  
  # Additional outputs for other sections (e.g., 'groesse', 'baujahr', 'renovierung', etc.)
  output$groesse_ug <- renderText({ format_output(renderGroesseOutput(input$groesse, "low")) })
  output$groesse_oue <- renderText({ HTML(paste0("<strong>", format_output(renderGroesseOutput(input$groesse, "med")), "</strong>")) })
  output$groesse_og <- renderText({ format_output(renderGroesseOutput(input$groesse, "hi")) })
  output$groesse_info <- renderText({ if (is.null(input$groesse) || input$groesse == "") "Pflichtangabe fehlt" else "" })
  
  output$adresse_info <- renderText({ if (is.null(input$adresse) || input$adresse == "") "Pflichtangabe fehlt" else generate_address_info(input$adresse) })
  output$adresse_ug <- renderText({ format_output(renderAdresseOutput(input$adresse, input$groesse, "low")) })
  output$adresse_oue <- renderText({ HTML(paste0("<strong>", format_output(renderAdresseOutput(input$adresse, input$groesse, "med")), "</strong>")) })
  output$adresse_og <- renderText({ format_output(renderAdresseOutput(input$adresse, input$groesse, "hi")) })
  
  output$baujahr_info <- renderText({ if (is.null(input$baujahr) || input$baujahr == "") "Pflichtangabe fehlt" else generate_baujahr_info(input$baujahr) })
  output$baujahr_ug <- renderText({ format_output(renderBaujahrOutput(input$baujahr, sum_ug_reactive())) })
  output$baujahr_oue <- renderText({ format_output(renderBaujahrOutput(input$baujahr, sum_oue_reactive())) })
  output$baujahr_og <- renderText({ format_output(renderBaujahrOutput(input$baujahr, sum_og_reactive())) })
  
  output$renovierung_info <- renderText({ if (is.null(input$renovierung) || length(input$renovierung) == 0) "Pflichtangabe fehlt" else generate_renovation_info(input$renovierung) })
  output$renovierung_ug <- renderText({ format_output(renderRenovationGroesseOutput(input$renovierung, sum_ug_reactive())) })
  output$renovierung_oue <- renderText({ format_output(renderRenovationGroesseOutput(input$renovierung, sum_oue_reactive())) })
  output$renovierung_og <- renderText({ format_output(renderRenovationGroesseOutput(input$renovierung, sum_og_reactive())) })
  
  output$sanitaer_info <- renderText({ if (is.null(input$sanitaer) || length(input$sanitaer) == 0 ) "Pflichtangabe fehlt" else generate_sanitaer_info(input$sanitaer) })
  output$sanitaer_ug <- renderText({ format_output(renderSanitaerOutput(input$sanitaer, sum_ug_reactive())) })
  output$sanitaer_oue <- renderText({ format_output(renderSanitaerOutput(input$sanitaer, sum_oue_reactive())) })
  output$sanitaer_og <- renderText({ format_output(renderSanitaerOutput(input$sanitaer, sum_og_reactive())) })
  
  output$ausstattung_info <- renderText({ if (is.null(input$ausstattung) || length(input$ausstattung) == 0) "Pflichtangabe fehlt" else generate_ausstattung_info(input$ausstattung) })
  output$ausstattung_ug <- renderText({ format_output(renderAusstattungOutput(input$ausstattung, sum_ug_reactive())) })
  output$ausstattung_oue <- renderText({ format_output(renderAusstattungOutput(input$ausstattung, sum_oue_reactive())) })
  output$ausstattung_og <- renderText({ format_output(renderAusstattungOutput(input$ausstattung, sum_og_reactive())) })
}



shinyApp(ui = ui, server = server)
