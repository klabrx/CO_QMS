library(shiny)
library(dplyr)
library(readr)
library(DT)
library(shinyBS)


# Create an empty result table with the columns
#  "Merkmal" (char),
#  "Angaben" (char),
#  "Anteil" (num, displayed as %),
#  "low" (num, displayed as currency (EUR)),
#  "med" (num, displayed as currency (EUR)),
#  "hi" (num, displayed as currency (EUR))
#
# to be filled with the results of a successive sequence of calculations and to
# be finally displayed as the result of the app.

result <- data.table::data.table(
     Merkmal = c("Größe", "Adresse", "Baujahr", "Renovierung", "Sanitärausstattung", "Ausstattung", "Zusammenfassung"),
     Angaben = c("k.A.", "k.A.", "k.A.", "k.A.", "k.A.", "k.A.", "k.A."),
      Anteil = c(0L, 0L, 0L, 0L, 0L, 0L, 0L),
         low = c(0L, 0L, 0L, 0L, 0L, 0L, 0L),
         med = c(0L, 0L, 0L, 0L, 0L, 0L, 0L),
          hi = c(0L, 0L, 0L, 0L, 0L, 0L, 0L)
)



# Create reference tables that are used to provide
# options for dropdown_inputs.

# First element: Size of the appartment, as size is the most relevant attribute
ref_groesse <- tibble::tribble(
    ~von, ~bis_unter, ~low,  ~med,   ~hi,
     25L,        26L, 9.84, 11.86, 13.88,
     26L,        27L, 9.66, 11.64, 13.61,
     27L,        28L, 9.48, 11.43, 13.37,
     28L,        29L, 9.32, 11.23, 13.14,
     29L,        30L, 9.17, 11.05, 12.93,
     30L,        31L, 9.03, 10.89, 12.74,
     31L,        32L,  8.9, 10.73, 12.55,
     32L,        33L, 8.78, 10.58, 12.38,
     33L,        34L, 8.67, 10.44, 12.22,
     34L,        35L, 8.56, 10.31, 12.06,
     35L,        36L, 8.46, 10.19, 11.92,
     36L,        37L, 8.36, 10.07, 11.79,
     37L,        38L, 8.27,  9.96, 11.66,
     38L,        39L, 8.18,  9.86, 11.54,
     39L,        40L,  8.1,  9.76, 11.42,
     40L,        41L, 8.02,  9.67, 11.31,
     41L,        42L, 7.95,  9.58, 11.21,
     42L,        43L, 7.88,  9.49, 11.11,
     43L,        44L, 7.81,  9.41, 11.01,
     44L,        45L, 7.75,  9.33, 10.92,
     45L,        46L, 7.69,  9.26, 10.83,
     46L,        47L, 7.63,  9.19, 10.75,
     47L,        48L, 7.57,  9.12, 10.67,
     48L,        49L, 7.52,  9.06,  10.6,
     49L,        50L, 7.47,     9, 10.52,
     50L,        51L, 7.42,  8.94, 10.45,
     51L,        52L, 7.37,  8.88, 10.39,
     52L,        53L, 7.32,  8.82, 10.32,
     53L,        54L, 7.28,  8.77, 10.26,
     54L,        55L, 7.24,  8.72,  10.2,
     55L,        56L,  7.2,  8.67, 10.14,
     56L,        57L, 7.16,  8.62, 10.09,
     57L,        59L,  7.1,  8.55, 10.01,
     59L,        61L, 7.03,  8.47,  9.91,
     61L,        64L, 6.95,  8.37,  9.79,
     64L,        67L, 6.86,  8.26,  9.67,
     67L,        70L, 6.77,  8.16,  9.55,
     70L,        77L, 6.65,  8.01,  9.38,
     77L,        81L, 6.53,  7.86,   9.2,
     81L,        87L, 6.44,  7.76,  9.08,
     87L,        93L, 6.35,  7.65,  8.94,
     93L,       100L, 6.25,  7.53,  8.82,
    100L,       108L, 6.16,  7.42,  8.69,
    108L,       120L, 6.06,   7.3,  8.54,
    120L,       132L, 5.96,  7.18,   8.4,
    132L,       141L, 5.88,  7.09,  8.29,
    141L,       150L, 5.82,  7.02,  8.21
    )
# Amend ref_groesse with a new first collumn "options" that contains the
# strings "von bis unter" for each row, e.g. "64 bis unter 67 m²"

ref_groesse <- ref_groesse %>%
  mutate(options = paste0(von, " bis unter ", bis_unter))

# import adress data from a csv file
ref_adressen <- read_csv("data/adr2024.csv")
# Korrigiere Tippfehler in allen Spalten
# ref_adressen <- typo_correction(ref_adressen)

# Erstelle die WL_FAKTOR-Spalte basierend auf der WL_2024-Spalte
ref_adressen <- ref_adressen %>%
  mutate(ADRESS_ID_KURZ = substr(ADRESS_ID, 6, nchar(ADRESS_ID))) %>%
  arrange(STRASSE, ADRESS_ID_KURZ) %>%
  select(-ADRESS_ID_KURZ) %>%
  mutate(WL_FAKTOR = case_when(
    WL_2024 == "A" ~ 0.00,
    WL_2024 == "B" ~ -0.07,
    WL_2024 == "C" ~ -0.10,
    TRUE ~ NA_real_  # Falls kein passender Wert gefunden wird
  ))

# Options and factors (percentage) for the year of build, to be used in the
# selectInput "Baujahr"
#  Erstelle ref_baujahr
# Define year ranges and factors
Baujahre <- tibble::tribble(
  ~Baujahr, ~Faktor,
  "bis 1918", 0.00,
  "1919 - 1945", -0.07,
  "1946 - 1977", -0.10,
  "1978 - 1984", -0.05,
  "1985 - 1989", -0.01,
  "1990 - 1995", -0.01,
  "1996 - 2004", +0.06,
  "2005 - 2012", +0.12,
  "2013 - 2018", +0.19,
  "2019 - 2023", +0.24)


# Define the renovation items
renovation_items <- c(
  "Keine Sanierung/Renovierung bekannt",
  "Vollsanierung seit 2013 (nur bei Baujahr vor 1990)",
  "Sanitärbereich (mind. Fliesen, Wanne, WC) erneuert",
  "Elektroinstallation (zeitgemäß) erneuert",
  "Heizanlage/Warmwasserversorgung erneuert",
  "Schallschutz eingebaut",
  "Fußböden erneuert",
  "Fenster-/Rahmenerneuerung",
  "Innen- und Wohnungstüren erneuert",
  "Treppenhaus, Eingangsbereich erneuert",
  "barrierearme Ausstattung geschaffen (Mindestvoraussetzung: schwellenfrei (max. 4cm Höhe), stufenloser Zugang, bodengleiche Dusche)",
  "Grundriss verbessert",
  "Dachsanierung",
  "Fassadensanierung"
)

# Define Sanitärausstattung items
sanitaer_items <- c(
  "zwei oder mehr abgeschlossene Badezimmer in der Wohnung vorhanden",
  "zweites WC/Gäste-WC vorhanden",
  "(separate) Einzeldusche",
  "Fußbodenheizung",
  "Belüftung(sanlage)",
  "separater WC-Raum vorhanden",
  "Handtuchheizkörper",
  "zweites Waschbecken im selben Badezimmer"
)

# Define the Ausstattung items with corresponding percentages and full descriptions
ausstattung_items <- data.frame(
  label = c("Einbauküche", "Terrasse/Dachterrasse", "Aufzug (< 5 Stockwerke)", 
            "Parkett/Dielenboden", "Energiebedarfsklasse F, G, H", 
            "Teppichboden (nicht modernisiert)"),
  value = c(0.04, 0.06, 0.07, 0.03, -0.09, -0.11),  # Factors associated with each item
  description = c(
    "Einbauküche mit mindestens zwei Elektroeinbaugeräten (z. B. Herd/Ofen, Gefrierschrank/-truhe, Kühlschrank, Geschirrspülmaschine) wird vom Vermieter ohne zusätzlichen Mietzuschlag gestellt.",
    "Terrasse oder Dachterrasse",
    "Aufzug in Gebäuden mit weniger als 5 Stockwerken",
    "Überwiegend Parkett-, Dielen- oder Steinfußboden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut",
    "Energiebedarfsklasse lt. Energiebedarfsausweis lautet F, G oder H; bzw. der Wert kWh/m2a ist größer oder gleich 200",
    "Teppichboden, PVC- oder Linoleum-Boden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut, welcher seit 2013 nicht modernisiert bzw. saniert wurde"
  ),
  stringsAsFactors = FALSE
)

# Create a named list for the label to pass into selectizeInput
ausstattung_choices <- setNames(as.list(ausstattung_items$label), ausstattung_items$label)






# function to take care of currency formatting
cur <- function(value, show_plus = FALSE) {
  # Ensure value is numeric
  value <- as.numeric(value)
  
  # Format with 2 decimal places and replace decimal point with a comma
  formatted_value <- formatC(abs(value), format = "f", digits = 2, decimal.mark = ",")
  
  # Determine the sign
  if (value < 0) {
    return(paste0("-", formatted_value))
  } else if (value > 0 && show_plus) {
    return(paste0("+", formatted_value))
  } else {
    return(paste0("", formatted_value))
  }
}



# Create a shiny app frame with fixed (non-scrolling) header and footer,
# containing two colums (width 4:8) with a sidebar and a main panel.
# In the sidebar, create a selectize dropdown menu based in ref_groesse$options.

# The dropdown will display "Pflichtangabe" as a placeholder until an option is 
# selected. "Pflichtangabe will *NOT* be just the first option, but a placeholder

# When a selection is made, react to that input and write the first line
# into results (exemplary):
# "Größe", "64 bis unter 67 m²", 100%, 6.95, 8.37, 9.79
# and display the result table in the main panel.
#
# The next dropdown is planned to be "Adresse" and will be filled with the
# unique values of ref_adressen$STRASSE_HS. The selection will then be used
# to determine the WL_2024 anhd the WL_Faktor. The resulting row will be
# (e.g.): "Adresse: Abteistraße 10, Wohnlage B, -7%," plus the three values
# lo|med|hi (results of "Groesse"), each multipied with the WL_Faktor, i.e.
# lo = -0,49, med = -0,60, hi = -0,70.
# The row will be attached to the results table and displayed in the main panel.
# Additionally, append a summary row at the bottom of the result table, showing
# "Ortsübliche Vergleichsmiete", sum(lo), sum(med), sum(hi)
#
# Creating the summary table needs to be separate from observing the inputs.
# Any input to create/update its respective row in the results table.




ui <- fluidPage(
  titlePanel("Mietspiegelrechner 2024"),
  sidebarLayout(
    sidebarPanel(
      selectInput('groesse', 'Wohnungsgröße (m²)', c(Pflichtangabe='', ref_groesse$options), selectize=TRUE),
      selectInput('adresse', 'Adresse', c(Pflichtangabe='', unique(ref_adressen$STRASSE_HS)), selectize=TRUE),
      selectInput('baujahr', 'Baujahr', c(Pflichtangabe='', Baujahre$Baujahr), selectize=TRUE),
      selectInput('renovierung', 'Renovierungen', c(Pflichtangabe='', renovation_items), multiple = TRUE, selectize=TRUE),
      selectInput('sanitaer', 'Sanitärausstattung (mind. 3)', c(Pflichtangabe='', sanitaer_items), multiple = TRUE, selectize=TRUE),
      selectizeInput(
        'ausstattung', 
        'Ausstattung', 
        choices = ausstattung_choices, 
        multiple = TRUE,
        options = list(
          render = I('{
            option: function(item, escape) {
              var descriptions = {
                "Einbauküche": "Einbauküche mit mindestens zwei Elektroeinbaugeräten (z. B. Herd/Ofen, Gefrierschrank/-truhe, Kühlschrank, Geschirrspülmaschine) wird vom Vermieter ohne zusätzlichen Mietzuschlag gestellt.",
                "Terrasse/Dachterrasse": "Terrasse oder Dachterrasse",
                "Aufzug (< 5 Stockwerke)": "Aufzug in Gebäuden mit weniger als 5 Stockwerken",
                "Parkett/Dielenboden": "Überwiegend Parkett-, Dielen- oder Steinfußboden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut",
                "Energiebedarfsklasse F, G, H": "Energiebedarfsklasse lt. Energiebedarfsausweis lautet F, G oder H; bzw. der Wert kWh/m2a ist größer oder gleich 200",
                "Teppichboden (nicht modernisiert)": "Teppichboden, PVC- oder Linoleum-Boden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut, welcher seit 2013 nicht modernisiert bzw. saniert wurde"
              };
              var description = descriptions[item.label];
              return "<div title=\'" + escape(description) + "\'>" + escape(item.label) + "</div>";
            }
          }')
        )
      )
    ),
    mainPanel(
      h3("Zusammenfassung der Angaben"),
      htmlOutput('groesse'),
      htmlOutput('adresse'),
      htmlOutput('baujahr'),
      htmlOutput('renovierung'),
      htmlOutput('sanitaer'),
      htmlOutput('ausstattung'),
      DTOutput('result_table')
    )
  )
)

server <- function(input, output, session) {
  # Formatting logic for Wohnungsgröße output
  output$groesse <- renderText({
    if (input$groesse == "Pflichtangabe" || is.null(input$groesse) || input$groesse == "") {
      return("Bitte wählen Sie eine Wohnungsgröße aus.")
    }
    groesse <- ref_groesse %>% filter(options == input$groesse)
    paste0(groesse$options, " m²: Ortsübliche Basismiete pro m² im Bereich ",
           cur(groesse$low), " EUR < ",
           "<b>", cur(groesse$med), " EUR</b> < ",
           cur(groesse$hi), " EUR")
  })
  
  # Formatting logic for Adresse output
  output$adresse <- renderText({
    if (input$adresse == "Pflichtangabe" || is.null(input$adresse) || input$adresse == "") {
      return("Bitte wählen Sie eine Adresse aus.")
    }
    
    # Find the corresponding address entry in ref_adressen
    adresse <- ref_adressen %>% filter(STRASSE_HS == input$adresse)
    
    if (nrow(adresse) == 0) {
      return("Adresse nicht gefunden.")
    }
    
    # Extract WL_FAKTOR and calculate new low, med, and hi values based on Wohnungsgröße values
    groesse <- ref_groesse %>% filter(options == input$groesse)
    if (nrow(groesse) == 0) {
      return("Bitte wählen Sie zuerst eine gültige Wohnungsgröße aus.")
    }
    
    # Updated calculation logic for Adresse adjustments without adding 1 to WL_FAKTOR
    low_adresse <- cur(groesse$low * adresse$WL_FAKTOR)
    med_adresse <- cur(groesse$med * adresse$WL_FAKTOR)
    hi_adresse <- cur(groesse$hi * adresse$WL_FAKTOR)
    
    paste0(adresse$STRASSE_HS, " (Wohnlage ", adresse$WL_2024, ": ",
           cur(adresse$WL_FAKTOR * 100), "%) ergibt einen Abzug von ", low_adresse, " EUR < ",
           "<b>", med_adresse, " EUR</b> < ", hi_adresse, " EUR")
  })
  
  # Logic for Baujahr output
  # Take the input from the Baujahr dropdown
  # create a result line similar to the groesse and adresse output
  # and append it to the result table
  output$baujahr <- renderText({
    if (input$baujahr == "Pflichtangabe" || is.null(input$baujahr) || input$baujahr == "") {
      return("Bitte wählen Sie ein Baujahr aus.")
    }
    
    # Find the corresponding Baujahr entry in Baujahre
    baujahr <- Baujahre %>% filter(Baujahr == input$baujahr)
    
    if (nrow(baujahr) == 0) {
      return("Baujahr nicht gefunden.")
    }
    
    # Extract Faktor and calculate new low, med, and hi values based on Wohnungsgröße values
    groesse <- ref_groesse %>% filter(options == input$groesse)
    if (nrow(groesse) == 0) {
      return("Bitte wählen Sie zuerst eine gültige Wohnungsgröße aus.")
    }
    
    low_baujahr <- cur(groesse$low * baujahr$Faktor)
    med_baujahr <- cur(groesse$med * baujahr$Faktor)
    hi_baujahr <- cur(groesse$hi * baujahr$Faktor)
    
    paste0("Baujahr ", baujahr$Baujahr, " (Faktor ",
           cur(baujahr$Faktor * 100), "%) ergibt einen Abzug/Zuschlag von ",
           low_baujahr, " EUR < ",
           "<b>", med_baujahr, " EUR</b> < ",
           hi_baujahr, " EUR")
  })
  
  # Logic for output$renovierung
  # Source for the dropdown: renovation_items as defined above
  # multiple selections are allowed
  # restraints:
  # item 1 ('keine Sanierung...') and item 2 ('Vollsanierung ...') MUST stand alone,
  # i.e. selecting one of those unselects all of the others.
  # item 2 ('Vollsanierung ...') can only be selected if one of the first five
  # options of 'Baujahre' is selected, input$renovierung has to observe 'Baujahre'
  # and react accordingly i.e. a formerly valid selection 'Vollsanierung ...' 
  # unselected if 'Baujahre is changed to a selection that does not allow
  # 'Vollsanierung. 
  # Items 3-14 can be multi-selected, but one one of them is selected, items 1
  # must be deselected.
  
  # Observe the changes in the "Baujahr" field
  observeEvent(input$baujahr, {
    baujahr_selected <- input$baujahr
    
    # Define the default options for renovation
    renovation_options <- renovation_items
    
    # If Baujahr is outside the range for "Vollsanierung", remove option 2
    if (!baujahr_selected %in% Baujahre$Baujahr[1:5]) {
      renovation_options <- renovation_options[!renovation_options %in% "Vollsanierung seit 2013 (nur bei Baujahr vor 1990)"]
    }
    
    # Update the renovation dropdown options based on Baujahr selection
    updateSelectInput(session, "renovierung", choices = renovation_options, selected = NULL)
  })
  
  # Observe changes in the renovation input
  observeEvent(input$renovierung, {
    renovation_selected <- input$renovierung
    
    # If "Keine Sanierung" is selected, remove all other options
    if ("Keine Sanierung/Renovierung bekannt" %in% renovation_selected) {
      updateSelectInput(session, "renovierung", selected = "Keine Sanierung/Renovierung bekannt", choices = c("Keine Sanierung/Renovierung bekannt"))
    }
    
    # If "Vollsanierung" is selected, remove all other options
    else if ("Vollsanierung seit 2013 (nur bei Baujahr vor 1990)" %in% renovation_selected) {
      updateSelectInput(session, "renovierung", selected = "Vollsanierung seit 2013 (nur bei Baujahr vor 1990)", choices = c("Vollsanierung seit 2013 (nur bei Baujahr vor 1990)"))
    }
    
    # If any of the options 3-14 are selected, remove options 1 and 2
    else if (any(renovation_selected %in% renovation_items[3:14])) {
      available_options <- renovation_items[3:14]
      updateSelectInput(session, "renovierung", choices = available_options, selected = renovation_selected)
    }
  })
  
  
  # Zusammenfassung output based on the sum of all results so far
  output$zusammenfassung <- renderText({
    # Ensure that we have valid inputs for groesse and adresse
    if (input$groesse == "Pflichtangabe" || is.null(input$groesse) || input$adresse == "Pflichtangabe" || is.null(input$adresse)) {
      return("Bitte wählen Sie zuerst eine Wohnungsgröße und eine Adresse aus.")
    }

    # Get groesse and adresse values
    groesse <- ref_groesse %>% filter(options == input$groesse)
    adresse <- ref_adressen %>% filter(STRASSE_HS == input$adresse)

    # if (nrow(groesse) == 0 || nrow(adresse) == 0) {
    #   return("Ungültige Wohnungsgröße oder Adresse.")
    # }

    # Calculate the summed results for lo, med, and hi
    lo_result <- groesse$low +
      groesse$low * adresse$WL_FAKTOR +
      groesse$low * Baujahre$Faktor
    med_result <- groesse$med +
      groesse$med * adresse$WL_FAKTOR +
      groesse$med * Baujahre$Faktor
    hi_result <- groesse$hi +
      groesse$hi * adresse$WL_FAKTOR +groesse$hi * Baujahre$Faktor

    # Generate the summary line
    paste0("Die ortsübliche Vergleichsmiete liegt im Bereich: ",
           cur(lo_result), " EUR < ",
           "<b>", cur(med_result), " EUR</b> < ",
           cur(hi_result), " EUR")
  })
  
  # output$baujahr <- renderPrint({input$baujahr})
  output$result_table <- renderDT({
    # Format each numeric column (Anteil, low, med, hi)
    formatted_result <- result
    
    # Apply formatting to each column
    formatted_result$Anteil <- sapply(result$Anteil, function(x) {
      if (x < 0) {
        return(sprintf("%+.2f", x))  # Negative values
      } else if (x == 0) {
        return("\u00B1 0")  # Zero values with ± symbol
      } else {
        return(sprintf("- %.2f", x))  # Positive values with leading space and minus
      }
    })
    
    formatted_result$low <- sapply(result$low, function(x) {
      if (x < 0) {
        return(sprintf("%+.2f", x))  # Negative values
      } else if (x == 0) {
        return("\u00B1 0")  # Zero values with ± symbol
      } else {
        return(sprintf("- %.2f", x))  # Positive values with leading space and minus
      }
    })
    
    formatted_result$med <- sapply(result$med, function(x) {
      if (x < 0) {
        return(sprintf("%+.2f", x))  # Negative values
      } else if (x == 0) {
        return("\u00B1 0")  # Zero values with ± symbol
      } else {
        return(sprintf("- %.2f", x))  # Positive values with leading space and minus
      }
    })
    
    formatted_result$hi <- sapply(result$hi, function(x) {
      if (x < 0) {
        return(sprintf("%+.2f", x))  # Negative values
      } else if (x == 0) {
        return("\u00B1 0")  # Zero values with ± symbol
      } else {
        return(sprintf("- %.2f", x))  # Positive values with leading space and minus
      }
    })
    # Create an output$renovierung similar to output$groesse and output$adresse
    # using the following factors (multiplied with the groesse results):
    # If selection (renovierung) = "Keine Sanierung/Renovierung bekannt" : 0%
    # If selection (renovierung) = "Vollsanierung ..." : +11%
    # If number of selections out of options 3-14 < 3: 0%, else 6%
    # lo/mid/hi as above
    
    output$renovierung <- renderText({
      # Ensure the user has selected some renovation option
      if (is.null(input$renovierung) || length(input$renovierung) == 0 || input$renovierung == "Pflichtangabe") {
        return("Bitte wählen Sie eine Renovierungsoption aus.")
      }
      
      # Check which renovation items were selected
      renovierung <- renovation_items[renovation_items %in% input$renovierung]
      
      if (length(renovierung) == 0) {
        return("Renovierungsoption nicht gefunden.")
      }
      
      # Find the selected "groesse" (size)
      groesse <- ref_groesse %>% filter(options == input$groesse)
      
      # Ensure groesse is valid before proceeding
      if (nrow(groesse) == 0) {
        return("Bitte wählen Sie zuerst eine gültige Wohnungsgröße aus.")
      }
      
      # Determine the renovation factor:
      # 1. "Keine Sanierung/Renovierung bekannt" -> factor = 0
      # 2. "Vollsanierung seit 2013" -> factor = 0.11
      # 3. 3 or more of options 3-14 -> factor = 0.06, otherwise factor = 0
      if ("Keine Sanierung/Renovierung bekannt" %in% renovierung) {
        factor <- 0
      } else if ("Vollsanierung seit 2013 (nur bei Baujahr vor 1990)" %in% renovierung) {
        factor <- 0.11
      } else if (length(renovierung) >= 3) {
        factor <- 0.06
      } else {
        factor <- 0
      }
      
      # Apply the factor to calculate low, med, and high values for renovation
      low_renovierung <- cur(groesse$low * factor)
      med_renovierung <- cur(groesse$med * factor)
      hi_renovierung <- cur(groesse$hi * factor)
      
      # Combine selected options into a string
      selected_options <- paste(renovierung, collapse = ", ")
      
      # Display the selected option(s) along with the calculated values
      paste0(selected_options, " ergibt einen Zuschlag von ",
             low_renovierung, " EUR < ",
             "<b>", med_renovierung, " EUR</b> < ",
             hi_renovierung, " EUR")
    })
    
    # create output$sanitaer similar to output$groesse, output$adresse, output$baujahr
    # using a multiplier of 6% if at least three items are selected, 0% otherwise
    # display the selected items, then the resp. low/mid/hi values, as above
    
    output$sanitaer <- renderText({
      # Ensure the user has selected some Sanitärausstattung option
      if (is.null(input$sanitaer) || length(input$sanitaer) == 0 || input$sanitaer == "Pflichtangabe") {
        return("Bitte wählen Sie mindestens eine Sanitärausstattungsoption aus.")
      }
      
      # Check which Sanitärausstattung items were selected
      sanitaer <- sanitaer_items[sanitaer_items %in% input$sanitaer]
      
      if (length(sanitaer) == 0) {
        return("Sanitärausstattungsoption nicht gefunden.")
      }
      
      # Find the selected "groesse" (size)
      groesse <- ref_groesse %>% filter(options == input$groesse)
      
      # Ensure groesse is valid before proceeding
      if (nrow(groesse) == 0) {
        return("Bitte wählen Sie zuerst eine gültige Wohnungsgröße aus.")
      }
      
      # Determine the Sanitärausstattung factor:
      # 1. 3 or more of the selected options -> factor = 0.06, otherwise factor = 0
      if (length(sanitaer) >= 3) {
        factor <- 0.06
      } else {
        factor <- 0
      }
      
      # Apply the factor to calculate low, med, and high values for Sanitärausstattung
      low_sanitaer <- cur(groesse$low * factor)
      med_sanitaer <- cur(groesse$med * factor)
      hi_sanitaer <- cur(groesse$hi * factor)
      
      # Combine selected options into a string
      selected_options <- paste(sanitaer, collapse = ", ")
      
      # Display the selected option(s) along with the calculated values
      paste0(selected_options, " ergibt einen Zuschlag von ",
             low_sanitaer, " EUR < ",
             "<b>", med_sanitaer, " EUR</b> < ",
             hi_sanitaer, " EUR")
    })
    
    # create output$ausstattung similar to output$groesse, output$adresse, output$baujahr
    # using the values from the 'ausstattung_items' data frame
    # the factor that is multiplied with the groesse results is the sum of the values of the selected items
    # display the selected items, then the resp. low/mid/hi values, as above
    
    output$ausstattung <- renderText({
      # Ensure the user has selected some Ausstattung option
      if (is.null(input$ausstattung) || length(input$ausstattung) == 0 || input$ausstattung == "Pflichtangabe") {
        return("Bitte wählen Sie mindestens eine Ausstattungsoption aus.")
      }
      
      # Check which Ausstattung items were selected
      ausstattung <- ausstattung_items[ausstattung_items$label %in% input$ausstattung, "value"]
      
      if (length(ausstattung) == 0) {
        return("Ausstattungsoption nicht gefunden.")
      }
      
      # Find the selected "groesse" (size)
      groesse <- ref_groesse %>% filter(options == input$groesse)
      
      # Ensure groesse is valid before proceeding
      if (nrow(groesse) == 0) {
        return("Bitte wählen Sie zuerst eine gültige Wohnungsgröße aus.")
      }
      
      # Calculate the sum of the selected Ausstattung values
      factor <- sum(ausstattung)
      
      # Apply the factor to calculate low, med, and high values for Ausstattung
      low_ausstattung <- cur(groesse$low * factor)
      med_ausstattung <- cur(groesse$med * factor)
      hi_ausstattung <- cur(groesse$hi * factor)
      
      # Combine selected options into a string
      selected_options <- paste(input$ausstattung, collapse = ", ")
      
      # Display the selected option(s) along with the calculated values
      # then the sum of the selectred values, multiplied with the groesse results
      # as above
      paste0(selected_options, " ergibt einen Zuschlag von ",
             low_ausstattung, " EUR < ",
             "<b>", med_ausstattung, " EUR</b> < ",
             hi_ausstattung, " EUR")

    })
    
    # Render the datatable with bold formatting for the last row and the 'med' column
    datatable(formatted_result, options = list(pageLength = 7, autoWidth = TRUE, dom = 't'), rownames = FALSE) %>%
      formatStyle(
        'Merkmal', 
        target = 'row',
        fontWeight = styleEqual('Zusammenfassung', 'bold')  # Make the last row bold
      ) %>%
      formatStyle(
        'med', 
        fontWeight = 'bold'  # Make the entire 'med' column bold
      )
  })
  
  # # Add tooltips for each Ausstattung option
  # lapply(seq_along(ausstattung_items), function(i) {
  #   bsTooltip(
  #     id = paste0("ausstattung-", i), 
  #     title = ausstattung_tooltips[[i]],
  #     placement = "right"
  #   )
  # })
  
  # Display the selected Ausstattung
  output$ausstattung <- renderText({
    if (is.null(input$ausstattung) || length(input$ausstattung) == 0) {
      return("Keine Ausstattung ausgewählt.")
    }
    paste("Ausgewählte Ausstattung:", paste(input$ausstattung, collapse = ", "))
  })
  
  # The empty result table needs to be filled with the results of the
  # groesse, adresse, baujahr, renovierung, sanitaer, and ausstattung
  # outputs. The results are to be written into the respective lines.
  # The last line is to be the sum of the results. Use the DT 'result' table
  # as a template and fill it with the results of the outputs.
  
  
  
}
shinyApp(ui = ui, server = server)
