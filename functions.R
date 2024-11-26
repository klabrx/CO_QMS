# functions.R

#---- function for formatting numerics to 2 decimals, german punctuation
#.    and a chosen unit

format_value <- function(value, unit = "") {
  if (is.na(value)) {
    return("NA")
  }
  formatted_value <- format(
    round(value, 2),   # round to two decimal before formatting
    nsmall = 2,         # Always show two decimal places
    big.mark = ".",     # Use a dot for thousand separators
    decimal.mark = ","  # Use a comma for the decimal point
  )
  paste0(formatted_value, unit)
}


# Format to create percentage results
format_percentage <- function(value) {
  if (value < 0) {
    return(paste0("Abschlag -", abs(value * 100), "%"))
  } else if (value == 0) {
    return("kein Zu-/Abschlag: ±0%")
  } else {
    return(paste0("Zuschlag +", value * 100, "%"))
  }
}



# Format numeric output for currency display with two decimal places
format_output <- function(value) {
  formatted_value <- formatC(abs(value), format = "f", digits = 2, decimal.mark = ",")
  if (value < 0) {
    return(paste0("-", formatted_value, " EUR"))
  } else {
    return(paste0(formatted_value, " EUR"))
  }
}

# Convert text to numeric, handling cases where EUR formatting is used
get_numeric_value <- function(text) {
  as.numeric(gsub(",", ".", gsub(" EUR", "", text)))
}

# Generate output for Groesse selection based on low/med/hi columns
renderGroesseOutput <- function(input_value, column_name) {
  groesse_row <- ref_groesse %>% filter(options == input_value)
  if (nrow(groesse_row) == 0) return(0)
  return(groesse_row[[column_name]])
}

# Reactive functions to calculate 'groesse' values
reactive_groesse_ug <- function(input_groesse) {
  renderGroesseOutput(input_groesse, "low")
}

reactive_groesse_oue <- function(input_groesse) {
  renderGroesseOutput(input_groesse, "med")
}

reactive_groesse_og <- function(input_groesse) {
  renderGroesseOutput(input_groesse, "hi")
}

# Generate output for Adresse selection, applying WL_FAKTOR adjustment
renderAdresseOutput <- function(input_adresse, input_groesse, column_name) {
  if (is.null(input_adresse) || input_adresse == "" || is.null(input_groesse) || input_groesse == "") return(0)
  
  adresse_row <- ref_adresse %>% filter(STRASSE_HS == input_adresse)
  if (nrow(adresse_row) == 0) return(0)
  
  wl_factor <- adresse_row$WL_FAKTOR
  groesse_value <- renderGroesseOutput(input_groesse, column_name)
  
  return(groesse_value * wl_factor)
}

# Generate output for Baujahr selection, applying Factor adjustment
renderBaujahrOutput <- function(input_baujahr, groesse_value) {
  if (is.null(input_baujahr) || input_baujahr == "") return(0)
  
  baujahr_row <- ref_baujahr %>% filter(Baujahr == input_baujahr)
  if (nrow(baujahr_row) == 0) return(0)
  
  factor <- baujahr_row$Factor
  return(groesse_value * factor)
}

# Generate output for Renovierung selection with conditional logic
renderRenovationGroesseOutput <- function(input_renovierung, groesse_value) {
  # Check for "Keine Sanierung/Renovierung bekannt" as a valid, non-contributing input
  if (!is.null(input_renovierung) && "Keine Sanierung/Renovierung bekannt" %in% input_renovierung) {
    return(0)  # No surcharge
  } else if ("Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)" %in% input_renovierung) {
    return(groesse_value * 0.11)  # 11% surcharge for Vollmodernisierung
  } else if (length(input_renovierung) >= 3) {
    return(groesse_value * 0.06)  # 6% surcharge for 3 or more selections
  } else {
    return(0)  # Less than 3 selections, no surcharge
  }
}

renderSanitaerOutput <- function(input_sanitaer, groesse_value) {
  # Check if "Keine besondere Sanitärausstattung" is selected and treat it as a valid 0% selection
  if (!is.null(input_sanitaer) && "Keine besondere Sanitärausstattung" %in% input_sanitaer) {
    return(0)  # Valid selection with no surcharge
  }
  
  # Exclude "Keine besondere Sanitärausstattung" from the count
  valid_selections <- setdiff(input_sanitaer, "Keine besondere Sanitärausstattung")
  
  # Apply surcharge if 3 or more valid options are selected
  if (length(valid_selections) >= 3) {
    return(groesse_value * 0.06)  # 6% surcharge for 3 or more selections
  } else {
    return(0)  # Less than 3 valid selections, no surcharge
  }
}

# Generate output for Ausstattung selection, summing all selected factors
renderAusstattungOutput <- function(input_ausstattung, groesse_value) {
  if (is.null(input_ausstattung) || length(input_ausstattung) == 0) return(0)
  
  selected_factors <- unlist(ref_ausstattung[input_ausstattung])
  total_factor <- sum(selected_factors, na.rm = TRUE)
  
  return(groesse_value * total_factor)
}

# Generate information display for Adresse selection
generate_address_info <- function(input_adresse) {
  if (is.null(input_adresse) || input_adresse == "Pflichtangabe") {
    return("Pflichtangabe fehlt")
  }
  
  adresse_row <- ref_adresse %>% filter(STRASSE_HS == input_adresse)
  if (nrow(adresse_row) == 0) {
    return("Adresse nicht gefunden")
  }
  
  wl_category <- adresse_row$WL_2024
  wl_factor <- adresse_row$WL_FAKTOR
  factor_display <- ifelse(wl_factor < 0, paste0("-", abs(wl_factor) * 100, "%"), paste0("+", wl_factor * 100, "%"))
  
  return(paste0("Wohnlage ", wl_category, ", ", factor_display))
}

# Generate information display for Baujahr selection
generate_baujahr_info <- function(input_baujahr) {
  if (is.null(input_baujahr) || input_baujahr == "Pflichtangabe") {
    return("Pflichtangabe fehlt")
  }
  
  baujahr_row <- ref_baujahr %>% filter(Baujahr == input_baujahr)
  if (nrow(baujahr_row) == 0) {
    return("Baujahr nicht gefunden")
  }
  
  factor <- baujahr_row$Factor
  factor_display <- format_percentage(factor)
  
  return(paste0("Baujahresbereich ", input_baujahr, ": ", factor_display))
}


# Generate information display for Renovierung selection
generate_renovation_info <- function(input_renovierung) {
  if (is.null(input_renovierung) || "Keine Sanierung/Renovierung bekannt" %in% input_renovierung) {
    return("Keine Sanierung/Renovierung bekannt: 0%")
  } else if ("Vollmodernisierung seit 2013 (nur bei Baujahr vor 1990)" %in% input_renovierung) {
    return("Vollmodernisierung: 11%")
  } else if (length(input_renovierung) >= 3) {
    return("Teilmodernisierung (mind. 3 Maßnahmen): +6%")
  } else {
    return("Unter 3 Maßnahmen: 0%")
  }
}

# Generate information display for Sanitaer selection
generate_sanitaer_info <- function(input_sanitaer) {
  if (is.null(input_sanitaer) || "Keine besondere Sanitärausstattung" %in% input_sanitaer) {
    return("Keine besondere Sanitärausstattung: 0%")
  } else if (length(input_sanitaer) >= 3) {
    return("Gehobene Sanitärausstattung (mind. 3 Verbesserungen): +6%")
  } else {
    return("Normale Sanitärausstattung (weniger als 3 Verbesserungen): 0%")
  }
}

# Generate information display for Ausstattung selection
generate_ausstattung_info <- function(input_ausstattung) {
  if (is.null(input_ausstattung) || length(input_ausstattung) == 0) {
    return("Keine besondere Sanitärausstattung: 0%")
  }
  
  selected_factors <- unlist(ref_ausstattung[input_ausstattung])
  total_factor <- sum(selected_factors, na.rm = TRUE)
  
  # Use format_percentage to format the total factor
  factor_display <- format_percentage(total_factor)
  
  return(paste0("Ausstattung: ", factor_display))
}


# Sum functions for Zusammenfassung
sum_ug_values <- function(groesse_ug, adresse_ug, baujahr_ug, renovation_ug, sanitaer_ug, ausstattung_ug) {
  sum(groesse_ug, adresse_ug, baujahr_ug, renovation_ug, sanitaer_ug, ausstattung_ug, na.rm = TRUE)
}

sum_oue_values <- function(groesse_oue, adresse_oue, baujahr_oue, renovation_oue, sanitaer_oue, ausstattung_oue) {
  sum(groesse_oue, adresse_oue, baujahr_oue, renovation_oue, sanitaer_oue, ausstattung_oue, na.rm = TRUE)
}

sum_og_values <- function(groesse_og, adresse_og, baujahr_og, renovation_og, sanitaer_og, ausstattung_og) {
  sum(groesse_og, adresse_og, baujahr_og, renovation_og, sanitaer_og, ausstattung_og, na.rm = TRUE)
}

showHintIfEmpty <- function(input, input_id, hint_id, session) {
  observe({
    # Check if the input is null or contains only empty strings
    if (is.null(input[[input_id]]) || all(input[[input_id]] == "")) {
      shinyjs::show(hint_id)
    } else {
      shinyjs::hide(hint_id)
    }
  })
}

format_for_markdown_list <- function(input_vector, empty_message = "Keine Auswahl") {
  if (!is.null(input_vector) && length(input_vector) > 0) {
    paste("- ", input_vector, collapse = "\n")
  } else {
    empty_message
  }
}
