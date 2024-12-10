# functions.R


format_value <- function(value, unit = "", add_plus = FALSE) {
  if (is.na(value)) {
    return("NA")
  }
disable_options <- function(session, input_id, disable_choices) {
    js_code <- sprintf(
      "$('#%s input[value=\"%s\"]').prop('disabled', true).parent().css('color', 'gray');",
      input_id,
      paste(disable_choices, collapse = '", "#%s input[value="')
    )
    shinyjs::runjs(js_code)
  }
  
enable_options <- function(session, input_id, enable_choices) {
    js_code <- sprintf(
      "$('#%s input[value=\"%s\"]').prop('disabled', false).parent().css('color', 'black');",
      input_id,
      paste(enable_choices, collapse = '", "#%s input[value="')
    )
    shinyjs::runjs(js_code)
  }
  
  
  # Define the format string
  fmt <- if (add_plus) "%+.2f" else "%.2f"
  
  # Format the value with rounding and separators
  formatted_value <- sprintf(fmt, round(value, 2)) # Apply rounding and format
  
  # Replace the decimal point with a comma and add thousand separators
  formatted_value <- gsub("\\.", ",", formatted_value) # Replace decimal with comma
  formatted_value <- gsub("(?<=\\d)(?=(\\d{3})+,)", ".", formatted_value, perl = TRUE) # Add thousand separators
  
  # Append the unit
  paste0(formatted_value, unit)
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

showHintIfEmpty <- function(input, input_id, hint_id, row_id, session) {
  observe({
    # Check if the input is null or contains only empty strings
    if (is.null(input[[input_id]]) || all(input[[input_id]] == "")) {
      shinyjs::show(hint_id) # Show the hint
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('framed-row-complete').addClass('framed-row-incomplete');",
        row_id
      )) # Mark the row as incomplete
    } else {
      shinyjs::hide(hint_id) # Hide the hint
      shinyjs::runjs(sprintf(
        "$('#%s').removeClass('framed-row-incomplete').addClass('framed-row-complete');",
        row_id
      )) # Mark the row as complete
    }
  })
}
