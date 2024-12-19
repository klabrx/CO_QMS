# functions.R


format_value <- function(value, unit = "", add_plus = FALSE) {
  if (is.na(value)) {
    return("NA")
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


check_if_complete <- function(input, input_id, hint_id, row_id, session) {
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
