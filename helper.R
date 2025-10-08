# Helper functions for SoilData-ctb
# author: Alessandro Samuel-Rosa
# data: 2025

# Describe soil data ###############################################################################
# This function summarizes a data.frame containing soil data.
# It prints the column names, number of layers, number of events, and number of georeferenced events.
# x: data.frame containing soil data
# na.rm: logical, whether to remove NA values (default is TRUE)
# Returns: None, prints the summary to the console
# Example usage: summary_soildata(soil_data, na.rm = TRUE)
# Note: The function assumes that the data.table package is loaded and that the data has columns
# 'dataset_id', 'observacao_id', 'coord_x', and 'coord_y'.
summary_soildata <- function(x, na.rm = TRUE) {
  id <- c("dataset_id", "observacao_id")
  cat("Column names:")
  cat("\n", paste(names(x)), collapse = " ")
  cat("\nLayers:", nrow(x))
  cat("\nEvents:", nrow(unique(x[, ..id])))
  cat("\nGeoreferenced events:", nrow(unique(x[!is.na(coord_x) & !is.na(coord_y), ..id])))
  cat("\n")
}

# Read Google Sheet ################################################################################
# This function reads a Google Sheet and returns it as a data.table.
# gs: Google Sheet ID
# gid: Google Sheet GID
# Returns: data.table with the contents of the Google Sheet
# Example usage: google_sheet("1A2B3C4D5E6F7G8H9I0J", "123456789")
# Note: Ensure that the Google Sheet is publicly accessible or shared with the appropriate
# permissions.
google_sheet <- function(gs, gid) {
  sheet_path <- paste0(
    "https://docs.google.com/spreadsheets/u/1/d/",
    gs,
    "/export?format=tsv&id=",
    gs,
    "&gid=",
    gid
  )
  dt <- data.table::fread(
    sheet_path,
    dec = ",", sep = "\t", na.strings = c("NA", "NaN", "-", "#N/A"), header = TRUE
  )
  return(dt)
}

# Read SoilData Catalog ############################################################################
# This function reads a spreadsheet catalog containing the spreadsheet ID and GID for each dataset.
# It returns a data.table with the dataset ID, Google Sheet ID, and GID.
# ctb: character string, the dataset ID (e.g., "ctb0001")
# Returns: data.table with the dataset ID, Google Sheet ID, and GID
# Example usage: soildata_catalog("ctb0001")
# Note: The function assumes that the google_sheet function is available and that the Google Sheet
# with the catalog is publicly accessible.
soildata_catalog <- function(ctb) {
  # Read the catalog from the Google Sheet
  catalog <- google_sheet(gs = "13_6nt97aNc3bWHrfXW-OpkmtvVh-D37DLgRnu6Yps48", gid = 0)
  # Keep only relevant columns
  # ID, gs_id	gid_citation	gid_event	gid_layer	gid_validation
  catalog <- catalog[, .(ID, gs_id, gid_citation, gid_event, gid_layer, gid_validation)]
  # Filter by ID == ctb
  catalog <- catalog[ID == ctb]
  # Check if catalog is empty
  if (nrow(catalog) == 0) {
    stop(paste("No catalog found for dataset ID:", ctb))
    }
  return(catalog)
}

# Solve plus sign in layer depth limits ############################################################
# This function checks if a string (soil layer depth, generally the last record of the 'profund_inf'
# column) ends with a plus sign ('+'). This generraly occurs when the final depth of a soil layer is
# not specified, indicating that the layer extends beyond a certain depth. If the string ends with
# a plus sign, it evaluates the string as an R expression and adds a specified depth value
# (default is 20) to it. The result is returned as a character string.
# x: string representing the soil layer depth
# plus.depth: numeric value to be added to the depth if it ends with a plus sign (default is 20)
# Returns: character string representing the updated soil layer depth
# Example usage: depth_plus("100+") # returns "120"
# Note: The function uses `eval(parse(...))` to evaluate the string as an R expression,
# which allows for dynamic calculations based on the string content.
depth_plus <- function (x, plus.depth = 20) {
  if (grepl("\\+$", x)) {
    x <- eval(parse(text = paste0(x, plus.depth)))
    x <- as.character(x)
  }
  return(x)
}

# Solve slash in layer depth limits ################################################################
# This function checks if a string contains a slash ('/'), specifically, the depth limits of soil
# layers. This is often used to represent a range of depths in a soil layer with irregular, wavy, 
# or broken boundaries. If a slash is found, it replaces it with a plus sign ('+'), evaluates the
# string as an R expression, and divides the result by the number of slashes plus one. The final
# result is returned as a character string. This function is useful for processing depth limits
# that are represented as ranges, allowing for a more standardized representation of soil layer
# depths.
# x: string representing the depth limits of soil layers
# Returns: character string representing the processed depth limits
# Example usage: depth_slash("10/20") # returns "15"
# Note: The function uses `eval(parse(...))` to evaluate the string as an R expression,
# which allows for dynamic calculations based on the string content.
depth_slash <- function(x) {
  if (grepl("/", x)) {
    n_slash <- sum(grepl("/", x))
    x <- gsub("/", "+", x)
    x <- eval(parse(text = x)) / (n_slash + 1)
    x <- as.character(x)
  }
  return(x)
}

# Check for missing layers #########################################################################
# This function checks for missing layers in a data.table containing soil layer data. It identifies
# gaps in the depth intervals of soil layers for each observation ID. Gaps are defined as instances
# where the upper depth of one layer does not match the lower depth of the next layer. They occur
# when soil sampling in the field does not cover the entire depth range, leading to missing layers
# in the dataset. The function returns a data.table containing the missing layers, including the
# observation ID, layer name, and depth intervals.
# layer_data: data.table containing soil layer data
# Returns: data.table with missing layers, including 'observacao_id', 'camada_nome',
#          'profund_sup', and 'profund_inf'
# Example usage: any_missing_layer(layer_data)
# Note: The function assumes that the data.table package is loaded and that the data has the necessary
# columns for observation ID, layer name, and depth intervals. If missing layers are found, a message
# is printed to the console.
any_missing_layer <- function(layer_data) {
  # Start by sorting the data.table by observacao_id, profund_sup, and profund_inf
  layer_data <- layer_data[order(observacao_id, profund_sup, profund_inf)]
  # Check for each observacao_id if there is a missing layer
  missing_layers <- layer_data[
    shift(profund_inf) != profund_sup & profund_sup > 0,
    .(observacao_id, camada_nome, profund_sup, profund_inf)
  ]
  if (nrow(missing_layers) > 0) {
    message(
      "Missing layers were found. ",
      "Check the source dataset if this is correct or if there has been an error ",
      "when recording the layer depth limits."
    )
  }
  # Return the missing layers
  return(missing_layers)
}
check_missing_layer <- any_missing_layer

# Add missing layers ###############################################################################
# This function adds missing layers to a data.table containing soil layer data.
# It checks for each event ID if the top layer is missing (i.e., if the minimum depth of the top
# layer is greater than 0). If the top layer is missing, a new row is added to the data.table with
# depth_top set to 0 and depth_bottom set to the minimum depth of the top layer. The function also
# identifies gaps in the depth intervals of soil layers for each event ID and adds rows for these
# missing layers. The final result is a data.table with all layers, including the added missing
# layers, ordered by event ID and depth_top.
# x: data.table containing soil layer data with columns for event ID, depth top, depth bottom, and layer ID.
# event.id: column name for event ID (default is "observacao_id")
# depth.top: column name for depth top (default is "profund_sup")
# depth.bottom: column name for depth bottom (default is "profund_inf")
# layer.id: column name for layer ID (default is "camada_id")
# Returns: data.table with all layers, including added missing layers, ordered by event ID and depth_top.
# Example usage: add_missing_layer(layer_data)
# Note: The function assumes that the data.table package is loaded and that the data has the
# necessary columns for event ID, depth top, depth bottom, and layer ID.
add_missing_layer <- function(
    x, event.id = "observacao_id", depth.top = "profund_sup", depth.bottom = "profund_inf",
    layer.id = "camada_id") {
  
  # Ensure x is a data.table
  data.table::setDT(x)

  # Rename columns
  old_names <- c(event.id, depth.top, depth.bottom, layer.id)
  new_names <- c("event_id", "depth_top", "depth_bottom", "layer_id")
  data.table::setnames(x, old = old_names, new = new_names)

  # Check for each event_id if it is missing the top layer, i.e. min(depth_top) > 0
  x[, missing_top := min(depth_top) > 0, by = event_id]
  
  # If the top layer is missing, add a row to the data.table
  # For the new row, set depth_top = 0 and depth_bottom = min(depth_top)
  x <- rbind(x, x[missing_top == TRUE, .(
      event_id = event_id,
      depth_top = 0,
      depth_bottom = min(depth_top)
    )], fill = TRUE)
  x[, missing_top := NULL]
  
  # Order x by profile_id and depth_top
  data.table::setorder(x, event_id, depth_top)

  # Create a new data.table to store the missing layers
  missing_layers <- x[, .(
    depth_top = depth_bottom[-.N],
    depth_bottom = shift(depth_top, type = "lead")[-.N]
  ), by = event_id][depth_top != depth_bottom]

  # Combine the original data with the missing layers
  result <- rbind(x, missing_layers, fill = TRUE)

  # Order the result by event.id and depth_top
  data.table::setorder(result, event_id, depth_top)

  # Reset layer_id according to the new order
  result[, layer_id := seq_len(.N), by = event_id]

  # Rename columns
  data.table::setnames(result, old = new_names, new = old_names)

  # Return the result
  return(result)
}

# Check for repeated layers ########################################################################
# This function checks for repeated layers in a data.table containing soil layer data.
# It identifies instances where the same layer name, upper depth, and lower depth appear multiple
# times for the same observation ID. Repeated layers can occur due to data entry errors or
# inconsistencies in the dataset. The function returns a data.table containing the repeated layers,
# including the observation ID, layer name, upper depth, lower depth, and the count of occurrences.
# data: data.table containing soil layer data with columns 'observacao_id', 'camada_nome',
#       'profund_sup', and 'profund_inf'.
# Returns: data.table with repeated layers, including 'observacao_id', 'camada_nome',
#          'profund_sup', 'profund_inf', and 'N' (count of occurrences).
# Example usage: check_repeated_layer(layer_data)
# Note: The function assumes that the data.table package is loaded and that the data has the necessary
# columns for observation ID, layer name, and depth intervals. If repeated layers are found, a
# message is printed to the console.
check_repeated_layer <- function(data) {
  repeated_layers <- data[, .N, by = .(observacao_id, camada_nome, profund_sup, profund_inf)][N > 1]
  if (nrow(repeated_layers) > 0) {
    message(
      "Repeated layers were found. ",
      "Check the source dataset if this is correct or if there has been an error ",
      "during data entry."
    )
  }
  return(repeated_layers)
}

# Check for empty layers ###########################################################################
# This function checks for empty layers in a data.table containing soil layer data.
# It identifies layers where a specific variable (e.g., 'argila', 'silte', 'areia') has missing values (NA).
# The function returns a data.table containing the empty layers, including the observation ID,
# layer name, upper depth, lower depth, and the variable with missing values.
# x: data.table containing soil layer data with columns 'observacao_id', 'camada_nome',
#     'profund_sup', 'profund_inf', and the variable to check for missing values (e.g., 'argila').
# var: character string representing the variable to check for missing values (e.g., 'argila')
# Returns: data.table with empty layers, including 'observacao_id', 'camada_nome',
#          'profund_sup', 'profund_inf', and the variable with missing values.
# Example usage: check_empty_layer(layer_data, "argila")
# Note: The function assumes that the data.table package is loaded and that the data has the necessary
# columns for observation ID, layer name, depth intervals, and the variable to check. If empty
# layers are found, a message is printed to the console.
find_empty_layer <- function(x, var) {
  # Select only the rows where the specified variable is NA
  empty_layers <- x[is.na(get(var)),
    .(observacao_id, camada_nome, profund_sup, profund_inf, get(var))]
  if (nrow(empty_layers) > 0) {
    message(
      "Empty layers were found for variable '", var, "'. ",
      "Check the source dataset if this is correct or if there has been an error ",
      "during data entry."
    )
  }
  return(empty_layers)
}
check_empty_layer <- find_empty_layer

# Spline function to fill empty layers #############################################################
# This function fills missing values in a numeric vector using spline interpolation.
# It takes a numeric vector 'y' with missing values (NA) and a corresponding numeric vector 'x'
# representing the x-coordinates. The function applies several checks to determine if spline
# interpolation is appropriate. If the conditions are met, it performs spline interpolation to
# fill the missing values. Otherwise, it returns the original vector 'y' unchanged.
# y: numeric vector with missing values (NA) to be filled
# x: numeric vector representing the x-coordinates corresponding to 'y'
# Returns: numeric vector with missing values filled using spline interpolation, or the original
#          vector 'y' if conditions are not met
# Example usage: fill_empty_layer(c(1, NA, 3), c(1, 2, 3)) # returns c(1, 2, 3)
# Note: The function includes several checks to ensure that spline interpolation is only applied
# when appropriate, such as when there are enough non-missing values and no consecutive missing
# values. It prints a message to the console indicating whether interpolation was performed.
fill_empty_layer <- function(y, x) {
  # Check if y is numeric
  if (!is.numeric(y)) {
    stop("y must be a numeric vector")
  }
  # Standard output message when conditions for interpolation are not met
  no_interpolation_message <-
    "Conditions not met. Spline interpolation not applied. Returning original vector."

  # If only one point, return it
  if (length(y) == 1) {
    message(no_interpolation_message)
    return(y)
  }
  # If more NA than not NA, return y
  if (sum(!is.na(y)) < sum(is.na(y))) {
    message(no_interpolation_message)
    return(y)
  }
  # If length(y) == 2, one NA, return y
  if (length(y) == 2 & sum(is.na(y)) == 1) {
    message(no_interpolation_message)
    return(y)
  }
  # If y[1] is NA, return NA
  if (is.na(y[1])) {
    message(no_interpolation_message)
    return(y)
  }
  # If all NA, return y
  if (sum(!is.na(y)) == 0) {
    message(no_interpolation_message)
    return(y)
  }
  # If three points, two not NA, and y[3] is NA, return y
  if (length(y) == 3 & sum(!is.na(y)) == 2 & is.na(y[3])) {
    message(no_interpolation_message)
    return(y)
  }
  # If two consecutive points are NA, return y
  if (sum(is.na(y)) > 1 & any(diff(is.na(y)) == 1)) {
    message(no_interpolation_message)
    return(y)
  }
  # Else, return spline
  message("Conditions met. Spline interpolation applied.")
  spline(y = y, x = x, xout = x, method = "natural")$y
}

# Set GPS precision ################################################################################
# This function sets the GPS precision for soil data collection.
# It returns a numeric value representing the precision in meters.
# Returns: numeric value representing the GPS precision (default is 30 meters)
# Example usage: set_gps_precision() # returns 30
# Note: The function can be modified to return different precision values based on the requirements
# of the soil data collection protocol.
set_gps_precision <- function() {
  return(30)
}

# Select columns for final output ##################################################################
# This function selects specific columns from a data.table containing soil data.
# It checks if the target columns exist in the data and returns a subset of the data.table
# containing only those columns. If any of the target columns are missing, it raises an error.
# data: data.table containing soil data
# Returns: data.table with selected columns
# Example usage: select_output_columns(soil_data)
# Note: The function assumes that the data.table package is loaded and that the data has the
# necessary columns for soil data. The target columns are predefined and include identifiers,
# coordinates, country, state, municipality, sample area, taxon information, layer name,
# sample ID, depth limits, and soil properties.
select_output_columns <- function(data) {
  target_columns <- c(
    "dataset_id", "dataset_titulo", "dataset_licenca",
    "observacao_id",
    "data_ano", "ano_fonte",
    "coord_x", "coord_y", "coord_datum", "coord_fonte", "coord_precisao",
    "pais_id", "estado_id", "municipio_id",
    "amostra_area",
    "taxon_sibcs", "taxon_st",
    "pedregosidade", "rochosidade",
    "camada_nome", "amostra_id",
    "profund_sup", "profund_inf",
    "terrafina",
    "argila", "silte", "areia",
    "carbono", "ph", "ctc", "dsi"
  )
  missing_cols <- setdiff(target_columns, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  # Check if spatial coordinates are in decimal degrees (WGS84). If not, raise an error,
  # warning the user to convert them before proceeding.
  if (any(data$coord_x < -180 | data$coord_x > 180 | data$coord_y < -90 | data$coord_y > 90, na.rm = TRUE)) {
    stop("Spatial coordinates (coord_x, coord_y) must be in decimal degrees (WGS84).\n"
    "Please convert them before proceeding.")
  }
  # Check if the sample points that have spatial coordinates (coord_x, coord_y) fall within the
  # bounding box of the Brazilian territory.
  bb <- c(-73.9872354804, -33.7683777809, -34.7299934555, 5.24448639569)
  if (any(data$coord_x < bb[1] | data$coord_x > bb[2] | data$coord_y < bb[3] | data$coord_y > bb[4], na.rm = TRUE)) {
    stop("Some spatial coordinates (coord_x, coord_y) fall outside the bounding box of Brazil.\n"
    "Please check and correct them before proceeding.")
  } 
  return(data[, ..target_columns])
}
