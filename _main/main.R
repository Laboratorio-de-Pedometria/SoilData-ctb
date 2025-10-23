# title: SoilData Data Processing
# subtitle: Process all R scripts in the project
# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install callr if not already installed. This is a lightweight package with no dependencies.
if (!require("callr")) {
  install.packages("callr")
  library("callr")
}

# Identify all R scripts in the current directory and its subdirectories
# Exceptions:
# - General purpose scripts: _main/main.R, helper.R
# - Scripts with known issues or not reviewed yet
exceptions <- c(
  "_main/main.R",
  "helper.R",
  "ctb0067/ctb0067.R", # needs review
  "ctb0068/ctb0068.R", # new
  "ctb0069/ctb0069.R", # review
  "ctb0070/ctb0070.R", # new
  "ctb0075/ctb0075.R", # check decimal separator for clay
  "ctb0079/ctb0079.R", # new
  "ctb0080/ctb0080.R", # check sand check coordinates
  "ctb0082/ctb0082.R", # check coordinates
  "ctb0083/ctb0083.R", # check coordinates
  "ctb0085/ctb0085.R", # check coordinates
  "ctb0086/ctb0086.R", # new
  "ctb0094/ctb0094.R", # new
  "ctb0095/ctb0095.R", # new
  "ctb0097/ctb0097.R", # new
  "ctb0098/ctb0098.R", # new
  "ctb0099/ctb0099.R", # new
  "ctb00101/ctb00101.R" # Included in ctb0054
)
r_scripts <- list.files(pattern = "\\.R$", recursive = TRUE)
r_scripts <- r_scripts[!r_scripts %in% exceptions]
r_scripts <- sample(r_scripts) # randomize order

# Source each R script
# Run each R script in a separate, clean R session to avoid variable conflicts.
# Use a try-catch block to handle errors gracefully, interrupting the loop if a script fails.
for (script in r_scripts) {
  message(paste("Processing script:", script))
  # Use callr::rscript to run the script in a new R process.
  # `show = TRUE` streams the output to the console in real-time.
  # `fail_on_status = TRUE` makes the function throw an error if the script fails.
  result <- try(callr::rscript(script, show = TRUE, fail_on_status = TRUE))

  # If an error occurs, `try()` returns an object of class "try-error".
  # We check for this and stop the execution if an error was found.
  if (inherits(result, "try-error")) {
    stop(paste("Execution halted due to an error in script:", script), call. = FALSE)
  }
  cat("================================\n") # Print a newline for better readability between scripts
}

