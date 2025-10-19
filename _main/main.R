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
_main/main.R,
helper.R,
ctb0067 review

ctb0068 new

ctb0069 review

ctb0070 new

ctb0075 check decimal separator for clay

ctb0079 new

ctb0080 check sand check coordinates

ctb0082 check coordinates

ctb0083 check coordinates

ctb0085 check coordinates

ctb0086 new

ctb0093 new

ctb0094 new

ctb0095 new

ctb0097 new

ctb0098 new

ctb0099 new
)
r_scripts <- list.files(pattern = "\\.R$", recursive = TRUE)
r_scripts <- r_scripts[!r_scripts %in% exceptions]

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

