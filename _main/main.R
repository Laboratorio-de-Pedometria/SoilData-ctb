# title: SoilData Data Processing
# subtitle: Process all R scripts in the project
# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Identify all R scripts in the current directory and its subdirectories
# Exceptions: _main/main.R, helper.R
r_scripts <- list.files(pattern = "\\.R$", recursive = TRUE)
r_scripts <- r_scripts[!r_scripts %in% c("_main/main.R", "helper.R")]
r_scripts

# Source each R script
for (script in r_scripts) {
  source(script)
}
