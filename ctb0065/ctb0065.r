# autor: Felipe Brun Vergani
# data: 2025

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("sf")) {
  install.packages("sf")
  library("sf")
}
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
}
if (!require("parzer")) {
  install.packages("parzer")
  library("parzer")
}

# Source helper functions
source("./helper.R")


# Google Sheet #####################################################################################
# ctb0065
# Dados de "Solos de três áreas de  restinga. 1. Morfologia, caracterização e classificação"
# 
# https://drive.google.com/drive/folders/1N90u15zgwHxxOU__vQJKfivjubk5_gb0


ctb0065_ids <- soildata_catalog("ctb0065")

# validation #####################################################################################

ctb0065_validation <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_validation)
str(ctb0065_validation)

# Check for negative validation results
sum(ctb0065_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0065_citation <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_citation)
str(ctb0065_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0065_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0065_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0065_citation <- data.table::data.table(
  dataset_id = "ctb0065",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0065_citation)

# event #####################################################################################
ctb0065_event <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_event)
str(ctb0065_event)

#PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0065_event, old = "ID do evento", new = "observacao_id")
ctb0065_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0065_event[, observacao_id]) > 1)


# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0065_event, old = "Ano (coleta)", new = "data_ano")
ctb0065_event[, data_ano := NA_character_]


# data_fonte
ctb0065_event[!is.na(data_ano), data_fonte := "original"]
ctb0065_event[, .N, by = data_fonte]


# coord_datum
# Datum (coord) -> coord_datum
data.table::setnames(ctb0065_event, old = "Datum (coord)", new = "coord_datum")
ctb0065_event[, coord_datum := NA_real_]


# Missing seconds precision on Longitude / Latitude
# The parzer library was added to convert the characters into values appropriate for the coordinate table.
# Longitude -> coord_x
data.table::setnames(ctb0065_event, old = "Longitude", new = "coord_x")
ctb0065_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0065_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0065_event, old = "Latitude", new = "coord_y")
ctb0065_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0065_event[, coord_y])

# Check for duplicate coordinates
any(ctb0065_event[, .N, by = .(coord_x, coord_y)][, N] > 1)

# Precisão (coord) [m] -> coord_precisao
# Coordinates were attributed with little knowledge of the precision. We set it to NA_real_
data.table::setnames(ctb0065_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0065_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0065_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0065_event[, coord_fonte := NA_real_]

# País -> pais_id
data.table::setnames(ctb0065_event, old = "País", new = "pais_id")
ctb0065_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0065_event, old = "Estado (UF)", new = "estado_id")
ctb0065_event[, estado_id := "RJ"]

# Município -> municipio_id
data.table::setnames(ctb0065_event, old = "Município", new = "municipio_id")
ctb0065_event[, municipio_id := as.character(municipio_id)]
ctb0065_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0065_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0065_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0065_event[, amostra_area])

# taxon_sibcs
ctb0065_event[, taxon_sibcs := NA_character_]

# taxon_st_1999

ctb0065_event[, taxon_st := NA_character_]



str(ctb0065_event)

# layers ###########################################################################################
ctb0065_layer <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_layer)
str(ctb0065_layer)

# PROCESS FIELDS



