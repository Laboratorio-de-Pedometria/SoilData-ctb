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
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0092
# Dados de "Dados de Carbono de Solos - Projeto SIGecotur/Projeto Forense"
# 
# https://docs.google.com/spreadsheets/d/1vTspAdJTVhhRU73Ddqg_4zSQQEG-YwINZxhONezGIe4/edit?usp=sharing


ctb0092_ids <- soildata_catalog("ctb0092")

# validation #####################################################################################

ctb0092_validation <- google_sheet(ctb0092_ids$gs_id, ctb0092_ids$gid_validation)
str(ctb0092_validation)

# Check for negative validation results
sum(ctb0092_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0092_citation <- google_sheet(ctb0092_ids$gs_id, ctb0092_ids$gid_citation)
str(ctb0092_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0092_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0092_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0092_citation <- data.table::data.table(
  dataset_id = "ctb0092",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0092_citation)

# event #####################################################################################
ctb0092_event <- google_sheet(ctb0092_ids$gs_id, ctb0092_ids$gid_event)
str(ctb0092_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0092_event, old = "ID do evento", new = "observacao_id")
ctb0092_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0092_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0092_event, old = "Ano (coleta)", new = "data_ano")
ctb0092_event[, data_ano := as.integer(data_ano)]
ctb0092_event[, .N, by = data_ano]

# ano_fonte
ctb0092_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0092_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0092_event, old = "Longitude", new = "coord_x")
ctb0092_event[, coord_x := as.numeric(coord_x)]
summary(ctb0092_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0092_event, old = "Latitude", new = "coord_y")
ctb0092_event[, coord_y := as.numeric(coord_y)]
summary(ctb0092_event[, coord_y])

# Check for duplicate coordinates
ctb0092_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# already in WGS84
data.table::setnames(ctb0092_event, old = "Datum (coord)", new = "coord_datum")
ctb0092_event[coord_datum == "WGS84", coord_datum := 4326]

# Precisão (coord) -> coord_precisao
# We set it to NA_real_ (missing)
data.table::setnames(ctb0092_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0092_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
# GPS Garmin
data.table::setnames(ctb0092_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0092_event[, coord_fonte := as.character(coord_fonte)]


# País -> pais_id
data.table::setnames(ctb0092_event, old = "País", new = "pais_id")
ctb0092_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0092_event, old = "Estado (UF)", new = "estado_id")
ctb0092_event[, estado_id := as.character(estado_id)]
ctb0092_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0092_event, old = "Município", new = "municipio_id")
ctb0092_event[, municipio_id := as.character(municipio_id)]
ctb0092_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0092_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0092_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0092_event[, amostra_area])

# SiBCS  -> taxon_sibcs
# is missing in this document.
ctb0092_event[, taxon_sibcs := NA_character_]

# taxon_st 
# missing this soil taxonomy on document
ctb0092_event[, taxon_st := NA_character_]
ctb0092_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
# missing in this document.

ctb0092_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# missing in this document.

ctb0092_event[, rochosidade := NA_character_]



str(ctb0092_event)

# layers ###########################################################################################
ctb0092_layer <- google_sheet(ctb0092_ids$gs_id, ctb0092_ids$gid_layer)
str(ctb0092_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0092_layer, old = "ID do evento", new = "observacao_id")
ctb0092_layer[, observacao_id := as.character(observacao_id)]
ctb0092_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0092_layer, old = "ID da camada", new = "camada_nome")
ctb0092_layer[, camada_nome := as.character(camada_nome)]
ctb0092_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0092_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0092_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0092_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0092_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0092_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0092_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0092_layer[, profund_inf])

#areia
#missing in this document.
ctb0092_layer[, areia := NA_real_]

#silte
#missing in this document.
ctb0092_layer[, silte := NA_real_]

#argila
#missing in this document.
ctb0092_layer[, argila := NA_real_]

#terrafina
#missing in this document.
ctb0092_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0092_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0092_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0092_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C total [%]
# new: carbono
data.table::setnames(ctb0092_layer, old = "C total [%]", new = "carbono")
ctb0092_layer[, carbono := (as.numeric(carbono)*10)]
ctb0092_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0092_layer[, carbono])

# ctc
#missing in this document.
ctb0092_layer[, ctc := NA_real_]


# ph
#missing in this document.
ctb0092_layer[, ph := NA_real_]

# dsi 
#missing in this document.
ctb0092_layer[, dsi := NA_real_]




str(ctb0092_layer)

# Merge ############################################################################################
# events and layers
ctb0092 <- merge(ctb0092_event, ctb0092_layer, all = TRUE)
ctb0092[, dataset_id := "ctb0092"]
# citation
ctb0092 <- merge(ctb0092, ctb0092_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0092)

#Layers: 70
#Events: 38
#Georeferenced events: 38


# Plot using mapview
if (FALSE) {
  ctb0092_sf <- sf::st_as_sf(
    ctb0092[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0092_sf["carbono"])
}

# Write to disk ####################################################################################
ctb0092 <- select_output_columns(ctb0092)
data.table::fwrite(ctb0092, "ctb0092/ctb0092.csv")
data.table::fwrite(ctb0092_event, "ctb0092/ctb0092_event.csv")
data.table::fwrite(ctb0092_layer, "ctb0092/ctb0092_layer.csv")
