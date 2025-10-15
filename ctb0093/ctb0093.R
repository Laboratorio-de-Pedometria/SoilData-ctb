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
# ctb0093
# Dados de "Dados de Carbono de Solos - Projeto SIGecotur/Projeto Forense"
# 
# https://docs.google.com/spreadsheets/d/1vTspAdJTVhhRU73Ddqg_4zSQQEG-YwINZxhONezGIe4/edit?usp=sharing


ctb0093_ids <- soildata_catalog("ctb0093")

# validation #####################################################################################

ctb0093_validation <- google_sheet(ctb0093_ids$gs_id, ctb0093_ids$gid_validation)
str(ctb0093_validation)

# Check for negative validation results
sum(ctb0093_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0093_citation <- google_sheet(ctb0093_ids$gs_id, ctb0093_ids$gid_citation)
str(ctb0093_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0093_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0093_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0093_citation <- data.table::data.table(
  dataset_id = "ctb0093",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0093_citation)

# event #####################################################################################
ctb0093_event <- google_sheet(ctb0093_ids$gs_id, ctb0093_ids$gid_event)
str(ctb0093_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0093_event, old = "ID do evento", new = "observacao_id")
ctb0093_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0093_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0093_event, old = "Ano (coleta)", new = "data_ano")
ctb0093_event[, data_ano := as.integer(data_ano)]
ctb0093_event[, .N, by = data_ano]

# ano_fonte
ctb0093_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0093_event[, .N, by = ano_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0093_event, old = "Longitude", new = "coord_x")
ctb0093_event[, coord_x := as.numeric(gsub(",", ".", coord_x))]
summary(ctb0093_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0093_event, old = "Latitude", new = "coord_y")
ctb0093_event[, coord_y := as.numeric(gsub(",", ".", coord_y))]
summary(ctb0093_event[, coord_y])

#utilizado para plotagem
dados_completos <- ctb0093[!is.na(coord_x) & !is.na(coord_y)]

# Check for duplicate coordinates
ctb0093_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# already in WGS84
data.table::setnames(ctb0093_event, old = "Datum (coord)", new = "coord_datum")
ctb0093_event[coord_datum == "WGS84", coord_datum := 4326]

# Precisão (coord) -> coord_precisao
# We set it to NA_real_ (missing)
data.table::setnames(ctb0093_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0093_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
# GPS Garmin
data.table::setnames(ctb0093_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0093_event[, coord_fonte := as.character(coord_fonte)]


# País -> pais_id
data.table::setnames(ctb0093_event, old = "País", new = "pais_id")
ctb0093_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0093_event, old = "Estado (UF)", new = "estado_id")
ctb0093_event[, estado_id := as.character(estado_id)]
ctb0093_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0093_event, old = "Município", new = "municipio_id")
ctb0093_event[, municipio_id := as.character(municipio_id)]
ctb0093_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0093_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0093_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0093_event[, amostra_area])

# SiBCS  -> taxon_sibcs
# is missing in this document.
ctb0093_event[, taxon_sibcs := NA_character_]

# taxon_st 
# missing this soil taxonomy on document
ctb0093_event[, taxon_st := NA_character_]
ctb0093_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
# missing in this document.

ctb0093_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# missing in this document.

ctb0093_event[, rochosidade := NA_character_]



str(ctb0093_event)

# layers ###########################################################################################
ctb0093_layer <- google_sheet(ctb0093_ids$gs_id, ctb0093_ids$gid_layer)
str(ctb0093_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0093_layer, old = "ID do evento", new = "observacao_id")
ctb0093_layer[, observacao_id := as.character(observacao_id)]
ctb0093_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0093_layer, old = "ID da camada", new = "camada_nome")
ctb0093_layer[, camada_nome := as.character(camada_nome)]
ctb0093_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0093_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0093_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0093_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0093_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0093_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0093_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0093_layer[, profund_inf])

#areia
#missing in this document.
ctb0093_layer[, areia := NA_real_]

#silte
#missing in this document.
ctb0093_layer[, silte := NA_real_]

#argila
#missing in this document.
ctb0093_layer[, argila := NA_real_]

#terrafina
#missing in this document.
ctb0093_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0093_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0093_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0093_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C [%]
# new: carbono
data.table::setnames(ctb0093_layer, old = "C [%]", new = "carbono")
ctb0093_layer[, carbono := (as.numeric(carbono)*10)]
ctb0093_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0093_layer[, carbono])

# ctc
#missing in this document.
ctb0093_layer[, ctc := NA_real_]


# ph
#missing in this document.
ctb0093_layer[, ph := NA_real_]

# dsi 
#missing in this document.
ctb0093_layer[, dsi := NA_real_]




str(ctb0093_layer)

# Merge ############################################################################################
# events and layers
ctb0093 <- merge(ctb0093_event, ctb0093_layer, all = TRUE)
ctb0093[, dataset_id := "ctb0093"]
# citation
ctb0093 <- merge(ctb0093, ctb0093_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0093)

#Layers: 819
#Events: 363
#Georeferenced events: 360


# Plot using mapview
if (FALSE) {
  ctb0093_sf <- sf::st_as_sf(
    dados_completos,
    coords = c("coord_x", "coord_y"),
    crs = 4326
  )
  mapview::mapview(ctb0093_sf["carbono"])
}

# Write to disk ####################################################################################
ctb0093 <- select_output_columns(ctb0093)
data.table::fwrite(ctb0093, "ctb0093/ctb0093.csv")
data.table::fwrite(ctb0093_event, "ctb0093/ctb0093_event.csv")
data.table::fwrite(ctb0093_layer, "ctb0093/ctb0093_layer.csv")
