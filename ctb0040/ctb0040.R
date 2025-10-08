# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("mapview")) {
  install.packages("mapview")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0040
# Fertilidade do Solo do Sítio Capinzal
# https://drive.google.com/drive/u/0/folders/1cd8iV6W0JteUNVUD9Fk7-9i0yfCIQO-5
gs <- "1kz5Tb5IVdK6P7i6m_OakCk-oFoRMN3WIQUu00UVkOJ0"
gid_citation <- 0
gid_validation <- 88779986
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0040_validation <- google_sheet(gs, gid_validation)
str(ctb0040_validation)

# Check for negative validation results
sum(ctb0040_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0040_citation <- google_sheet(gs, gid_citation)
str(ctb0040_citation)

# dataset_titulo
dataset_titulo <- ctb0040_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0040_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0040_citation <- data.table::data.table(
  dataset_id = "ctb0040",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0040_citation)

# event ############################################################################################
ctb0040_event <- google_sheet(gs, gid_event)
str(ctb0040_event)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0040_event, old = "ID do evento", new = "observacao_id")
ctb0040_event[, observacao_id := as.character(observacao_id)]
ctb0040_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0040_event, old = "Ano (coleta)", new = "data_ano")
ctb0040_event[, data_ano := as.integer(data_ano)]
ctb0040_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0040_event[, ano_fonte := "original"]

# Longitude -> coord_x
data.table::setnames(ctb0040_event, old = "Longitude", new = "coord_x")
ctb0040_event[, coord_x := as.numeric(coord_x)]
summary(ctb0040_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0040_event, old = "Latitude", new = "coord_y")
ctb0040_event[, coord_y := as.numeric(coord_y)]
summary(ctb0040_event[, coord_y])

# Datum (coord) -> coord_datum
# WGS84
data.table::setnames(ctb0040_event, old = "Datum (coord)", new = "coord_datum")
ctb0040_event[coord_datum == "WGS84", coord_datum := 4326]
ctb0040_event[, coord_datum := as.integer(coord_datum)]
ctb0040_event[, .N, by = coord_datum]

# Check for duplicated coordinates
ctb0040_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0040_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0040_event[, coord_fonte := as.character(coord_fonte)]
ctb0040_event[, .N, by = coord_fonte]

# Precisão (coord) [m] -> coord_precisao
data.table::setnames(ctb0040_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0040_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0040_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0040_event, old = "País", new = "pais_id")
ctb0040_event[, pais_id := as.character(pais_id)]
ctb0040_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0040_event, old = "Estado (UF)", new = "estado_id")
ctb0040_event[, estado_id := as.character(estado_id)]
ctb0040_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0040_event, old = "Município", new = "municipio_id")
ctb0040_event[, municipio_id := as.character(municipio_id)]
ctb0040_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0040_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0040_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0040_event[, amostra_area])

# taxon_sibcs
# Soil classification is missing. We set it to NA_character_
ctb0040_event[, taxon_sibcs := NA_character_]

# taxon_st
# Soil taxonomy is missing. We set it to NA_character_
ctb0040_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# this document don't  have pedregosidade info

ctb0040_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# this document don't  have rochosidade info

ctb0040_event[, rochosidade := NA_character_]

str(ctb0040_event)

# layer ############################################################################################
ctb0040_layer <- google_sheet(gs, gid_layer)
str(ctb0040_layer)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0040_layer, old = "ID do evento", new = "observacao_id")
ctb0040_layer[, observacao_id := as.character(observacao_id)]
ctb0040_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0040_layer, old = "ID da camada", new = "camada_nome")
ctb0040_layer[, camada_nome := as.character(camada_nome)]
ctb0040_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0040_layer, old = "ID da amostra", new = "amostra_id")
ctb0040_layer[, amostra_id := as.character(amostra_id)]
ctb0040_layer[, .N, by = amostra_id]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0040_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0040_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0040_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0040_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0040_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0040_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0040_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0040_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0040_layer[, profund_inf])

# check for missing layers
check_missing_layer(ctb0040_layer)

# compute mid depth
ctb0040_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0040_layer[, profund_mid])

# camada_id
ctb0040_layer <- ctb0040_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0040_layer[, camada_id := 1:.N, by = observacao_id]
ctb0040_layer[, .N, by = camada_id]

# Terra fina [%v] -> terrafina
# A proporção da fração terra fina foi estimada visualmente pelo autor dos dados como sendo de
# aproximadamente 50% (~50).
data.table::setnames(ctb0040_layer, old = "Terra fina [%v]", new = "terrafina")
ctb0040_layer[, terrafina := as.character(terrafina)]
ctb0040_layer[, terrafina := gsub("~", "", terrafina)]
ctb0040_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0040_layer[, terrafina])

# Argila [g/kg] -> argila
data.table::setnames(ctb0040_layer, old = "Argila [g/kg]", new = "argila")
ctb0040_layer[, argila := as.numeric(argila)]
summary(ctb0040_layer[, argila])

# Silte [g/kg] -> silte
data.table::setnames(ctb0040_layer, old = "Silte [g/kg]", new = "silte")
ctb0040_layer[, silte := as.numeric(silte)]
summary(ctb0040_layer[, silte])

# Areia [g/kg] -> areia
data.table::setnames(ctb0040_layer, old = "Areia [g/kg]", new = "areia")
ctb0040_layer[, areia := as.numeric(areia)]
summary(ctb0040_layer[, areia])

# C [g/kg] -> carbono
data.table::setnames(ctb0040_layer, old = "C [g/kg]", new = "carbono")
ctb0040_layer[, carbono := as.numeric(carbono)]
summary(ctb0040_layer[, carbono])

# pH H2O -> ph
data.table::setnames(ctb0040_layer, old = "pH H2O", new = "ph")
ctb0040_layer[, ph := as.numeric(ph)]
summary(ctb0040_layer[, ph])

# CTC pH 7 -> ctc
data.table::setnames(ctb0040_layer, old = "CTC pH 7", new = "ctc")
ctb0040_layer[, ctc := as.numeric(ctc)]
summary(ctb0040_layer[, ctc])

# dsi
# soil bulk density is missing. We set it to NA_real_
ctb0040_layer[, dsi := NA_real_]

str(ctb0040_layer)

# Merge ############################################################################################
# events and layers
ctb0040 <- merge(ctb0040_event, ctb0040_layer, all = TRUE)
ctb0040[, dataset_id := "ctb0040"]
# citation
ctb0040 <- merge(ctb0040, ctb0040_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0040)
# Layers: 32
# Events: 8
# Georeferenced events: 8

# Plot using mapview
if (FALSE) {
  ctb0040_sf <- sf::st_as_sf(
    ctb0040[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0040_sf["argila"])
}

# Write to disk ####################################################################################
ctb0040 <- select_output_columns(ctb0040)
data.table::fwrite(ctb0040, "ctb0040/ctb0040.csv")
data.table::fwrite(ctb0040_event, "ctb0040/ctb0040_event.csv")
data.table::fwrite(ctb0040_layer, "ctb0040/ctb0040_layer.csv")
