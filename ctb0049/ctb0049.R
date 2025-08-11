# author: Alessandro Samuel-Rosa
# data: 2024 CC-BY
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
# ctb0049
# Dados do Assentamento Nazareh
# https://drive.google.com/drive/folders/1ZMdq0UILeBJKIsI2pjeB3Qg-H_l0kBYN
gs <- "1joye9VKUJsXQITZ5AqZvcWBHDIYj_k6QhABzJ9uzjns"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0049_validation <- google_sheet(gs, gid_validation)
str(ctb0049_validation)

# Check for negative validation results
sum(ctb0049_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0049_citation <- google_sheet(gs, gid_citation)
str(ctb0049_citation)

# dataset_titulo
dataset_titulo <- ctb0049_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0049_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0049_citation <- data.table::data.table(
  dataset_id = "ctb0049",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0049_citation)

# event ############################################################################################
ctb0049_event <- google_sheet(gs, gid_event)
str(ctb0049_event)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0049_event, old = "ID do evento", new = "observacao_id")
ctb0049_event[, observacao_id := as.character(observacao_id)]
ctb0049_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0049_event, old = "Ano (coleta)", new = "data_ano")
ctb0049_event[, data_ano := as.integer(data_ano)]
ctb0049_event[, .N, by = data_ano]

# data_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0049_event[!is.na(data_ano), data_fonte := "original"]
ctb0049_event[, .N, by = data_fonte]

# Coord. X -> coord_x
data.table::setnames(ctb0049_event, old = "Coord. X", new = "coord_x")
ctb0049_event[, coord_x := as.numeric(coord_x)]
summary(ctb0049_event[, coord_x])

# Coord. Y -> coord_y
data.table::setnames(ctb0049_event, old = "Coord. Y", new = "coord_y")
ctb0049_event[, coord_y := as.numeric(coord_y)]
summary(ctb0049_event[, coord_y])

# Datum (coord) -> coord_datum
# SIRGAS 2000 Zona 21S
data.table::setnames(ctb0049_event, old = "Datum (coord)", new = "coord_datum")
ctb0049_event[coord_datum == "SIRGAS 2000 Zona 21S", coord_datum := 31981]
ctb0049_event[, coord_datum := as.integer(coord_datum)]
ctb0049_event[, .N, by = coord_datum]

# Transform coordinates to WGS84
ctb0049_event_sf <- sf::st_as_sf(
  ctb0049_event[coord_datum == 31981],
  coords = c("coord_x", "coord_y"), crs = 31981
)
ctb0049_event_sf <- sf::st_transform(ctb0049_event_sf, 4326)
ctb0049_event_sf <- sf::st_coordinates(ctb0049_event_sf)
ctb0049_event[coord_datum == 31981, coord_x := ctb0049_event_sf[, 1]]
ctb0049_event[coord_datum == 31981, coord_y := ctb0049_event_sf[, 2]]
ctb0049_event[coord_datum == 31981, coord_datum := 4326]
rm(ctb0049_event_sf)
summary(ctb0049_event[, .(coord_datum, coord_x, coord_y)])

# Check for duplicated coordinates
ctb0049_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0049_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0049_event[, coord_fonte := as.character(coord_fonte)]
ctb0049_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is missing. As the source of the coordinates is GPS, we set
# it to 30 m.
data.table::setnames(ctb0049_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0049_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0049_event[is.na(coord_precisao) & coord_fonte == "GPS", coord_precisao := 30]
summary(ctb0049_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0049_event, old = "País", new = "pais_id")
ctb0049_event[, pais_id := as.character(pais_id)]
ctb0049_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0049_event, old = "Estado (UF)", new = "estado_id")
ctb0049_event[, estado_id := as.character(estado_id)]
ctb0049_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0049_event, old = "Município", new = "municipio_id")
ctb0049_event[, municipio_id := as.character(municipio_id)]
ctb0049_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0049_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0049_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0049_event[, amostra_area])

# Classificação do solo -> taxon_sibcs
data.table::setnames(ctb0049_event, old = "Classificação do solo", new = "taxon_sibcs")
ctb0049_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0049_event[, .N, by = taxon_sibcs]

# taxon_st
# Soil taxonomy is missing in this dataset. We set it to NA_character_
ctb0049_event[, taxon_st := NA_character_]

str(ctb0049_event)

# layer ############################################################################################
ctb0049_layer <- google_sheet(gs, gid_layer)
str(ctb0049_layer)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0049_layer, old = "ID do evento", new = "observacao_id")
ctb0049_layer[, observacao_id := as.character(observacao_id)]
ctb0049_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0049_layer, old = "ID da camada", new = "camada_nome")
ctb0049_layer[, camada_nome := as.character(camada_nome)]
ctb0049_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0049_layer, old = "ID da amostra", new = "amostra_id")
ctb0049_layer[, amostra_id := as.character(amostra_id)]
ctb0049_layer[, .N, by = amostra_id]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0049_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0049_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0049_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0049_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0049_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0049_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0049_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0049_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0049_layer[, profund_inf])

# check for missing layers
check_missing_layer(ctb0049_layer)

# compute mid depth
ctb0049_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0049_layer[, profund_mid])

# camada_id
ctb0049_layer <- ctb0049_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0049_layer[, camada_id := 1:.N, by = observacao_id]
ctb0049_layer[, .N, by = camada_id]

# Terra fina [%] * 10 -> terrafina
data.table::setnames(ctb0049_layer, old = "Terra fina [%]", new = "terrafina")
ctb0049_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0049_layer[, terrafina])

# Argila [%] * 10 -> argila
data.table::setnames(ctb0049_layer, old = "Argila [%]", new = "argila")
ctb0049_layer[, argila := as.numeric(argila) * 10]
summary(ctb0049_layer[, argila])

# Silte [%] * 10 -> silte
data.table::setnames(ctb0049_layer, old = "Silte [%]", new = "silte")
ctb0049_layer[, silte := as.numeric(silte) * 10]
summary(ctb0049_layer[, silte])

# Areia [%] * 10 -> areia
data.table::setnames(ctb0049_layer, old = "Areia [%]", new = "areia")
ctb0049_layer[, areia := as.numeric(areia) * 10]
summary(ctb0049_layer[, areia])

# Check the particle size distribution
ctb0049_layer[, psd := argila + silte + areia]
ctb0049_layer[psd != 1000, .N]

# MO [g/dm^3] * 0.58 -> carbono
data.table::setnames(ctb0049_layer, old = "MO [g/dm^3]", new = "carbono")
ctb0049_layer[, carbono := as.numeric(carbono) * 0.58]
summary(ctb0049_layer[, carbono])

# pH [H_2O] -> ph
data.table::setnames(ctb0049_layer, old = "pH [H_2O]", new = "ph")
ctb0049_layer[, ph := as.numeric(ph)]
summary(ctb0049_layer[, ph])

# T [cmolc/dm^3] -> ctc
data.table::setnames(ctb0049_layer, old = "T [cmolc/dm^3]", new = "ctc")
ctb0049_layer[, ctc := as.numeric(ctc)]
summary(ctb0049_layer[, ctc])

# dsi
# Soil bulk density is missing. We set it to NA_real_
ctb0049_layer[, dsi := NA_real_]

str(ctb0049_layer)

# Merge ############################################################################################
# events and layers
ctb0049 <- merge(ctb0049_event, ctb0049_layer, all = TRUE)
ctb0049[, dataset_id := "ctb0049"]
summary_soildata(ctb0049)
# Layers: 20
# Events: 12
# Georeferenced events: 12

# Plot using mapview
if (FALSE) {
  ctb0049_sf <- sf::st_as_sf(
    ctb0049[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0049_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0049 <- select_output_columns(ctb0049)
data.table::fwrite(ctb0049, "ctb0049/ctb0049.csv")
data.table::fwrite(ctb0049_event, "ctb0049/ctb0049_event.csv")
data.table::fwrite(ctb0049_layer, "ctb0049/ctb0049_layer.csv")
