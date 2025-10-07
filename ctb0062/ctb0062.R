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
# ctb0062
# Bacia Hidrográfica do Córrego Guariroba
# https://drive.google.com/drive/folders/11VEdPHP49RzQInHMYp1oYJTzHvqLriq5
gs <- "1D50NPhzzKZVrAci5l-1LCVh6Ferq5xF7ieEbKen6WaE"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0062_validation <- google_sheet(gs, gid_validation)
str(ctb0062_validation)

# Check for negative validation results
sum(ctb0062_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0062_citation <- google_sheet(gs, gid_citation)
str(ctb0062_citation)

# dataset_titulo
dataset_titulo <- ctb0062_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0062_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0062_citation <- data.table::data.table(
  dataset_id = "ctb0062",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0062_citation)

# event ############################################################################################
ctb0062_event <- google_sheet(gs, gid_event)
str(ctb0062_event)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0062_event, old = "ID do evento", new = "observacao_id")
ctb0062_event[, observacao_id := as.character(observacao_id)]
ctb0062_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0062_event, old = "Ano (coleta)", new = "data_ano")
ctb0062_event[, data_ano := as.integer(data_ano)]
ctb0062_event[, .N, by = data_ano]

# ano_fonte
# Original
ctb0062_event[, ano_fonte := "original"]

# Coord. X [m] -> coord_x
data.table::setnames(ctb0062_event, old = "Coord. X [m]", new = "coord_x")
ctb0062_event[, coord_x := as.numeric(coord_x)]
summary(ctb0062_event[, coord_x])

# Coord. Y [m] -> coord_y
data.table::setnames(ctb0062_event, old = "Coord. Y [m]", new = "coord_y")
ctb0062_event[, coord_y := as.numeric(coord_y)]
summary(ctb0062_event[, coord_y])

# Datum (coord) -> coord_datum
# SIRGAS 2000 Zona 21S
data.table::setnames(ctb0062_event, old = "Datum (coord)", new = "coord_datum")
ctb0062_event[, coord_datum := gsub("SIRGAS 2000 Zona 21S", "31981", coord_datum)]
ctb0062_event[, coord_datum := as.integer(coord_datum)]
ctb0062_event[, .N, by = coord_datum]

# Transform coordinates to WGS84
ctb0062_event_sf <- sf::st_as_sf(
  ctb0062_event[coord_datum == 31981],
  coords = c("coord_x", "coord_y"), crs = 31981
)
ctb0062_event_sf <- sf::st_transform(ctb0062_event_sf, 4326)
ctb0062_event_sf <- sf::st_coordinates(ctb0062_event_sf)
ctb0062_event[coord_datum == 31981, coord_x := ctb0062_event_sf[, 1]]
ctb0062_event[coord_datum == 31981, coord_y := ctb0062_event_sf[, 2]]
ctb0062_event[coord_datum == 31981, coord_datum := 4326]
rm(ctb0062_event_sf)
summary(ctb0062_event[, .(coord_datum, coord_x, coord_y)])

# Check for duplicated coordinates
ctb0062_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0062_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0062_event[, coord_fonte := as.character(coord_fonte)]
ctb0062_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is missing. As the source of the coordinates is GPS, we set
# it to 30 m.
data.table::setnames(ctb0062_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0062_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0062_event[is.na(coord_precisao) & coord_fonte == "GPS", coord_precisao := 30]
summary(ctb0062_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0062_event, old = "País", new = "pais_id")
ctb0062_event[, pais_id := as.character(pais_id)]
ctb0062_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0062_event, old = "Estado (UF)", new = "estado_id")
ctb0062_event[, estado_id := as.character(estado_id)]
ctb0062_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0062_event, old = "Município", new = "municipio_id")
ctb0062_event[, municipio_id := as.character(municipio_id)]
ctb0062_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0062_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0062_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0062_event[, amostra_area])

# taxon_sibcs
# 2025-05-26 Soil classification is missing, but the author may be able to deduce. Lisiane will
# check. For now we set it to NA_character_
ctb0062_event[, taxon_sibcs := NA_character_]

# taxon_st
# Soil taxonomy is missing. We set it to NA_character_
ctb0062_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# review the work at another time

ctb0062_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0062_event[, rochosidade := ("Não Rochoso")]

str(ctb0062_event)

# layer ############################################################################################
ctb0062_layer <- google_sheet(gs, gid_layer)
str(ctb0062_layer)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0062_layer, old = "ID do evento", new = "observacao_id")
ctb0062_layer[, observacao_id := as.character(observacao_id)]
ctb0062_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
# Soil layers are named using a combination of letters and numbers. It is not evident what the
# designation means, but it is obvious that it is not related to genetic soil horizons. We reset
# the layer names to the depth limits.
data.table::setnames(ctb0062_layer, old = "ID da camada", new = "camada_nome")
ctb0062_layer[, camada_nome := as.character(camada_nome)]
ctb0062_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# The sample id is missing. We set it to NA_character_
ctb0062_layer[, amostra_id := NA_character_]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0062_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0062_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0062_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0062_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0062_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0062_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0062_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0062_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0062_layer[, profund_inf])

# Reset layer names
ctb0062_layer[, camada_nome := paste(profund_sup, profund_inf, sep = "-")]
ctb0062_layer[, .N, by = camada_nome]

# Check for missing layers
check_missing_layer(ctb0062_layer)

# Compute mid depth
ctb0062_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0062_layer[, profund_mid])

# camada_id
ctb0062_layer <- ctb0062_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0062_layer[, camada_id := 1:.N, by = observacao_id]
ctb0062_layer[, .N, by = camada_id]

# Terra fina [%] * 10 -> terrafina
data.table::setnames(ctb0062_layer, old = "Terra fina [%]", new = "terrafina")
ctb0062_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0062_layer[, terrafina])

# Argila [%] * 10 -> argila
data.table::setnames(ctb0062_layer, old = "Argila [%]", new = "argila")
ctb0062_layer[, argila := as.numeric(argila) * 10]
summary(ctb0062_layer[, argila])

# Silte [%] * 10 -> silte
data.table::setnames(ctb0062_layer, old = "Silte [%]", new = "silte")
ctb0062_layer[, silte := as.numeric(silte) * 10]
summary(ctb0062_layer[, silte])

# Areia [%] * 10 -> areia
data.table::setnames(ctb0062_layer, old = "Areia [%]", new = "areia")
ctb0062_layer[, areia := as.numeric(areia) * 10]
summary(ctb0062_layer[, areia])

# Check the particle size distribution
ctb0062_layer[, psd := argila + silte + areia]
ctb0062_layer[psd != 1000, .N]

# MO [g/dm^3] * 0.58 -> carbono
data.table::setnames(ctb0062_layer, old = "MO [g/dm^3]", new = "carbono")
ctb0062_layer[, carbono := as.numeric(carbono) * 0.58]
summary(ctb0062_layer[, carbono])

# pH [H_2O] -> ph
data.table::setnames(ctb0062_layer, old = "pH [H_2O]", new = "ph")
ctb0062_layer[, ph := as.numeric(ph)]
summary(ctb0062_layer[, ph])

# T [cmolc/dm^3] -> ctc
data.table::setnames(ctb0062_layer, old = "T [cmolc/dm^3]", new = "ctc")
ctb0062_layer[, ctc := as.numeric(ctc)]
summary(ctb0062_layer[, ctc])

# dsi
# Soil bulk density is missing. We set it to NA_real_
ctb0062_layer[, dsi := NA_real_]

str(ctb0062_layer)

# Merge ############################################################################################
# events and layers
ctb0062 <- merge(ctb0062_event, ctb0062_layer, all = TRUE)
ctb0062[, dataset_id := "ctb0062"]
# citation
ctb0062 <- merge(ctb0062, ctb0062_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0062)
# Layers: 28
# Events: 7
# Georeferenced events: 7

# Plot using mapview
if (FALSE) {
  ctb0062_sf <- sf::st_as_sf(
    ctb0062[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0062_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0062 <- select_output_columns(ctb0062)
data.table::fwrite(ctb0062, "ctb0062/ctb0062.csv")
data.table::fwrite(ctb0062_event, "ctb0062/ctb0062_event.csv")
data.table::fwrite(ctb0062_layer, "ctb0062/ctb0062_layer.csv")
