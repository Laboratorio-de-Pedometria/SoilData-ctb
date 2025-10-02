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
# ctb0024
# Dados de "Influência da heterogeneidade ambiental sobre comunidades e populações de palmeiras em
# florestas de terra firme na Amazônia Oriental"
# https://drive.google.com/drive/u/1/folders/1m9y-L8eU4h2wgjHAvj5yZEinVwtZnrJ-
gs <- "1CGXEGhpy91v6kxtFQBVnpRQ9qCuV2aRoS72A97y6RiQ"
gid_citation <- 0
gid_validation <- 88779986
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0024_validation <- google_sheet(gs, gid_validation)
str(ctb0024_validation)

# Check for negative validation results
sum(ctb0024_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0024_citation <- google_sheet(gs, gid_citation)
str(ctb0024_citation)

# dataset_titulo
dataset_titulo <- ctb0024_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0024_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0024_citation <- data.table::data.table(
  dataset_id = "ctb0024",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0024_citation)

# event ############################################################################################
ctb0024_event <- google_sheet(gs, gid_event)
str(ctb0024_event)

# Process fields
# ID do evento -> observacao_id
# We found many inconsistencies in the identification of the sampling points. Thus, after a careful
# analysis, we decided to create an alternative identification for the sampling points with
# inconsistencies. We still have to consult with the author of the dataset to confirm the
# identification of the sampling points.
data.table::setnames(ctb0024_event, old = "ID do evento", new = "observacao_id")
ctb0024_event[, observacao_id := as.character(observacao_id)]
ctb0024_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0024_event, old = "Ano (coleta)", new = "data_ano")
ctb0024_event[, data_ano := as.integer(data_ano)]
ctb0024_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta está especificada no documento de origem dos dados.
ctb0024_event[, ano_fonte := "original"]
ctb0024_event[, .N, by = ano_fonte]

# coord x [m] -> coord_x
data.table::setnames(ctb0024_event, old = "coord x [m]", new = "coord_x")
ctb0024_event[, coord_x := as.numeric(coord_x)]
summary(ctb0024_event[, coord_x])

# coord y [m] -> coord_y 
data.table::setnames(ctb0024_event, old = "coord y [m]", new = "coord_y")
ctb0024_event[, coord_y := as.numeric(coord_y)]
summary(ctb0024_event[, coord_y])

# coord_datum
# Datum (coord)
# The coordinate reference system is missing. The coordinates are in UTM. We assume it is 31981
# (SIRGAS 2000 / UTM zone 21S)
data.table::setnames(ctb0024_event, old = "Datum (coord)", new = "coord_datum")
ctb0024_event[, coord_datum := as.integer(coord_datum)]
ctb0024_event[, coord_datum := 31981]
ctb0024_event[, .N, by = coord_datum]

# check for duplicated coordinates
# Soil samples come from transects. The dataset contains only the centroid of the transect.
# We jitter the coordinates to avoid overlapping points. We add up to 50 m as this was the distance
# between the sampling points along the transect.
ctb0024_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Transform coordinates do WGS84
ctb0024_event_sf <- sf::st_as_sf(ctb0024_event[coord_datum == 31981],
  coords = c("coord_x", "coord_y"), crs = 31981
)
set.seed(1984)
ctb0024_event_sf <- sf::st_jitter(ctb0024_event_sf, amount = 125)
ctb0024_event_sf <- sf::st_transform(ctb0024_event_sf, crs = 4326)
ctb0024_event_sf <- sf::st_coordinates(ctb0024_event_sf)
ctb0024_event[coord_datum == 31981, coord_x := ctb0024_event_sf[, 1]]
ctb0024_event[coord_datum == 31981, coord_y := ctb0024_event_sf[, 2]]
ctb0024_event[coord_datum == 31981, coord_datum := 4326]
rm(ctb0024_event_sf)
summary(ctb0024_event[, .(coord_datum, coord_x, coord_y)])

# coord_fonte
# Fonte (coord)
data.table::setnames(ctb0024_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0024_event[, coord_fonte := as.character(coord_fonte)]
ctb0024_event[, .N, by = coord_fonte]

# Precisão (coord) [m] -> coord_precisao
# The precision of the coordinates is missing. As the source of the coordinates is GPS, we set
# it to 30 m.
data.table::setnames(ctb0024_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0024_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0024_event[is.na(coord_precisao) & coord_fonte == "GPS", coord_precisao := 30]
summary(ctb0024_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0024_event, old = "País", new = "pais_id")
ctb0024_event[, pais_id := as.character(pais_id)]
ctb0024_event[, .N, by = pais_id]

# Estado (UF) -> estado_id 
data.table::setnames(ctb0024_event, old = "Estado (UF)", new = "estado_id")
ctb0024_event[, estado_id := as.character(estado_id)]
ctb0024_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0024_event, old = "Município", new = "municipio_id")
ctb0024_event[, municipio_id := as.character(municipio_id)]
ctb0024_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0024_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0024_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0024_event[, amostra_area])

# taxon_sibcs
# Classificação do solo segundo o Sistema Brasileiro de Classificação de Solos (SiBCS) não está
# disponível neste dataset.
ctb0024_event[, taxon_sibcs := NA_character_]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0024_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# review the work at another time

ctb0024_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0024_event[, rochosidade := ("Não Rochoso")]

str(ctb0024_event)

# layer ############################################################################################
ctb0024_layer <- google_sheet(gs, gid_layer)
str(ctb0024_layer)

# Process fields
# ID do evento -> observacao_id
# Each sampling point has two layers. We check if there are profiles with a different number of
# layers.
data.table::setnames(ctb0024_layer, old = "ID do evento", new = "observacao_id")
ctb0024_layer[, observacao_id := as.character(observacao_id)]
ctb0024_layer[, .N, by = observacao_id][N != 2]

# ID da camada -> camada_nome
data.table::setnames(ctb0024_layer, old = "ID da camada", new = "camada_nome")
ctb0024_layer[, camada_nome := as.character(camada_nome)]
ctb0024_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0024_layer, old = "ID da amostra", new = "amostra_id")
ctb0024_layer[, amostra_id := as.integer(amostra_id)]
ctb0024_layer[, .N, by = amostra_id]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0024_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0024_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0024_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0024_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0024_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0024_layer[, profund_inf])

# Check for missing layers
check_missing_layer(ctb0024_layer)

# Check for repeated layers
check_repeated_layer(ctb0024_layer)

# camada_id
ctb0024_layer <- ctb0024_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0024_layer[, camada_id := 1:.N, by = observacao_id]
ctb0024_layer[, .N, by = camada_id]

# Fine earth [g/kg] -> terrafina
# The fine earth content is missing. We assume it is 1000 g/kg as the literature suggests for
# the area (Flona do Tapajós).
data.table::setnames(ctb0024_layer, old = "Fine earth [g/kg]", new = "terrafina")
ctb0024_layer[, terrafina := as.numeric(terrafina)]
ctb0024_layer[is.na(terrafina), terrafina := 1000]
summary(ctb0024_layer[, terrafina])

# Total_Clay [g/kg] -> argila
data.table::setnames(ctb0024_layer, old = "Total_Clay [g/kg]", new = "argila")
ctb0024_layer[, argila := as.numeric(argila)]
summary(ctb0024_layer[, argila])

# Silt [g/kg] -> silte
data.table::setnames(ctb0024_layer, old = "Silt [g/kg]", new = "silte")
ctb0024_layer[, silte := as.numeric(silte)]
summary(ctb0024_layer[, silte])

# Sand [g/kg] -> areia
# Coarse_sand [g/kg] + Fine_sand [g/kg] = areia
data.table::setnames(ctb0024_layer, old = "Coarse_sand [g/kg]", new = "areia_grossa")
data.table::setnames(ctb0024_layer, old = "Fine_sand [g/kg]", new = "areia_fina")
ctb0024_layer[, areia := as.numeric(areia_grossa) + as.numeric(areia_fina)]
summary(ctb0024_layer[, areia])

# Check the particle size distribution
ctb0024_layer[, psd := argila + silte + areia]
ctb0024_layer[abs(psd - 1000) <= 50, argila := round(argila / psd * 1000)]
ctb0024_layer[abs(psd - 1000) <= 50, areia := round(areia / psd * 1000)]
ctb0024_layer[abs(psd - 1000) <= 50, silte := 1000 - argila - areia]
ctb0024_layer[, psd := argila + silte + areia]
ctb0024_layer[psd != 1000, .N]

# OM [g/kg] * 0.58 -> carbono
data.table::setnames(ctb0024_layer, old = "OM [g/kg]", new = "carbono")
ctb0024_layer[, carbono := as.numeric(carbono) * 0.58]
summary(ctb0024_layer[, carbono])

# ctc
# The CEC is missing. We set it to NA
ctb0024_layer[, ctc := NA_real_]

# pH -> ph
data.table::setnames(ctb0024_layer, old = "pH", new = "ph")
ctb0024_layer[, ph := as.numeric(ph)]
summary(ctb0024_layer[, ph])

# dsi
# The soil bulk density is missing. We set it to NA
ctb0024_layer[, dsi := NA_real_]

str(ctb0024_layer)

# Merge ############################################################################################
# events and layers
ctb0024 <- merge(ctb0024_event, ctb0024_layer, all = TRUE)
ctb0024[, dataset_id := "ctb0024"]
# citation
ctb0024 <- merge(ctb0024, ctb0024_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0024)
# Layers: 252
# Events: 126
# Georeferenced events: 126

# Plot using mapview
if (FALSE) {
  ctb0024_sf <- sf::st_as_sf(
    ctb0024[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0024_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0024 <- select_output_columns(ctb0024)
data.table::fwrite(ctb0024, "ctb0024/ctb0024.csv")
data.table::fwrite(ctb0024_event, "ctb0024/ctb0024_event.csv")
data.table::fwrite(ctb0024_layer, "ctb0024/ctb0024_layer.csv")
