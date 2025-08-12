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
# ctb0044
# Dados de "Caracterização de Solos e Avaliação da Vulnerabilidade de Ambientes no Parque Nacional
# de Itatiaia, Brasil"
# https://drive.google.com/drive/folders/1j_CPeBHyMT-LpDc651xQATnr2RuoVVwO
gs <- "14isyQnb-2cc8F51vF-C-DVosqJr801kwUIWrxoKspLc"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0044_validation <- google_sheet(gs, gid_validation)
str(ctb0044_validation)

# Check for negative validation results
sum(ctb0044_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0044_citation <- google_sheet(gs, gid_citation)
str(ctb0044_citation)

# dataset_titulo
dataset_titulo <- ctb0044_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0044_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0044_citation <- data.table::data.table(
  dataset_id = "ctb0044",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0044_citation)

# event ############################################################################################
ctb0044_event <- google_sheet(gs, gid_event)
str(ctb0044_event)

# Process fields
# ID do evento -> observacao_id
# There are some events named "google[number]". It is not clear what they are as they do not contain
# any soil data. We remove them.
data.table::setnames(ctb0044_event, old = "ID do evento", new = "observacao_id")
ctb0044_event[, observacao_id := as.character(observacao_id)]
ctb0044_event <- ctb0044_event[!grepl("google", observacao_id)]
ctb0044_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0044_event, old = "Ano (coleta)", new = "data_ano")
ctb0044_event[, data_ano := as.integer(data_ano)]
ctb0044_event[, .N, by = data_ano]

# data_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0044_event[!is.na(data_ano), data_fonte := "original"]
ctb0044_event[, .N, by = data_fonte]

# Longitude (m) -> coord_x
data.table::setnames(ctb0044_event, old = "Longitude (m)", new = "coord_x")
ctb0044_event[, coord_x := as.numeric(coord_x)]
summary(ctb0044_event[, coord_x])

# Latitude (m) -> coord_y
data.table::setnames(ctb0044_event, old = "Latitude (m)", new = "coord_y")
ctb0044_event[, coord_y := as.numeric(coord_y)]
summary(ctb0044_event[, coord_y])

# Sistema de coordenadas -> coord_datum
# UTM-SAD 69 Zona 23S -> 29193
data.table::setnames(ctb0044_event, old = "Sistema de coordenadas", new = "coord_datum")
ctb0044_event[coord_datum == "UTM-SAD 69 Zona 23S", coord_datum := 29193]
ctb0044_event[, coord_datum := as.integer(coord_datum)]
ctb0044_event[, .N, by = coord_datum]

# Check for duplicated coordinates
ctb0044_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Transform the coordinates to WGS84
ctb0044_event_sf <- sf::st_as_sf(
  ctb0044_event[coord_datum == 29193],
  coords = c("coord_x", "coord_y"), crs = 29193
)
ctb0044_event_sf <- sf::st_transform(ctb0044_event_sf, 4326)
ctb0044_event_sf <- sf::st_coordinates(ctb0044_event_sf)
ctb0044_event[coord_datum == 29193, coord_x := ctb0044_event_sf[, 1]]
ctb0044_event[coord_datum == 29193, coord_y := ctb0044_event_sf[, 2]]
ctb0044_event[coord_datum == 29193, coord_datum := 4326]
ctb0044_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0044_event_sf)

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0044_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0044_event[, coord_fonte := as.character(coord_fonte)]
ctb0044_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is missing. Because the source of the coordinates if GPS, we set
# the precision to 30 meters.
data.table::setnames(ctb0044_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0044_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0044_event[is.na(coord_precisao) & !is.na(coord_fonte), coord_precisao := 30]
summary(ctb0044_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0044_event, old = "País", new = "pais_id")
ctb0044_event[, pais_id := as.character(pais_id)]
ctb0044_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0044_event, old = "Estado (UF)", new = "estado_id")
ctb0044_event[, estado_id := as.character(estado_id)]
ctb0044_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0044_event, old = "Município", new = "municipio_id")
ctb0044_event[, municipio_id := as.character(municipio_id)]
ctb0044_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# A área do evento não está disponível no trabalho de origem. Como se trata de perfis de solo e
# tradagens, consideramos a área do evento como 1 m².
data.table::setnames(ctb0044_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0044_event[, amostra_area := as.double(1)]
summary(ctb0044_event[, amostra_area])

# SIBCS -> taxon_sibcs
data.table::setnames(ctb0044_event, old = "SIBCS", new = "taxon_sibcs")
ctb0044_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0044_event[, taxon_sibcs := gsub("  ", " ", taxon_sibcs)]
ctb0044_event[, taxon_sibcs := gsub("Rock outcrop", "Afloramento de rocha", taxon_sibcs)]
ctb0044_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0044_event[, taxon_st := NA_character_]

str(ctb0044_event)

# layer ############################################################################################
ctb0044_layer <- google_sheet(gs, gid_layer)
str(ctb0044_layer)

# Process fields
# ID do evento -> observacao_id
# There are some events named "google[number]". It is not clear what they are as they do not contain
# any soil data. We remove them.
data.table::setnames(ctb0044_layer, old = "ID do evento", new = "observacao_id")
ctb0044_layer[, observacao_id := as.character(observacao_id)]
ctb0044_layer <- ctb0044_layer[!grepl("google", observacao_id)]
ctb0044_layer[, .N, by = observacao_id]

# Layer name -> camada_nome
data.table::setnames(ctb0044_layer, old = "Layer name", new = "camada_nome")
ctb0044_layer[, camada_nome := as.character(camada_nome)]
ctb0044_layer[, .N, by = camada_nome]

# amostra_id
# The sample id is missing. We set it to NA_character_.
ctb0044_layer[, amostra_id := NA_character_]

# top [cm] -> profund_sup
# Some events have no layers. Thus the top is missing. We remove them.
data.table::setnames(ctb0044_layer, old = "top [cm]", new = "profund_sup")
ctb0044_layer <- ctb0044_layer[!is.na(profund_sup), ]
ctb0044_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0044_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0044_layer[, profund_sup])

# bottom [cm] -> profund_inf
data.table::setnames(ctb0044_layer, old = "bottom [cm]", new = "profund_inf")
ctb0044_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0044_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0044_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0044_layer[, profund_inf])

# Check for missing layers
check_missing_layer(ctb0044_layer)

# Check for repeated layers
check_repeated_layer(ctb0044_layer)

# Compute mid depth
ctb0044_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0044_layer[, profund_mid])

# camada_id
ctb0044_layer <- ctb0044_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0044_layer[, camada_id := 1:.N, by = observacao_id]
ctb0044_layer[, .N, by = camada_id]

# TFSA [%] * 10 -> terrafina
data.table::setnames(ctb0044_layer, old = "TFSA [%]", new = "terrafina")
ctb0044_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0044_layer[, terrafina])
check_empty_layer(ctb0044_layer, "terrafina")
# Fill empty layers
ctb0044_layer[, terrafina := fill_empty_layer(y = terrafina, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "terrafina")

# clay [%] * 10 -> argila
data.table::setnames(ctb0044_layer, old = "clay [%]", new = "argila")
ctb0044_layer[, argila := as.numeric(argila) * 10]
summary(ctb0044_layer[, argila])
check_empty_layer(ctb0044_layer, "argila")
# Fill empty layers
ctb0044_layer[, argila := fill_empty_layer(y = argila, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "argila")

# silt [%] * 10 -> silte
data.table::setnames(ctb0044_layer, old = "silt [%]", new = "silte")
ctb0044_layer[, silte := as.numeric(silte) * 10]
summary(ctb0044_layer[, silte])
check_empty_layer(ctb0044_layer, "silte")
# Fill empty layers
ctb0044_layer[, silte := fill_empty_layer(y = silte, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "silte")

# sand [%] * 10 -> areia
data.table::setnames(ctb0044_layer, old = "sand [%]", new = "areia")
ctb0044_layer[, areia := as.numeric(areia) * 10]
summary(ctb0044_layer[, areia])
check_empty_layer(ctb0044_layer, "areia")
# Fill empty layers
ctb0044_layer[, areia := fill_empty_layer(y = areia, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "areia")

# Check the particle size distribution
ctb0044_layer[, psd := argila + silte + areia]
ctb0044_layer[psd != 1000, ]
ctb0044_layer[abs(psd - 1000) <= 50, argila := round(argila / psd * 1000)]
ctb0044_layer[abs(psd - 1000) <= 50, areia := round(areia / psd * 1000)]
ctb0044_layer[abs(psd - 1000) <= 50, silte := 1000 - argila - areia]
ctb0044_layer[, psd := argila + silte + areia]
ctb0044_layer[psd != 1000, ]
ctb0044_layer[, psd := NULL]

# C [%] * 10 -> carbono
data.table::setnames(ctb0044_layer, old = "C [%]", new = "carbono")
ctb0044_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0044_layer[, carbono])
check_empty_layer(ctb0044_layer, "carbono")
# Fill empty layers
ctb0044_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "carbono")

# pH H2O -> ph
data.table::setnames(ctb0044_layer, old = "pH H2O", new = "ph")
ctb0044_layer[, ph := as.numeric(ph)]
summary(ctb0044_layer[, ph])
check_empty_layer(ctb0044_layer, "ph")
# Fill empty layers
ctb0044_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "ph")

# Na [cmolc/kg] + K [cmolc/kg] + Ca [cmolc/kg] + Mg [cmolc/kg] + H_AL [cmolc/kg] -> ctc
data.table::setnames(ctb0044_layer, old = "Na [cmolc/kg]", new = "na")
data.table::setnames(ctb0044_layer, old = "K [cmolc/kg]", new = "k")
data.table::setnames(ctb0044_layer, old = "Ca [cmolc/kg]", new = "ca")
data.table::setnames(ctb0044_layer, old = "Mg [cmolc/kg]", new = "mg")
data.table::setnames(ctb0044_layer, old = "H_AL [cmolc/kg]", new = "hal")
ctb0044_layer[, ctc := na + k + ca + mg + hal]
summary(ctb0044_layer[, ctc])
check_empty_layer(ctb0044_layer, "ctc")
# Fill empty layers
ctb0044_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "ctc")

# BD [g/dm^3] -> dsi
data.table::setnames(ctb0044_layer, old = "BD [g/dm^3]", new = "dsi")
ctb0044_layer[, dsi := as.numeric(dsi)]
summary(ctb0044_layer[, dsi])
check_empty_layer(ctb0044_layer, "dsi")
# Fill empty layers
ctb0044_layer[, dsi := fill_empty_layer(y = dsi, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0044_layer, "dsi")

str(ctb0044_layer)

# Merge ############################################################################################
# events and layers
ctb0044 <- merge(ctb0044_event, ctb0044_layer, all = TRUE)
ctb0044[, dataset_id := "ctb0044"]
# citation
ctb0044 <- merge(ctb0044, ctb0044_citation, by = "dataset_id", all.x = TRUE)
# There are some events that consist of rock outcrops. We remove them.
ctb0044 <- ctb0044[use != "rock"]
summary_soildata(ctb0044)
# Layers: 357
# Events: 92
# Georeferenced events: 92

# Plot using mapview
if (FALSE) {
  ctb0044_sf <- sf::st_as_sf(
    ctb0044[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0044_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0044 <- select_output_columns(ctb0044)
data.table::fwrite(ctb0044, "ctb0044/ctb0044.csv")
data.table::fwrite(ctb0044_event, "ctb0044/ctb0044_event.csv")
data.table::fwrite(ctb0044_layer, "ctb0044/ctb0044_layer.csv")
