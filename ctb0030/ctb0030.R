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
# ctb0030
# Dados de "Caracterização pedológica em áreas de encosta do Rebordo do Planalto do RS"
# https://drive.google.com/drive/u/0/folders/1V45dze-P9ZPWxC22PhopaltD5Kgp8oNk
gs <- "1RnPPJrkOtorJ_KiG71j5CdC6Ae8BEfymKYdHm1INE8s"
gid_citation <- 85414049
gid_validation <- 1168241699
gid_event <- 1428766046
gid_layer <- 1257720428

# validation #######################################################################################
ctb0030_validation <- google_sheet(gs, gid_validation)
str(ctb0030_validation)

# Check for negative validation results
sum(ctb0030_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0030_citation <- google_sheet(gs, gid_citation)
str(ctb0030_citation)

# dataset_titulo
dataset_titulo <- ctb0030_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0030_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0030_citation <- data.table::data.table(
  dataset_id = "ctb0030",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0030_citation)

# event ############################################################################################
ctb0030_event <- google_sheet(gs, gid_event)
str(ctb0030_event)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0030_event, old = "ID do evento", new = "observacao_id")
ctb0030_event[, observacao_id := as.character(observacao_id)]
ctb0030_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0030_event, old = "Ano (coleta)", new = "data_ano")
ctb0030_event[, data_ano := as.integer(data_ano)]
ctb0030_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta dos perfis de solo está especificada no documento de origem dos dados
ctb0030_event[!is.na(data_ano), ano_fonte := "original"]
ctb0030_event[, .N, by = ano_fonte]

# There is one event missing the sampling year. It was obtained from Poelking (2007).
# We assume it is 2006.
ctb0030_event[is.na(data_ano) & observacao_id == "PERFIL-07", data_ano := 2006]
ctb0030_event[, .N, by = data_ano]
# ano_fonte for this event is set to "estimativa"
ctb0030_event[observacao_id == "PERFIL-07", ano_fonte := "estimativa"]
ctb0030_event[, .N, by = ano_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0030_event, old = "Longitude", new = "coord_x")
ctb0030_event[, coord_x := as.numeric(coord_x)]
summary(ctb0030_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0030_event, old = "Latitude", new = "coord_y")
ctb0030_event[, coord_y := as.numeric(coord_y)]
summary(ctb0030_event[, coord_y])

# Datum (coord) -> coord_datum
data.table::setnames(ctb0030_event, old = "Datum (coord)", new = "coord_datum")
ctb0030_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0030_event[, coord_datum := as.integer(coord_datum)]
ctb0030_event[, .N, by = coord_datum]

# Harmonize CRS
# Two events have CRS 32722
ctb0030_event_sf <- ctb0030_event[coord_datum == "32722", ]
ctb0030_event_sf <- sf::st_as_sf(ctb0030_event_sf, coords = c("coord_x", "coord_y"), crs = 32722)
ctb0030_event_sf <- sf::st_transform(ctb0030_event_sf, crs = 4326)
ctb0030_event_sf <- sf::st_coordinates(ctb0030_event_sf)
ctb0030_event[coord_datum == "32722", coord_x := ctb0030_event_sf[, 1]]
ctb0030_event[coord_datum == "32722", coord_y := ctb0030_event_sf[, 2]]
ctb0030_event[coord_datum == 32722, coord_datum := 4326]
ctb0030_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0030_event_sf)
ctb0030_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0030_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0030_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0030_event[, coord_fonte := as.character(coord_fonte)]
ctb0030_event[, .N, by = coord_fonte]

# Precisão (coord) [m] -> coord_precisao
data.table::setnames(ctb0030_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0030_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0030_event[, coord_precisao])

# pais_id
# País
data.table::setnames(ctb0030_event, old = "País", new = "pais_id")
ctb0030_event[, pais_id := as.character(pais_id)]
ctb0030_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0030_event, old = "Estado (UF)", new = "estado_id")
ctb0030_event[, estado_id := as.character(estado_id)]
ctb0030_event[, .N, by = estado_id]

# municipio_id
data.table::setnames(ctb0030_event, old = "Município", new = "municipio_id")
ctb0030_event[, municipio_id := as.character(municipio_id)]
ctb0030_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0030_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0030_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0030_event[, amostra_area])

# Classificação do solo -> taxon_sibcs
data.table::setnames(ctb0030_event, old = "Classificação do solo", new = "taxon_sibcs")
ctb0030_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0030_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0030_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# review the work at another time

ctb0030_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0030_event[, rochosidade := ("Não Rochoso")]

str(ctb0030_event)

# layer ############################################################################################
ctb0030_layer <- google_sheet(gs, gid_layer)
str(ctb0030_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0030_layer, old = "ID do evento", new = "observacao_id")
ctb0030_layer[, observacao_id := as.character(observacao_id)]
ctb0030_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0030_layer, old = "ID da camada", new = "camada_nome")
ctb0030_layer[, camada_nome := as.character(camada_nome)]
ctb0030_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0030_layer[, amostra_id := NA_character_]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0030_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0030_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0030_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0030_layer$profund_sup)

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0030_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0030_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0030_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0030_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0030_layer$profund_inf)

# check for missing layers
any_missing_layer(ctb0030_layer)

# camada_id
ctb0030_layer <- ctb0030_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0030_layer[, camada_id := 1:.N, by = observacao_id]
ctb0030_layer[, .N, by = camada_id]

# terrafina
# terra fina is missing. We assume it is NA for most of the layers.
# We set it to 1000 for the layers that are known to be terra fina after carefull analysis of the 
# source document.
ctb0030_layer[, terrafina := NA_real_]
ctb0030_layer[observacao_id == "PERFIL-01", terrafina := 1000]
ctb0030_layer[observacao_id == "PERFIL-03", terrafina := 1000]
ctb0030_layer[observacao_id == "PERFIL-06" & camada_nome %in% c("A", "E", "B"), terrafina := 1000]

# Argila [g/kg] -> argila
data.table::setnames(ctb0030_layer, old = "Argila (<0,002 mm) [g/kg]", new = "argila")
ctb0030_layer[, argila := as.numeric(argila)]
summary(ctb0030_layer[, argila])

# Silte [g/kg] -> silte
data.table::setnames(ctb0030_layer, old = "Silte (0,05-0,002 mm) [g/kg]", new = "silte")
ctb0030_layer[, silte := as.numeric(silte)]
summary(ctb0030_layer[, silte])

# Areia Total [g/kg] -> areia
data.table::setnames(ctb0030_layer, old = "Areia Total [g/kg]", new = "areia")
ctb0030_layer[, areia := as.numeric(areia)]
summary(ctb0030_layer[, areia])

# Check the particle size distribution
ctb0030_layer[, psd := argila + silte + areia]
psd_lims <- 990:1010
ctb0030_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 24 layers with psd not in the range 990-1010
# Print the rows with psd not in the range
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0030_layer[!psd %in% psd_lims & !is.na(psd), ][, ..cols]
# The following profules have layers with psd not in the range:
# PERFIL-01 (all layers), PERFIL-02 (all layers), PERFIL-03 (all layers), PERFIL-04 (all layers),
# PERFIL-05 (all layers), PERFIL-06 (all layers). All of these profiles were sampled and a anlysed
# by the study author. The soil profiles with no issues were compiled from a previous study. Also,
# for a given soil profile, the sum of the particle size distribution is approximatelly the same
# accross all layers: ~70% for PERFIL-01, ~88% for PERFIL-02, ~90% for PERFIL-03, ~66% for PERFIL-04,
# ~94% for PERFIL-06. We could not find the source of the error. So we assume that the errors is
# distributed proportionally across all particle size fractions.
ctb0030_layer[psd != 1000, argila := round(argila / psd * 1000)]
ctb0030_layer[psd != 1000, silte := round(silte / psd * 1000)]
ctb0030_layer[psd != 1000, areia := round(areia / psd * 1000)]
ctb0030_layer[, psd := argila + silte + areia]
ctb0030_layer[psd != 1000, ]
ctb0044_layer[, psd := NULL]

# C Orgânico [g/kg] -> carbono
data.table::setnames(ctb0030_layer, old = "C Orgânico [g/kg]", new = "carbono")
ctb0030_layer[, carbono := as.numeric(carbono)]
summary(ctb0030_layer[, carbono])

# CTC pH 7 [cmolc/kg] -> ctc
data.table::setnames(ctb0030_layer, old = "CTC pH 7 [cmolc/kg]", new = "ctc")
ctb0030_layer[, ctc := as.numeric(ctc)]
summary(ctb0030_layer[, ctc])

# pH H2O (1:1) -> ph
data.table::setnames(ctb0030_layer, old = "pH H2O (1:1)", new = "ph")
ctb0030_layer[, ph := as.numeric(ph)]
summary(ctb0030_layer[, ph])

# dsi
ctb0030_layer[, dsi := NA_real_]

str(ctb0030_layer)

# Merge ############################################################################################
# events and layers
ctb0030 <- merge(ctb0030_event, ctb0030_layer, all = TRUE)
ctb0030[, dataset_id := "ctb0030"]
# citation
ctb0030 <- merge(ctb0030, ctb0030_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0030)
# Layers: 46
# Events: 11
# Georeferenced events: 11

# Plot using mapview
if (FALSE) {
  ctb0030_sf <- sf::st_as_sf(
    ctb0030[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0030_sf["argila"])
}

# Write to disk ####################################################################################
ctb0030 <- select_output_columns(ctb0030)
data.table::fwrite(ctb0030, "ctb0030/ctb0030.csv")
data.table::fwrite(ctb0030_event, "ctb0030/ctb0030_event.csv")
data.table::fwrite(ctb0030_layer, "ctb0030/ctb0030_layer.csv")
