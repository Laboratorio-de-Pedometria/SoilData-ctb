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
# check for duplicate observacao_id
ctb0030_event[, .N, by = observacao_id][N > 1]

# The study contains one soil profile from Poelking (2007), as regiteted in "Fonte dos dados".
# We will drop this profile from the dataset after the merge.
ctb0030_event[, .N, by = "Fonte dos dados"]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0030_event, old = "Ano (coleta)", new = "data_ano")
ctb0030_event[, data_ano := as.integer(data_ano)]
ctb0030_event[, .N, by = data_ano]

# ano_fonte
# The sampling year is specified in the source document for most of the profiles.
ctb0030_event[!is.na(data_ano), ano_fonte := "original"]
ctb0030_event[, .N, by = ano_fonte]
# There is one event missing the sampling year. It was obtained from Poelking (2007).
# We will ignore it, as it is not part of the original study.

# Longitude -> coord_x
# We have coordinates in geographic and UTM. 
data.table::setnames(ctb0030_event, old = "Longitude", new = "coord_x")
ctb0030_event[, coord_x := as.numeric(coord_x)]
summary(ctb0030_event[, coord_x])

# Latitude -> coord_y
# We have coordinates in geographic and UTM.
data.table::setnames(ctb0030_event, old = "Latitude", new = "coord_y")
ctb0030_event[, coord_y := as.numeric(coord_y)]
summary(ctb0030_event[, coord_y])

# Datum (coord) -> coord_datum
data.table::setnames(ctb0030_event, old = "Datum (coord)", new = "coord_datum")
ctb0030_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0030_event[, coord_datum := as.integer(coord_datum)]
ctb0030_event[, .N, by = coord_datum]

# Harmonize CRS
# Two events have CRS 32722 (UTM 22S). We will convert them to 4326 (WGS84).
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

# País
# pais_id
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

# old: SiBCS (2006)
# new: taxon_sibcs
data.table::setnames(ctb0030_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0030_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0030_event[, .N, by = taxon_sibcs]

# taxon_st
# The soil classification according to Soil Taxonomy is not available in this dataset.
ctb0030_event[, taxon_st := NA_character_]

# Pedregosidade
# pedregosidade
data.table::setnames(ctb0030_event, old = "Pedregosidade", new = "pedregosidade")
ctb0030_event[, pedregosidade := as.character(pedregosidade)]
ctb0030_event[, .N, by = pedregosidade]

# Rochosidade
# rochosidade
data.table::setnames(ctb0030_event, old = "Rochosidade", new = "rochosidade")
ctb0030_event[, rochosidade := as.character(rochosidade)]
ctb0030_event[, .N, by = rochosidade]

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

# old: Terra fina [g/kg]
# new: terrafina
data.table::setnames(ctb0030_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0030_layer[, terrafina := as.numeric(terrafina)]
# The fine earth fraction is reported in the source document. However, for some layers, we can infer
# it from the soil texture, soil classification and additional information in the source document.
# We set it to 1000 for the layers that are known to be terra fina after carefull analysis of the
# source document.
# Argissolo Bruno-Acinzentado Alítico abrúptico
ctb0030_layer[observacao_id == "PERFIL-01", terrafina := ifelse(is.na(terrafina), 1000, terrafina)]
# Argissolo Vermelho Alítico típico
ctb0030_layer[observacao_id == "PERFIL-03", terrafina := ifelse(is.na(terrafina), 1000, terrafina)]
# Planossolo Háplico Alítico típico
ctb0030_layer[
  observacao_id == "PERFIL-06" & camada_nome %in% c("A", "E", "B"),
  terrafina := ifelse(is.na(terrafina), 1000, terrafina)
]
summary(ctb0030_layer[, terrafina])
# Check for missing terrafina
check_empty_layer(ctb0030_layer, "terrafina")

# Argila [g/kg] -> argila
data.table::setnames(ctb0030_layer, old = "Argila (<0,002 mm) [g/kg]", new = "argila")
ctb0030_layer[, argila := as.numeric(argila)]
summary(ctb0030_layer[, argila])
# Check for missing argila
check_empty_layer(ctb0030_layer, "argila")

# Silte [g/kg] -> silte
data.table::setnames(ctb0030_layer, old = "Silte (0,05-0,002 mm) [g/kg]", new = "silte")
ctb0030_layer[, silte := as.numeric(silte)]
summary(ctb0030_layer[, silte])
# Check for missing silte
check_empty_layer(ctb0030_layer, "silte")

# Areia Total [g/kg] -> areia
data.table::setnames(ctb0030_layer, old = "Areia Total [g/kg]", new = "areia")
ctb0030_layer[, areia := as.numeric(areia)]
summary(ctb0030_layer[, areia])
# Check for missing areia
check_empty_layer(ctb0030_layer, "areia")

# Check the particle size distribution
ctb0030_layer[, psd := argila + silte + areia]
psd_lims <- 900:1100
ctb0030_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 16 layers with psd not in the range 900-1100
# Print the rows with psd not in the range
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0030_layer[!psd %in% psd_lims & !is.na(psd), ][, ..cols]
# The following profiles have layers with psd not in the range:
# PERFIL-01 (all seven layers), PERFIL-02 (all two layers), PERFIL-03 (three out of six layers),
# PERFIL-04 (all three layers), PERFIL-05 (all layers). PERFIL-06 also has some inconsistencies,
# but the psd is within the range. All of these profiles were sampled and analysed by the study
# author. The soil profiles with no issues were compiled from previous studies.
# We note that, for a given soil profile, the sum of the particle size distribution is
# approximately the same across all layers: ~70% for PERFIL-01, ~88% for PERFIL-02, ~90% for
# PERFIL-03, ~66% for PERFIL-04, and ~59% for PERFIL-05. We could not find the source of the error
# even after careful analysis of the source document. Given the pattern of the errors, it seems to
# be a systematic error during the laboratory analysis or data transcription.
# We will correct the particle size distribution values by adjusting the individual fractions.
ctb0030_layer[psd != 1000, argila := round(argila / psd * 1000)]
ctb0030_layer[psd != 1000, silte := round(silte / psd * 1000)]
ctb0030_layer[psd != 1000, areia := round(areia / psd * 1000)]
ctb0030_layer[, psd := argila + silte + areia]
ctb0030_layer[psd != 1000, ]
ctb0030_layer[, psd := NULL]

# C Orgânico [g/kg] -> carbono
data.table::setnames(ctb0030_layer, old = "C Orgânico [g/kg]", new = "carbono")
ctb0030_layer[, carbono := as.numeric(carbono)]
summary(ctb0030_layer[, carbono])
# Check for missing carbono
check_empty_layer(ctb0030_layer, "carbono")

# CTC pH 7 [cmolc/kg] -> ctc
data.table::setnames(ctb0030_layer, old = "CTC pH 7 [cmolc/kg]", new = "ctc")
ctb0030_layer[, ctc := as.numeric(ctc)]
summary(ctb0030_layer[, ctc])
# Check for missing ctc
check_empty_layer(ctb0030_layer, "ctc")

# pH H2O (1:1) -> ph
data.table::setnames(ctb0030_layer, old = "pH H2O (1:1)", new = "ph")
ctb0030_layer[, ph := as.numeric(ph)]
summary(ctb0030_layer[, ph])
# Check for missing ph
check_empty_layer(ctb0030_layer, "ph")

# dsi
# The dataset does not contain data on the bulk soil density.
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

# Keep only original study profiles
ctb0030 <- ctb0030[`Fonte dos dados` == "Original", ]

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
