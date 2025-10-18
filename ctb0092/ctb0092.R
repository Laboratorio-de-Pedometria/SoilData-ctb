# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}
if (!requireNamespace("parzer")) {
  install.packages("parzer")
}
if (!requireNamespace("dplyr")) {
  install.packages("dplyr")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0092
# Dados de Carbono de Solos - Projeto SIGecotur/Projeto Forense
#
# Google Drive: https://drive.google.com/drive/u/0/folders/1RsV8ViDb7lsvz7BgxOOhqTHhDWe1qsLE

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

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0092_event, old = "ID do evento", new = "observacao_id")
ctb0092_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
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
check_equal_coordinates(ctb0092_event)

# Datum (coord) -> coord_datum
data.table::setnames(ctb0092_event, old = "Datum (coord)", new = "coord_datum")
ctb0092_event[, coord_datum := as.character(coord_datum)]
ctb0092_event[, coord_datum := gsub("WGS84", 4326, coord_datum)]
ctb0092_event[, coord_datum := as.integer(coord_datum)]
ctb0092_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
# GPS Garmin
data.table::setnames(ctb0092_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0092_event[, coord_fonte := as.character(coord_fonte)]
ctb0092_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is not informed in this dataset. However, the coordinates were
# collected using a GPS device. Therefore, we will assume a precision of 30 meters.
data.table::setnames(ctb0092_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0092_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0092_event[is.na(coord_precisao), coord_precisao := 30]
summary(ctb0092_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0092_event, old = "País", new = "pais_id")
ctb0092_event[, pais_id := as.character(pais_id)]
ctb0092_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0092_event, old = "Estado (UF)", new = "estado_id")
ctb0092_event[, estado_id := as.character(estado_id)]
ctb0092_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0092_event, old = "Município", new = "municipio_id")
ctb0092_event[, municipio_id := as.character(municipio_id)]
ctb0092_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0092_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0092_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0092_event[, amostra_area])

# SiBCS  -> taxon_sibcs
# The soil classification according to SiBCS is not informed in this document.
ctb0092_event[, taxon_sibcs := NA_character_]

# taxon_st
# The soil classification according to US soil taxonomy is not informed in this document.
ctb0092_event[, taxon_st := NA_character_]

# pedregosidade
# Soil stoniness is not informed in this dataset. But the authors could have described it in their
# field notes. Maybe they have photographs of the soil profiles. We will leave this field as NA for
# now.
ctb0092_event[, pedregosidade := NA_character_]

# rochosidade
# Soil rockiness is not informed in this dataset. But the authors could have described it in their
# field notes. Maybe they have photographs of the soil profiles. We will leave this field as NA for
# now.
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
# Most soil profiles have two layers, but a few have only one layer. Maybe in these profiles the
# authors encountered the bedrock.

# ID da camada -> camada_nome
data.table::setnames(ctb0092_layer, old = "ID da camada", new = "camada_nome")
ctb0092_layer[, camada_nome := as.character(camada_nome)]
ctb0092_layer[, .N, by = camada_nome]
# Most layers are 0-20 cm and 20-40 cm. A few profiles have layers with inferior limits such as 15,
# 25, and 35. These layers could indicate that the authors reached some lithic contact.

# ID da amostra -> amostra_id
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

# Check for missing layers
check_missing_layer(ctb0092_layer)

# terrafina
# The fine earth fraction is not informed in this dataset. Maybe the authors have this information.
ctb0092_layer[, terrafina := NA_real_]

# areia
# The sand content is not informed in this dataset. Maybe the authors have this information.
ctb0092_layer[, areia := NA_real_]

# silte
# The silt content is not informed in this dataset. Maybe the authors have this information.
ctb0092_layer[, silte := NA_real_]

# argila
# The clay content is not informed in this dataset. Maybe the authors have this information.
ctb0092_layer[, argila := NA_real_]

# carbono
# old: C total [%]
# new: carbono
data.table::setnames(ctb0092_layer, old = "C total [%]", new = "carbono")
ctb0092_layer[, carbono := as.numeric(carbono) * 10] # convert to g/kg
summary(ctb0092_layer[, carbono])
# There is one layer missing carbon content (B27, 10-20 cm). This is the only sampling point with a
# 10-20 cm layer. Maybe this is a typo.
check_empty_layer(ctb0092_layer, "carbono")

# ctc
# The cation exchange capacity is not informed in this dataset. Maybe the authors have this
# information.
ctb0092_layer[, ctc := NA_real_]

# ph
# The pH is not informed in this dataset. Maybe the authors have this information.
ctb0092_layer[, ph := NA_real_]

# dsi
# The soil bulk density is not informed in this dataset. Maybe the authors have this information.
ctb0092_layer[, dsi := NA_real_]

str(ctb0092_layer)

# Merge ############################################################################################
# events and layers
ctb0092 <- merge(ctb0092_event, ctb0092_layer, all = TRUE)
ctb0092[, dataset_id := "ctb0092"]

# citation
ctb0092 <- merge(ctb0092, ctb0092_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0092)
# Layers: 70
# Events: 38
# Georeferenced events: 38

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
