# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0093
# Dados de "Dados de Carbono de Solos - Projeto SIGecotur/Projeto Forense"
# 
# Google Drive: https://drive.google.com/drive/u/0/folders/13Q2Nth6sBT8IiwdSMwXv1Vci08k8_fD3
ctb0093_ids <- soildata_catalog("ctb0093")

# validation #####################################################################################
# Load validation sheet and check
ctb0093_validation <- google_sheet(ctb0093_ids$gs_id, ctb0093_ids$gid_validation)
check_sheet_validation(ctb0093_validation)

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

# PROCESS FIELDS

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0093_event, old = "ID do evento", new = "observacao_id")
ctb0093_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0093_event[, observacao_id]) > 1)

# data_ano
# old: Ano (coleta)
# new: data_ano
data.table::setnames(ctb0093_event, old = "Ano (coleta)", new = "data_ano")
ctb0093_event[, data_ano := as.integer(data_ano)]
ctb0093_event[, .N, by = data_ano]
# There are 36 events from the year of 2008. This is the same year of the events from ctb0092, a
# study from the same research group in the same region. We need to check with them if these are
# the same events.

# ano_fonte
# The year of data collection is informed in the document.
ctb0093_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0093_event[, .N, by = ano_fonte]

# coord_x
# old: Longitude
# new: coord_x
data.table::setnames(ctb0093_event, old = "Longitude", new = "coord_x")
ctb0093_event[, coord_x := as.numeric(coord_x)]
summary(ctb0093_event[, coord_x])
# There are three events missing x coordinates. The reason is unknown.
ctb0093_event[is.na(coord_x), .(observacao_id)]

# Latitude -> coord_y
data.table::setnames(ctb0093_event, old = "Latitude", new = "coord_y")
ctb0093_event[, coord_y := as.numeric(coord_y)]
summary(ctb0093_event[, coord_y])
# There are three events missing y coordinates. The reason is unknown.
ctb0093_event[is.na(coord_y), .(observacao_id)]

# Datum (coord) -> coord_datum
# already in WGS84
data.table::setnames(ctb0093_event, old = "Datum (coord)", new = "coord_datum")
ctb0093_event[coord_datum == "WGS84", coord_datum := 4326]
ctb0093_event[, coord_datum := as.integer(coord_datum)]
ctb0093_event[, .N, by = coord_datum]

# Check for duplicated coordinates
ctb0093_event[!is.na(coord_x) & !is.na(coord_y), coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0093_event[coord_duplicated == TRUE, .(observacao_id, coord_x, coord_y)]
# There are a few dupplicated coordinates: PAN17 and PAN18, SSC14 and SSC15, and SC2B13 and SC2B15.
# Add small jitter to coord_x and coord_y
set.seed(12345)
amount <- 1 # meter
ctb0093_event_sf <- sf::st_as_sf(
  ctb0093_event[coord_duplicated == TRUE],
  coords = c("coord_x", "coord_y"),
  crs = 4326
)
# Transform to UTM
ctb0093_event_sf <- sf::st_transform(ctb0093_event_sf, crs = 31983) # SIRGAS 2000 / UTM zone 23S
# Add jitter
ctb0093_event_sf <- sf::st_jitter(ctb0093_event_sf, amount = amount)
# Transform back to WGS84
ctb0093_event_sf <- sf::st_transform(ctb0093_event_sf, crs = 4326)
# Update the coordinates in the original data.table
ctb0093_event[coord_duplicated == TRUE, coord_x := sf::st_coordinates(ctb0093_event_sf)[, 1]]
ctb0093_event[coord_duplicated == TRUE, coord_y := sf::st_coordinates(ctb0093_event_sf)[, 2]]
rm(ctb0093_event_sf)

# Fonte (coord) -> coord_fonte
# GPS Garmin
data.table::setnames(ctb0093_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0093_event[, coord_fonte := as.character(coord_fonte)]
ctb0093_event[coord_duplicated == TRUE, coord_fonte := "GPS Garmin + 1-m jitter"]
ctb0093_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision with which the coordinates were recorded in the field is not informed in the
# document. However, the coordinates were collected using a Garmin GPS. So we can assume a precision
# of 30 meters.
data.table::setnames(ctb0093_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0093_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0093_event[is.na(coord_precisao), coord_precisao := 30]
summary(ctb0093_event[, coord_precisao])
# Update the precision of the coordinates (coord_precisao) using the Pytagorean theorem to account
# for the jitter applied above.
ctb0093_event[coord_duplicated == TRUE, coord_precisao := sqrt(coord_precisao^2 + amount^2)]
summary(ctb0093_event[, coord_precisao])
ctb0093_event[, coord_duplicated := NULL]

# País -> pais_id
data.table::setnames(ctb0093_event, old = "País", new = "pais_id")
ctb0093_event[, pais_id := as.character(pais_id)]
ctb0093_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0093_event, old = "Estado (UF)", new = "estado_id")
ctb0093_event[, estado_id := as.character(estado_id)]
ctb0093_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0093_event, old = "Município", new = "municipio_id")
ctb0093_event[, municipio_id := as.character(municipio_id)]
ctb0093_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0093_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0093_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0093_event[, amostra_area])

# SiBCS  -> taxon_sibcs
# Soil classification according to SiBCS is missing in this document.
ctb0093_event[, taxon_sibcs := NA_character_]

# taxon_st
# Soil classification according to US Soil Taxonomy is missing in this document.
ctb0093_event[, taxon_st := NA_character_]

# pedregosidade
# Data on stoniness is missing in this document.
ctb0093_event[, pedregosidade := NA_character_]

# rochosidade
# Data on rockiness is missing in this document.
ctb0093_event[, rochosidade := NA_character_]

str(ctb0093_event)

# layers ###########################################################################################
ctb0093_layer <- google_sheet(ctb0093_ids$gs_id, ctb0093_ids$gid_layer)
str(ctb0093_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0093_layer, old = "ID do evento", new = "observacao_id")
ctb0093_layer[, observacao_id := as.character(observacao_id)]
ctb0093_layer[, .N, by = observacao_id][order(N)]

# ID da camada -> camada_nome
data.table::setnames(ctb0093_layer, old = "ID da camada", new = "camada_nome")
ctb0093_layer[, camada_nome := as.character(camada_nome)]
ctb0093_layer[, .N, by = camada_nome]
# Most layers have round numbers for the layer names, like 0-10, 10-20, etc. A few have broken
# ranges like 0-15, 0-25, etc. This could mean that the authors reached the bedrock before reaching
# the standard layer depth. we need to check this with them.

# ID da amostra -> amostra_id
# The laboratory sample identifier is available in the source.
data.table::setnames(ctb0093_layer, old = "ID da amostra", new = "amostra_id")
ctb0093_layer[, amostra_id := as.character(amostra_id)]
ctb0093_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0093_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
# Resolve irregular depth intervals
ctb0093_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0093_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0093_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0093_layer, old = "Profundidade final [cm]", new = "profund_inf")
# Resolve irregular depth intervals
ctb0093_layer[, profund_inf := depth_slash(profund_inf), by = .I]
# Resolve censored depths
ctb0093_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0093_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0093_layer[, profund_inf])

# Check for equal layer depths
ctb0093_layer[profund_sup == profund_inf]

# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile. Order by observacao_id and mid_depth.
ctb0093_layer[, mid_depth := (profund_sup + profund_inf) / 2]
data.table::setorder(ctb0093_layer, observacao_id, mid_depth)
ctb0093_layer[, camada_id := seq_len(.N), by = observacao_id]
ctb0093_layer[, .N, by = camada_id]
# Most soil profiles have two layers. A few (28) have three layers.

# Check for duplicated layers in the same soil profile
check_repeated_layer(ctb0093_layer)
# ATTENTION. There are duplicated layers in four soil profiles: SCM15, SCM25, SCM49, SCM60. It is
# not evident why this happened. We need to check with the data providers. For now, we will drop one
# of them.
ctb0093_layer <- ctb0093_layer[,
  .SD[1],
  by = .(observacao_id, profund_sup, profund_inf)
]
# Check again for duplicated layers
check_repeated_layer(ctb0093_layer)

# Check missing layers
check_missing_layer(ctb0093_layer)
# There are missing layers in 15 soil profiles. In some cases the topsoil layer is missing, in
# others it is a subsoil layer. We need to check with the data providers the reason for these
# missing layers. For now, we will add them.
ctb0093_layer <- add_missing_layer(ctb0093_layer)
ctb0093_layer[, .N, by = camada_nome]

# terrafina
# Data on fine earth (terrafina) content is missing in this document. The authors are still running
# the laboratory analysis.
ctb0093_layer[, terrafina := NA_real_]

# areia
# Data on sand content is missing in this document. The authors are still running the laboratory
# analysis.
ctb0093_layer[, areia := NA_real_]

# silte
# Data on silt content is missing in this document. The authors are still running the laboratory
# analysis.
ctb0093_layer[, silte := NA_real_]
# analysis.
ctb0093_layer[, silte := NA_real_]

# argila
# Data on clay content is missing in this document. The authors are still running the laboratory
# analysis.
ctb0093_layer[, argila := NA_real_]

# Check that the sum of sand, silt, and clay is 1000 g/kg
# AS SOON AS THE DATA IS AVAILABLE

# carbono
# old: C [%]
# new: carbono
# Convert from percentage to g/kg
data.table::setnames(ctb0093_layer, old = "C [%]", new = "carbono")
ctb0093_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0093_layer[, carbono])
# There are 18 missing values for carbon content, most of them in the added missing layers.
check_empty_layer(ctb0093_layer, "carbono")
# Fill empty layers
ctb0093_layer[,
  carbono := fill_empty_layer(y = carbono, x = mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]

# ctc
# Data on cation exchange capacity (ctc) is missing in this document. The authors are still running
# the laboratory analysis.
ctb0093_layer[, ctc := NA_real_]

# ph
# Data on pH is missing in this document. The authors are still running the laboratory analysis.
ctb0093_layer[, ph := NA_real_]

# dsi
# Data on soil bulk density (dsi) is missing in this document. The authors are still running
# the laboratory analysis.
ctb0093_layer[, dsi := NA_real_]

str(ctb0093_layer)

# Merge ############################################################################################
# events and layers
ctb0093 <- merge(ctb0093_event, ctb0093_layer, all = TRUE)
ctb0093[, dataset_id := "ctb0093"]

# citation
ctb0093 <- merge(ctb0093, ctb0093_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0093)
# Layers: 759
# Events: 373
# Georeferenced events: 370

# Plot using mapview
if (FALSE) {
  ctb0093_sf <- sf::st_as_sf(
    ctb0093[!is.na(coord_x) & !is.na(coord_y)],
    coords = c("coord_x", "coord_y"),
    crs = 4326
  )
  mapview::mapview(ctb0093_sf["carbono"])
}

# Write to disk ####################################################################################
ctb0093 <- select_output_columns(ctb0093)
data.table::fwrite(ctb0093, "ctb0093/ctb0093.csv")
