# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0092
# Dados de Carbono de Solos - Projeto SIGecotur/Projeto Forense
#
# Google Drive: https://drive.google.com/drive/u/0/folders/1RsV8ViDb7lsvz7BgxOOhqTHhDWe1qsLE
ctb0092_ids <- soildata_catalog("ctb0092")

# validation #####################################################################################
# Load validation sheet and check
ctb0092_validation <- google_sheet(ctb0092_ids$gs_id, ctb0092_ids$gid_validation)
check_sheet_validation(ctb0092_validation)

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
# Soil profiles have different number of layers. This is because the authors sampled the soil
# profiles until they reached the bedrock or some lithic contact.

# ID da camada -> camada_nome
data.table::setnames(ctb0092_layer, old = "ID da camada", new = "camada_nome")
ctb0092_layer[, camada_nome := as.character(camada_nome)]
ctb0092_layer[, .N, by = camada_nome]
# Most layers are 0-20 cm and 20-40 cm, and some of them are R layers added later one based on 
# communication with the authors.

# ID da amostra -> amostra_id
data.table::setnames(ctb0092_layer, old = "ID da amostra", new = "amostra_id")
ctb0092_layer[, amostra_id := as.character(amostra_id)]
ctb0092_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0092_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0092_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0092_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0092_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0092_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0092_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0092_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0092_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0092_layer[, profund_inf])

# Check for duplicated layers
check_duplicated_layer(ctb0092_layer)

# Check for layers with equal top and bottom depths
check_equal_depths(ctb0092_layer)

# Check for negative layer depths
check_depth_inversion(ctb0092_layer)

# camada_id
# We will create a unique identifier for each layer.
ctb0092_layer <- ctb0092_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0092_layer[, camada_id := 1:.N, by = observacao_id]
ctb0092_layer[, .N, by = camada_id]

# mid_depth
ctb0092_layer[, mid_depth := (profund_sup + profund_inf) / 2]
summary(ctb0092_layer[, mid_depth])

# Check for missing layers
# There are no missing layers in this dataset.
check_missing_layer(ctb0092_layer)

# Terra fina [%] -> terrafina
# The fine earth fraction was not informed in this dataset. Maybe the author did not measure it, but
# could still provide some qualitative information.
data.table::setnames(ctb0092_layer, old = "Terra fina [%]", new = "terrafina")
ctb0092_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0092_layer[, terrafina])

# Areia total [%] -> areia
# areia = areia * 10
data.table::setnames(ctb0092_layer, old = "Areia total [%]", new = "areia")
ctb0092_layer[, areia := as.numeric(areia) * 10]
summary(ctb0092_layer[, areia])
# There are are 39 layers with missing sand content. One of them is soil profile SC2_B27 (10-20). We
# added this layer mannually to the dataset as to ensure completeness of the soil profile. However,
# this is the only event with a a gap between the two layers (0-10 cm and 20-40 cm). It could be a
# typo, or maybe the authors did not complete the laboratory analysis for this layer yet. The other
# 38 layers with missing are R layers added later one based on communication with the authors.
check_empty_layer(ctb0092_layer, "areia")
# Fill empty layer
ctb0092_layer[,
  areia := fill_empty_layer(y = areia, x = mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty layers
check_empty_layer(ctb0092_layer, "areia")
# The interpolation did not fill any new layer.

# Silte [%] -> silte
# silte = silte * 10
data.table::setnames(ctb0092_layer, old = "Silte [%]", new = "silte")
ctb0092_layer[, silte := as.numeric(silte) * 10]
summary(ctb0092_layer[, silte])
# There are 39 layers with missing silt content. See comments for sand content.
check_empty_layer(ctb0092_layer, "silte")
# Fill empty layer
ctb0092_layer[,
  silte := fill_empty_layer(y = silte, x = mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty layers
check_empty_layer(ctb0092_layer, "silte")
# The interpolation did not fill any new layer.

# argila
# old: Argila [%]
# new: argila
# argila = argila * 10
data.table::setnames(ctb0092_layer, old = "Argila [%]", new = "argila")
ctb0092_layer[, argila := as.numeric(argila) * 10]
summary(ctb0092_layer[, argila])
# There are 39 layers with missing clay content. See comments for sand content.
check_empty_layer(ctb0092_layer, "argila")
# Fill empty layer
ctb0092_layer[, argila := fill_empty_layer(argila, mid_depth, c(0, 1000)), by = observacao_id]
# Check again for empty layers
check_empty_layer(ctb0092_layer, "argila")
# The interpolation did not fill any new layer.

# Check the particle size distribution
# We accept a maximum difference of 100 g/kg (10%) in the sum of the particle size fractions.
# Round the values to avoid floating point issues.
ctb0092_layer[, areia := round(areia)]
ctb0092_layer[, silte := round(silte)]
ctb0092_layer[, argila := round(argila)]
ctb0092_layer[, psd_sum := areia + silte + argila]
ctb0092_layer[, psd_diff := abs(1000 - psd_sum)]
ctb0092_layer[
  psd_diff > 100,
  .(observacao_id, camada_nome, profund_sup, profund_inf, areia, silte, argila)
]
# There are no layers with particle size sum differing more than 100 g/kg.

# carbono
# old: C total [%]
# new: carbono
data.table::setnames(ctb0092_layer, old = "C total [%]", new = "carbono")
ctb0092_layer[, carbono := as.numeric(carbono) * 10] # convert to g/kg
summary(ctb0092_layer[, carbono])
# There are 39 layers with missing carbon content. See comments for sand content.
check_empty_layer(ctb0092_layer, "carbono")
# Fill empty layer
ctb0092_layer[,
  carbono := fill_empty_layer(y = carbono, x = mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty layers
check_empty_layer(ctb0092_layer, "carbono")

# ctc
# The cation exchange capacity is not informed in this dataset. Maybe the authors have this
# information. We need to check this with them.
ctb0092_layer[, ctc := NA_real_]

# ph
# The pH is not informed in this dataset. Maybe the authors have this information. We need to check
# this with them.
ctb0092_layer[, ph := NA_real_]

# dsi
# The soil bulk density is not informed in this dataset. Maybe the authors have this information.
# We need to check this with them.
ctb0092_layer[, dsi := NA_real_]

str(ctb0092_layer)

# Merge ############################################################################################
# events and layers
ctb0092 <- merge(ctb0092_event, ctb0092_layer, all = TRUE)
ctb0092[, dataset_id := "ctb0092"]

# citation
ctb0092 <- merge(ctb0092, ctb0092_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0092)
# Layers: 108
# Events: 38
# Georeferenced events: 38

# Plot using mapview
if (interactive()) {
  ctb0092_sf <- sf::st_as_sf(
    ctb0092[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0092_sf["argila"])
}

# Write to disk ####################################################################################
ctb0092 <- select_output_columns(ctb0092)
data.table::fwrite(ctb0092, "ctb0092/ctb0092.csv")
