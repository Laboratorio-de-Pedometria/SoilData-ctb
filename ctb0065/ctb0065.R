# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("sf")) {
  install.packages("sf")
  library("sf")
}
if (!require("parzer")) {
  install.packages("parzer")
  library("parzer")
}

# Source helper functions
source("./helper.R")


# Google Sheet #####################################################################################
# ctb0065
# Dados de "Solos de três áreas de  restinga. 1. Morfologia, caracterização e classificação"
# https://drive.google.com/drive/folders/1N90u15zgwHxxOU__vQJKfivjubk5_gb0
ctb0065_ids <- soildata_catalog("ctb0065")

# validation #####################################################################################
ctb0065_validation <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_validation)
str(ctb0065_validation)

# Check for negative validation results
sum(ctb0065_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0065_citation <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_citation)
str(ctb0065_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0065_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0065_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0065_citation <- data.table::data.table(
  dataset_id = "ctb0065",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0065_citation)

# event ############################################################################################
ctb0065_event <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_event)
str(ctb0065_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0065_event, old = "ID do evento", new = "observacao_id")
ctb0065_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
any(table(ctb0065_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
# The year of data collection is missing. the author started the master's degree in 1992, so we
# estimate that he obtained the data collection in 1993. It is necessary to contact the author to
# find out the actual year of collection.
data.table::setnames(ctb0065_event, old = "Ano (coleta)", new = "data_ano")
ctb0065_event[, data_ano := as.integer(data_ano)]
ctb0065_event[, .N, by = data_ano]

# ano_fonte
ctb0065_event[is.na(data_ano), ano_fonte := "estimativa"]
ctb0065_event[, .N, by = ano_fonte]

# Set data_ano to 1993
ctb0065_event[is.na(data_ano), data_ano := 1993]
ctb0065_event[, .N, by = data_ano]

# Missing seconds precision on Longitude / Latitude
# The parzer library was added to convert the characters into values appropriate for the coordinate
# table.
# Longitude -> coord_x
data.table::setnames(ctb0065_event, old = "Longitude", new = "coord_x")
ctb0065_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0065_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0065_event, old = "Latitude", new = "coord_y")
ctb0065_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0065_event[, coord_y])

# Datum (coord) -> coord_datum
data.table::setnames(ctb0065_event, old = "Datum (coord)", new = "coord_datum")
ctb0065_event[, coord_datum := as.character(coord_datum)]
ctb0065_event[, .N, by = coord_datum]
# The datum is missing in the spreadsheet ("#N/A"). However, based on the approximate sampling year,
# we can infer that the datum used was SAD69, which was widely used in Brazil until the mid-1990s.
# Thus, we set the missing datum to "SAD69"., EPSG code 4618.
ctb0065_event[is.na(coord_datum), coord_datum := 4618L]
ctb0065_event[, .N, by = coord_datum]

# Transform coordinates to WGS84
# Filter only data in SAD69 (4618) for transformation
ctb0065_event_sf <- sf::st_as_sf(
  ctb0065_event[coord_datum == 4618],
  coords = c("coord_x", "coord_y"), crs = 4618
)
# Transform coordinates to WGS84
ctb0065_event_sf <- sf::st_transform(ctb0065_event_sf, 4326)
# Update the coordinates in the data.table
ctb0065_event[coord_datum == 4618, coord_x := sf::st_coordinates(ctb0065_event_sf)[, 1]]
ctb0065_event[coord_datum == 4618, coord_y := sf::st_coordinates(ctb0065_event_sf)[, 2]]
ctb0065_event[coord_datum == 4618, coord_datum := 4326]
rm(ctb0065_event_sf)
ctb0065_event[, .N, by = coord_datum]
summary(ctb0065_event[, .(coord_x, coord_y)])

# Precisão (coord) [m] -> coord_precisao
# The document does not specify whether the coordinates were obtained via GPS, from a particular
# mapping project, or what the measurement error or precision is. They appear to have been obtained
# from maps. Therefore, we will set the precision to 1800 meters for all events.
data.table::setnames(ctb0065_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0065_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0065_event[is.na(coord_precisao), coord_precisao := 1800]
summary(ctb0065_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0065_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0065_event[, coord_fonte := as.character(coord_fonte)]
ctb0065_event[, .N, by = coord_fonte]

# Events in the water
# The current coordinates of the events LGR-P, LGR-MD, and LGR-MS place them in the water. We add
# new coordinates for these events based the description in the document and analysis of satellite
# images in Google Earth.
# LGR-P: 22°11'17"S 41°25'56"W
ctb0065_event[observacao_id == "LGR-P", `:=`(
  coord_x = -(41 + 25 / 60 + 56 / 3600),
  coord_y = -(22 + 11 / 60 + 17 / 3600),
  coord_fonte = "Historical images in Google Earth"
)]
# LGR-MD: 22°11'25"S 41°26'14"W
ctb0065_event[observacao_id == "LGR-MD", `:=`(
  coord_x = -(41 + 26 / 60 + 14 / 3600),
  coord_y = -(22 + 11 / 60 + 25 / 3600),
  coord_fonte = "Historical images in Google Earth"
)]
# LGR-MS: 22°11'13"S 41°26'15"W
ctb0065_event[observacao_id == "LGR-MS", `:=`(
  coord_x = -(41 + 26 / 60 + 15 / 3600),
  coord_y = -(22 + 11 / 60 + 13 / 3600),
  coord_fonte = "Historical images in Google Earth"
)]

# Check for duplicate coordinates
ctb0065_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0065_event[coord_duplicated == TRUE, .(observacao_id, coord_x, coord_y)]
# Some of the other events share the same coordinates. We will add a small jitter to pass checks.
ctb0065_event_sf <- sf::st_as_sf(
  ctb0065_event[coord_duplicated == TRUE],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Transform to UTM
ctb0065_event_sf <- sf::st_transform(ctb0065_event_sf, 31983)
# Jitter
set.seed(42)
amount <- 1 # meters
ctb0065_event_sf <- sf::st_jitter(ctb0065_event_sf, amount = amount)
# Transform back to WGS84
ctb0065_event_sf <- sf::st_transform(ctb0065_event_sf, 4326)
# Update the coordinates in the data.table()
ctb0065_event[coord_duplicated == TRUE, coord_x := sf::st_coordinates(ctb0065_event_sf)[, 1]]
ctb0065_event[coord_duplicated == TRUE, coord_y := sf::st_coordinates(ctb0065_event_sf)[, 2]]
# Update coord_precisao
# The precision is updated by combining the original precision with the jitter amount using
# the Pythagorean theorem.
ctb0065_event[
  coord_duplicated == TRUE,
  coord_precisao := sqrt(coord_precisao^2 + (amount * sqrt(2))^2)
]
summary(ctb0065_event[, coord_precisao])
# Remove temporary columns
ctb0065_event[, coord_duplicated := NULL]
rm(ctb0065_event_sf)
# Check again for duplicate coordinates
ctb0065_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0065_event[coord_duplicated == TRUE, .(observacao_id, coord_x, coord_y)]
# No more duplicate coordinates
ctb0065_event[, coord_duplicated := NULL]

# País -> pais_id
data.table::setnames(ctb0065_event, old = "País", new = "pais_id")
ctb0065_event[, pais_id := as.character(pais_id)]
ctb0065_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0065_event, old = "Estado (UF)", new = "estado_id")
ctb0065_event[, estado_id := as.character(estado_id)]
ctb0065_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0065_event, old = "Município", new = "municipio_id")
ctb0065_event[, municipio_id := as.character(municipio_id)]
ctb0065_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0065_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0065_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0065_event[, amostra_area])

# Classificação de solo (1988) <- taxon_sibcs
data.table::setnames(ctb0065_event, old = "Classificação do Solo (1988)", new = "taxon_sibcs")
ctb0065_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0065_event[, .N, by = taxon_sibcs]

# Soil Taxonomy (1975) -> taxon_st
# The Soil Taxonomy classification is available in the original document, but the spreadsheet
# provided does not contain this information.
# data.table::setnames(ctb0065_event, old = "Soil Taxonomy (1975)", new = "taxon_st")
ctb0065_event[, taxon_st := NA_character_]
ctb0065_event[, taxon_st := as.character(taxon_st)]
ctb0065_event[, .N, by = taxon_st]

# pedregosidade
# The report does not mention the presence of stones or gravel in the profiles studied. However,
# the layer data indicates some presence of gravel in certain layers. We will set this field to NA
# in the event data and address it in the layer data.
ctb0065_event[, pedregosidade := NA_character_]

# rochosidade
# The document does not provide information about rockiness. Based on an analysis of the data and
# document, we can infer that the profiles do not exhibit rockiness. 
ctb0065_event[, rochosidade := "ausente"]

str(ctb0065_event)

# layers ###########################################################################################
ctb0065_layer <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_layer)
str(ctb0065_layer)

# PROCESS FIELDS

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0065_layer, old = "ID do evento", new = "observacao_id")
ctb0065_layer[, observacao_id := as.character(observacao_id)]
ctb0065_layer[, .N, by = observacao_id]

# camada_nome
# old: ID da camada
# new: camada_nome
data.table::setnames(ctb0065_layer, old = "ID da camada", new = "camada_nome")
ctb0065_layer[, camada_nome := as.character(camada_nome)]
ctb0065_layer[, .N, by = camada_nome]

# amostra_id
# old: ID da amostra
# new: amostra_id
data.table::setnames(ctb0065_layer, old = "ID da amostra", new = "amostra_id")
ctb0065_layer[, amostra_id := as.character(amostra_id)]
ctb0065_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0065_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0065_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0065_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0065_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0065_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0065_layer[, profund_inf])

# camada_id
ctb0065_layer <- ctb0065_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0065_layer[, camada_id := 1:.N, by = observacao_id]
ctb0065_layer[, .N, by = camada_id]
summary(ctb0065_layer[, camada_id])

# terrafina
# old: Fração fina [< 2mm~] (%)
# new: terrafina
data.table::setnames(ctb0065_layer, old = "Fração fina [< 2mm~] (%)", new = "terrafina")
ctb0065_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0065_layer[, terrafina])
# Fine earth fraction is missing for three layers.
check_empty_layer(ctb0065_layer, "terrafina")
# Two layers consist of organic material only (O1 and O2 horizons), while the third is a surface
# layer composed of recent alluvium. We keep these layers as NA for now.

# areia_grossa
# old: Areia grossa (%)
# new: areia_grossa
# Areia_grossa is missing for some -13, -10 and -10, -5 layers.
data.table::setnames(ctb0065_layer, old = "Areia grossa (%)", new = "areia_grossa")
ctb0065_layer[, areia_grossa := as.numeric(areia_grossa) * 10]
summary(ctb0065_layer[, areia_grossa])
# Coarse sand is missing for three layers, the same layers where fine earth fraction is missing.
check_empty_layer(ctb0065_layer, "areia_grossa")

# areia_fina
# old: Areia fina (%)
# new: areia_fina
data.table::setnames(ctb0065_layer, old = "Areia fina (%)", new = "areia_fina")
ctb0065_layer[, areia_fina := as.numeric(areia_fina) * 10]
summary(ctb0065_layer[, areia_fina])
# Fine sand is missing for three layers, the same layers where fine earth fraction is missing.
check_empty_layer(ctb0065_layer, "areia_fina")

# areia
# criação da coluna areia
ctb0065_layer[, areia := areia_grossa + areia_fina]
summary(ctb0065_layer[, areia])

# silte
# old: Silte (%)
# new: silte
data.table::setnames(ctb0065_layer, old = "Silte (%)", new = "silte")
ctb0065_layer[, silte := as.numeric(silte) * 10]
summary(ctb0065_layer[, silte])
# Silt is missing for three layers, the same layers where fine earth fraction is missing.
check_empty_layer(ctb0065_layer, "silte")

# argila
# old: Argila (%)
# new: argila
data.table::setnames(ctb0065_layer, old = "Argila (%)", new = "argila")
ctb0065_layer[, argila := as.numeric(argila) * 10]
summary(ctb0065_layer[, argila])
# Clay is missing for three layers, the same layers where fine earth fraction is missing.
check_empty_layer(ctb0065_layer, "argila")

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0065_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0065_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.

# carbono
# old: C (orgânico) (g/kg)
# new: carbono
data.table::setnames(ctb0065_layer, old = "C (orgânico) (g/kg)", new = "carbono")
ctb0065_layer[, carbono := as.numeric(carbono)]
summary(ctb0065_layer[, carbono])
# The carbon content is missing for two layers. One corresponds to the A0 horizon, which is
# recent alluvium. The other is O1 horizon, which is organic material. We will keep these as NA for
# now.
check_empty_layer(ctb0065_layer, "carbono")

# ctc
# old: CTC pH 7,0 (cmolc/kg)
# new: ctc
data.table::setnames(ctb0065_layer, old = "CTC pH 7,0 (cmolc/kg)", new = "ctc")
ctb0065_layer[, ctc := as.numeric(ctc)]
summary(ctb0065_layer[, ctc])
# CTC is missing for two layers, the same layers where carbon content is missing.
check_empty_layer(ctb0065_layer, "ctc")

# ph
# old: H2O (pH)
# new: ph
data.table::setnames(ctb0065_layer, old = "H2O (pH)", new = "ph")
ctb0065_layer[, ph := as.numeric(ph)]
summary(ctb0065_layer[, ph])
# pH is missing for two layers, the same layers where carbon content is missing.
check_empty_layer(ctb0065_layer, "ph")

# dsi
# old: Densidade do solo (g/cm^3)
# new: dsi
data.table::setnames(ctb0065_layer, old = "Densidade do solo (g/cm^3)", new = "dsi")
ctb0065_layer[, dsi := as.numeric(dsi)]
summary(ctb0065_layer[, dsi])
# The bulk density is missing for 18 layers. According to the document, this happened due to the
# difficulty in collecting undisturbed samples in layers with too much organic material, too many
# roots, or other issues such as extreme cimentation (Bshx layer) or excess water in very sandy
# layers. We will keep these as NA for now.
check_empty_layer(ctb0065_layer, "dsi")

str(ctb0065_layer)

# Merge ############################################################################################
# events and layers
ctb0065 <- merge(ctb0065_event, ctb0065_layer, all = TRUE)
ctb0065[, dataset_id := "ctb0065"]

# citation
ctb0065 <- merge(ctb0065, ctb0065_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0065)
# Layers: 36
# Events: 8
# Georeferenced events: 7

# Plot using mapview
if (FALSE) {
  ctb0065_sf <- sf::st_as_sf(
    ctb0065[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0065_sf["argila"])
}

# Write to disk ####################################################################################
ctb0065 <- select_output_columns(ctb0065)
data.table::fwrite(ctb0065, "ctb0065/ctb0065.csv")
data.table::fwrite(ctb0065_event, "ctb0065/ctb0065_event.csv")
data.table::fwrite(ctb0065_layer, "ctb0065/ctb0065_layer.csv")