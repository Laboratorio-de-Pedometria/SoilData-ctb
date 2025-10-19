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

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0101
# Dados de "Sistema pedológico Planossolo-Plintossolo no Pantanal de Barão de Melgaço-MT"
# 
# Google Drive: https://drive.google.com/drive/folders/1C_l6qxu3yhUY8BlLBi6rhnfZKeRCtFjF
# NotebookLM: https://notebooklm.google.com/notebook/0f4fa5bd-6c4f-43b1-bd03-019e6c2302e8
ctb0101_ids <- soildata_catalog("ctb0101")

# validation #####################################################################################
ctb0101_validation <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_validation)
check_sheet_validation(ctb0101_validation)

# citation #####################################################################################
ctb0101_citation <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_citation)
str(ctb0101_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0101_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0101_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0101_citation <- data.table::data.table(
  dataset_id = "ctb0101",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0101_citation)

# event ############################################################################################
ctb0101_event <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_event)
str(ctb0101_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0101_event, old = "ID do evento", new = "observacao_id")
ctb0101_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
any(table(ctb0101_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0101_event, old = "Ano (coleta)", new = "data_ano")
ctb0101_event[, data_ano := as.integer(data_ano)]
ctb0101_event[, .N, by = data_ano]

# ano_fonte
ctb0101_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0101_event[, .N, by = ano_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0101_event, old = "Longitude", new = "coord_x")
ctb0101_event[, coord_x := as.numeric(coord_x)]
summary(ctb0101_event[, coord_x])
# UTM coordinates

# Latitude -> coord_y
data.table::setnames(ctb0101_event, old = "Latitude", new = "coord_y")
ctb0101_event[, coord_y := as.numeric(coord_y)]
summary(ctb0101_event[, coord_y])
# UTM coordinates

# Check for duplicate coordinates
check_equal_coordinates(ctb0101_event)

# Datum (coord) -> coord_datum
# UTM-SAD 69 Zona 21s : EPSG == 29191
# https://spatialreference.org/ref/epsg/29181/
data.table::setnames(ctb0101_event, old = "Datum (coord)", new = "coord_datum")
ctb0101_event[, coord_datum := as.character(coord_datum)]
ctb0101_event[, coord_datum := gsub("UTM-SAD 69 Zona 21s", 29191, coord_datum)]
ctb0101_event[, coord_datum := as.integer(coord_datum)]
ctb0101_event[, .N, by = coord_datum]

# Transform UTM-SAD 69 Zona 21s to WGS84
ctb0101_event_sf <- sf::st_as_sf(
  ctb0101_event[coord_datum == 29191],
  coords = c("coord_x", "coord_y"),
  crs = 29191 # Informa o sistema de coordenadas de origem
)
# Transforma as coordenadas para WGS84 (padrão GPS, EPSG: 4326)
ctb0101_event_sf <- sf::st_transform(ctb0101_event_sf, 4326)
# Atualiza o data.table original com as novas coordenadas e o novo datum
ctb0101_event[coord_datum == 29191, coord_x := sf::st_coordinates(ctb0101_event_sf)[, 1]]
ctb0101_event[coord_datum == 29191, coord_y := sf::st_coordinates(ctb0101_event_sf)[, 2]]
ctb0101_event[coord_datum == 29191, coord_datum := 4326]
rm(ctb0101_event_sf)

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0101_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0101_event[, coord_fonte := as.character(coord_fonte)]
ctb0101_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The coordinates were estimated using Google Maps, but the precision is not informed in this
# dataset. Therefore, we will assume a precision of 100 meters.
data.table::setnames(ctb0101_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0101_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0101_event[is.na(coord_precisao), coord_precisao := 100]
summary(ctb0101_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0101_event, old = "País", new = "pais_id")
ctb0101_event[, pais_id := as.character(pais_id)]
ctb0101_event[, .N, by = pais_id]

# Estado -> estado_id
data.table::setnames(ctb0101_event, old = "Estado", new = "estado_id")
ctb0101_event[, estado_id := as.character(estado_id)]
ctb0101_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0101_event, old = "Município", new = "municipio_id")
ctb0101_event[, municipio_id := as.character(municipio_id)]
ctb0101_event[, .N, by = municipio_id]

# Área do evento [m2] -> amostra_area
data.table::setnames(ctb0101_event, old = "Área do evento [m2]", new = "amostra_area")
ctb0101_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0101_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0101_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0101_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0101_event[, .N, by = taxon_sibcs]

# taxon_st 
# missing this soil taxonomy on document
ctb0101_event[, taxon_st := NA_character_]

# pedregosidade
# This study does not have qualitative information about stoniness.
ctb0101_event[, pedregosidade := NA_character_]

# rochosidade
# This study does not have qualitative information about rockiness.
ctb0101_event[, rochosidade := NA_character_]

str(ctb0101_event)

# layers ###########################################################################################
ctb0101_layer <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_layer)
str(ctb0101_layer)

# Process fields

# This study analyzed both the soil matrix and the nodules (plintites and petroplintites)
# separately. This is represented in the "Material" column. Our target is the soil matrix
# only. Therefore, we will filter the dataset to keep only the layers where "Material" is
# "matriz".
ctb0101_layer <- ctb0101_layer[Material == "matriz"]

# ID do evento -> observacao_id
data.table::setnames(ctb0101_layer, old = "ID do evento", new = "observacao_id")
ctb0101_layer[, observacao_id := as.character(observacao_id)]
ctb0101_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0101_layer, old = "ID da camada", new = "camada_nome")
ctb0101_layer[, camada_nome := as.character(camada_nome)]
ctb0101_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# Soil sample ID is not informed in this document.
ctb0101_layer[, amostra_id := NA_real_]

# Filter broken layers
# The soil profile PT1 has some complicated broken depth intervals. Maybe there was some error
# during the recording of the data in the field or during the transcription to the document. We
# prefer to deal with them manually here (see below).

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0101_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0101_layer[observacao_id == "PT1" & camada_nome == "B/E", profund_sup := 120]
# Resolve broken depth intervals with slash "/"
ctb0101_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0101_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0101_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0101_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0101_layer[observacao_id == "PT1" & camada_nome == "E", profund_inf := 60]
ctb0101_layer[observacao_id == "PT1" & camada_nome == "EB", profund_inf := 120]
ctb0101_layer[observacao_id == "PT1" & camada_nome == "B/E", profund_inf := 165]
# Resolve irregular layer transition (slash)
ctb0101_layer[, profund_inf := depth_slash(profund_inf), by = .I]
# Resolve censored layer depth (plus)
ctb0101_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0101_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0101_layer[, profund_inf])

# Temporarily correct error in PT6
# In profile PT6, there is a layer C4 that apparently does not exist in the original source document.
# This layer is recorded in the spreadsheet with the depth interval that should belong to layer Cf.
# We will remove the C4 layer and correct the depth of layer Cf. However, this needs to be confirmed
# by consulting the original source document.
# Drop the C4 layer
ctb0101_layer <- ctb0101_layer[!(observacao_id == "PT6" & camada_nome == "C4")]
# Correct the depth of layer Cf if necessary
ctb0101_layer[observacao_id == "PT6" & camada_nome == "Cf", profund_sup := fifelse(
  profund_sup == 186, 145, profund_sup
)]
ctb0101_layer[observacao_id == "PT6" & camada_nome == "Cf", profund_inf := fifelse(
  profund_inf >= 200, 186, profund_inf
)]

# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile. Order by observacao_id and mid_depth.
ctb0101_layer[, mid_depth := (profund_sup + profund_inf) / 2]
ctb0101_layer <- ctb0101_layer[order(observacao_id, mid_depth)]
ctb0101_layer[, camada_id := 1:.N, by = observacao_id]
ctb0101_layer[, .N, by = camada_id]

# Check for duplicated layers
check_repeated_layer(ctb0101_layer)

# Check for missing layers
check_missing_layer(ctb0101_layer)

# terrafina
# The source document mentions that the coarse fraction was determined, but does not provide the data.
# Therefore, we will set the terrafina (fine earth fraction) as NA.
ctb0101_layer[, terrafina := NA_real_]

# areia
# old: Areia Total [%]
# new: areia
data.table::setnames(ctb0101_layer, old = "Areia Total [%]", new = "areia")
ctb0101_layer[, areia := as.numeric(areia) * 10]
summary(ctb0101_layer[, areia])
# There is one layer with missing areia: PT6, layer Cf. This looks like an error in the source 
# spreadsheet.
check_empty_layer(ctb0101_layer, "areia")

# silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0101_layer, old = "Silte [%]", new = "silte")
ctb0101_layer[, silte := as.numeric(silte)*10]
summary(ctb0101_layer[, silte])
# There is one layer with missing silte: PT6, layer Cf. This looks like an error in the source
# spreadsheet.
check_empty_layer(ctb0101_layer, "silte")

# argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0101_layer, old = "Argila [%]", new = "argila")
ctb0101_layer[, argila := as.numeric(argila)*10]
summary(ctb0101_layer[, argila])
# There is one layer with missing argila: PT6, layer Cf. This looks like an error in the source
# spreadsheet.
check_empty_layer(ctb0101_layer, "argila")

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0101_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0101_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0101_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: Corg [g/kg]
# new: carbono
data.table::setnames(ctb0101_layer, old = "Corg [g/kg]", new = "carbono")
ctb0101_layer[, carbono := as.numeric(carbono)]
summary(ctb0101_layer[, carbono])
# There is one layer with missing carbono: PT6, layer Cf. This looks like an error in the source
# spreadsheet.
check_empty_layer(ctb0101_layer, "carbono")

# ctc
# old: T [mmolc/kg]
# new: ctc
data.table::setnames(ctb0101_layer, old = "T [mmolc/kg]", new = "ctc")
ctb0101_layer[, ctc := as.numeric(ctc)]
summary(ctb0101_layer[, ctc])
# There is one layer with missing ctc: PT6, layer Cf. This looks like an error in the source
# spreadsheet.
check_empty_layer(ctb0101_layer, "ctc")

# ph
# old: pH H_2O
# new: ph
data.table::setnames(ctb0101_layer, old = "pH H_2O", new = "ph")
ctb0101_layer[, ph := as.numeric(ph)]
summary(ctb0101_layer[, ph])
# There is one layer with missing ph: PT6, layer Cf. This looks like an error in the source
# spreadsheet.
check_empty_layer(ctb0101_layer, "ph")

# dsi 
# The soil density is not informed in this document.
ctb0101_layer[, dsi := NA_real_]
summary(ctb0101_layer[, dsi])

str(ctb0101_layer)

# Merge ############################################################################################
# events and layers
ctb0101 <- merge(ctb0101_event, ctb0101_layer, all = TRUE)
ctb0101[, dataset_id := "ctb0101"]

# citation
ctb0101 <- merge(ctb0101, ctb0101_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0101)
# Layers: 51
# Events: 8
# Georeferenced events: 8

# Plot using mapview
if (FALSE) {
  ctb0101_sf <- sf::st_as_sf(
    ctb0101[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0101_sf["argila"])
}

# Write to disk ####################################################################################
ctb0101 <- select_output_columns(ctb0101)
data.table::fwrite(ctb0101, "ctb0101/ctb0101.csv")
