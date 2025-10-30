# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Install and load required packages
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0045
# Dados de "Parâmetros Físicos e Químicos de Referência em Solos de Unidades de Conservação
# Florestal da Bacia do Paraná 3, Brasil"
# 
# Google Drive: https://drive.google.com/drive/folders/1Dzp7lP2A70rugRwuQSQORntkbdG7przn
# NotebookLM: https://notebooklm.google.com/notebook/cd6a491e-da10-4110-bd65-5317f3653a05
ctb0045_ids <- soildata_catalog("ctb0045")

# validation #####################################################################################
ctb0045_validation <- google_sheet(ctb0045_ids$gs_id, ctb0045_ids$gid_validation)
check_sheet_validation(ctb0045_validation)

# citation #####################################################################################
ctb0045_citation <- google_sheet(ctb0045_ids$gs_id, ctb0045_ids$gid_citation)
str(ctb0045_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0045_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0045_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0045_citation <- data.table::data.table(
  dataset_id = "ctb0045",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0045_citation)

# event #####################################################################################
ctb0045_event <- google_sheet(ctb0045_ids$gs_id, ctb0045_ids$gid_event)
str(ctb0045_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0045_event, old = "ID do evento", new = "observacao_id")
ctb0045_event[, observacao_id := as.character(observacao_id)]
ctb0045_event[, .N, by = observacao_id]
# Check for duplicate observacao_id
any(table(ctb0045_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0045_event, old = "Ano (coleta)", new = "data_ano")
ctb0045_event[, data_ano := as.integer(data_ano)]
ctb0045_event[, .N, by = data_ano]

# ano_fonte
ctb0045_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0045_event[, .N, by = ano_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0045_event, old = "Longitude", new = "coord_x")
ctb0045_event[, coord_x := as.numeric(coord_x)]
summary(ctb0045_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0045_event, old = "Latitude", new = "coord_y")
ctb0045_event[, coord_y := as.numeric(coord_y)]
summary(ctb0045_event[, coord_y])

# Check for duplicate coordinates
check_equal_coordinates(ctb0045_event)

# DATUM -> coord_datum
# WGS84
data.table::setnames(ctb0045_event, old = "Datum (coord)", new = "coord_datum")
ctb0045_event[, coord_datum := as.character(coord_datum)]
ctb0045_event[, .N, by = coord_datum]
ctb0045_event[coord_datum == "WGS84", coord_datum := 4326]
ctb0045_event[, coord_datum := as.integer(coord_datum)]
ctb0045_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0045_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0045_event[, coord_fonte := as.character(coord_fonte)]
ctb0045_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is not informed in this dataset. However, the coordinates were
# obtained using GPS device, so we assume a precision of 10 meters.
data.table::setnames(ctb0045_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0045_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0045_event[is.na(coord_precisao), coord_precisao := 10]
summary(ctb0045_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0045_event, old = "País", new = "pais_id")
ctb0045_event[, pais_id := as.character(pais_id)]
ctb0045_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0045_event, old = "Estado (UF)", new = "estado_id")
ctb0045_event[, estado_id := as.character(estado_id)]
ctb0045_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0045_event, old = "Município", new = "municipio_id")
ctb0045_event[, municipio_id := as.character(municipio_id)]
ctb0045_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0045_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0045_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0045_event[, amostra_area])

# Classe do solo  -> taxon_sibcs
# The authors determined the soil class at the sampling points through a combination of existing
# external sources (maps and databases) and subsequent field verification/classification.
data.table::setnames(ctb0045_event, old = "Classe do solo", new = "taxon_sibcs")
ctb0045_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0045_event[, .N, by = taxon_sibcs]

# taxon_st 
# missing this soil taxonomy on document
ctb0045_event[, taxon_st := NA_character_]
ctb0045_event[, .N, by = taxon_st]

# pedregosidade
# The stoniness was infered by our team based on the visual asessment of soil profile pictures
# provided in the original document.
data.table::setnames(ctb0045_event, old = "Pedregosidade", new = "pedregosidade")
ctb0045_event[, pedregosidade := as.character(pedregosidade)]
ctb0045_event[, .N, by = pedregosidade]

# rochosidade
# missing in this document.
ctb0045_event[, rochosidade := NA_character_]

str(ctb0045_event)

# layers ###########################################################################################
ctb0045_layer <- google_sheet(ctb0045_ids$gs_id, ctb0045_ids$gid_layer)
str(ctb0045_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0045_layer, old = "ID do evento", new = "observacao_id")
ctb0045_layer[, observacao_id := as.character(observacao_id)]
ctb0045_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
# The authors originally sampled only A horizons. However, we noticed that in the original
# spreadsheet, the upper limit of the sampled layers started at 5 cm depth. The document describes
# that the authors removed the litter layer before sampling, but it does not mention how thick it
# was. Therefore, we assume that the litter layer was approximately 5 cm thick, and we added a new
# "Oo" horizon from 0 to 5 cm depth for all soil profiles. This still has to be checked with the
# authors.
data.table::setnames(ctb0045_layer, old = "ID da camada", new = "camada_nome")
ctb0045_layer[, camada_nome := as.character(camada_nome)]
ctb0045_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0045_layer[, amostra_id := NA_real_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0045_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0045_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0045_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0045_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0045_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0045_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0045_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0045_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0045_layer[, profund_inf])

# Check for duplicated layers
check_duplicated_layer(ctb0045_layer)

# Check for layers with equal top and bottom depths
check_equal_depths(ctb0045_layer)

# Check for negative layer depths
check_depth_inversion(ctb0045_layer)

# camada_id
# We will create a unique identifier for each layer.
ctb0045_layer <- ctb0045_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0045_layer[, camada_id := 1:.N, by = observacao_id]
ctb0045_layer[, .N, by = camada_id]

# mid_depth
ctb0045_layer[, mid_depth := (profund_sup + profund_inf) / 2]
summary(ctb0045_layer[, mid_depth])

# Check for missing layers
# There are no missing layers in this dataset.
check_missing_layer(ctb0045_layer)

# terrafina
# old: Terra fina [%]
# new: terrafina
data.table::setnames(ctb0045_layer, old = "Terra fina [%]", new = "terrafina")
ctb0045_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0045_layer[, terrafina])
# All layers are missing the content of fine earth (terrafina).

# areia
# old: Areia [%]
# new: areia
data.table::setnames(ctb0045_layer, old = "Areia [%]", new = "areia")
ctb0045_layer[, areia := as.numeric(areia) * 10]
summary(ctb0045_layer[, areia])
# There are 73 layers with missing "areia" values.
check_empty_layer(ctb0045_layer, "areia")
# All of them are Oo horizons.

# silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0045_layer, old = "Silte [%]", new = "silte")
ctb0045_layer[, silte := as.numeric(silte)*10]
summary(ctb0045_layer[, silte])
# There are 73 layers with missing "silte" values. All of them are Oo horizons.

# argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0045_layer, old = "Argila [%]", new = "argila")
ctb0045_layer[, argila := as.numeric(argila) * 10]
summary(ctb0045_layer[, argila])
# There are 73 layers with missing "argila" values. All of them are Oo horizons.

# Check the particle size distribution
# Round the values to avoid floating point issues
ctb0045_layer[, areia := round(areia)]
ctb0045_layer[, silte := round(silte)]
ctb0045_layer[, argila := round(argila)]
# The sum of argila, silte and areia should be 1000 g/kg
ctb0045_layer[, psd := rowSums(.SD, na.rm = TRUE), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
# Check the limits
ctb0045_layer[!psd %in% psd_lims & !is.na(psd), ..cols]
# 73 layers have a sum of the particle size distribution outside the limits. All of them are Oo
# horizons.
# Print the rows with psd != 1000
ctb0045_layer[!psd %in% psd_lims & !is.na(psd), ..cols]
# Again, all of them are Oo horizons.
# Remove the psd column
ctb0045_layer[, psd := NULL]

# carbono
# old: Carbono Orgânico [g/Kg]
# new: carbono
data.table::setnames(ctb0045_layer, old = "Carbono Orgânico [g/Kg]", new = "carbono")
ctb0045_layer[, carbono := as.numeric(carbono)]
summary(ctb0045_layer[, carbono])
# There are 73 layers with missing "carbono" values. All of them are Oo horizons.

# ctc
# old: T ou CTC [cmol/dm3]
# new: ctc
data.table::setnames(ctb0045_layer, old = "T ou CTC [cmol/dm3]", new = "ctc")
ctb0045_layer[, ctc := as.numeric(ctc)]
summary(ctb0045_layer[, ctc])
# There are 73 layers with missing "ctc" values. All of them are Oo horizons.

# ph
# old: pH
# new: ph
data.table::setnames(ctb0045_layer, old = "pH", new = "ph")
ctb0045_layer[, ph := as.numeric(ph)]
summary(ctb0045_layer[, ph])
# There are 73 layers with missing "ph" values. All of them are Oo horizons.

# dsi 
# Soil bulk density (dsi) is missing in this document.
ctb0045_layer[, dsi := NA_real_]

str(ctb0045_layer)

# Merge ############################################################################################
# events and layers
ctb0045 <- merge(ctb0045_event, ctb0045_layer, all = TRUE)
ctb0045[, dataset_id := "ctb0045"]

# citation
ctb0045 <- merge(ctb0045, ctb0045_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0045)
# Layers: 146
# Events: 73
# Georeferenced events: 73

# Plot using mapview
if (interactive()) {
  ctb0045_sf <- sf::st_as_sf(
    ctb0045[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0045_sf["argila"])
}

# Write to disk ####################################################################################
ctb0045 <- select_output_columns(ctb0045)
data.table::fwrite(ctb0045, "ctb0045/ctb0045.csv")
data.table::fwrite(ctb0045_event, "ctb0045/ctb0045_event.csv")
data.table::fwrite(ctb0045_layer, "ctb0045/ctb0045_layer.csv")
