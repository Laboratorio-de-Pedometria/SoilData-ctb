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
# ctb0074
# Dados de "Levantamento de reconhecimento de solos de alta intensidade do alto curso do Rio Descoberto, DF/GO, escala 1:100.000"
# 
# Google Drive: https://drive.google.com/drive/folders/18VRa4FVZ7XlZi9CPORvpIL80ABecObt2
# 
ctb0074_ids <- soildata_catalog("ctb0074")

# validation #####################################################################################
ctb0074_validation <- google_sheet(ctb0074_ids$gs_id, ctb0074_ids$gid_validation)
check_sheet_validation(ctb0074_validation)

# citation #####################################################################################
ctb0074_citation <- google_sheet(ctb0074_ids$gs_id, ctb0074_ids$gid_citation)
str(ctb0074_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0074_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0074_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0074_citation <- data.table::data.table(
  dataset_id = "ctb0074",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0074_citation)

# event ############################################################################################
ctb0074_event <- google_sheet(ctb0074_ids$gs_id, ctb0074_ids$gid_event)
str(ctb0074_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0074_event, old = "ID do evento", new = "observacao_id")
ctb0074_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
any(table(ctb0074_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0074_event, old = "Ano (coleta)", new = "data_ano")
ctb0074_event[, data_ano := NA_character_]
ctb0074_event[, .N, by = data_ano]

# ano_fonte
ctb0074_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0074_event[, .N, by = ano_fonte]

# Longitude -> coord_x
ctb0074_event[, coord_x := NA_character_]
summary(ctb0074_event[, coord_x])
# UTM coordinates

# Latitude -> coord_y
ctb0074_event[, coord_y := NA_character_]
summary(ctb0074_event[, coord_y])
# UTM coordinates

# Datum (coord) -> coord_datum
data.table::setnames(ctb0074_event, old = "Datum (coord)", new = "coord_datum")
ctb0074_event[, coord_datum := NA_character_]
ctb0074_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0074_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0074_event[, coord_fonte := NA_character_]
ctb0074_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
data.table::setnames(ctb0074_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0074_event[, coord_precisao := NA_character_]

# País -> pais_id
data.table::setnames(ctb0074_event, old = "País", new = "pais_id")
ctb0074_event[, pais_id := as.character(pais_id)]
ctb0074_event[, .N, by = pais_id]

# Estado -> estado_id
data.table::setnames(ctb0074_event, old = "Estado (UF)", new = "estado_id")
ctb0074_event[, estado_id := as.character(estado_id)]
ctb0074_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0074_event, old = "Município", new = "municipio_id")
ctb0074_event[, municipio_id := NA_character_]
ctb0074_event[, .N, by = municipio_id]

# Área do evento [m2] -> amostra_area
data.table::setnames(ctb0074_event, old = "Área do evento [m2]", new = "amostra_area")
ctb0074_event[, amostra_area := NA_character_]
summary(ctb0074_event[, amostra_area])

# SiBCS (1999) -> taxon_sibcs
data.table::setnames(ctb0074_event, old = "SiBCS (1999)", new = "taxon_sibcs")
ctb0074_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0074_event[, .N, by = taxon_sibcs]

# taxon_st 
# missing this soil taxonomy on document
ctb0074_event[, taxon_st := NA_character_]

# pedregosidade
# This study does not have qualitative information about stoniness.
ctb0074_event[, pedregosidade := NA_character_]

# rochosidade
# This study does not have qualitative information about rockiness.
ctb0074_event[, rochosidade := NA_character_]

# cobertura
# Concatenates one or more source columns (e.g. situacao, uso_atual, cobertura) into a single
# field. Adjust the vector below with the names of the already-renamed source columns.
ctb0074_event[, cobertura := NA_character_] 
cobertura_cols <- c("cobertura")
concat_columns(ctb0074_event, target = "cobertura", sources = cobertura_cols)

#vegetacao
ctb0074_event[, vegetacao := NA_character_]
ctb0074_event[, .N, by = vegetacao]

# erosao
ctb0074_event[, erosao := NA_character_]
erosao_cols <- c("erosao")
concat_columns(ctb0074_event, target = "erosao", sources = erosao_cols)
ctb0074_event[, .N, by = erosao]



str(ctb0074_event)

# layers ###########################################################################################
ctb0074_layer <- google_sheet(ctb0074_ids$gs_id, ctb0074_ids$gid_layer)
str(ctb0074_layer)

# Process fields

# This study analyzed both the soil matrix and the nodules (plintites and petroplintites)
# separately. This is represented in the "Material" column. Our target is the soil matrix
# only. Therefore, we will filter the dataset to keep only the layers where "Material" is
# "matriz".
ctb0074_layer <- ctb0074_layer[Material == "matriz"]

# ID do evento -> observacao_id
data.table::setnames(ctb0074_layer, old = "ID do evento", new = "observacao_id")
ctb0074_layer[, observacao_id := as.character(observacao_id)]
ctb0074_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0074_layer, old = "ID da camada", new = "camada_nome")
ctb0074_layer[, camada_nome := as.character(camada_nome)]
ctb0074_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# Soil sample ID is not informed in this document.
ctb0074_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0074_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0074_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0074_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0074_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0074_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0074_layer[, profund_inf := depth_slash(profund_inf), by = .I]
# Resolve censored layer depth (plus)
ctb0074_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0074_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0074_layer[, profund_inf])


# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile. Order by observacao_id and mid_depth.
ctb0074_layer[, mid_depth := (profund_sup + profund_inf) / 2]
ctb0074_layer <- ctb0074_layer[order(observacao_id, mid_depth)]
ctb0074_layer[, camada_id := 1:.N, by = observacao_id]
ctb0074_layer[, .N, by = camada_id]

# Check for duplicated layers
check_repeated_layer(ctb0074_layer)

# Check for missing layers
check_missing_layer(ctb0074_layer)

# terrafina
# The source document mentions that the coarse fraction was determined, but does not provide the data.
# Therefore, we will set the terrafina (fine earth fraction) as NA.
ctb0074_layer[, terrafina := NA_real_]

# areia
# old: Areia [g/kg]
# new: areia
data.table::setnames(ctb0074_layer, old = "Silte [g/kg]", new = "areia")
ctb0074_layer[, areia := as.numeric(areia)]
summary(ctb0074_layer[, areia])
check_empty_layer(ctb0074_layer, "areia")

# silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0074_layer, old = "Silte [g/kg]", new = "silte")
ctb0074_layer[, silte := as.numeric(silte)]
summary(ctb0074_layer[, silte])
check_empty_layer(ctb0074_layer, "silte")

# argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0074_layer, old = "Argila [g/kg]", new = "argila")
ctb0074_layer[, argila := as.numeric(argila)]
summary(ctb0074_layer[, argila])
check_empty_layer(ctb0074_layer, "argila")

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0074_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0074_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0074_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C [g/kg]
# new: carbono
data.table::setnames(ctb0074_layer, old = "C [g/kg]", new = "carbono")
ctb0074_layer[, carbono := as.numeric(carbono)]
summary(ctb0074_layer[, carbono])
check_empty_layer(ctb0074_layer, "carbono")

# ctc
# old: T [cmolc/kg]
# new: ctc
data.table::setnames(ctb0074_layer, old = "T [cmolc/kg]", new = "ctc")
ctb0074_layer[, ctc := as.numeric(ctc)]
summary(ctb0074_layer[, ctc])
check_empty_layer(ctb0074_layer, "ctc")

# ph
# old: pH (H_2O)
# new: ph
data.table::setnames(ctb0074_layer, old = "pH (H_2O)", new = "ph")
ctb0074_layer[, ph := as.numeric(ph)]
summary(ctb0074_layer[, ph])
check_empty_layer(ctb0074_layer, "ph")

# dsi 
# The soil density is not informed in this document.
ctb0074_layer[, dsi := NA_real_]
summary(ctb0074_layer[, dsi])

str(ctb0074_layer)

# Merge ############################################################################################
# events and layers
ctb0074 <- merge(ctb0074_event, ctb0074_layer, all = TRUE)
ctb0074[, dataset_id := "ctb0074"]

# citation
ctb0074 <- merge(ctb0074, ctb0074_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0074)
# Layers: 51
# Events: 8
# Georeferenced events: 8

# Plot using mapview
if (FALSE) {
  ctb0074_sf <- sf::st_as_sf(
    ctb0074[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0074_sf["argila"])
}

# Write to disk ####################################################################################
ctb0074 <- select_output_columns(ctb0074)
data.table::fwrite(ctb0074, "ctb0074/ctb0074.csv")
