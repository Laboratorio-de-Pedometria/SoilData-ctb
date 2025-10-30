# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0104
# Dados de "ALTERAÇÕES DE PROPRIEDADES FÍSICO-HÍDRICAS EM SOLOS ARENOSOS DO BIOMA PAMPA SOB USOS
# DISTINTOS"
# 
# Google Drive: https://drive.google.com/drive/u/0/folders/1OpI7ElGbEN1gfRieg2JwYfcN-5Nezu_x
# NotebookLM: https://notebooklm.google.com/notebook/e937b687-f6eb-4929-b6aa-a403bed44f2c
ctb0104_ids <- soildata_catalog("ctb0104")

# validation #####################################################################################
ctb0104_validation <- google_sheet(ctb0104_ids$gs_id, ctb0104_ids$gid_validation)
check_sheet_validation(ctb0104_validation)

# citation #####################################################################################
ctb0104_citation <- google_sheet(ctb0104_ids$gs_id, ctb0104_ids$gid_citation)
str(ctb0104_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0104_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0104_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0104_citation <- data.table::data.table(
  dataset_id = "ctb0104",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0104_citation)

# event #####################################################################################
ctb0104_event <- google_sheet(ctb0104_ids$gs_id, ctb0104_ids$gid_event)
str(ctb0104_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0104_event, old = "ID do evento", new = "observacao_id")
ctb0104_event[, observacao_id := as.character(observacao_id)]
# Check for duplicate observacao_id
any(table(ctb0104_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0104_event, old = "Ano (coleta)", new = "data_ano")
ctb0104_event[, data_ano := as.integer(data_ano)]
ctb0104_event[, .N, by = data_ano]

# ano_fonte
ctb0104_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0104_event[, .N, by = ano_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0104_event, old = "Longitude", new = "coord_x")
ctb0104_event[, coord_x := as.numeric(coord_x)]
summary(ctb0104_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0104_event, old = "Latitude", new = "coord_y")
ctb0104_event[, coord_y := as.numeric(coord_y)]
summary(ctb0104_event[, coord_y])

# Check for duplicate coordinates
check_equal_coordinates(ctb0104_event)

# DATUM -> coord_datum
data.table::setnames(ctb0104_event, old = "Datum (coord)", new = "coord_datum")
ctb0104_event[, coord_datum := as.character(coord_datum)]
ctb0104_event[coord_datum == "WGS84", coord_datum := 4326]
ctb0104_event[, coord_datum := as.integer(coord_datum)]
ctb0104_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0104_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0104_event[, coord_fonte := as.character(coord_fonte)]
ctb0104_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The coordinates were estimated by our team using Google Maps and utilizing the images available in
# the original document. So we will set a precision of approximately 100 meters.
data.table::setnames(ctb0104_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0104_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0104_event[is.na(coord_precisao), coord_precisao := 100]
summary(ctb0104_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0104_event, old = "País", new = "pais_id")
ctb0104_event[, pais_id := as.character(pais_id)]
ctb0104_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0104_event, old = "Estado (UF)", new = "estado_id")
ctb0104_event[, estado_id := as.character(estado_id)]
ctb0104_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0104_event, old = "Município", new = "municipio_id")
ctb0104_event[, municipio_id := as.character(municipio_id)]
ctb0104_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0104_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0104_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0104_event[, amostra_area])

# taxon_sibcs
# old: Classificação do Solo
# new: taxon_sibcs
# Soil classification is missing in this document. However, based on the detailed descriptions of
# the soil characteristics provided in the sources, it is highly probable that the soils studied
# are Arenosols, which correspond to the classification Neossolo Quartzarênico in the Brazilian
# System of Soil Classification (SiBCS).
data.table::setnames(ctb0104_event, old = "Classificação do Solo", new = "taxon_sibcs")
ctb0104_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0104_event[is.na(taxon_sibcs), taxon_sibcs := "Neossolo Quartzarênico"]
ctb0104_event[, .N, by = taxon_sibcs]

# taxon_st 
# missing this soil taxonomy on document
ctb0104_event[, taxon_st := NA_character_]
ctb0104_event[, .N, by = taxon_st]

# pedregosidade
# The stoniness information is missing in this document. However, based on the detailed descriptions
# of the soil characteristics provided in the sources, it is highly probable that the soils studied
# have no stoniness.
ctb0104_event[, pedregosidade := "Sem Pedregosidade"]
ctb0104_event[, .N, by = pedregosidade]

# rochosidade
# The rockiness information is missing in this document. However, based on the detailed descriptions
# of the soil characteristics provided in the sources, it is highly probable that the soils studied
# have no rockiness.
ctb0104_event[, rochosidade := "Sem Rochosidade"]
ctb0104_event[, .N, by = rochosidade]

str(ctb0104_event)

# layers ###########################################################################################
ctb0104_layer <- google_sheet(ctb0104_ids$gs_id, ctb0104_ids$gid_layer)
str(ctb0104_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0104_layer, old = "ID do evento", new = "observacao_id")
ctb0104_layer[, observacao_id := as.character(observacao_id)]
ctb0104_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0104_layer, old = "ID da camada", new = "camada_nome")
ctb0104_layer[, camada_nome := as.character(camada_nome)]
ctb0104_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0104_layer[, amostra_id := NA_real_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0104_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0104_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0104_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0104_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0104_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0104_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0104_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0104_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0104_layer[, profund_inf])

# Check for duplicated layers
check_duplicated_layer(ctb0104_layer)

# Check for layers with equal top and bottom depths
check_equal_depths(ctb0104_layer)

# Check for negative layer depths
check_depth_inversion(ctb0104_layer)

# camada_id
# We will create a unique identifier for each layer.
ctb0104_layer <- ctb0104_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0104_layer[, camada_id := 1:.N, by = observacao_id]
ctb0104_layer[, .N, by = camada_id]

# mid_depth
ctb0104_layer[, mid_depth := (profund_sup + profund_inf) / 2]
summary(ctb0104_layer[, mid_depth])

# Check for missing layers
# There are no missing layers in this dataset.
check_missing_layer(ctb0104_layer)

# terrafina
# old: Terra fina [g/kg]
# new: terrafina
# The original document does not report the proportion of coarse fragments or the exact proportion
# of the fine earth in relation to the whole soil. However, based on the detailed descriptions of
# the soil characteristics provided in the sources, it is highly probable that the soils studied
# are sandy soils with a very low high content of fine particles.
data.table::setnames(ctb0104_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0104_layer[, terrafina := as.numeric(terrafina)]
ctb0104_layer[is.na(terrafina), terrafina := 1000]
summary(ctb0104_layer[, terrafina])

# areia
# old: Areia total [g/kg]
# new: areia
data.table::setnames(ctb0104_layer, old = "Areia total [g/kg]", new = "areia")
ctb0104_layer[, areia := as.numeric(areia)]
summary(ctb0104_layer[, areia])

# silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0104_layer, old = "Silte [g/kg]", new = "silte")
ctb0104_layer[, silte := as.numeric(silte)]
summary(ctb0104_layer[, silte])

# argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0104_layer, old = "Argila [g/kg]", new = "argila")
ctb0104_layer[, argila := as.numeric(argila)]
summary(ctb0104_layer[, argila])



# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile. Order by observacao_id and mid_depth.
ctb0104_layer[, mid_depth := (profund_sup + profund_inf) / 2]
ctb0104_layer <- ctb0104_layer[order(observacao_id, mid_depth)]
ctb0104_layer[, camada_id := 1:.N, by = observacao_id]
ctb0104_layer[, .N, by = camada_id]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0104_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0104_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0104_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
#missing in this document.
ctb0104_layer[, carbono := NA_real_]

# ctc
#missing in this document.
ctb0104_layer[, ctc := NA_real_]

# ph
#missing in this document.
ctb0104_layer[, ph := NA_real_]

# dsi 
#missing in this document.
ctb0104_layer[, dsi := NA_real_]




str(ctb0104_layer)

# Merge ############################################################################################
# events and layers
ctb0104 <- merge(ctb0104_event, ctb0104_layer, all = TRUE)
ctb0104[, dataset_id := "ctb0104"]
# citation
ctb0104 <- merge(ctb0104, ctb0104_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0104)

#Layers: 36
#Events: 9
#Georeferenced events: 9 


# Plot using mapview
if (TRUE) {
  ctb0104_sf <- sf::st_as_sf(
    ctb0104[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0104_sf["carbono"])
}

# Write to disk ####################################################################################
ctb0104 <- select_output_columns(ctb0104)
data.table::fwrite(ctb0104, "ctb0104/ctb0104.csv")
data.table::fwrite(ctb0104_event, "ctb0104/ctb0104_event.csv")
data.table::fwrite(ctb0104_layer, "ctb0104/ctb0104_layer.csv")
