# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0102
# Dados de "Solos do Oeste do Paraná"
# 
# Google Drive: https://drive.google.com/drive/u/0/folders/1LyFwLmAtNVcGInJksqG0IKKzXvhhrqoV
# NotebookLM: https://notebooklm.google.com/notebook/39204d76-b4d2-4007-a110-7de7830385da
ctb0102_ids <- soildata_catalog("ctb0102")

# validation #######################################################################################
ctb0102_validation <- google_sheet(ctb0102_ids$gs_id, ctb0102_ids$gid_validation)
check_sheet_validation(ctb0102_validation)

# citation #########################################################################################
ctb0102_citation <- google_sheet(ctb0102_ids$gs_id, ctb0102_ids$gid_citation)
str(ctb0102_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0102_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0102_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0102_citation <- data.table::data.table(
  dataset_id = "ctb0102",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0102_citation)

# event #####################################################################################
ctb0102_event <- google_sheet(ctb0102_ids$gs_id, ctb0102_ids$gid_event)
str(ctb0102_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0102_event, old = "ID do evento", new = "observacao_id")
ctb0102_event[, observacao_id := as.character(observacao_id)]
# Check for duplicate observacao_id
any(table(ctb0102_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0102_event, old = "Ano (coleta)", new = "data_ano")
ctb0102_event[, data_ano := as.integer(data_ano)]
ctb0102_event[, .N, by = data_ano]

# ano_fonte
ctb0102_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0102_event[, .N, by = ano_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0102_event, old = "Longitude", new = "coord_x")
# We use gsub to switch, because in the spreadsheets we have everything in ,
ctb0102_event[, coord_x := gsub(",", ".", coord_x, fixed = TRUE)]
ctb0102_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0102_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0102_event, old = "Latitude", new = "coord_y")
# We use gsub to switch, because in the spreadsheets we have everything in ,
ctb0102_event[, coord_y := gsub(",", ".", coord_y, fixed = TRUE)]
ctb0102_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0102_event[, coord_y])

# Check for duplicate coordinates
check_equal_coordinates(ctb0102_event)

# Datum (coord) -> coord_datum
# The document does not specify the datum used for the coordinates. However, given the year of data
# collection (2013) and the common practices in Brazil, it is reasonable to assume that the WGS84
# datum was used. Therefore, we set coord_datum to 4326 (EPSG code for WGS84).
data.table::setnames(ctb0102_event, old = "Datum (coord)", new = "coord_datum")
ctb0102_event[, coord_datum := as.character(coord_datum)]
ctb0102_event[, .N, by = coord_datum]
ctb0102_event[is.na(coord_datum), coord_datum := 4326]
ctb0102_event[, coord_datum := as.integer(coord_datum)]
ctb0102_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
# The document does not provide specific information about the source of the coordinates. However,
# the sources do include highly specific location data for every profile studied, which strongly
# implies the use of a precise geographic measurement tool, such as a GPS device.
data.table::setnames(ctb0102_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0102_event[, coord_fonte := as.character(coord_fonte)]
ctb0102_event[is.na(coord_fonte), coord_fonte := "GPS device"]
ctb0102_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The document does not provide specific information about the precision of the coordinates. However,
# since the coordinates were likely obtained using a GPS device, we assume a precision of 10 meters.
data.table::setnames(ctb0102_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0102_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0102_event[is.na(coord_precisao), coord_precisao := 10]
summary(ctb0102_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0102_event, old = "País", new = "pais_id")
ctb0102_event[, pais_id := as.character(pais_id)]
ctb0102_event[, .N, by = pais_id]

# Estado -> estado_id
data.table::setnames(ctb0102_event, old = "Estado (UF)", new = "estado_id")
ctb0102_event[, estado_id := as.character(estado_id)]
ctb0102_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0102_event, old = "Município", new = "municipio_id")
ctb0102_event[, municipio_id := as.character(municipio_id)]
ctb0102_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0102_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0102_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0102_event[, amostra_area])

# SiBCS (2013) -> taxon_sibcs
data.table::setnames(ctb0102_event, old = "SiBCS (2013)", new = "taxon_sibcs")
ctb0102_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0102_event[, .N, by = taxon_sibcs]

# taxon_st
# missing this soil taxonomy on document
ctb0102_event[, taxon_st := NA_character_]
ctb0102_event[, .N, by = taxon_st]

# pedregosidade
# old: Pedregosidade
# new: pedregosidade
data.table::setnames(ctb0102_event, old = "Pedregosidade", new = "pedregosidade")
ctb0102_event[, pedregosidade := as.character(pedregosidade)]
ctb0102_event[, .N, by = pedregosidade]

# rochosidade
# old: Rochosidade
# new: rochosidade
data.table::setnames(ctb0102_event, old="Rochosidade", new = "rochosidade")
ctb0102_event[, rochosidade := as.character(rochosidade)]
ctb0102_event[, .N, by = rochosidade]

str(ctb0102_event)

# layers ###########################################################################################
ctb0102_layer <- google_sheet(ctb0102_ids$gs_id, ctb0102_ids$gid_layer)
str(ctb0102_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0102_layer, old = "ID do evento", new = "observacao_id")
ctb0102_layer[, observacao_id := as.character(observacao_id)]
ctb0102_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0102_layer, old = "ID da camada", new = "camada_nome")
ctb0102_layer[, camada_nome := as.character(camada_nome)]
ctb0102_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0102_layer[, amostra_id := NA_real_]

# profund_sup
# old: Profundidade Inicial [cm]
# new: profund_sup
data.table::setnames(ctb0102_layer, old = "Profundidade Inicial [cm]", new = "profund_sup")
ctb0102_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0102_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0102_layer[, profund_sup])

# profund_inf
# old: Profundidade Final [cm]
# new: profund_inf
data.table::setnames(ctb0102_layer, old = "Profundidade Final [cm]", new = "profund_inf")
ctb0102_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0102_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0102_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0102_layer[, profund_inf])

# Check for duplicated layers
check_duplicated_layer(ctb0102_layer)

# Check for layers with equal top and bottom depths
check_equal_depths(ctb0102_layer)

# Check for negative layer depths
check_depth_inversion(ctb0102_layer)

# camada_id
# We will create a unique identifier for each layer.
ctb0102_layer <- ctb0102_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0102_layer[, camada_id := 1:.N, by = observacao_id]
ctb0102_layer[, .N, by = camada_id]

# profund_mid
ctb0102_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0102_layer[, profund_mid])

# Check for missing layers
# There are no missing layers in this dataset.
check_missing_layer(ctb0102_layer)

# terrafina
# old: "Terra Fina (< 2 mm) [%]"
# new: terrafina
data.table::setnames(ctb0102_layer, old= "Terra Fina (< 2 mm) [%]", new = "terrafina")
ctb0102_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0102_layer[, terrafina])
# There are four layers missing "terrafina" values. These are B/C and C horizons.
check_empty_layer(ctb0102_layer, "terrafina")

# This work has Coarse Sand and Fine Sand
# areia grossa
# old: Areia Grossa (2 - 0,20 mm) [%]
# new: areia_grossa
data.table::setnames(ctb0102_layer, old = "Areia Grossa (2 - 0,20 mm) [%]", new = "areia_grossa")
ctb0102_layer[, areia_grossa := as.numeric(areia_grossa) * 10]
summary(ctb0102_layer[, areia_grossa])
# There are two layers missing "areia_grossa" values. These are C horizons.
check_empty_layer(ctb0102_layer, "areia_grossa")

# areia fina
# old: Areia Fina (0,20 - 0,05 mm) [%]
# new: areia_fina
data.table::setnames(ctb0102_layer, old = "Areia Fina (0,20 - 0,05 mm) [%]", new = "areia_fina")
ctb0102_layer[, areia_fina := as.numeric(areia_fina) * 10]
summary(ctb0102_layer[, areia_fina])
# There are two layers missing "areia_fina" values. These are C horizons.
check_empty_layer(ctb0102_layer, "areia_fina")

# areia
ctb0102_layer[, areia := areia_grossa + areia_fina]
summary(ctb0102_layer[, areia])
# There are two layers missing "areia" values. These are C horizons.
check_empty_layer(ctb0102_layer, "areia")
# Fill empty layers
ctb0102_layer[,
  areia := fill_empty_layer(y = areia, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty areia values
check_empty_layer(ctb0102_layer, "areia")
# All missing areia values have been filled.

# silte
# old: Silte (0,05 - 0,002 Mm) [%]
# new: silte
data.table::setnames(ctb0102_layer, old = "Silte (0,05 - 0,002 Mm) [%]", new = "silte")
ctb0102_layer[, silte := as.numeric(silte) * 10]
summary(ctb0102_layer[, silte])
# There are two layers missing "silte" values. These are C horizons.
check_empty_layer(ctb0102_layer, "silte")
# Fill empty layers
ctb0102_layer[,
  silte := fill_empty_layer(y = silte, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty silte values
check_empty_layer(ctb0102_layer, "silte")
# All missing silte values have been filled.

# argila
# old: Argila (< 0,002 mm) [%]
# new: argila
data.table::setnames(ctb0102_layer, old = "Argila (< 0,002 mm) [%]", new = "argila")
ctb0102_layer[, argila := as.numeric(argila) * 10]
summary(ctb0102_layer[, argila])
# There are two layers missing "argila" values. These are C horizons.
check_empty_layer(ctb0102_layer, "argila")
# Fill empty layers
ctb0102_layer[,
  argila := fill_empty_layer(y = argila, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty argila values
check_empty_layer(ctb0102_layer, "argila")
# All missing argila values have been filled.

# Check the particle size distribution
# Round argila, silte and areia
ctb0102_layer[, argila := round(argila)]
ctb0102_layer[, silte := round(silte)]
ctb0102_layer[, areia := round(areia)]
# The sum of argila, silte and areia should be 1000 g/kg
ctb0102_layer[, psd := rowSums(.SD, na.rm = TRUE), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0102_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0102_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C (orgânico) [%]
# new: carbono
# multiply by 10 to convert to g/kg
data.table::setnames(ctb0102_layer, old = "C (orgânico) [%]", new = "carbono")
ctb0102_layer[, carbono := as.numeric(carbono)]
ctb0102_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0102_layer[, carbono])

# ctc
# old: Valor T (CTC) [cmolc/dm3]
# new: ctc
data.table::setnames(ctb0102_layer, old = "Valor T (CTC) [cmolc/dm3]", new = "ctc")
ctb0102_layer[, ctc := as.numeric(ctc)]
summary(ctb0102_layer[, ctc])
check_empty_layer(ctb0102_layer, "ctc")

# ph
# old: pH (H2O - 1:2,5)
# new: ph
data.table::setnames(ctb0102_layer, old = "pH (H2O - 1:2,5)", new = "ph")
ctb0102_layer[, ph := as.numeric(ph)]
summary(ctb0102_layer[, ph])
check_empty_layer(ctb0102_layer, "ph")

# dsi 
# no dsi information in this work
ctb0102_layer[, dsi := NA_real_]
summary(ctb0102_layer[, dsi])



str(ctb0102_layer)

# Merge ############################################################################################
# events and layers
ctb0102 <- merge(ctb0102_event, ctb0102_layer, all = TRUE)
ctb0102[, dataset_id := "ctb0102"]
# citation
ctb0102 <- merge(ctb0102, ctb0102_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0102)

#Layers: 152
#Events: 30
#Georeferenced events: 30


# Plot using mapview
if (TRUE) {
  ctb0102_sf <- sf::st_as_sf(
    ctb0102[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0102_sf["argila"])
}

# Write to disk ####################################################################################
ctb0102 <- select_output_columns(ctb0102)
data.table::fwrite(ctb0102, "ctb0102/ctb0102.csv")
data.table::fwrite(ctb0102_event, "ctb0102/ctb0102_event.csv")
data.table::fwrite(ctb0102_layer, "ctb0102/ctb0102_layer.csv")
