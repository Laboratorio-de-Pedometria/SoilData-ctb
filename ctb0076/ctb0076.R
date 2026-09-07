# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0076
# 	
#	Título	Dados de "Caracterização e classificação de terras pretas arqueológicas na Região do Médio Rio Madeira"
# 
# Google Drive: https://drive.google.com/drive/folders/1_E8LRe6eDz6yIq2GgKLHGyx_Mkq7r-u-?usp=drive_link

ctb0076_ids <- soildata_catalog("ctb0076")

# validation #######################################################################################
ctb0076_validation <- google_sheet(ctb0076_ids$gs_id, ctb0076_ids$gid_validation)
check_sheet_validation(ctb0076_validation)

# citation #########################################################################################
ctb0076_citation <- google_sheet(ctb0076_ids$gs_id, ctb0076_ids$gid_citation)
str(ctb0076_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0076_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0076_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0076_citation <- data.table::data.table(
  dataset_id = "ctb0076",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0076_citation)

# event #####################################################################################
ctb0076_event <- google_sheet(ctb0076_ids$gs_id, ctb0076_ids$gid_event)
str(ctb0076_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0076_event, old = "ID do evento", new = "observacao_id")
ctb0076_event[, observacao_id := as.character(observacao_id)]
# Check for duplicate observacao_id
any(table(ctb0076_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0076_event, old = "Ano (coleta)", new = "data_ano")
ctb0076_event[, data_ano := NA_character_]
ctb0076_event[, .N, by = data_ano]

# ano_fonte
ctb0076_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0076_event[, .N, by = ano_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0076_event, old = "Longitude", new = "coord_x")
ctb0076_event[, coord_x := NA_character_]
summary(ctb0076_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0076_event, old = "Latitude", new = "coord_y")
ctb0076_event[, coord_y := NA_character_]
summary(ctb0076_event[, coord_y])

# Check for duplicate coordinates
check_equal_coordinates(ctb0076_event)

# Datum (coord) -> coord_datum
data.table::setnames(ctb0076_event, old = "Datum (coord)", new = "coord_datum")
ctb0076_event[, coord_datum := NA_character_]
ctb0076_event[, .N, by = coord_datum]


# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0076_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0076_event[, coord_fonte := NA_character_]
ctb0076_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
data.table::setnames(ctb0076_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0076_event[, coord_precisao := NA_character_]
summary(ctb0076_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0076_event, old = "País", new = "pais_id")
ctb0076_event[, pais_id := as.character(pais_id)]
ctb0076_event[, .N, by = pais_id]

# Estado -> estado_id
data.table::setnames(ctb0076_event, old = "Estado (UF)", new = "estado_id")
ctb0076_event[, estado_id := as.character(estado_id)]
ctb0076_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0076_event, old = "Município", new = "municipio_id")
ctb0076_event[, municipio_id := as.character(municipio_id)]
ctb0076_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0076_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0076_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0076_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0076_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0076_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0076_event[, .N, by = taxon_sibcs]

# taxon_st
# missing this soil taxonomy on document
ctb0076_event[, taxon_st := NA_character_]
ctb0076_event[, .N, by = taxon_st]

# pedregosidade
ctb0076_event[, pedregosidade := NA_character_]
ctb0076_event[, .N, by = pedregosidade]

# rochosidade
ctb0076_event[, rochosidade := NA_character_]
ctb0076_event[, .N, by = rochosidade]

# cobertura
# Concatenates one or more source columns (e.g. situacao, uso_atual, cobertura) into a single
# field. Adjust the vector below with the names of the already-renamed source columns.
data.table::setnames(ctb0076_event, old = "Uso e cbertura da terra", new = "uso_atual")
cobertura_cols <- c("uso_atual")
concat_columns(ctb0076_event, target = "cobertura", sources = cobertura_cols)

#vegetacao
ctb0076_event[, vegetacao := NA_character_]
ctb0076_event[, .N, by = vegetacao]

# erosao
ctb0076_event[, erosao := NA_character_]
ctb0076_event[, .N, by = erosao]

str(ctb0076_event)

# layers ###########################################################################################
ctb0076_layer <- google_sheet(ctb0076_ids$gs_id, ctb0076_ids$gid_layer)
str(ctb0076_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0076_layer, old = "ID do evento", new = "observacao_id")
ctb0076_layer[, observacao_id := as.character(observacao_id)]
ctb0076_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0076_layer, old = "ID da camada", new = "camada_nome")
ctb0076_layer[, camada_nome := as.character(camada_nome)]
ctb0076_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0076_layer[, amostra_id := NA_real_]

# profund_sup
# old: Profundidade Inicial [cm]
# new: profund_sup
data.table::setnames(ctb0076_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0076_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0076_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0076_layer[, profund_sup])

# profund_inf
# old: Profundidade Final [cm]
# new: profund_inf
data.table::setnames(ctb0076_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0076_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0076_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0076_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0076_layer[, profund_inf])

# Check for duplicated layers
check_duplicated_layer(ctb0076_layer)

# Check for layers with equal top and bottom depths
check_equal_depths(ctb0076_layer)

# Check for negative layer depths
check_depth_inversion(ctb0076_layer)

# camada_id
# We will create a unique identifier for each layer.
ctb0076_layer <- ctb0076_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0076_layer[, camada_id := 1:.N, by = observacao_id]
ctb0076_layer[, .N, by = camada_id]

# profund_mid
ctb0076_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0076_layer[, profund_mid])

# Check for missing layers
# There are no missing layers in this dataset.
check_missing_layer(ctb0076_layer)

# terrafina
ctb0076_layer[, terrafina := NA_character_]
summary(ctb0076_layer[, terrafina])
check_empty_layer(ctb0076_layer, "terrafina")

# This work has Coarse Sand and Fine Sand
# areia grossa
# old: Areia grossa [g/kg]
# new: areia_grossa
data.table::setnames(ctb0076_layer, old = "Areia grossa [g/kg]", new = "areia_grossa")
ctb0076_layer[, areia_grossa := as.numeric(areia_grossa)]
summary(ctb0076_layer[, areia_grossa])
check_empty_layer(ctb0076_layer, "areia_grossa")

# areia fina
# old: Areia fina [g/kg]
# new: areia_fina
data.table::setnames(ctb0076_layer, old = "Areia fina [g/kg]", new = "areia_fina")
ctb0076_layer[, areia_fina := as.numeric(areia_fina)]
summary(ctb0076_layer[, areia_fina])
check_empty_layer(ctb0076_layer, "areia_fina")

# areia
ctb0076_layer[, areia := areia_grossa + areia_fina]
summary(ctb0076_layer[, areia])
check_empty_layer(ctb0076_layer, "areia")
# Fill empty layers
ctb0076_layer[,
  areia := fill_empty_layer(y = areia, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty areia values
check_empty_layer(ctb0076_layer, "areia")
# All missing areia values have been filled.

# silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0076_layer, old = "Silte [g/kg]", new = "silte")
ctb0076_layer[, silte := as.numeric(silte)]
summary(ctb0076_layer[, silte])
check_empty_layer(ctb0076_layer, "silte")
# Fill empty layers
ctb0076_layer[,
  silte := fill_empty_layer(y = silte, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
check_empty_layer(ctb0076_layer, "silte")


# argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0076_layer, old = "Argila [g/kg]", new = "argila")
ctb0076_layer[, argila := as.numeric(argila) * 10]
summary(ctb0076_layer[, argila])
check_empty_layer(ctb0076_layer, "argila")
ctb0076_layer[,
  argila := fill_empty_layer(y = argila, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
check_empty_layer(ctb0076_layer, "argila")

# Check the particle size distribution
# Round argila, silte and areia
ctb0076_layer[, argila := round(argila)]
ctb0076_layer[, silte := round(silte)]
ctb0076_layer[, areia := round(areia)]
# The sum of argila, silte and areia should be 1000 g/kg
ctb0076_layer[, psd := rowSums(.SD, na.rm = TRUE), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0076_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0076_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: COT [g/kg]
# new: carbono
data.table::setnames(ctb0076_layer, old = "COT [g/kg]", new = "carbono")
ctb0076_layer[, carbono := as.numeric(carbono)]
ctb0076_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0076_layer[, carbono])
check_empty_layer(ctb0076_layer, "carbono")
# Fill empty layers
ctb0076_layer[,
  carbono := fill_empty_layer(y = carbono, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Check again for empty carbono values
check_empty_layer(ctb0076_layer, "carbono")
# All missing carbono values have been filled.

# ctc
# old: CTC [cmolc/kg]
# new: ctc
data.table::setnames(ctb0076_layer, old = "CTC [cmolc/kg]", new = "ctc")
ctb0076_layer[, ctc := as.numeric(ctc)]
summary(ctb0076_layer[, ctc])
check_empty_layer(ctb0076_layer, "ctc")
# Fill empty layers
ctb0076_layer[,
  ctc := fill_empty_layer(y = ctc, x = profund_mid),
  by = observacao_id
]
# Check again for empty ctc values
check_empty_layer(ctb0076_layer, "ctc")
# All missing ctc values have been filled.

# ph
# old: pH em H_2O
# new: ph
data.table::setnames(ctb0076_layer, old = "pH em H_2O", new = "ph")
ctb0076_layer[, ph := as.numeric(ph)]
summary(ctb0076_layer[, ph])
# There are two layers missing "ph" values. These are C horizons.
check_empty_layer(ctb0076_layer, "ph")
# Fill empty layers
ctb0076_layer[,
  ph := fill_empty_layer(y = ph, x = profund_mid),
  by = observacao_id
]
# Check again for empty ph values
check_empty_layer(ctb0076_layer, "ph")
# All missing ph values have been filled.

# dsi
# Soil bulk density (dsi) is missing in this document.
ctb0076_layer[, dsi := NA_real_]
summary(ctb0076_layer[, dsi])

str(ctb0076_layer)

# Merge ############################################################################################
# events and layers
ctb0076 <- merge(ctb0076_event, ctb0076_layer, all = TRUE)
ctb0076[, dataset_id := "ctb0076"]

# citation
ctb0076 <- merge(ctb0076, ctb0076_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0076)
#Layers: 20
#Events: 4
#Georeferenced events: 0

# Plot using mapview
if (FALSE) {
  ctb0076_sf <- sf::st_as_sf(
    ctb0076[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0076_sf["argila"])
}

# Write to disk ####################################################################################
ctb0076 <- select_output_columns(ctb0076)
data.table::fwrite(ctb0076, "ctb0076/ctb0076.csv")
