# autor: Felipe Brun Vergani
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
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
}
if (!require("parzer")) {
  install.packages("parzer")
  library("parzer")
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0045
# Dados de "Parâmetros Físicos e Químicos de Referência em Solos de Unidades de Conservação Florestal da Bacia do Paraná 3, Brasil"
# 
# https://docs.google.com/spreadsheets/d/1dEOi5WTEwpBk8xvHvYfYNPf3neQRRn-nQFyPc0vx-kE/edit?usp=sharing


ctb0045_ids <- soildata_catalog("ctb0045")

# validation #####################################################################################

ctb0045_validation <- google_sheet(ctb0045_ids$gs_id, ctb0045_ids$gid_validation)
str(ctb0045_validation)

# Check for negative validation results
sum(ctb0045_validation == FALSE, na.rm = TRUE)

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

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0045_event, old = "ID do evento", new = "observacao_id")
ctb0045_event[, observacao_id := as.character(observacao_id)]
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
ctb0045_event[, .N, by = .(coord_x, coord_y)][N > 1]

# DATUM -> coord_datum
data.table::setnames(ctb0045_event, old = "Datum (coord)", new = "coord_datum")
ctb0045_event[, coord_datum := 4326]

# Precisão (coord) -> coord_precisao
# We set it to NA_real_ (missing)
data.table::setnames(ctb0045_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0045_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0045_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0045_event[, coord_fonte := as.character(coord_fonte)]


# País -> pais_id
data.table::setnames(ctb0045_event, old = "País", new = "pais_id")
ctb0045_event[, pais_id := "BR"]


# Estado (UF) -> estado_id
data.table::setnames(ctb0045_event, old = "Estado (UF)", new = "estado_id")
ctb0045_event[, estado_id := as.character(estado_id)]
ctb0045_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0045_event, old = "Município", new = "municipio_id")
ctb0045_event[, municipio_id := as.character(municipio_id)]
ctb0045_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0045_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0045_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0045_event[, amostra_area])

# Classe do solo  -> taxon_sibcs
data.table::setnames(ctb0045_event, old = "Classe do solo", new = "taxon_sibcs")
ctb0045_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0045_event[, .N, by = taxon_sibcs]


# taxon_st 
# missing this soil taxonomy on document
ctb0045_event[, taxon_st := NA_character_]
ctb0045_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
data.table::setnames(ctb0045_event, old = "Pedregosidade", new = "pedregosidade")
ctb0045_event[, pedregosidade := as.character(pedregosidade)]
ctb0045_event[, .N, by = pedregosidade]

# Rochosidade (superficie)
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
ctb0045_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0045_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0045_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0045_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0045_layer[, profund_inf])

#areia 
# old: Areia [%]
# new: areia
data.table::setnames(ctb0045_layer, old = "Areia [%]", new = "areia")
ctb0045_layer[, areia := as.numeric(areia)*10]
summary(ctb0045_layer[, areia])

#silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0045_layer, old = "Silte [%]", new = "silte")
ctb0045_layer[, silte := as.numeric(silte)*10]
summary(ctb0045_layer[, silte])

#argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0045_layer, old = "Argila [%]", new = "argila")
ctb0045_layer[, argila := as.numeric(argila)*10]
summary(ctb0045_layer[, argila])


#terrafina
#is missingg in this document.
ctb0045_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0045_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0045_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0045_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: Carbono Orgânico [g/Kg]
# new: carbono
data.table::setnames(ctb0045_layer, old = "Carbono Orgânico [g/Kg]", new = "carbono")
ctb0045_layer[, carbono := as.numeric(carbono)]
ctb0045_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0045_layer[, carbono])

# ctc
# old: T ou CTC [cmol/dm3]
# new: ctc
data.table::setnames(ctb0045_layer, old = "T ou CTC [cmol/dm3]", new = "ctc")
ctb0045_layer[, ctc := as.numeric(ctc)]
summary(ctb0045_layer[, ctc])
check_empty_layer(ctb0045_layer, "ctc")

# ph
# old: pH
# new: ph
data.table::setnames(ctb0045_layer, old = "pH", new = "ph")
ctb0045_layer[, ph := as.numeric(ph)]
summary(ctb0045_layer[, ph])
check_empty_layer(ctb0045_layer, "ph")

# dsi 
#missing in this document.
ctb0045_layer[, dsi := NA_real_]




str(ctb0045_layer)

# Merge ############################################################################################
# events and layers
ctb0045 <- merge(ctb0045_event, ctb0045_layer, all = TRUE)
ctb0045[, dataset_id := "ctb0045"]
# citation
ctb0045 <- merge(ctb0045, ctb0045_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0045)

#Layers: 73
#Events: 73
#Georeferenced events: 73


# Plot using mapview
if (TRUE) {
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
