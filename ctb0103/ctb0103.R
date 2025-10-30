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
# ctb0103
# Dados de "Ontogenetic shifts in habitat-association of tree species in a neotropical wetland"
# 
#https://docs.google.com/spreadsheets/d/1GjqbsJs0MhPLyrg_C0ox7miYPwiMcHsLU76GLz-0paI/edit?usp=sharing


ctb0103_ids <- soildata_catalog("ctb0103")

# validation #####################################################################################

ctb0103_validation <- google_sheet(ctb0103_ids$gs_id, ctb0103_ids$gid_validation)
str(ctb0103_validation)

# Check for negative validation results
sum(ctb0103_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0103_citation <- google_sheet(ctb0103_ids$gs_id, ctb0103_ids$gid_citation)
str(ctb0103_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0103_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0103_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0103_citation <- data.table::data.table(
  dataset_id = "ctb0103",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0103_citation)

# event #####################################################################################
ctb0103_event <- google_sheet(ctb0103_ids$gs_id, ctb0103_ids$gid_event)
str(ctb0103_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0103_event, old = "ID do evento", new = "observacao_id")
ctb0103_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0103_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0103_event, old = "Ano (coleta)", new = "data_ano")
ctb0103_event[, data_ano := as.integer(data_ano)]
ctb0103_event[, .N, by = data_ano]

# ano_fonte
ctb0103_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0103_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0103_event, old = "Longitude", new = "coord_x")
ctb0103_event[, coord_x := as.numeric(coord_x)]
summary(ctb0103_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0103_event, old = "Latitude", new = "coord_y")
ctb0103_event[, coord_y := as.numeric(coord_y)]
summary(ctb0103_event[, coord_y])

# Check for duplicate coordinates
ctb0103_event[, .N, by = .(coord_x, coord_y)][N > 1]

# DATUM -> coord_datum
data.table::setnames(ctb0103_event, old = "Datum (coord)", new = "coord_datum")
ctb0103_event[, coord_datum := 4326]

# Precisão (coord) -> coord_precisao
data.table::setnames(ctb0103_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0103_event[, coord_precisao := as.numeric(coord_precisao)]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0103_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0103_event[, coord_fonte := as.character(coord_fonte)]


# País -> pais_id
data.table::setnames(ctb0103_event, old = "País", new = "pais_id")
ctb0103_event[, pais_id := "BR"]


# Estado (UF) -> estado_id
data.table::setnames(ctb0103_event, old = "Estado (UF)", new = "estado_id")
ctb0103_event[, estado_id := as.character(estado_id)]
ctb0103_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0103_event, old = "Município", new = "municipio_id")
ctb0103_event[, municipio_id := as.character(municipio_id)]
ctb0103_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0103_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0103_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0103_event[, amostra_area])

# taxon_sibcs
# missing this soil taxon.
ctb0103_event[, taxon_sibcs := NA_character_]


# taxon_st 
# missing this soil taxonomy on document
ctb0103_event[, taxon_st := NA_character_]
ctb0103_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
# missing in this document.
ctb0103_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# missing in this document.

ctb0103_event[, rochosidade := NA_character_]



str(ctb0103_event)

# layers ###########################################################################################
ctb0103_layer <- google_sheet(ctb0103_ids$gs_id, ctb0103_ids$gid_layer)
str(ctb0103_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0103_layer, old = "ID do evento", new = "observacao_id")
ctb0103_layer[, observacao_id := as.character(observacao_id)]
ctb0103_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
# missing in this document.
ctb0103_layer[, camada_nome := NA_character_]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0103_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0103_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0103_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0103_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0103_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0103_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0103_layer[, profund_inf])

#areia 
# old: Areia [g/kg]
# new: areia
data.table::setnames(ctb0103_layer, old = "Areia [g/kg]", new = "areia")
ctb0103_layer[, areia := as.numeric(areia)]
summary(ctb0103_layer[, areia])

#silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0103_layer, old = "Silte [g/kg]", new = "silte")
ctb0103_layer[, silte := as.numeric(silte)]
summary(ctb0103_layer[, silte])

#argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0103_layer, old = "Argila [g/kg]", new = "argila")
ctb0103_layer[, argila := as.numeric(argila)]
summary(ctb0103_layer[, argila])


#terrafina
#is missingg in this document.
ctb0103_layer[, terrafina := NA_real_]

# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile. Order by observacao_id and mid_depth.
ctb0103_layer[, mid_depth := (profund_sup + profund_inf) / 2]
ctb0103_layer <- ctb0103_layer[order(observacao_id, mid_depth)]
ctb0103_layer[, camada_id := 1:.N, by = observacao_id]
ctb0103_layer[, .N, by = camada_id]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0103_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0103_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0103_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: MO [g/dm]
# new: carbono
data.table::setnames(ctb0103_layer, old = "MO [g/dm]", new = "carbono")
ctb0103_layer[, carbono := as.numeric(carbono)/1.742]
ctb0103_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0103_layer[, carbono])

# ctc
ctb0103_layer[, ctc := NA_real_]

# ph
ctb0103_layer[, ph := NA_real_]

# dsi 
#missing in this document.
ctb0103_layer[, dsi := NA_real_]




str(ctb0103_layer)

# Merge ############################################################################################
# events and layers
ctb0103 <- merge(ctb0103_event, ctb0103_layer, all = TRUE)
ctb0103[, dataset_id := "ctb0103"]
# citation
ctb0103 <- merge(ctb0103, ctb0103_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0103)

#Layers: 237
#Events: 92
#Georeferenced events: 91


# Plot using mapview
if (TRUE) {
  ctb0103_sf <- sf::st_as_sf(
    ctb0103[coord_datum == 4326 & !is.na(coord_x) & !is.na(coord_y)],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0103_sf["carbono"])
}

# Write to disk ####################################################################################
ctb0103 <- select_output_columns(ctb0103)
data.table::fwrite(ctb0103, "ctb0103/ctb0103.csv")
data.table::fwrite(ctb0103_event, "ctb0103/ctb0103_event.csv")
data.table::fwrite(ctb0103_layer, "ctb0103/ctb0103_layer.csv")
