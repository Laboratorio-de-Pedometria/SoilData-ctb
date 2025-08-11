# author: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0007
# Dados de "Avaliação da cor e comportamento espectral de algumas classes de solos do Rio Grande do
# Sul"
# https://drive.google.com/drive/u/0/folders/1sZ8iE_lrunYooDa6INUQJKfOvzZXzsSn
gs <- "1yH3S0lFCCGFQ2JupPH_5KAoleBA-v5zoU7RRlfzvNyo"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0007_validation <- google_sheet(gs, gid_validation)
str(ctb0007_validation)

# Check for negative validation results
sum(ctb0007_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0007_citation <- google_sheet(gs, gid_citation)

str(ctb0007_citation)

# dataset_titulo
dataset_titulo <- ctb0007_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0007_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0007_citation <- data.table::data.table(
  dataset_id = "ctb0007",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0007_citation)

# event ############################################################################################
ctb0007_event <- google_sheet(gs, gid_event)
str(ctb0007_event)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0007_event, old = "ID do evento", new = "observacao_id")
ctb0007_event[, observacao_id := as.character(observacao_id)]
# Check if there are duplicated IDs
any(table(ctb0007_event[, observacao_id]) > 1)

# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0007_event, old = "Ano (coleta)", new = "data_ano")
ctb0007_event[, data_ano := as.integer(data_ano)]
ctb0007_event[, .N, by = data_ano]

# data_fonte
ctb0007_event[!is.na(data_ano), data_fonte := "original"]
ctb0007_event[, .N, by = data_fonte]

# Longitude -> coord_x
data.table::setnames(ctb0007_event, old = "Longitude", new = "coord_x")
ctb0007_event[, coord_x := as.numeric(coord_x)]
summary(ctb0007_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0007_event, old = "Latitude", new = "coord_y")
ctb0007_event[, coord_y := as.numeric(coord_y)]
summary(ctb0007_event[, coord_y])

# coord_datum
data.table::setnames(ctb0007_event, old = "Datum (coord)", new = "coord_datum")
ctb0007_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0007_event[, coord_datum := as.integer(coord_datum)]
# Check if there is more than one CRS
# There actually are no coordinates in this dataset
ctb0007_event[, .N, by = coord_datum]
# 32721 -> 4326
ctb0007_event_sf <- sf::st_as_sf(ctb0007_event[coord_datum == "32721"],
  coords = c("coord_x", "coord_y"), crs = 32721
)
ctb0007_event_sf <- sf::st_transform(ctb0007_event_sf, crs = 4326)
xy <- sf::st_coordinates(ctb0007_event_sf)
ctb0007_event[coord_datum == "32721", coord_x := xy[, 1]]
ctb0007_event[coord_datum == "32721", coord_y := xy[, 2]]
ctb0007_event[coord_datum == 32721, coord_datum := 4326]
# 32722 -> 4326
ctb0007_event_sf <- sf::st_as_sf(ctb0007_event[coord_datum == "32722"],
  coords = c("coord_x", "coord_y"), crs = 32722
)
ctb0007_event_sf <- sf::st_transform(ctb0007_event_sf, crs = 4326)
xy <- sf::st_coordinates(ctb0007_event_sf)
ctb0007_event[coord_datum == "32722", coord_x := xy[, 1]]
ctb0007_event[coord_datum == "32722", coord_y := xy[, 2]]
ctb0007_event[coord_datum == 32722, coord_datum := 4326]

# Check for duplicate coordinates
ctb0007_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Precisão (coord) [m] -> coord_precisao
data.table::setnames(ctb0007_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0007_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0007_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0007_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0007_event[, coord_fonte := as.character(coord_fonte)]
ctb0007_event[, .N, by = coord_fonte]

# pais_id
# País -> pais_id
data.table::setnames(ctb0007_event, old = "País", new = "pais_id")
ctb0007_event[, pais_id := as.character(pais_id)]
ctb0007_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0007_event, old = "Estado (UF)", new = "estado_id")
ctb0007_event[, estado_id := as.character(estado_id)]
ctb0007_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0007_event, old = "Município", new = "municipio_id")
ctb0007_event[, municipio_id := as.character(municipio_id)]
ctb0007_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0007_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0007_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0007_event[, amostra_area])

# taxon_sibcs
# old: SiBCS (1999)
# new: taxon_sibcs
data.table::setnames(ctb0007_event, old = "SiBCS (1999)", new = "taxon_sibcs")
ctb0007_event[, taxon_sibcs := as.character(taxon_sibcs)]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0007_event[, taxon_st := NA_character_]

str(ctb0007_event)

# layer ############################################################################################
ctb0007_layer <- google_sheet(gs, gid_layer)
str(ctb0007_layer)

# Prepare columns

# ID do evento -> observacao_id
data.table::setnames(ctb0007_layer, old = "ID do evento", new = "observacao_id")
ctb0007_layer[, observacao_id := as.character(observacao_id)]
ctb0007_layer[, .N, by = observacao_id]

# camada_nome
# old: Nome da camada
# new: camada_nome
data.table::setnames(ctb0007_layer, old = "Nome da camada", new = "camada_nome")
ctb0007_layer[, camada_nome := as.character(camada_nome)]
ctb0007_layer[, .N, by = camada_nome]

# amostra_id
# old: ID da amostra
# new: amostra_id
data.table::setnames(ctb0007_layer, old = "ID da amostra", new = "amostra_id")
ctb0007_layer[, amostra_id := as.character(amostra_id)]
ctb0007_layer[, .N, by = amostra_id]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0007_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0007_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0007_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0007_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0007_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0007_layer[, profund_inf])

# any missing layer?
any_missing_layer(ctb0007_layer)

# camada_id
# old: ID da camada
# new: camada_id
ctb0007_layer <- ctb0007_layer[order(observacao_id, profund_sup, profund_inf)]
data.table::setnames(ctb0007_layer, old = "ID da camada", new = "camada_id")
ctb0007_layer[, camada_id := 1:.N, by = observacao_id]
ctb0007_layer[, .N, by = camada_id]

# terrafina
# terrafina is missing. We set it to 1000 g/kg
ctb0007_layer[, terrafina := 1000]

# Argila -> argila
data.table::setnames(ctb0007_layer, old = "Argila", new = "argila")
ctb0007_layer[, argila := as.numeric(argila)]
summary(ctb0007_layer[, argila])

# Silte -> silte
data.table::setnames(ctb0007_layer, old = "Silte", new = "silte")
ctb0007_layer[, silte := as.numeric(silte)]
summary(ctb0007_layer[, silte])

# Areia -> areia
data.table::setnames(ctb0007_layer, old = "Areia", new = "areia")
ctb0007_layer[, areia := as.numeric(areia)]
summary(ctb0007_layer[, areia])

# MO -> carbono
data.table::setnames(ctb0007_layer, old = "MO", new = "carbono")
ctb0007_layer[, carbono := as.numeric(carbono) * 0.58]
summary(ctb0007_layer[, carbono])

# CTC pH7 -> ctc
data.table::setnames(ctb0007_layer, old = "CTC pH7", new = "ctc")
ctb0007_layer[, ctc := as.numeric(ctc)]
summary(ctb0007_layer[, ctc])

# pH em H_2O -> ph
data.table::setnames(ctb0007_layer, old = "pH em H2O", new = "ph")
ctb0007_layer[, ph := as.numeric(ph)]
summary(ctb0007_layer[, ph])

# dsi
ctb0007_layer[, dsi := NA_real_]

# Merge ############################################################################################
# events and layers
ctb0007 <- merge(ctb0007_event, ctb0007_layer, all = TRUE)
ctb0007[, dataset_id := "ctb0007"]
# citation
ctb0007 <- merge(ctb0007, ctb0007_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0007)
# Layers: 24
# Events: 12
# Georeferenced events: 12

# Convert to sf and check if the data is correctly georeferenced
# There actually are no coordinates in this dataset
if (FALSE) {
  ctb0007_sf <- sf::st_as_sf(
    ctb0007[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0007_sf["argila"])
}

# Write to disk ####################################################################################
ctb0007 <- select_output_columns(ctb0007)
data.table::fwrite(ctb0007, "ctb0007/ctb0007.csv")
data.table::fwrite(ctb0007_event, "ctb0007/ctb0007_event.csv")
data.table::fwrite(ctb0007_layer, "ctb0007/ctb0007_layer.csv")
