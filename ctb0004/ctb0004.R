# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("sf")) {
  install.packages("sf")
  library("sf")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0004
# Dados de "Fate of fipronil in soils under sugar cane cultivation from the Northeast of Brazil:
# sorption and degradation"
# https://drive.google.com/drive/u/0/folders/1hZemS_Srcp5tqGlIFEuJT5Ri6ab-DSJf
ctb0004_ids <- soildata_catalog("ctb0004")

# validation #######################################################################################
ctb0004_validation <- google_sheet(ctb0004_ids$gs_id, ctb0004_ids$gid_validation)
str(ctb0004_validation)

# Check for negative validation results
sum(ctb0004_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0004_citation <- google_sheet(ctb0004_ids$gs_id, ctb0004_ids$gid_citation)
str(ctb0004_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0004_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0004_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0004_citation <- data.table::data.table(
  dataset_id = "ctb0004",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0004_citation)

# event ############################################################################################
ctb0004_event <- google_sheet(ctb0004_ids$gs_id, ctb0004_ids$gid_event)
str(ctb0004_event)

# Process fields

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0004_event, old = "ID do evento", new = "observacao_id")
ctb0004_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0004_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0004_event, old = "Ano (coleta)", new = "data_ano")
ctb0004_event[, data_ano := as.integer(data_ano)]
ctb0004_event[, .N, by = data_ano]

# ano_fonte
ctb0004_event[!is.na(data_ano), ano_fonte := "original"]
ctb0004_event[, .N, by = ano_fonte]

# coord_datum
# Datum (coord) -> coord_datum
data.table::setnames(ctb0004_event, old = "Datum (coord)", new = "coord_datum")
ctb0004_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0004_event[, coord_datum := as.integer(coord_datum)]
ctb0004_event[, .N, by = coord_datum]

# Longitude -> coord_x
data.table::setnames(ctb0004_event, old = "Longitude", new = "coord_x")
ctb0004_event[, coord_x := as.numeric(coord_x)]
summary(ctb0004_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0004_event, old = "Latitude", new = "coord_y")
ctb0004_event[, coord_y := as.numeric(coord_y)]
summary(ctb0004_event[, coord_y])

# Check for duplicate coordinates
any(ctb0004_event[, .N, by = .(coord_x, coord_y)][, N] > 1)

# Various events have the same coordinates. We need to add a small random number (jitter) to them
# (up to 30 m).
ctb0004_event_sf <- sf::st_as_sf(
  ctb0004_event,
  coords = c("coord_x", "coord_y"), crs = 4326
)
ctb0004_event_sf <- sf::st_transform(ctb0004_event_sf, 32724)
ctb0004_event_sf <- sf::st_jitter(ctb0004_event_sf, amount = 30)
ctb0004_event_sf <- sf::st_transform(ctb0004_event_sf, 4326)
ctb0004_event_sf <- sf::st_coordinates(ctb0004_event_sf)
ctb0004_event[, coord_x := ctb0004_event_sf[, 1]]
ctb0004_event[, coord_y := ctb0004_event_sf[, 2]]
rm(ctb0004_event_sf)

# Precisão (coord) [m] -> coord_precisao
# Coordinates were attributed with little knowledge of the precision. We set it to NA_real_
data.table::setnames(ctb0004_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0004_event[, coord_precisao := NA_real_]
summary(ctb0004_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0004_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0004_event[, coord_fonte := as.character(coord_fonte)]
ctb0004_event[, .N, by = coord_fonte]

# País -> pais_id
data.table::setnames(ctb0004_event, old = "País", new = "pais_id")
ctb0004_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0004_event, old = "Estado (UF)", new = "estado_id")
ctb0004_event[, estado_id := "PE"]

# Município -> municipio_id
data.table::setnames(ctb0004_event, old = "Município", new = "municipio_id")
ctb0004_event[, municipio_id := as.character(municipio_id)]
ctb0004_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0004_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0004_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0004_event[, amostra_area])

# taxon_sibcs
ctb0004_event[, taxon_sibcs := NA_character_]

# Soil Taxonomy [1999] -> taxon_st
data.table::setnames(ctb0004_event, old = "Soil Taxonomy [1999]", new = "taxon_st")
ctb0004_event[, taxon_st := as.character(taxon_st)]
ctb0004_event[, .N, by = taxon_st]

# Pedregosidade (superficie)
# review the work at another time

ctb0004_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0004_event[, rochosidade := ("Não Rochoso")]

str(ctb0004_event)

# layers ###########################################################################################
ctb0004_layer <- google_sheet(ctb0004_ids$gs_id, ctb0004_ids$gid_layer)
str(ctb0004_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0004_layer, old = "ID do evento", new = "observacao_id")
ctb0004_layer[, observacao_id := as.character(observacao_id)]
ctb0004_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
# This study has data from horizons (soil profiles) and layers (0-15 and 15-30 cm)
data.table::setnames(ctb0004_layer, old = "ID da camada", new = "camada_nome")
ctb0004_layer[, camada_nome := as.character(camada_nome)]
ctb0004_layer[, .N, by = camada_nome]

# amostra_id
# amostra_id is missing. We set it to NA_character_
ctb0004_layer[, amostra_id := NA_character_]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0004_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0004_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0004_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0004_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0004_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0004_layer[, profund_inf])

# missing layers
# are there any?
ctb0004_layer[
  shift(profund_inf) != profund_sup & profund_sup > 0,
  .(observacao_id, profund_sup, profund_inf)
]

# camada_id
ctb0004_layer <- ctb0004_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0004_layer[, camada_id := 1:.N, by = observacao_id]
ctb0004_layer[, .N, by = camada_id]

# terrafina
# terrafina is missing. We set it to 1000 g/kg
ctb0004_layer[, terrafina := 1000.0]

# Clay [%] -> argila
# Clay is missing for some all 15-30 layers and some 0-15 layers
data.table::setnames(ctb0004_layer, old = "Clay [%]", new = "argila")
ctb0004_layer[, argila := as.numeric(argila) * 10]
summary(ctb0004_layer[, argila])
ctb0004_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# Silt [%] -> silte
# Silt is missing for some all 0-15 and 15-30 layers
data.table::setnames(ctb0004_layer, old = "Silt [%]", new = "silte")
ctb0004_layer[, silte := as.numeric(silte) * 10]
summary(ctb0004_layer[, silte])
ctb0004_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# Sand [%] -> areia
# Sand is missing for some all 0-15 and 15-30 layers
data.table::setnames(ctb0004_layer, old = "Sand [%]", new = "areia")
ctb0004_layer[, areia := as.numeric(areia) * 10]
summary(ctb0004_layer[, areia])

# Org. C [g/kg] -> carbono
# Organic carbon is missing for all 15-30 layers and some 0-15 layers
data.table::setnames(ctb0004_layer, old = "Org. C [g/kg]", new = "carbono")
ctb0004_layer[, carbono := as.numeric(carbono)]
summary(ctb0004_layer[, carbono])

# CEC [cmolc/kg] -> ctc
# CEC is missing for all 0-15 and 15-30 layers
data.table::setnames(ctb0004_layer, old = "CEC [cmolc/kg]", new = "ctc")
ctb0004_layer[, ctc := as.numeric(ctc)]
summary(ctb0004_layer[, ctc])

# pH H2O OR pH CaCl2 -> ph
# pH is missing for some 0-15 layers and all 15-30 layers
data.table::setnames(ctb0004_layer, old = "pH H2O", new = "ph_h2o")
data.table::setnames(ctb0004_layer, old = "pH CaCl2", new = "ph_cacl2")
ctb0004_layer[, ph := as.numeric(ph_h2o)]
ctb0004_layer[is.na(ph), ph := as.numeric(ph_cacl2)]
ctb0004_layer[, ph_h2o := NULL]
ctb0004_layer[, ph_cacl2 := NULL]
summary(ctb0004_layer[, ph])

# Bulk density [g/cm^3] -> dsi
# Bulk density is missing for all 0-15 and 15-30 layers
data.table::setnames(ctb0004_layer, old = "Bulk density [g/cm^3]", new = "dsi")
ctb0004_layer[, dsi := as.numeric(dsi)]
summary(ctb0004_layer[, dsi])

# Merge ############################################################################################
# events and layers
ctb0004 <- merge(ctb0004_event, ctb0004_layer, all = TRUE)
ctb0004[, dataset_id := "ctb0004"]
# citation
ctb0004 <- merge(ctb0004, ctb0004_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0004)
# Layers: 92
# Events: 40
# Georeferenced events: 40

# Plot using mapview
if (FALSE) {
  ctb0004_sf <- sf::st_as_sf(
    ctb0004[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0004_sf["argila"])
}

# Write to disk ####################################################################################
ctb0004 <- select_output_columns(ctb0004)
data.table::fwrite(ctb0004, "ctb0004/ctb0004.csv")
data.table::fwrite(ctb0004_event, "ctb0004/ctb0004_event.csv")
data.table::fwrite(ctb0004_layer, "ctb0004/ctb0004_layer.csv")
