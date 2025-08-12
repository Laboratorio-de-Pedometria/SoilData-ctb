# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("openxlsx")) {
  install.packages("openxlsx")
}
if (!require("sf")) {
  install.packages("sf")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0035
# Banco de solos do Estado de Minas Gerais
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0035/2020-10-03-ctb0035.xlsx")

# citation #########################################################################################
ctb0035_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0035_citation <- data.table::as.data.table(ctb0035_citation)
str(ctb0035_citation)

# dataset_titulo
dataset_titulo <- ctb0035_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0035_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0035_citation <- data.table::data.table(
  dataset_id = "ctb0035",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0035_citation)

# event ############################################################################################
ctb0035_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0035_event <- data.table::as.data.table(ctb0035_event)
str(ctb0035_event)

# Process fields
# observacao_id
ctb0035_event[, observacao_id := as.character(observacao_id)]
ctb0035_event[, .N, by = observacao_id][N > 1]

# observacao_data -> data_ano
# 2025-05-26 The year of collection is missing. Analysis of the existing documents shows that the
# project started in the year 2009 and produced data that was used to set reference values for soil 
# quality in Minas Gerais that were published in the year 2011. We will set the year of collection 
# to 2010.
data.table::setnames(ctb0035_event, old = "observacao_data", new = "data_ano")
ctb0035_event[, data_ano := 2010]
ctb0035_event[, .N, by = data_ano]

# data_fonte
ctb0035_event[, data_fonte := "estimativa"]

# coord_x
ctb0035_event[, coord_x := as.numeric(coord_x)]
summary(ctb0035_event[, coord_x])

# coord_y
ctb0035_event[, coord_y := as.numeric(coord_y)]
summary(ctb0035_event[, coord_y])

# coord_sistema -> coord_datum
# South American Datum 1969 (SAD69) is EPSG:4618
data.table::setnames(ctb0035_event, old = "coord_sistema", new = "coord_datum")
ctb0035_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0035_event[, .N, by = coord_datum]
# 4618 to 4326
ctb0035_event_sf <- sf::st_as_sf(ctb0035_event[coord_datum == "4618"],
  coords = c("coord_x", "coord_y"), crs = 4618
)
ctb0035_event_sf <- sf::st_transform(ctb0035_event_sf, crs = 4326)
ctb0035_event_sf <- sf::st_coordinates(ctb0035_event_sf)
ctb0035_event[coord_datum == "4618", coord_x := ctb0035_event_sf[, 1]]
ctb0035_event[coord_datum == "4618", coord_y := ctb0035_event_sf[, 2]]
ctb0035_event[coord_datum == "4618", coord_datum := 4326]
ctb0035_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0035_event_sf)
ctb0035_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0035_event[, .N, by = .(coord_y, coord_x)][N > 1]
# There are 15 duplicated pairs of coordinates. One pair is duplicated 3 times.
# The others are duplicated 2 times. It is not evident how to fix them.
# We keep them for now and wait to check the layer data.
# Create a column to identify if the coordinates are duplicated
ctb0035_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0035_event[coord_duplicated == TRUE, .N]

# add some jitter to the duplicated coordinates (30 m)
ctb0035_event_sf <- sf::st_as_sf(ctb0035_event[coord_duplicated == TRUE & coord_datum == 4326],
  coords = c("coord_x", "coord_y"), crs = 4326
)
ctb0035_event_sf <- sf::st_transform(ctb0035_event_sf, crs = 32722)
set.seed(1984)
ctb0035_event_sf <- sf::st_jitter(ctb0035_event_sf, amount = 30)
ctb0035_event_sf <- sf::st_transform(ctb0035_event_sf, crs = 4326)
ctb0035_event_sf <- sf::st_coordinates(ctb0035_event_sf)
ctb0035_event[coord_duplicated == TRUE & coord_datum == 4326, coord_x := ctb0035_event_sf[, 1]]
ctb0035_event[coord_duplicated == TRUE & coord_datum == 4326, coord_y := ctb0035_event_sf[, 2]]
rm(ctb0035_event_sf)
ctb0035_event[, coord_duplicated := NULL]
ctb0035_event[, .N, by = .(coord_y, coord_x)][N > 1]

# coord_fonte
ctb0035_event[, coord_fonte := as.character(coord_fonte)]
ctb0035_event[, .N, by = coord_fonte]

# coord_precisao
# coord_precisao is missing. Because coord_fonte is GPS, we can assume a precision of 30 meters
ctb0035_event[, coord_precisao := 30]

# pais_id
ctb0035_event[, pais_id := "BR"]

# estado_id
ctb0035_event[, estado_id := as.character(estado_id)]
ctb0035_event[, .N, by = estado_id]

# municipio_id
ctb0035_event[, municipio_id := as.character(municipio_id)]
ctb0035_event[, .N, by = municipio_id]

# amostra_area
ctb0035_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0035_event[, amostra_area])

# taxon_sibcs
# Brazilian Soil Classification is missing
ctb0035_event[, taxon_sibcs := NA_character_]

# taxon_st
# US Soil Taxonomy is missing
ctb0035_event[, taxon_st := NA_character_]

str(ctb0035_event)

# layer ############################################################################################
ctb0035_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0035_layer <- data.table::as.data.table(ctb0035_layer)
str(ctb0035_layer)

# Process fields
# observacao_id
ctb0035_layer[, observacao_id := as.character(observacao_id)]
ctb0035_layer[, .N, by = observacao_id]

# camada_nome
ctb0035_layer[, camada_nome := as.character(camada_nome)]
ctb0035_layer[, .N, by = camada_nome]

# amostra_id
ctb0035_layer[, amostra_id := as.character(amostra_id)]
ctb0035_layer[, .N, by = amostra_id]

# profund_sup
ctb0035_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0035_layer[, profund_sup])

# profund_inf
ctb0035_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0035_layer[, profund_inf])

# check for missing layers
any_missing_layer(ctb0035_layer)

# camada_id
# For each observacao_id, sort by profund_sup and profunf_inf and assign a unique camada_id
ctb0035_layer <- ctb0035_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0035_layer[, camada_id := 1:.N, by = observacao_id]
ctb0035_layer[, .N, by = camada_id]

# terrafina
# terrafina is missing. We assume it is 1000 g/kg
ctb0035_layer[, terrafina := 1000]

# Argila -> argila
data.table::setnames(ctb0035_layer, old = "Argila", new = "argila")
ctb0035_layer[, argila := as.numeric(argila) * 10]
summary(ctb0035_layer[, argila])

# Silte -> silte
data.table::setnames(ctb0035_layer, old = "Silte", new = "silte")
ctb0035_layer[, silte := as.numeric(silte) * 10]
summary(ctb0035_layer[, silte])

# Areia Grossa + Areia Fina -> areia
data.table::setnames(ctb0035_layer, old = "Areia.Grossa", new = "areia_grossa")
data.table::setnames(ctb0035_layer, old = "Areia.Fina", new = "areia_fina")
ctb0035_layer[, areia := as.numeric(areia_grossa) + as.numeric(areia_fina)]
ctb0035_layer[, areia := areia * 10]
summary(ctb0035_layer[, areia])

# MOS -> carbono
data.table::setnames(ctb0035_layer, old = "MOS", new = "carbono")
ctb0035_layer[, carbono := as.numeric(carbono) * 0.58 * 10]
summary(ctb0035_layer[, carbono])

# pH.H2O -> ph
data.table::setnames(ctb0035_layer, old = "pH.H2O", new = "ph")
ctb0035_layer[, ph := as.numeric(ph)]
summary(ctb0035_layer[, ph])

# T -> ctc
data.table::setnames(ctb0035_layer, old = "T", new = "ctc")
ctb0035_layer[, ctc := as.numeric(ctc)]
summary(ctb0035_layer[, ctc])

# dsi
ctb0035_layer[, dsi := NA_real_]

str(ctb0035_layer)

# Merge ############################################################################################
# events and layers
ctb0035 <- merge(ctb0035_event, ctb0035_layer, all = TRUE)
ctb0035[, dataset_id := "ctb0035"]
# citation
ctb0035 <- merge(ctb0035, ctb0035_citation, by = "dataset_id", all.x = TRUE)

# Order by coord_x
# Print the layers with duplicated coordinates, checking key soil properties
if (FALSE) {
  ctb0035 <- ctb0035[order(coord_x, coord_y)]
  ctb0035[coord_duplicated == TRUE, .(observacao_id, coord_x, coord_y, argila, carbono, ctc)]
}
# Some observations with neighboring IDs (e.g. 90 and 91 or 216 and 217) that have the same
# coordinates have similar values for argila, carbono, and ctc. However, this is not true for all
# observations. We will keep them for now and add some noise to the coordinates (jitter).
summary_soildata(ctb0035)
# Layers: 685
# Events: 685
# Georeferenced events: 667

# Plot using mapview
if (FALSE) {
  ctb0035_sf <- sf::st_as_sf(ctb0035[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0035_sf, zcol = "carbono")
}

# Write to disk ####################################################################################
ctb0035 <- select_output_columns(ctb0035)
data.table::fwrite(ctb0035, "ctb0035/ctb0035.csv")
data.table::fwrite(ctb0035_event, "ctb0035/ctb0035_event.csv")
data.table::fwrite(ctb0035_layer, "ctb0035/ctb0035_layer.csv")
