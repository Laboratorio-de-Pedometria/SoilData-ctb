# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("mapview")) {
  install.packages("mapview")
}

# Source helper functions
source("./helper.R")


# ATTENTION: MOVED TO GOOGLE DRIVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# ownCloud #########################################################################################
# ctb0053
# Inventário Florestal Nacional - Rondônia

# Citation #########################################################################################
ctb0053_citation <- data.table::data.table(
  dataset_id = "ctb0053",
  dataset_titulo = "Inventário Florestal Nacional - Rondônia",
  dataset_licenca = "CC-BY"
)

# event ############################################################################################
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0053-Inventário Florestal Nacional - Rondônia (Google Drive)/base/IFN-RO_unidades_amostrais_19-04-2022.csv")
ctb0053_event <- data.table::fread(file_path, dec = ",")
str(ctb0053_event)

# Process fields
# observacao_id
# old: UA
data.table::setnames(ctb0053_event, old = "UA", new = "observacao_id")
ctb0053_event[, observacao_id := as.character(observacao_id)]
ctb0053_event[, .N, by = observacao_id][N > 1]

# data_ano
# old: DATA
data.table::setnames(ctb0053_event, old = "DATA", new = "data_ano")
ctb0053_event[, data_ano := as.Date(data_ano, format = "%d/%m/%Y")]
ctb0053_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0053_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta é informada no trabalho de origem dos dados.
ctb0053_event[!is.na(data_ano), ano_fonte := "original"]

# coord_x
# old: E
# new: coord_x
data.table::setnames(ctb0053_event, old = "E", new = "coord_x")
ctb0053_event[, coord_x := as.numeric(coord_x)]
summary(ctb0053_event[, coord_x])

# coord_y
# old: N
# new: coord_y
data.table::setnames(ctb0053_event, old = "N", new = "coord_y")
ctb0053_event[, coord_y := as.numeric(coord_y)]
# Incorrectly recorded as 880611 (falls in Antarctica)
ctb0053_event[observacao_id == "RO_279", coord_y := 8806011]
summary(ctb0053_event[, coord_y])

# coord_datum
# "ZONA" is a letter between H and P.
# Replace the values of "ZONA" with "S" or "N" according to the following matching table:
utm_zone <- c(H = "S", J = "S", K = "S", L = "S", M = "S", N = "N", P = "N")
ctb0053_event[, ZONA := utm_zone[ZONA]]
ctb0053_event[, .N, by = ZONA]
# Now paste "FUSO" and "ZONA" to get the datum
ctb0053_event[!is.na(ZONA), coord_datum := paste0(FUSO, ZONA), by = .I]
ctb0053_event[, .N, coord_datum]
# Replace the values of "coord_datum" with the EPSG code according to the following matching table:
epsg_code <- c(
  `18S` = "EPSG:32718",
  `19S` = "EPSG:32719",
  `20S` = "EPSG:32720",
  `21S` = "EPSG:32721",
  `22S` = "EPSG:32722",
  `23S` = "EPSG:32723",
  `24S` = "EPSG:32724",
  `25S` = "EPSG:32725"
)
ctb0053_event[, coord_datum := epsg_code[coord_datum]]
ctb0053_event[, .N, by = coord_datum]
# Convert to numeric
ctb0053_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0053_event[, coord_datum := as.integer(coord_datum)]
ctb0053_event[, .N, by = coord_datum]
# convert the three coordinate systems to WGS84
crs_list <- unique(ctb0053_event$coord_datum)
crs_list <- na.exclude(crs_list)
for (crs in crs_list) {
  ctb0053_event_sf <- sf::st_as_sf(
    ctb0053_event[coord_datum == crs, ],
    coords = c("coord_x", "coord_y"), crs = crs
  )
  ctb0053_event_sf <- sf::st_transform(ctb0053_event_sf, crs = 4326)
  ctb0053_event_sf <- sf::st_coordinates(ctb0053_event_sf)
  ctb0053_event[coord_datum == crs, coord_x := ctb0053_event_sf[, 1]]
  ctb0053_event[coord_datum == crs, coord_y := ctb0053_event_sf[, 2]]
  ctb0053_event[coord_datum == crs, coord_datum := 4326]
  rm(ctb0053_event_sf)
}
ctb0053_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0053_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0053_event[coord_duplicated == TRUE & !is.na(coord_x), .(observacao_id, coord_x, coord_y)]
ctb0053_event[, coord_duplicated := NULL]

# coord_fonte
# The source of the coordinates is missing. The reports mention the use of GPS.
ctb0053_event[, coord_fonte := "GPS"]

# coord_precisao
# The precision of the coordinates is missing. We assume it is 30 meters.
ctb0053_event[, coord_precisao := 30]

# pais_id
# pais_id is missing. We set it to "BR"
ctb0053_event[, pais_id := "BR"]

# estado_id
# old: UF
# new: estado_id
data.table::setnames(ctb0053_event, old = "UF", new = "estado_id")
ctb0053_event[, estado_id := as.character(estado_id)]
ctb0053_event[, .N, by = estado_id]

# municipio_id
# old: MUN
# new: municipio_id
data.table::setnames(ctb0053_event, old = "MUN", new = "municipio_id")
ctb0053_event[, municipio_id := as.character(municipio_id)]
ctb0053_event[, .N, by = municipio_id]

# amostra_area
# amostra_area is missing. We set it to round(pi * 0.1^2, 2)
ctb0053_event[, amostra_area := round(pi * 0.1^2, 2)]

# taxon_sibcs
# The soil classification is missing. We set it to NA_character_
ctb0053_event[, taxon_sibcs := NA_character_]

# taxon_st
# US Soil Taxonomy is missing. We set it to NA_character_
ctb0053_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# review the work at another time

ctb0053_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0053_event[, rochosidade := ("Não Rochoso")]

str(ctb0053_event)

# layer ############################################################################################
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0053-Inventário Florestal Nacional - Rondônia (Google Drive)/base/IFN-RO_solos_25-08-2022.csv")
ctb0053_layer <- data.table::fread(
  file_path,
  dec = ",", na.strings = c("NA", "NaN", "-", "#N/A", "Insufici", "não tem")
)
str(ctb0053_layer)

# Process fields
# observacao_id
# old: IDENTIFICAÇÃO RO-LOCAL
# The field "IDENTIFICAÇÃO RO-LOCAL" supposedly is the same as "UA" in the event table. The
# difference appears to be that "UA" uses "-_", while "IDENTIFICAÇÃO RO-LOCAL" uses "-".
# We convert all to "_". Also, "IDENTIFICAÇÃO RO-LOCAL" uses three digits do identify the soil
# profile. We will remove the zeros to the left.
data.table::setnames(ctb0053_layer, old = "IDENTIFICAÇÃO RO-LOCAL", new = "observacao_id")
ctb0053_layer[, observacao_id := as.character(observacao_id)]
ctb0053_layer[, observacao_id := gsub("-00", "_", observacao_id)]
ctb0053_layer[, observacao_id := gsub("-0", "_", observacao_id)]
ctb0053_layer[, observacao_id := gsub("-", "_", observacao_id)]
ctb0053_layer[, .N, by = observacao_id]
# Each event has two samples points. One consists of undisturbed soil samples, and the other
# consists of disturbed soil samples. This is recorded in the "COLETA - Tipo" column. We will
# rename the events to reflect this.
ctb0053_layer[, observacao_id := paste0(observacao_id, "_", `COLETA - Tipo`)]
ctb0053_layer[, `COLETA - Tipo` := NULL]
ctb0053_layer[, .N, by = observacao_id]

# camada_nome
# old: COLETA - Profundidade (cm)
# new: camada_nome
data.table::setnames(ctb0053_layer, old = "COLETA - Profundidade (cm)", new = "camada_nome")
ctb0053_layer[, camada_nome := as.character(camada_nome)]
ctb0053_layer[, .N, by = camada_nome]

# amostra_id
# old: PROTOCOLO DO LABORATÓRIO
# new: amostra_id
data.table::setnames(ctb0053_layer, old = "PROTOCOLO DO LABORATÓRIO", new = "amostra_id")
ctb0053_layer[, amostra_id := as.character(amostra_id)]
ctb0053_layer[, .N, by = amostra_id]

# profund_sup
# We have to get the depth from the "camada_nome" field.
ctb0053_layer[, profund_sup := strsplit(camada_nome, "-")[[1]][1], by = .I]
ctb0053_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0053_layer[, profund_sup])

# profund_inf
# We have to get the depth from the "camada_nome" field.
ctb0053_layer[, profund_inf := strsplit(camada_nome, "-")[[1]][2], by = .I]
ctb0053_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0053_layer[, profund_inf])

# camada_id
# We will create a unique identifier for each layer.
ctb0053_layer <- ctb0053_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0053_layer[, camada_id := 1:.N, by = observacao_id]
ctb0053_layer[, .N, by = camada_id]

# check for missing layers
check_missing_layer(ctb0053_layer)
# add missing layers: all samples are from 0-20 and 30-50 cm
ctb0053_layer <- add_missing_layer(ctb0053_layer)
check_missing_layer(ctb0053_layer)

# create layer name
ctb0053_layer[is.na(camada_nome), camada_nome := paste0(profund_sup, "-", profund_inf)]
ctb0053_layer[, .N, by = camada_nome]

# compute mid depth
ctb0053_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0053_layer[, profund_mid])

# terrafina
# Data on the fine earth fraction is missing. We set it to NA_real_.
ctb0053_layer[, terrafina := NA_real_]

# argila
# old: Argila (g/Kg)
# new: argila
data.table::setnames(ctb0053_layer, old = "Argila (g/Kg)", new = "argila")
ctb0053_layer[, argila := as.numeric(argila)]
check_empty_layer(ctb0053_layer, "argila")
# fill empty layers
ctb0053_layer[, argila := fill_empty_layer(y = argila, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0053_layer, "argila")
summary(ctb0053_layer[, argila])

# silte
# old: Silte (g/Kg)
# new: silte
data.table::setnames(ctb0053_layer, old = "Silte (g/Kg)", new = "silte")
ctb0053_layer[, silte := as.numeric(silte)]
check_empty_layer(ctb0053_layer, "silte")
# fill empty layers
ctb0053_layer[, silte := fill_empty_layer(y = silte, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0053_layer, "silte")
summary(ctb0053_layer[, silte])

# areia
# old: A.Grossa (g/Kg) + A.Fina (g/Kg)
# new: areia
data.table::setnames(ctb0053_layer, old = "A.Grossa (g/Kg)", new = "areia_grossa")
data.table::setnames(ctb0053_layer, old = "A.Fina (g/Kg)", new = "areia_fina")
ctb0053_layer[, areia := as.numeric(areia_grossa) + as.numeric(areia_fina)]
check_empty_layer(ctb0053_layer, "areia")
# fill empty layers
ctb0053_layer[, areia := fill_empty_layer(y = areia, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0053_layer, "areia")
summary(ctb0053_layer[, areia])

# carbono
# old: C (g/Kg)
# new: carbono
data.table::setnames(ctb0053_layer, old = "C (g/Kg)", new = "carbono")
ctb0053_layer[, carbono := as.numeric(carbono)]
check_empty_layer(ctb0053_layer, "carbono")
# fill empty layers
ctb0053_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0053_layer, "carbono")
summary(ctb0053_layer[, carbono])

# ph
# old: pH (H2O)
# new: ph
data.table::setnames(ctb0053_layer, old = "pH (H2O)", new = "ph")
ctb0053_layer[, ph := as.numeric(ph)]
check_empty_layer(ctb0053_layer, "ph")
# fill empty layers
ctb0053_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0053_layer, "ph")
summary(ctb0053_layer[, ph])

# ctc
# old: CTC Total pH 7
# new: ctc
data.table::setnames(ctb0053_layer, old = "CTC Total pH 7", new = "ctc")
ctb0053_layer[, ctc := as.numeric(ctc)]
check_empty_layer(ctb0053_layer, "ctc")
# fill empty layers
ctb0053_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0053_layer, "ctc")
summary(ctb0053_layer[, ctc])

# dsi
# Bulk soil density is missing. We set it to NA_real_
# The study reports the particle density
ctb0053_layer[, dsi := NA_real_]

str(ctb0053_layer)

# merge ############################################################################################
ctb0053_layer[, observacao_id_new := observacao_id] # Save the new observacao_id
ctb0053_layer[, observacao_id := gsub("_INDEFORMADA", "", observacao_id)]
ctb0053_layer[, observacao_id := gsub("_GRANEL", "", observacao_id)]

# events and layers
ctb0053 <- merge(ctb0053_event, ctb0053_layer, all = TRUE)

# Reset observacao_id
# Note that some events do not have layers.
ctb0053[!is.na(observacao_id_new), observacao_id := observacao_id_new]
ctb0053[, observacao_id_new := NULL]
ctb0053[, observacao_id]

# Three layers have no corresponding event data (RO_365_GRANEL, RO_365_INDEFORMADA, and
# RO_503_GRANEL). We set data_ano to the most frequent value in the dataset and set ano_fonte =
# "estimativa"
ctb0053[is.na(data_ano), ano_fonte := "estimativa"]
ctb0053[is.na(data_ano), data_ano := as.integer(names(which.max(table(ctb0053$data_ano))))]

# Set the dataset_id
ctb0053[, dataset_id := "ctb0053"]
# citation
ctb0053 <- merge(ctb0053, ctb0053_citation, by = "dataset_id", all.x = TRUE)

# Jitter coordinates of events to pass checks for duplicated observations
# Start by creating spatial object
ctb0053_sf <- sf::st_as_sf(
  ctb0053[coord_datum == 4326, ],
  coords = c("coord_x", "coord_y"), crs = 4326
)
# Then transform to projected coordinates
ctb0053_sf <- sf::st_transform(ctb0053_sf, crs = 32720)
ctb0053_sf <- data.table(
  observacao_id = ctb0053_sf$observacao_id,
  coord_x = sf::st_coordinates(ctb0053_sf)[, 1],
  coord_y = sf::st_coordinates(ctb0053_sf)[, 2]
)
# Jitter coordinates
set.seed(1984) # For reproducibility
ctb0053_sf[, coord_x := coord_x + runif(1, -1, 1), by = observacao_id]
set.seed(2001)
ctb0053_sf[, coord_y := coord_y + runif(1, -1, 1), by = observacao_id]
# Then transform back to WGS84
ctb0053_sf <- sf::st_as_sf(
  ctb0053_sf,
  coords = c("coord_x", "coord_y"), crs = 32720
)
ctb0053_sf <- sf::st_transform(ctb0053_sf, crs = 4326)
ctb0053[coord_datum == 4326, coord_x := sf::st_coordinates(ctb0053_sf)[, 1]]
ctb0053[coord_datum == 4326, coord_y := sf::st_coordinates(ctb0053_sf)[, 2]]
rm(ctb0053_sf)

summary_soildata(ctb0053)
# Layers: 1900
# Events: 722
# Georeferenced events: 614

# Plot with mapview
if (FALSE) {
  ctb0053_sf <- sf::st_as_sf(
    ctb0053[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0053_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0053 <- select_output_columns(ctb0053)
data.table::fwrite(ctb0053, "ctb0053/ctb0053.csv")
# event and layer tables were not processed
# data.table::fwrite(ctb0053_event, "ctb0053/ctb0053_event.csv")
# data.table::fwrite(ctb0053_layer, "ctb0053/ctb0053_layer.csv")
