# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library("openxlsx")
}
if (!require("sf")) {
  install.packages("sf")
  library("sf")
}
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0006
# Zoneamento edáfico de culturas para o Município de Santa Maria - RS, visando o ordenamento
# territorial
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0006/2022-05-25-ctb0006.xlsx")

# citation #########################################################################################
ctb0006_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0006_citation <- data.table::as.data.table(ctb0006_citation)
str(ctb0006_citation)

# dataset_titulo
dataset_titulo <- ctb0006_citation[campo == "dados_titulo", valor]
# dataset_licenca
dataset_licenca <- ctb0006_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0006_citation <- data.table::data.table(
  dataset_id = "ctb0006",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0006_citation)

# event ############################################################################################
ctb0006_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0006_event <- data.table::as.data.table(ctb0006_event)
str(ctb0006_event)

# Process fields

# observacao_id
ctb0006_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0006_event[, observacao_id]) > 1)

# data_ano
# observacao_data
data.table::setnames(ctb0006_event, old = "observacao_data", new = "data_ano")
ctb0006_event[data_ano > 2014, data_ano := 2014]
ctb0006_event[, data_ano := as.integer(data_ano)]
ctb0006_event[, .N, by = data_ano]

# ano_fonte
ctb0006_event[, ano_fonte := "original"]
ctb0006_event[, .N, by = ano_fonte]

# coord_datum
# coord_sistema
data.table::setnames(ctb0006_event, old = "coord_sistema", new = "coord_datum")
# Harmonize the coordinate reference system as registered in the coord_sistema column
# Start by splitting the dataset according to the coordinate reference system. Then, transform each
# subset to a spatial object with the respective coordinate reference system. Next, transform
# the coordinate reference system to WGS 84 (EPSG:4326). Finally, merge the subsets back into a
# single dataset and convert it to a data.table, including the coordinates in the data.table object.
# The coordinate reference system is reset to EPSG:4326.
ctb0006_event <- split(ctb0006_event, by = "coord_datum")
ctb0006_event <- lapply(ctb0006_event, function(x) {
  x <- sf::st_as_sf(x, coords = c("coord_x", "coord_y"), crs = x$coord_datum[1])
  x <- sf::st_transform(x, crs = 4326)
  dt <- data.table::as.data.table(x)
  xy <- sf::st_coordinates(x)
  x <- cbind(dt, coord_x = xy[, 1], coord_y = xy[, 2])
  x[, geometry := NULL]
  # Reset the coordinate reference system
  x$coord_datum <- 4326
  return(x)
})
ctb0006_event <- data.table::rbindlist(ctb0006_event, fill = TRUE)

# coord_x
ctb0006_event[, coord_x := as.numeric(coord_x)]
summary(ctb0006_event[, coord_x])

# coord_y
ctb0006_event[, coord_y := as.numeric(coord_y)]
summary(ctb0006_event[, coord_y])

# Check for duplicate coordinates
# observacao_id obs_146 and obs_248
# they are auger holes and have the same taxon_sibcs_2013: discard one of them as they seem to be
# the same observation
ctb0006_event[, .N, by = .(coord_x, coord_y)][N > 1]
ctb0006_event[coord_x == -53.9915 & coord_y == -29.7239, ]
ctb0006_event <- ctb0006_event[observacao_id != "obs_146", ]

# coord_fonte
ctb0006_event[, coord_fonte := as.character(coord_fonte)]
ctb0006_event[, .N, by = coord_fonte]

# coord_precisao
# coord_precisao is missing. We assume it is 30 m because the data was collected with a GPS
ctb0006_event[, coord_precisao := 30.0]

# pais_id
ctb0006_event[, pais_id := "BR"]

# estado_id
ctb0006_event[, estado_id := "RS"]

# municipio_id
ctb0006_event[, municipio_id := as.character(municipio_id)]
ctb0006_event[, .N, by = municipio_id]

# amostra_area
# amostra_area is missing. We assume it is 1.0 m^2 (soil profiles)
ctb0006_event[, amostra_area := 1.0]

# taxon_sibcs
# taxon_sibcs_2013
data.table::setnames(ctb0006_event, old = "taxon_sibcs_2013", new = "taxon_sibcs")
ctb0006_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0006_event[, .N, by = taxon_sibcs]

# Extract taxonomic levels
# Soil observations from auger holes are not classified at the same taxonomic level as soil
# profiles: they are classified at the suborder level.
ctb0006_event[, sibcs_order := strsplit(taxon_sibcs, " ")[[1]][1], by = .I]
ctb0006_event[, sibcs_suborder := strsplit(taxon_sibcs, " ")[[1]][2], by = .I]
ctb0006_event[, sibcs := paste(sibcs_order, sibcs_suborder)]
ctb0006_event[, sibcs_order := NULL]
ctb0006_event[, sibcs_suborder := NULL]
ctb0006_event[, .N, by = sibcs]

# taxon_st
# A classificação do solo segundo o Soil Taxonomy não foi realizada.
ctb0006_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# Não tenho acesso a este trabalho após a inserção das variaveis pedregosidade e rochosidade
# Logo, irei colocar NA_character_ para as variaveis.

ctb0006_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# review the work at another time

ctb0006_event[, rochosidade := NA_character_]

str(ctb0006_event)

# layer ############################################################################################
ctb0006_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0006_layer <- data.table::as.data.table(ctb0006_layer)
str(ctb0006_layer)

# Process fields

# observacao_id
ctb0006_layer[, observacao_id := as.character(observacao_id)]
ctb0006_layer[, .N, by = observacao_id]

# camada_nome
ctb0006_layer[, camada_nome := as.character(camada_nome)]
ctb0006_layer[, .N, by = camada_nome]

# amostra_id
ctb0006_layer[, amostra_id := as.integer(amostra_id)]
ctb0006_layer[, .N, by = amostra_id]

# profund_sup
ctb0006_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0006_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0006_layer[, profund_sup])

# profund_inf
ctb0006_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0006_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0006_layer[, profund_inf := as.numeric(profund_inf)]
ctb0006_layer[profund_inf == profund_sup, profund_inf := profund_inf + 20]
summary(ctb0006_layer[, profund_inf])

# missing layers
# are there any?
ctb0006_layer[
  shift(profund_inf) != profund_sup & profund_sup > 0,
  .(observacao_id, profund_sup, profund_inf)
]

# camada_id
# For each observacao_id, sort by profund_sup and profunf_inf
# and assign a unique camada_id
ctb0006_layer <- ctb0006_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0006_layer[, camada_id := as.integer(camada_id)]
ctb0006_layer[, .N, by = camada_id]

# terrafina_xxx -> terrafina
data.table::setnames(ctb0006_layer, old = "terrafina_xxx", new = "terrafina")
ctb0006_layer[, terrafina := as.numeric(terrafina)]
# Correct inconsistent values
ctb0006_layer[
  observacao_id == "perfil-20" & camada_nome == "Btg2",
  terrafina := ifelse(terrafina == 70, 670, terrafina)
]
ctb0006_layer[observacao_id == "perfil-20" & camada_nome == "Btg2", camada_curadoria := paste0(
  "2025-05-03 ASR: O valor original (70 g/kg) foi modificado para 670 g/kg após análise dos dados",
  " do perfil. Provavelmente houve um erro de digitação que aparece no documento de origem dos",
  " dados (planilha de Excel)."
)]
summary(ctb0006_layer[, terrafina])
ctb0006_layer[is.na(terrafina), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]

# argila_xxx_xxx -> argila
data.table::setnames(ctb0006_layer, old = "argila_xxx_xxx", new = "argila")
ctb0006_layer[, argila := as.numeric(argila)]
summary(ctb0006_layer[, argila])
ctb0006_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# silte_xxx_xxx -> silte
data.table::setnames(ctb0006_layer, old = "silte_xxx_xxx", new = "silte")
ctb0006_layer[, silte := as.numeric(silte)]
summary(ctb0006_layer[, silte])
ctb0006_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# areia
ctb0006_layer[, areia := as.numeric(areiagrossa2_xxx_xxx) + as.numeric(areiafina2_xxx_xxx)]
summary(ctb0006_layer[, areia])
ctb0006_layer[is.na(areia), .(observacao_id, camada_nome, profund_sup, profund_inf, areia)]

# carbono_xxx_xxx_xxx -> carbono
data.table::setnames(ctb0006_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0006_layer[, carbono := as.numeric(carbono)]
summary(ctb0006_layer[, carbono])
ctb0006_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]

# ctc_soma_calc -> ctc
data.table::setnames(ctb0006_layer, old = "ctc_soma_calc", new = "ctc")
ctb0006_layer[, ctc := as.numeric(ctc)]
summary(ctb0006_layer[, ctc])
ctb0006_layer[is.na(ctc), .(observacao_id, camada_nome, profund_sup, profund_inf, ctc)]

# ph_h2o_25_eletrodo -> ph
data.table::setnames(ctb0006_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0006_layer[, ph := as.numeric(ph)]
summary(ctb0006_layer[, ph])
ctb0006_layer[is.na(ph), .(observacao_id, camada_nome, profund_sup, profund_inf, ph)]

# dsi
ctb0006_layer[, dsi := NA_real_]

str(ctb0006_layer)

# Merge ############################################################################################

# Data imputation
# The data.table object ctb0006_event contains soil observations from auger holes and soil profiles.
# Soil observations from auger holes do not contain layer data (stored in ctb0006_layer): soil
# observations containing layers are tagged using the has_layers field.
# For soil observations with the same classification (sibcs), we find the nearest soil profile. We
# consider the nearest soil profile to be the one with the smallest Euclidean distance in space
# as measured between coordinates (coord_x and coord_y) to the auger hole. We use the data from the
# nearest soil profile to impute the missing data in the soil observations from auger holes.
ctb0006_event[, has_layer := observacao_id %in% ctb0006_layer$observacao_id]
ctb0006_event[, .N, by = has_layer]

ctb0006_event_sf <- sf::st_as_sf(ctb0006_event, coords = c("coord_x", "coord_y"), crs = 4326)
ctb0006_event_sf <- sf::st_transform(ctb0006_event_sf, crs = 29101)

unique_sibcs <- unique(ctb0006_event$sibcs)
for (i in unique_sibcs) {
  ctb0006_event_dist <- sf::st_distance(
    x = ctb0006_event_sf[ctb0006_event_sf$sibcs == i & ctb0006_event_sf$has_layer == FALSE, ],
    y = ctb0006_event_sf[ctb0006_event_sf$sibcs == i & ctb0006_event_sf$has_layer == TRUE, ]
  )
  id_nearest <- apply(ctb0006_event_dist, 1, which.min)
  nearest_event <- ctb0006_event[sibcs == i & has_layer == TRUE, observacao_id][id_nearest]
  ctb0006_event[sibcs == i & has_layer == FALSE, nearest := nearest_event]
}

# Display nearest soil profiles for auger holes
ctb0006_event[has_layer == FALSE, .(observacao_id, sibcs, nearest)]

# Merge events and layers
ctb0006 <- merge(ctb0006_event, ctb0006_layer, all = TRUE)

# Data imputation
get_cols <- c(
  "camada_nome", "camada_id", "amostra_id", "profund_sup", "profund_inf",
  "terrafina", "argila", "silte", "areia", "carbono", "ctc", "ph", "dsi"
)
auger_holes <- unique(ctb0006$observacao_id[ctb0006$has_layer == FALSE])
for (i in auger_holes) {
  # Get the nearest soil profile
  nearest_id <- ctb0006[observacao_id == i, nearest]
  # Get the data from the nearest soil profile
  data_nearest <- ctb0006[observacao_id == nearest_id, ..get_cols]
  # Get original data
  data_original <- ctb0006[observacao_id == i, -..get_cols, with = FALSE]
  # Combine the data
  auger <- cbind(data_original, data_nearest, fill = TRUE)
  auger[, fill := NULL]
  # Drop the auger hole data
  ctb0006 <- ctb0006[observacao_id != i, ]
  # Add the combined data
  ctb0006 <- rbind(ctb0006, auger)
}
ctb0006[, nearest := NULL]
ctb0006[, has_layer := NULL]
ctb0006[, sibcs := NULL]

# Merge citation data
ctb0006[, dataset_id := "ctb0006"]
ctb0006 <- merge(ctb0006, ctb0006_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0006)
# Layers: 2470
# Events: 604
# Georeferenced events: 604

# Convert to sf and check if the data is correctly georeferenced
if (FALSE) {
  ctb0006_sf <- sf::st_as_sf(
    ctb0006[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0006_sf["argila"])
}

# Write to disk ####################################################################################
ctb0006 <- select_output_columns(ctb0006)
data.table::fwrite(ctb0006, "ctb0006/ctb0006.csv")
data.table::fwrite(ctb0006_event, "ctb0006/ctb0006_event.csv")
data.table::fwrite(ctb0006_layer, "ctb0006/ctb0006_layer.csv")
