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
# ctb0025
# Os solos do Vale dos Vinhedos
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0025/2022-03-17-ctb0025.xlsx")

# citation #########################################################################################
ctb0025_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0025_citation <- data.table::as.data.table(ctb0025_citation)
str(ctb0025_citation)

# dataset_titulo
dataset_titulo <- ctb0025_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0025_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0025_citation <- data.table::data.table(
  dataset_id = "ctb0025",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0025_citation)

# event ############################################################################################
ctb0025_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0025_event <- data.table::as.data.table(ctb0025_event)
str(ctb0025_event)

# Process fields
# observacao_id
ctb0025_event[, observacao_id := as.character(observacao_id)]
ctb0025_event[, .N, by = observacao_id][N > 1]

# evento_tipo
ctb0025_event[grepl("Perfil", observacao_id), evento_tipo := "Perfil"]
ctb0025_event[grepl("Ponto", observacao_id), evento_tipo := "Tradagem"]
ctb0025_event[, .N, by = evento_tipo]

# observacao_data -> data_ano
data.table::setnames(ctb0025_event, old = "observacao_data", new = "data_ano")
t0 <- "1899-12-30"
ctb0025_event[, data_ano := as.Date(data_ano, origin = t0, format = "%Y-%m-%d")]
ctb0025_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0025_event[, .N, by = data_ano]

# data_fonte
# A data de coleta dos perfis de solo está especificada no documento de origem dos dados.
ctb0025_event[!is.na(data_ano), data_fonte := "original"]
ctb0025_event[, .N, by = data_fonte]

# Auger holes are missing data_ano. We set it to 2006 or 2007.
ctb0025_event[is.na(data_ano), .(observacao_id, data_ano)]
ctb0025_event[
  is.na(data_ano),
  data_ano := sample(c(2006, 2007), sum(is.na(data_ano)), replace = TRUE)
]
ctb0025_event[, .N, by = data_ano]

# data_fonte
# A data de coleta das tradagens não está especificada no documento de origem dos dados.
ctb0025_event[is.na(data_fonte), data_fonte := "estimativa"]
ctb0025_event[, .N, by = data_fonte]

# coord_x
# old: coord_longitude
# new: coord_x
data.table::setnames(ctb0025_event, old = "coord_longitude", new = "coord_x")
ctb0025_event[, coord_x := as.numeric(coord_x)]
summary(ctb0025_event[, coord_x])

# coord_y
# old: coord_latitude
# new: coord_y
data.table::setnames(ctb0025_event, old = "coord_latitude", new = "coord_y")
ctb0025_event[, coord_y := as.numeric(coord_y)]
summary(ctb0025_event[, coord_y])

# check for duplicate coordinates
# observacao_id Perfil-3 and Perfil-4 have the same coordinates
# It is difficult to say which are the correct coordinates: based on the elevation, the coordinates
# seem to match those of the Perfil-4. So we will keep both, adding a small random number (jitter)
# to Perfil-3.
ctb0025_event[, .N, by = .(coord_x, coord_y)][N > 1]
ctb0025_event[observacao_id %in% c("Perfil-3", "Perfil-4"), .(observacao_id, coord_y, coord_x)]
ctb0025_event_sf <- sf::st_as_sf(
  ctb0025_event[observacao_id == "Perfil-3", ],
  coords = c("coord_x", "coord_y"), crs = 4326
)
ctb0025_event_sf <- sf::st_transform(ctb0025_event_sf, 32724)
ctb0025_event_sf <- sf::st_jitter(ctb0025_event_sf, amount = 30)
ctb0025_event_sf <- sf::st_transform(ctb0025_event_sf, 4326)
ctb0025_event_sf <- sf::st_coordinates(ctb0025_event_sf)
ctb0025_event[observacao_id == "Perfil-3", coord_x := ctb0025_event_sf[, 1]]
ctb0025_event[observacao_id == "Perfil-3", coord_y := ctb0025_event_sf[, 2]]
rm(ctb0025_event_sf)

# coord_datum
# old: coord_datum_epsg
# new: coord_datum
data.table::setnames(ctb0025_event, old = "coord_datum_epsg", new = "coord_datum")
ctb0025_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0025_event[, coord_datum := as.integer(coord_datum)]
ctb0025_event_sf <- sf::st_as_sf(ctb0025_event, coords = c("coord_x", "coord_y"), crs = 4674)
ctb0025_event_sf <- sf::st_transform(ctb0025_event_sf, crs = 4326)
ctb0025_event_sf <- sf::st_coordinates(ctb0025_event_sf)
ctb0025_event[, coord_x := ctb0025_event_sf[, 1]]
ctb0025_event[, coord_y := ctb0025_event_sf[, 2]]
ctb0025_event[, coord_datum := 4326]
rm(ctb0025_event_sf)
ctb0025_event[, .N, by = coord_datum]

# coord_fonte
ctb0025_event[, coord_fonte := as.character(coord_fonte)]
ctb0025_event[, .N, by = coord_fonte]

# coord_precisao
# coord_precisao is missing. Because coord_fonte is GPS, we assume it is 30 m
ctb0025_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0025_event[!is.na(coord_fonte), coord_precisao := 30]
summary(ctb0025_event[, coord_precisao])

# pais_id
ctb0025_event[, pais_id := as.character(pais_id)]
ctb0025_event[, .N, by = pais_id]

# estado_sigla -> estado_id
data.table::setnames(ctb0025_event, old = "estado_sigla", new = "estado_id")
ctb0025_event[, estado_id := as.character(estado_id)]
ctb0025_event[, .N, by = estado_id]

# municipio_nome -> municipio_id
data.table::setnames(ctb0025_event, old = "municipio_nome", new = "municipio_id")
ctb0025_event[, municipio_id := as.character(municipio_id)]
ctb0025_event[, .N, by = municipio_id]

# amostra_area
# amostra_area is missing. Because these are soil profiles and boreholes, we assume it is 1.0 m^2.
ctb0025_event[, amostra_area := 1.0]

# taxon_sibcs_2006 -> taxon_sibcs
data.table::setnames(ctb0025_event, old = "taxon_sibcs_2006", new = "taxon_sibcs")
ctb0025_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0025_event[, .N, by = taxon_sibcs]

# replace accented characters with unaccented characters
old <- "áàâãéèêíìóòôõúùçÁÀÂÃÉÈÊÍÌÓÒÔÕÚÙÇ"
new <- "aaaeeeiiioooouucAAAEEEIIIOOOOUUC"
ctb0025_event[, taxon_sibcs := chartr(old = old, new = new, x = taxon_sibcs)]

# Extract taxonomic levels
ctb0025_event[, sibcs_order := strsplit(taxon_sibcs, " ", fixed = TRUE)[[1]][1], by = .I]
ctb0025_event[, sibcs_suborder := strsplit(taxon_sibcs, " ", fixed = TRUE)[[1]][2], by = .I]
ctb0025_event[, sibcs := paste(sibcs_order, sibcs_suborder), by = .I]
ctb0025_event[, sibcs_order := NULL]
ctb0025_event[, sibcs_suborder := NULL]
ctb0025_event[, .N, by = sibcs]

# taxon_st
# A classificação do solo segundo o Soil Taxonomy não foi realizada.
ctb0025_event[, taxon_st := NA_character_]

str(ctb0025_event)

# layer ############################################################################################
ctb0025_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0025_layer <- data.table::as.data.table(ctb0025_layer)
str(ctb0025_layer)

# Process fields
# observacao_id
ctb0025_layer[, observacao_id := as.character(observacao_id)]
ctb0025_layer[, .N, by = observacao_id]

# camada_nome
ctb0025_layer[, camada_nome := as.character(camada_nome)]
ctb0025_layer[, .N, by = camada_nome]

# amostra_id
ctb0025_layer[, amostra_id := as.character(amostra_id)]
ctb0025_layer[, .N, by = amostra_id]

# profund_sup
ctb0025_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0025_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0025_layer[, profund_sup])

# profund_inf
ctb0025_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0025_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0025_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0025_layer[, profund_inf])

# check for missing layers
any_missing_layer(ctb0025_layer)

# camada_id
ctb0025_layer <- ctb0025_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0025_layer[, camada_id := 1:.N, by = observacao_id]
ctb0025_layer[, .N, by = camada_id]

# terrafina_peneira -> terrafina
data.table::setnames(ctb0025_layer, old = "terrafina_peneira", new = "terrafina")
ctb0025_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0025_layer[, terrafina])
ctb0025_layer[is.na(terrafina), .(observacao_id, camada_nome, terrafina)]

# argila_naoh_densimetro -> argila
data.table::setnames(ctb0025_layer, old = "argila_naoh_densimetro", new = "argila")
ctb0025_layer[, argila := as.numeric(argila)]
ctb0025_layer[
  observacao_id == "Perfil-88" & camada_nome == "Ap",
  argila := ifelse(argila == 32, 320, argila)
]
summary(ctb0025_layer[, argila])
ctb0025_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# silte_0002mm0050mm_calc -> silte
# O conteúdo de silte_0002mm0050mm_calc registrado para a mesma camada é de 30 g/kg.
# Deduzo que o valor correto seja de aproximadamente 130 g/kg.
data.table::setnames(ctb0025_layer, old = "silte_0002mm0050mm_calc", new = "silte")
ctb0025_layer[, silte := as.numeric(silte)]
ctb0025_layer[
  observacao_id == "Perfil-33" & camada_nome == "A2",
  silte := ifelse(silte == 30, 130, silte)
]
ctb0025_layer[
  observacao_id == "Perfil-56" & camada_nome == "A2",
  silte := ifelse(silte == 32, 320, silte)
]
summary(ctb0025_layer[, silte])

# areia
# areia_0200mm2000mm_calc + areia_0050mm0200mm_peneira
# O conteúdo de areia_0050mm0200mm_peneira registrado para Perfil-33, camada A2, é de 130 g/kg.
# Deduzo que o correto seja próximo de 230 g/kg.
ctb0025_layer[, areia := as.numeric(areia_0200mm2000mm_calc)]
ctb0025_layer[
  observacao_id == "Perfil-33" & camada_nome == "A2",
  areia := ifelse(areia == 310, 310 + 230, areia)
]
ctb0025_layer[, areia := areia + as.numeric(areia_0050mm0200mm_peneira)]
summary(ctb0025_layer[, areia])

# carbono_cromo_xxx_mohr -> carbono
data.table::setnames(ctb0025_layer, old = "carbono_cromo_xxx_mohr", new = "carbono")
ctb0025_layer[, carbono := as.numeric(carbono)]
summary(ctb0025_layer[, carbono])

# ctc_soma_calc -> ctc
data.table::setnames(ctb0025_layer, old = "ctc_soma_calc", new = "ctc")
ctb0025_layer[, ctc := as.numeric(ctc)]
summary(ctb0025_layer[, ctc])

# ph_h2o_25_eletrodo -> ph
data.table::setnames(ctb0025_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0025_layer[, ph := as.numeric(ph)]
summary(ctb0025_layer[, ph])

# dsi
ctb0025_layer[, dsi := NA_real_]

# Merge ########################################################################################### 

# Data imputation
# The data.table object ctb0025_event contains soil observations from auger holes and soil profiles.
# Soil observations from auger holes do not contain layer data (stored in ctb0025_layer): soil
# observations containing layers are tagged using the has_layers field.
# For soil observations with the same classification (sibcs), we find the nearest soil profile. We
# consider the nearest soil profile to be the one with the smallest Euclidean distance in space
# as measured between coordinates (coord_x and coord_y) to the auger hole. We use the data from the
# nearest soil profile to impute the missing data in the soil observations from auger holes.
ctb0025_event[, has_layer := observacao_id %in% ctb0025_layer$observacao_id]
ctb0025_event[, .N, by = has_layer]
ctb0025_event_sf <- sf::st_as_sf(ctb0025_event, coords = c("coord_x", "coord_y"), crs = 4326)
ctb0025_event_sf <- sf::st_transform(ctb0025_event_sf, crs = 29101)

# Identify the unique 'sibcs' values that appear in the dataset where layer data is unavailable
unique_sibcs <- ctb0025_event[has_layer == FALSE, unique(sibcs)]
# now do the same for the dataset where layer data is available
unique_sibcs <- unique_sibcs[unique_sibcs %in% ctb0025_event[has_layer == TRUE, unique(sibcs)]]
for (i in unique_sibcs) {
  ctb0025_event_dist <- sf::st_distance(
    x = ctb0025_event_sf[ctb0025_event_sf$sibcs == i & ctb0025_event_sf$has_layer == FALSE, ],
    y = ctb0025_event_sf[ctb0025_event_sf$sibcs == i & ctb0025_event_sf$has_layer == TRUE, ]
  )
  id_nearest <- apply(ctb0025_event_dist, 1, which.min)
  nearest_event <- ctb0025_event[sibcs == i & has_layer == TRUE, observacao_id][id_nearest]
  ctb0025_event[sibcs == i & has_layer == FALSE, nearest := nearest_event]
}
# Display nearest soil profiles for auger holes
ctb0025_event[has_layer == FALSE, .(observacao_id, sibcs, nearest)]

# Merge events and layers
ctb0025 <- merge(ctb0025_event, ctb0025_layer, all = TRUE)

# Data imputation
get_cols <- c(
  "camada_nome", "camada_id", "amostra_id", "profund_sup", "profund_inf",
  "terrafina", "argila", "silte", "areia", "carbono", "ctc", "ph", "dsi"
)
auger_holes <- unique(ctb0025$observacao_id[ctb0025$has_layer == FALSE])
for (i in auger_holes) {
  # Get the nearest soil profile
  nearest_id <- ctb0025[observacao_id == i, nearest]
  # Get the data from the nearest soil profile
  data_nearest <- ctb0025[observacao_id == nearest_id, ..get_cols]
  # Get original data
  data_original <- ctb0025[observacao_id == i, -..get_cols, with = FALSE]
  # Combine the data
  auger <- cbind(data_original, data_nearest, fill = TRUE)
  auger[, fill := NULL]
  # Drop the auger hole data
  ctb0025 <- ctb0025[observacao_id != i, ]
  # Add the combined data
  ctb0025 <- rbind(ctb0025, auger)
}
ctb0025[, nearest := NULL]
ctb0025[, has_layer := NULL]
ctb0025[, sibcs := NULL]

# Add dataset_id
ctb0025[, dataset_id := "ctb0025"]
# citation
ctb0025 <- merge(ctb0025, ctb0025_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0025)
# Layers: 869
# Events: 237
# Georeferenced events: 237

# Plot using mapview
if (FALSE) {
  ctb0025_sf <- sf::st_as_sf(
    ctb0025[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0025_sf["argila"])
}

# Write to disk ####################################################################################
ctb0025 <- select_output_columns(ctb0025)
data.table::fwrite(ctb0025, "ctb0025/ctb0025.csv")
data.table::fwrite(ctb0025_event, "ctb0025/ctb0025_event.csv")
data.table::fwrite(ctb0025_layer, "ctb0025/ctb0025_layer.csv")
