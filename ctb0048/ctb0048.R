# author: Alessandro Samuel-Rosa
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("openxlsx")) {
  install.packages("openxlsx")
}
if (!require("mapview")) {
  install.packages("mapview")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0048
# Levantamento semidetalhado de solos do Município de Bela Vista do Paraíso - PR
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0048/2020-04-29-ctb0048.xlsx")

# citation #########################################################################################
ctb0048_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0048_citation <- data.table::as.data.table(ctb0048_citation)
str(ctb0048_citation)

# dataset_titulo
dataset_titulo <- ctb0048_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0048_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0048_citation <- data.table::data.table(
  dataset_id = "ctb0048",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0048_citation)

# event ############################################################################################
ctb0048_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0048_event <- data.table::as.data.table(ctb0048_event)
str(ctb0048_event)

# Process fields
# observacao_id
ctb0048_event[, observacao_id := as.character(observacao_id)]
ctb0048_event[, .N, by = observacao_id][N > 1]

# data_ano
# observacao_data
data.table::setnames(ctb0048_event, old = "observacao_data", new = "data_ano")
ctb0048_event[, data_ano := as.integer(data_ano)]
ctb0048_event[, .N, by = data_ano]

# data_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0048_event[, data_fonte := "original"]
ctb0048_event[, .N, by = data_fonte]

# coord_x
ctb0048_event[, coord_x := as.numeric(coord_x)]
summary(ctb0048_event[, coord_x])

# coord_y
ctb0048_event[, coord_y := as.numeric(coord_y)]
summary(ctb0048_event[, coord_y])

# coord_datum
# coord_sistema -> coord_datum
data.table::setnames(ctb0048_event, old = "coord_sistema", new = "coord_datum")
ctb0048_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0048_event[, .N, by = coord_datum]
# 29192 -> 4326
ctb0048_event_sf <- sf::st_as_sf(ctb0048_event[coord_datum == "29192"],
  coords = c("coord_x", "coord_y"), crs = 29192
)
ctb0048_event_sf <- sf::st_transform(ctb0048_event_sf, crs = 4326)
ctb0048_event_sf <- sf::st_coordinates(ctb0048_event_sf)
ctb0048_event[coord_datum == "29192", coord_x := ctb0048_event_sf[, 1]]
ctb0048_event[coord_datum == "29192", coord_y := ctb0048_event_sf[, 2]]
ctb0048_event[coord_datum == 29192, coord_datum := 4326]
ctb0048_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0048_event_sf)
ctb0048_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0048_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_precisao
ctb0048_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0048_event[, coord_precisao])

# coord_fonte
ctb0048_event[, coord_fonte := as.character(coord_fonte)]
ctb0048_event[, .N, by = coord_fonte]

# pais_id
ctb0048_event[, pais_id := as.character(pais_id)]
ctb0048_event[, .N, by = pais_id]

# estado_id
ctb0048_event[, estado_id := as.character(estado_id)]
ctb0048_event[, .N, by = estado_id]

# municipio_id
ctb0048_event[, municipio_id := as.character(municipio_id)]
ctb0048_event[, .N, by = municipio_id]

# amostra_area
ctb0048_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0048_event[, amostra_area])

# taxon_sibcs
# taxon_sibcs_2006
data.table::setnames(ctb0048_event, old = "taxon_sibcs_2006", new = "taxon_sibcs")
ctb0048_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0048_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0048_event[, taxon_st := NA_character_]

str(ctb0048_event)

# layer ############################################################################################
ctb0048_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0048_layer <- data.table::as.data.table(ctb0048_layer)
str(ctb0048_layer)

# Process fields
# observacao_id
# Each event has only two layers
ctb0048_layer[, observacao_id := as.character(observacao_id)]
ctb0048_layer[, .N, by = observacao_id]

# camada_nome
# Layers are from the A (topsoil) or B (subsoil) horizons
ctb0048_layer[, camada_nome := as.character(camada_nome)]
ctb0048_layer[, .N, by = camada_nome]

# amostra_id
ctb0048_layer[, amostra_id := as.character(amostra_id)]
ctb0048_layer[, .N, by = amostra_id]

# profund_sup
ctb0048_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0048_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0048_layer[, profund_sup])

# profund_inf
ctb0048_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0048_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0048_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0048_layer[, profund_inf])

# camada_id
ctb0048_layer <- ctb0048_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0048_layer[, camada_id := 1:.N, by = observacao_id]
ctb0048_layer[, .N, by = camada_id]

# check for missing layers
any_missing_layer(ctb0048_layer)
# there are 16 events missing intermediary layers: we add them
ctb0048_layer <- add_missing_layer(ctb0048_layer)

# compute mid depth
ctb0048_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0048_layer[, profund_mid])

# terrafina
# terrafina is missing. We assume it is 1000 g/kg because all layers are Ap, Bt, or Bw
ctb0048_layer[, terrafina := 1000]

# argila
# argila_xxx_xxx * 10
data.table::setnames(ctb0048_layer, old = "argila_xxx_xxx", new = "argila")
ctb0048_layer[, argila := as.numeric(argila) * 10]
summary(ctb0048_layer[, argila])
find_empty_layer(ctb0048_layer, "argila")
# Fill empty layers
ctb0048_layer[, argila := fill_empty_layer(y = argila, x = profund_mid), by = observacao_id]
ctb0048_layer[, argila := round(argila)]
find_empty_layer(ctb0048_layer, "argila")
summary(ctb0048_layer[, argila])

# silte
# silte_xxx_xxx * 10
data.table::setnames(ctb0048_layer, old = "silte_xxx_xxx", new = "silte")
ctb0048_layer[, silte := as.numeric(silte) * 10]
summary(ctb0048_layer[, silte])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0048_layer[, silte := fill_empty_layer(y = silte, x = profund_mid), by = observacao_id]
ctb0048_layer[, silte := round(silte)]
summary(ctb0048_layer[, silte])

# areia
# areia_xxx_xxx * 10
data.table::setnames(ctb0048_layer, old = "areia_xxx_xxx", new = "areia")
ctb0048_layer[, areia := as.numeric(areia) * 10]
summary(ctb0048_layer[, areia])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0048_layer[, areia := fill_empty_layer(y = areia, x = profund_mid), by = observacao_id]
ctb0048_layer[, areia := round(areia)]
summary(ctb0048_layer[, areia])

# carbono
# carbono_xxx_xxx_xxx [g/dm^3]
data.table::setnames(ctb0048_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0048_layer[, carbono := as.numeric(carbono)]
summary(ctb0048_layer[, carbono])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0048_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
ctb0048_layer[, carbono := round(carbono)]
summary(ctb0048_layer[, carbono])

# ph
# ph_h2o_xxx_xxx
data.table::setnames(ctb0048_layer, old = "ph_h2o_xxx_xxx", new = "ph")
ctb0048_layer[, ph := as.numeric(ph)]
summary(ctb0048_layer[, ph])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0048_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
ctb0048_layer[, ph := round(ph, 1)]
summary(ctb0048_layer[, ph])

# ctc
# ctc_xxx_xxx [cmol/dm^3]
data.table::setnames(ctb0048_layer, old = "ctc_xxx_xxx", new = "ctc")
ctb0048_layer[, ctc := as.numeric(ctc)]
summary(ctb0048_layer[, ctc])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0048_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
ctb0048_layer[, ctc := round(ctc)]
summary(ctb0048_layer[, ctc])

# dsi
# dsi is missing. We assume it is NA_real_
ctb0048_layer[, dsi := NA_real_]

# Remove unused columns
# profund_mid
ctb0048_layer[, profund_mid := NULL]

str(ctb0048_layer)

# Merge ############################################################################################
# events and layers
ctb0048 <- merge(ctb0048_event, ctb0048_layer, all = TRUE)
ctb0048[, dataset_id := "ctb0048"]
summary_soildata(ctb0048)
# Layers: 57
# Events: 25
# Georeferenced events: 17

# Plot with mapview
if (FALSE) {
  ctb0048_sf <- sf::st_as_sf(
    ctb0048[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0048_sf["argila"])
}

# Write to disk ####################################################################################
ctb0048 <- select_output_columns(ctb0048)
data.table::fwrite(ctb0048, "ctb0048/ctb0048.csv")
data.table::fwrite(ctb0048_event, "ctb0048/ctb0048_event.csv")
data.table::fwrite(ctb0048_layer, "ctb0048/ctb0048_layer.csv")
