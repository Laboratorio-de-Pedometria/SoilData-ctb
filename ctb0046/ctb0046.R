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
if (!require("openxlsx")) {
  install.packages("openxlsx")
}
if (!require("mapview")) {
  install.packages("mapview")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0046
# Levantamento de reconhecimento de alta intensidade dos solos de duas áreas dotadas de solos
# arenosos no Estado de Mato Grosso - Áreas Piloto I e II
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0046/2022-02-02-ctb0046.xlsx")

# citation #########################################################################################
ctb0046_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0046_citation <- data.table::as.data.table(ctb0046_citation)
str(ctb0046_citation)

# dataset_titulo
dataset_titulo <- ctb0046_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0046_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0046_citation <- data.table::data.table(
  dataset_id = "ctb0046",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0046_citation)

# event ############################################################################################
ctb0046_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0046_event <- data.table::as.data.table(ctb0046_event)
str(ctb0046_event)

# Process fields
# observacao_id
ctb0046_event[, observacao_id := as.character(observacao_id)]
ctb0046_event[, .N, by = observacao_id][N > 1]

# data_ano
# observacao_data
data.table::setnames(ctb0046_event, old = "observacao_data", new = "data_ano")
t0 <- "1899-12-30"
ctb0046_event[, data_ano := as.Date(data_ano, origin = t0)]
ctb0046_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0046_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0046_event[, ano_fonte := "original"]
ctb0046_event[, .N, by = ano_fonte]

# coord_x
ctb0046_event[, coord_x := as.numeric(coord_x)]
summary(ctb0046_event[, coord_x])

# coord_y
ctb0046_event[, coord_y := as.numeric(coord_y)]
summary(ctb0046_event[, coord_y])

# coord_datum
# coord_sistema
data.table::setnames(ctb0046_event, old = "coord_sistema", new = "coord_datum")
ctb0046_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0046_event[, .N, by = coord_datum]
# 31981 -> 4326
ctb0046_event_sf <- sf::st_as_sf(ctb0046_event[coord_datum == "31981"],
  coords = c("coord_x", "coord_y"), crs = 31981
)
ctb0046_event_sf <- sf::st_transform(ctb0046_event_sf, crs = 4326)
ctb0046_event_sf <- sf::st_coordinates(ctb0046_event_sf)
ctb0046_event[coord_datum == "31981", coord_x := ctb0046_event_sf[, 1]]
ctb0046_event[coord_datum == "31981", coord_y := ctb0046_event_sf[, 2]]
ctb0046_event[coord_datum == 31981, coord_datum := 4326]
ctb0046_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0046_event_sf)
ctb0046_event[, .N, by = coord_datum]

# coord_fonte
ctb0046_event[, coord_fonte := as.character(coord_fonte)]
ctb0046_event[, .N, by = coord_fonte]

# coord_precisao
# coord_precisao is missing. Because coord_fonte is "GPS", we can assume a precision of 30 meters.
ctb0046_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0046_event[is.na(coord_precisao), coord_precisao := 30]
summary(ctb0046_event[, coord_precisao])

# pais_id
# pais_id is missing
ctb0046_event[, pais_id := "BR"]

# estado_id
# estado_id is missing
ctb0046_event[, estado_id := "MT"]

# municipio_id
# municipio_id is missing
ctb0046_event[, municipio_id := NA_character_]

# amostra_area
# amostra_area is missing. Because these are soil profiles, we can assume an area of 1 m².
ctb0046_event[, amostra_area := 1]

# taxon_sibcs
# old: taxon_sibcs_xxx
data.table::setnames(ctb0046_event, old = "taxon_sibcs_xxx", new = "taxon_sibcs")
ctb0046_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0046_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0046_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# this document don't  have pedregosidade info

ctb0046_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# this document don't  have rochosidade info

ctb0046_event[, rochosidade := NA_character_]

str(ctb0046_event)

# layer ############################################################################################
ctb0046_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0046_layer <- data.table::as.data.table(ctb0046_layer)
str(ctb0046_layer)

# Process fields
# observacao_id
ctb0046_layer[, observacao_id := as.character(observacao_id)]
ctb0046_layer[, .N, by = observacao_id]

# camada_nome
ctb0046_layer[, camada_nome := as.character(camada_nome)]
ctb0046_layer[, .N, by = camada_nome]

# amostra_id
ctb0046_layer[, amostra_id := as.character(amostra_id)]
ctb0046_layer[, .N, by = amostra_id]

# profund_sup
ctb0046_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0046_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0046_layer[, profund_sup])

# profund_inf
ctb0046_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0046_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0046_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0046_layer[, profund_inf])

# camada_id
ctb0046_layer <- ctb0046_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0046_layer[, camada_id := 1:.N, by = observacao_id]
ctb0046_layer[, .N, by = camada_id]

# check for missing layers
any_missing_layer(ctb0046_layer)
idx <- any_missing_layer(ctb0046_layer)[, observacao_id]
ctb0046_layer[observacao_id %in% idx, .(observacao_id, camada_nome, profund_sup, profund_inf)]
# there are 47 events (AMOSTRA-EXTRA) missing and intermediary layer
# we simply add the missing layer
ctb0046_layer <- add_missing_layer(ctb0046_layer)

# mid depth
ctb0046_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0046_layer[, profund_mid])

# terrafina
# old: terrafina_xxx
# these are all sandy soils, so we can assume terrafina = 1000 g/kg
data.table::setnames(ctb0046_layer, old = "terrafina_xxx", new = "terrafina")
ctb0046_layer[, terrafina := as.numeric(terrafina)]
ctb0046_layer[is.na(terrafina), terrafina := 1000]
summary(ctb0046_layer[, terrafina])

# argila
# argila_naoh_xxx
data.table::setnames(ctb0046_layer, old = "argila_naoh_xxx", new = "argila")
ctb0046_layer[, argila := as.numeric(argila)]
summary(ctb0046_layer[, argila])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0046_layer[, argila := fill_empty_layer(y = argila, x = profund_mid), by = observacao_id]
summary(ctb0046_layer[, argila])

# silte
# silte_0002mm0050mm_calc
data.table::setnames(ctb0046_layer, old = "silte_0002mm0050mm_calc", new = "silte")
ctb0046_layer[, silte := as.numeric(silte)]
summary(ctb0046_layer[, silte])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0046_layer[, silte := fill_empty_layer(y = silte, x = profund_mid), by = observacao_id]
summary(ctb0046_layer[, silte])

# areia
# areia_0250mm2000mm_xxx + areia_0100mm0250mm_xxx + areia_0050mm0100mm_xxx
data.table::setnames(ctb0046_layer, old = "areia_0250mm2000mm_xxx", new = "areia_grossa")
data.table::setnames(ctb0046_layer, old = "areia_0100mm0250mm_xxx", new = "areia_media")
data.table::setnames(ctb0046_layer, old = "areia_0050mm0100mm_xxx", new = "areia_fina")
ctb0046_layer[, areia_grossa := as.numeric(areia_grossa)]
ctb0046_layer[, areia_media := as.numeric(areia_media)]
ctb0046_layer[, areia_fina := as.numeric(areia_fina)]
ctb0046_layer[, areia := areia_grossa + areia_media + areia_fina]
summary(ctb0046_layer[, areia])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0046_layer[, areia := fill_empty_layer(y = areia, x = profund_mid), by = observacao_id]
summary(ctb0046_layer[, areia])

# Check the particle size distribution
# The sum of the particle size distribution should be 1000.
ctb0046_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
ctb0046_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 2 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd not in the range
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0046_layer[!psd %in% psd_lims & !is.na(psd), ..cols]
# These are typos in the source document. Follows the correction:
ctb0046_layer[observacao_id == "PERFIL-01" & camada_nome == "CA", areia == 540 + 320 + 130]
ctb0046_layer[observacao_id == "PERFIL-01" & camada_nome == "C2", areia == 470 + 330 + 90]
# For the remaining layers, we will correct them by distributing the error evenly.
ctb0046_layer[psd != 1000, argila := round(argila / psd * 1000)]
ctb0046_layer[psd != 1000, silte := round(silte / psd * 1000)]
ctb0046_layer[psd != 1000, areia := round(areia / psd * 1000)]
# Check the particle size distribution again
ctb0046_layer[, psd := argila + silte + areia]
ctb0046_layer[psd != 1000, ..cols]
ctb0046_layer[, psd := NULL]

# carbono
# carbono_xxx_xxx_xxx
data.table::setnames(ctb0046_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0046_layer[, carbono := as.numeric(carbono)]
summary(ctb0046_layer[, carbono])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0046_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
summary(ctb0046_layer[, carbono])

# ph
# ph_h2o_25_eletrodo
data.table::setnames(ctb0046_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0046_layer[, ph := as.numeric(ph)]
summary(ctb0046_layer[, ph])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0046_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
summary(ctb0046_layer[, ph])

# ctc
# ctc_soma_calc
data.table::setnames(ctb0046_layer, old = "ctc_soma_calc", new = "ctc")
ctb0046_layer[, ctc := as.numeric(ctc)]
summary(ctb0046_layer[, ctc])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0046_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
summary(ctb0046_layer[, ctc])

# dsi
# densidade_solo_xxx
data.table::setnames(ctb0046_layer, old = "densidade_solo_xxx", new = "dsi")
ctb0046_layer[, dsi := as.numeric(dsi)]
summary(ctb0046_layer[, dsi])
# Fill missing values with the average of adjacent (lower and upper) layers of the same
# event (observacao_id). Use mid depth as predictor.
ctb0046_layer[, dsi := fill_empty_layer(y = dsi, x = profund_mid), by = observacao_id]
summary(ctb0046_layer[, dsi])

str(ctb0046_layer)

# Merge ############################################################################################
# events and layers
ctb0046 <- merge(ctb0046_event, ctb0046_layer, all = TRUE)
ctb0046[, dataset_id := "ctb0046"]
# citation
ctb0046 <- merge(ctb0046, ctb0046_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0046)
# Layers: 230
# Events: 68
# Georeferenced events: 68

# Plot with mapview
if (FALSE) {
  ctb0046_sf <- sf::st_as_sf(
    ctb0046[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0046_sf["argila"])
}

# Write to disk ####################################################################################
ctb0046 <- select_output_columns(ctb0046)
data.table::fwrite(ctb0046, "ctb0046/ctb0046.csv")
data.table::fwrite(ctb0046_event, "ctb0046/ctb0046_event.csv")
data.table::fwrite(ctb0046_layer, "ctb0046/ctb0046_layer.csv")
