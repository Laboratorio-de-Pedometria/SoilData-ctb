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
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library("openxlsx")
}
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0038
# Variabilidade de classes de solos, atributos morfológicos e físico-hídricos no Rebordo do
# Planalto Meridional
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0038/2022-02-02-ctb0038.xlsx")

# Soil profile descriptions are missing: the data is available in the dissertation.
# Data on soil organic carbon, soil classification and 
# Must check with authors.

# citation #########################################################################################
ctb0038_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0038_citation <- data.table::as.data.table(ctb0038_citation)
str(ctb0038_citation)

# dataset_titulo
dataset_titulo <- ctb0038_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0038_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0038_citation <- data.table::data.table(
  dataset_id = "ctb0038",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0038_citation)

# event ############################################################################################
ctb0038_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0038_event <- data.table::as.data.table(ctb0038_event)
str(ctb0038_event)

# Process fields
# observacao_id
ctb0038_event[, observacao_id := as.character(observacao_id)]
ctb0038_event[, .N, by = observacao_id][N > 1]

# data_ano
# observacao_data
data.table::setnames(ctb0038_event, old = "observacao_data", new = "data_ano")
ctb0038_event[, data_ano := as.integer(data_ano)]
ctb0038_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0038_event[!is.na(data_ano), ano_fonte := "original"]
ctb0038_event[, .N, by = ano_fonte]

# coord_x
ctb0038_event[, coord_x := as.numeric(coord_x)]
summary(ctb0038_event[, coord_x])

# coord_y
ctb0038_event[, coord_y := as.numeric(coord_y)]
summary(ctb0038_event[, coord_y])

# coord_datum
# coord_sistema
data.table::setnames(ctb0038_event, old = "coord_sistema", new = "coord_datum")
ctb0038_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0038_event[, .N, by = coord_datum]
# 31981 -> 4326
ctb0038_event_sf <- sf::st_as_sf(ctb0038_event[coord_datum == "31981"],
  coords = c("coord_x", "coord_y"), crs = 31981
)
ctb0038_event_sf <- sf::st_transform(ctb0038_event_sf, crs = 4326)
ctb0038_event_sf <- sf::st_coordinates(ctb0038_event_sf)
ctb0038_event[coord_datum == "31981", coord_x := ctb0038_event_sf[, 1]]
ctb0038_event[coord_datum == "31981", coord_y := ctb0038_event_sf[, 2]]
ctb0038_event[coord_datum == 31981, coord_datum := 4326]
ctb0038_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0038_event_sf)

# check for duplicated coordinates
ctb0038_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_precisao
ctb0038_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0038_event[, coord_precisao])

# coord_fonte
ctb0038_event[, coord_fonte := as.character(coord_fonte)]
ctb0038_event[, .N, by = coord_fonte]

# pais_id
ctb0038_event[, pais_id := as.character(pais_id)]
ctb0038_event[, .N, by = pais_id]

# estado_id
ctb0038_event[, estado_id := as.character(estado_id)]
ctb0038_event[, .N, by = estado_id]

# municipio_id
ctb0038_event[, municipio_id := as.character(municipio_id)]
ctb0038_event[, .N, by = municipio_id]

# amostra_area
ctb0038_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0038_event[, amostra_area])

# taxon_sibcs
ctb0038_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0038_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0038_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# review the work at another time

ctb0038_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0038_event[, rochosidade := ("Não Rochoso")]

str(ctb0038_event)

# layer ############################################################################################
ctb0038_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0038_layer <- data.table::as.data.table(ctb0038_layer)
str(ctb0038_layer)

# Process fields
# observacao_id
ctb0038_layer[, observacao_id := as.character(observacao_id)]
ctb0038_layer[, .N, by = observacao_id]

# camada_nome
ctb0038_layer[, camada_nome := as.character(camada_nome)]
ctb0038_layer[, .N, by = camada_nome]

# amostra_id
ctb0038_layer[, amostra_id := as.integer(amostra_id)]
ctb0038_layer[, .N, by = amostra_id]

# profund_sup
ctb0038_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0038_layer[, profund_sup])

# profund_inf
ctb0038_layer[, profund_inf := as.numeric(profund_inf)]
ctb0038_layer[profund_inf == profund_sup & camada_nome == "R", profund_inf := profund_sup + 20]
summary(ctb0038_layer[, profund_inf])

# check for missing layers
any_missing_layer(ctb0038_layer)
# add missing layers
ctb0038_layer <- add_missing_layer(ctb0038_layer)

# compute mid depth
ctb0038_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0038_layer[, profund_mid])

# camada_id
ctb0038_layer <- ctb0038_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0038_layer[, camada_id := 1:.N, by = observacao_id]
ctb0038_layer[, .N, by = camada_id]

# terrafina
# terrafina_xxx * 10
data.table::setnames(ctb0038_layer, old = "terrafina_xxx", new = "terrafina")
ctb0038_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0038_layer[, terrafina])
check_empty_layer(ctb0038_layer, "terrafina")
# Fill empty layers
ctb0038_layer[, terrafina := fill_empty_layer(y = terrafina, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "terrafina")

# argila
# argila_naoh_pipeta
data.table::setnames(ctb0038_layer, old = "argila_naoh_pipeta", new = "argila")
ctb0038_layer[, argila := as.numeric(argila)]
summary(ctb0038_layer[, argila])
check_empty_layer(ctb0038_layer, "argila")
# Fill empty layers
ctb0038_layer[, argila := fill_empty_layer(y = argila, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "argila")

# silte
# silte_naoh_calc
data.table::setnames(ctb0038_layer, old = "silte_naoh_calc", new = "silte")
ctb0038_layer[, silte := as.numeric(silte)]
summary(ctb0038_layer[, silte])
check_empty_layer(ctb0038_layer, "silte")
# Fill empty layers
ctb0038_layer[, silte := fill_empty_layer(y = silte, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "silte")

# areia
# areia_naoh_peneira
data.table::setnames(ctb0038_layer, old = "areia_naoh_peneira", new = "areia")
ctb0038_layer[, areia := as.numeric(areia)]
summary(ctb0038_layer[, areia])
check_empty_layer(ctb0038_layer, "areia")
# Fill empty layers
ctb0038_layer[, areia := fill_empty_layer(y = areia, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "areia")

# carbono
# Carbono
data.table::setnames(ctb0038_layer, old = "Carbono", new = "carbono")
ctb0038_layer[, carbono := as.numeric(carbono)]
summary(ctb0038_layer[, carbono])
check_empty_layer(ctb0038_layer, "carbono")
# Fill empty layers
ctb0038_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "carbono")

# ph
# ph_h2o_25_xxx
data.table::setnames(ctb0038_layer, old = "ph_h2o_25_xxx", new = "ph")
ctb0038_layer[, ph := as.numeric(ph)]
summary(ctb0038_layer[, ph])
check_empty_layer(ctb0038_layer, "ph")
# Fill empty layers
ctb0038_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "ph")

# ctc
# ctc_soma_calc
data.table::setnames(ctb0038_layer, old = "ctc_soma_calc", new = "ctc")
ctb0038_layer[, ctc := as.numeric(ctc)]
summary(ctb0038_layer[, ctc])
check_empty_layer(ctb0038_layer, "ctc")
# Fill empty layers
ctb0038_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "ctc")

# dsi
# dsi_cilindro
data.table::setnames(ctb0038_layer, old = "dsi_cilindro", new = "dsi")
ctb0038_layer[, dsi := as.numeric(dsi)]
summary(ctb0038_layer[, dsi])
check_empty_layer(ctb0038_layer, "dsi")
# Fill empty layers
ctb0038_layer[, dsi := fill_empty_layer(y = dsi, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0038_layer, "dsi")

str(ctb0038_layer)

# Merge ############################################################################################
# events and layers
ctb0038 <- merge(ctb0038_event, ctb0038_layer, all = TRUE)
ctb0038[, dataset_id := "ctb0038"]
# citation
ctb0038 <- merge(ctb0038, ctb0038_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0038)
# Layers: 136
# Events: 57
# Georeferenced events: 57

# Plot with mapview
if (FALSE) {
  ctb0038_sf <- sf::st_as_sf(
    ctb0038[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0038_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0038 <- select_output_columns(ctb0038)
data.table::fwrite(ctb0038, "ctb0038/ctb0038.csv")
data.table::fwrite(ctb0038_event, "ctb0038/ctb0038_event.csv")
data.table::fwrite(ctb0038_layer, "ctb0038/ctb0038_layer.csv")
