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
# ctb0047
# Levantamento semidetalhado de solos e diagnóstico dos Remanescentes Florestais do Município de Cambé - PR
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0047/2022-01-31-ctb0047.xlsx")

# citation #########################################################################################
ctb0047_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0047_citation <- data.table::as.data.table(ctb0047_citation)
str(ctb0047_citation)

# dataset_titulo
dataset_titulo <- ctb0047_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0047_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0047_citation <- data.table::data.table(
  dataset_id = "ctb0047",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0047_citation)

# event ############################################################################################
ctb0047_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0047_event <- data.table::as.data.table(ctb0047_event)
str(ctb0047_event)

# Process fields
# observacao_id
ctb0047_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0047_event[, observacao_id]) > 1)

# data_ano
# observacao_data
data.table::setnames(ctb0047_event, old = "observacao_data", new = "data_ano")
ctb0047_event[, .N, by = data_ano]
ctb0047_event[, data_ano := as.integer(2011)]
ctb0047_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0047_event[, ano_fonte := "original"]
ctb0047_event[, .N, by = ano_fonte]

# coord_x
ctb0047_event[, coord_x := as.numeric(coord_x)]
summary(ctb0047_event[, coord_x])

# coord_y
ctb0047_event[, coord_y := as.numeric(coord_y)]
summary(ctb0047_event[, coord_y])

# coord_datum
# coord_sistema -> coord_datum
data.table::setnames(ctb0047_event, old = "coord_sistema", new = "coord_datum")
ctb0047_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0047_event[, .N, by = coord_datum]
# 29192 -> 4326
ctb0047_event_sf <- sf::st_as_sf(ctb0047_event[coord_datum == "29192"],
  coords = c("coord_x", "coord_y"), crs = 29192
)
ctb0047_event_sf <- sf::st_transform(ctb0047_event_sf, crs = 4326)
ctb0047_event_sf <- sf::st_coordinates(ctb0047_event_sf)
ctb0047_event[coord_datum == "29192", coord_x := ctb0047_event_sf[, 1]]
ctb0047_event[coord_datum == "29192", coord_y := ctb0047_event_sf[, 2]]
ctb0047_event[coord_datum == 29192, coord_datum := 4326]
ctb0047_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0047_event_sf)

# coord_precisao
ctb0047_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0047_event[, coord_precisao])

# coord_fonte
ctb0047_event[, coord_fonte := as.character(coord_fonte)]
ctb0047_event[, .N, by = coord_fonte]

# pais_id
ctb0047_event[, pais_id := as.character(pais_id)]
ctb0047_event[, .N, by = pais_id]

# estado_id
ctb0047_event[, estado_id := as.character(estado_id)]
ctb0047_event[, .N, by = estado_id]

# municipio_id
ctb0047_event[, municipio_id := as.character(municipio_id)]
ctb0047_event[, .N, by = municipio_id]

# amostra_area
ctb0047_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0047_event[, amostra_area])

# taxon_sibcs
# taxon_sibcs_2006
data.table::setnames(ctb0047_event, old = "taxon_sibcs_2006", new = "taxon_sibcs")
ctb0047_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0047_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0047_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# this document don't  have pedregosidade info

ctb0047_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# this document don't  have rochosidade info

ctb0047_event[, rochosidade := NA_character_]

str(ctb0047_event)

# layer ############################################################################################
ctb0047_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0047_layer <- data.table::as.data.table(ctb0047_layer)
str(ctb0047_layer)

# Process fields
# observacao_id
ctb0047_layer[, observacao_id := as.character(observacao_id)]
ctb0047_layer[, .N, by = observacao_id]

# camada_nome
ctb0047_layer[, camada_nome := as.character(camada_nome)]
ctb0047_layer[, .N, by = camada_nome]

# amostra_id
ctb0047_layer[, amostra_id := as.character(amostra_id)]
ctb0047_layer[, .N, by = amostra_id]

# profund_sup
ctb0047_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0047_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0047_layer[, profund_sup])

# profund_inf
ctb0047_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0047_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0047_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0047_layer[, profund_inf])

# camada_id
ctb0047_layer <- ctb0047_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0047_layer[, camada_id := 1:.N, by = observacao_id]
ctb0047_layer[, camada_id := as.integer(camada_id)]
ctb0047_layer[, .N, by = camada_id]

# terrafina
# terrafina is missing. We assume it is 1000 g/kg because all layers are Ap, Bt, or Bw
ctb0047_layer[, terrafina := 1000]

# argila
# argila_xxx_xxx * 10
data.table::setnames(ctb0047_layer, old = "argila_xxx_xxx", new = "argila")
ctb0047_layer[, argila := as.numeric(argila) * 10]
summary(ctb0047_layer[, argila])

# silte
# silte_xxx_xxx * 10
data.table::setnames(ctb0047_layer, old = "silte_xxx_xxx", new = "silte")
ctb0047_layer[, silte := as.numeric(silte) * 10]
summary(ctb0047_layer[, silte])

# areia
# areia_xxx_xxx * 10
data.table::setnames(ctb0047_layer, old = "areia_xxx_xxx", new = "areia")
ctb0047_layer[, areia := as.numeric(areia) * 10]
summary(ctb0047_layer[, areia])

# carbono
# carbono_xxx_xxx_xxx * 10
data.table::setnames(ctb0047_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0047_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0047_layer[, carbono])

# ph
# ph_h2o_xxx_xxx
data.table::setnames(ctb0047_layer, old = "ph_h2o_xxx_xxx", new = "ph")
ctb0047_layer[, ph := as.numeric(ph)]
summary(ctb0047_layer[, ph])

# ctc
# ctc_xxx_xxx
data.table::setnames(ctb0047_layer, old = "ctc_xxx_xxx", new = "ctc")
ctb0047_layer[, ctc := as.numeric(ctc)]
summary(ctb0047_layer[, ctc])

# dsi
# Densidade do solo não está disponível neste dataset.
ctb0047_layer[, dsi := NA_real_]

str(ctb0047_layer)

# Merge ############################################################################################
# events and layers
ctb0047 <- merge(ctb0047_event, ctb0047_layer, all = TRUE)
ctb0047[, dataset_id := "ctb0047"]
# citation
ctb0047 <- merge(ctb0047, ctb0047_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0047)
# Layers: 44
# Events: 26
# Georeferenced events: 26

# Plot with mapview
if (FALSE) {
  ctb0047_sf <- sf::st_as_sf(ctb0047, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(ctb0047_sf["argila"])
}

# Write to disk ####################################################################################
ctb0047 <- select_output_columns(ctb0047)
data.table::fwrite(ctb0047, "ctb0047/ctb0047.csv")
data.table::fwrite(ctb0047_event, "ctb0047/ctb0047_event.csv")
data.table::fwrite(ctb0047_layer, "ctb0047/ctb0047_layer.csv")
