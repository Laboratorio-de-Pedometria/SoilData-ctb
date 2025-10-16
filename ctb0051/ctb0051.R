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
# ctb0051
# Projeto RADAMBRASIL. Folha SF.22 Paranapanema; Pedologia (volume 37)
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0051/2022-02-03-ctb0051.xlsx")

# citation #########################################################################################
ctb0051_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0051_citation <- data.table::as.data.table(ctb0051_citation)
str(ctb0051_citation)

# dataset_titulo
dataset_titulo <- ctb0051_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0051_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0051_citation <- data.table::data.table(
  dataset_id = "ctb0051",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0051_citation)

# event ############################################################################################
ctb0051_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0051_event <- data.table::as.data.table(ctb0051_event)
str(ctb0051_event)

# Process fields
# observacao_id
ctb0051_event[, observacao_id := as.character(observacao_id)]
ctb0051_event[, .N, by = observacao_id][N > 1]

# data_ano
# observacao_data
data.table::setnames(ctb0051_event, old = "observacao_data", new = "data_ano")
t0 <- "1899-12-30"
ctb0051_event[, data_ano := as.Date(data_ano, origin = t0)]
ctb0051_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0051_event[, .N, by = data_ano]

# ano_fonte
ctb0051_event[!is.na(data_ano), ano_fonte := "original"]
ctb0051_event[, .N, by = ano_fonte]

# Events without date are those compiled from previous works and extra/fertility samples. We assume
# that the later were collected in the same years as the complete soil profiles.
ctb0051_event[is.na(data_ano) & is.na(observacao_fonte), data_ano := 1982]
ctb0051_event[, .N, by = data_ano]

# ano_fonte
ctb0051_event[is.na(ano_fonte) & !is.na(data_ano), ano_fonte := "estimativa"]
ctb0051_event[, .N, by = ano_fonte]

# coord_x
ctb0051_event[, coord_x := as.numeric(coord_x)]
summary(ctb0051_event[, coord_x])
# Events without coordinates are those compiled from previous works and extra/fertility samples.
# We keep it as NA_real_.

# coord_y
ctb0051_event[, coord_y := as.numeric(coord_y)]
summary(ctb0051_event[, coord_y])
# Events without coordinates are those compiled from previous works and extra/fertility samples.
# We keep it as NA_real_.

# coord_datum
# coord_sistema -> coord_datum
data.table::setnames(ctb0051_event, old = "coord_sistema", new = "coord_datum")
ctb0051_event[, .N, by = coord_datum]
ctb0051_event[coord_datum == "SAD69", coord_datum := 4618]
ctb0051_event[, coord_datum := as.integer(coord_datum)]
ctb0051_event[, .N, by = coord_datum]
# 4618 -> 4326
ctb0051_event_sf <- sf::st_as_sf(
  ctb0051_event[coord_datum == 4618, ],
  coords = c("coord_x", "coord_y"), crs = 4618
)
ctb0051_event_sf <- sf::st_transform(ctb0051_event_sf, crs = 4326)
ctb0051_event_sf <- sf::st_coordinates(ctb0051_event_sf)
ctb0051_event[coord_datum == 4618, coord_x := ctb0051_event_sf[, 1]]
ctb0051_event[coord_datum == 4618, coord_y := ctb0051_event_sf[, 2]]
ctb0051_event[coord_datum == 4618, coord_datum := 4326]
rm(ctb0051_event_sf)
ctb0051_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0051_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0051_event[coord_duplicated == TRUE & !is.na(coord_x), .(observacao_id, coord_x, coord_y)]

# coord_precisao
# coord_precisao is missing. We assume it is 1 minute, which corresponds to 1.852 km at the equator
ctb0051_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0051_event[!is.na(coord_fonte), coord_precisao := 1852]
summary(ctb0051_event[, coord_precisao])

# coord_fonte
ctb0051_event[, coord_fonte := as.character(coord_fonte)]
ctb0051_event[, .N, by = coord_fonte]

# pais_id
# pais_id is missing. We set it to "BR"
ctb0051_event[, pais_id := "BR"]

# estado_id
# estado_id is missing. We set it to NA_character_
ctb0051_event[, estado_id := NA_character_]

# municipio_id
# municipio_id is missing. We set it to NA_character_
ctb0051_event[, municipio_id := NA_character_]

# amostra_area
# amostra_area is missing. We assume it is 1.0 m^2 (soil profiles)
ctb0051_event[, amostra_area := 1.0]

# taxon_sibcs
# taxon_sibcs_19xx_2
data.table::setnames(ctb0051_event, old = "taxon_sibcs_19xx_2", new = "taxon_sibcs")
ctb0051_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0051_event[, .N, by = taxon_sibcs]
# Events without taxon_sibcs are those compiled from previous works and extra/fertility samples.
# We keep it as NA_character_.

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset
ctb0051_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# this document don't  have pedregosidade info

ctb0051_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# this document don't  have rochosidade info

ctb0051_event[, rochosidade := NA_character_]

str(ctb0051_event)

# layer ############################################################################################
ctb0051_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0051_layer <- data.table::as.data.table(ctb0051_layer)
str(ctb0051_layer)

# Process fields
# observacao_id
ctb0051_layer[, observacao_id := as.character(observacao_id)]
ctb0051_layer[, .N, by = observacao_id]

# camada_nome
ctb0051_layer[, camada_nome := as.character(camada_nome)]
ctb0051_layer[, .N, by = camada_nome]

# amostra_id
ctb0051_layer[, amostra_id := as.character(amostra_id)]
ctb0051_layer[, .N, by = amostra_id]

# profund_sup
ctb0051_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0051_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0051_layer[, profund_sup])

# profund_inf
ctb0051_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0051_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0051_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0051_layer[, profund_inf])

# camada_id
ctb0051_layer <- ctb0051_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0051_layer[, camada_id := 1:.N, by = observacao_id]
ctb0051_layer[, .N, by = camada_id]

# check for missing layers
check_missing_layer(ctb0051_layer)
# add missing layers
ctb0051_layer <- add_missing_layer(ctb0051_layer)
check_missing_layer(ctb0051_layer)

# mid depth
ctb0051_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0051_layer[, profund_mid])

# terrafina
# terrafina_xxx * 10
data.table::setnames(ctb0051_layer, old = "terrafina_xxx", new = "terrafina")
ctb0051_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0051_layer[, terrafina])
check_empty_layer(ctb0051_layer, "terrafina")
# Fill empty layer
ctb0051_layer[, terrafina := fill_empty_layer(y = terrafina, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0051_layer, "terrafina")

# argila
# argila_naoh_xxx * 10
data.table::setnames(ctb0051_layer, old = "argila_naoh_xxx", new = "argila")
ctb0051_layer[, argila := as.numeric(argila) * 10]
summary(ctb0051_layer[, argila])
check_empty_layer(ctb0051_layer, "argila")
# Fill empty layer
ctb0051_layer[, argila := fill_empty_layer(y = argila, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0051_layer, "argila")

# silte
# silte_0002mm0050mm_calc * 10
data.table::setnames(ctb0051_layer, old = "silte_0002mm0050mm_calc", new = "silte")
ctb0051_layer[, silte := as.numeric(silte) * 10]
summary(ctb0051_layer[, silte])
check_empty_layer(ctb0051_layer, "silte")
# Fill empty layer
ctb0051_layer[, silte := fill_empty_layer(y = silte, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0051_layer, "silte")

# areia
# (areia_0200mm2000mm_calc + areia_0050mm0200mm_peneira) * 10
data.table::setnames(ctb0051_layer, old = "areia_0200mm2000mm_calc", new = "areia_grossa")
data.table::setnames(ctb0051_layer, old = "areia_0050mm0200mm_peneira", new = "areia_fina")
ctb0051_layer[, areia := as.numeric(areia_grossa) * 10]
ctb0051_layer[, areia := areia + as.numeric(areia_fina) * 10]
summary(ctb0051_layer[, areia])
check_empty_layer(ctb0051_layer, "areia")
# Fill empty layer
ctb0051_layer[, areia := fill_empty_layer(y = areia, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0051_layer, "areia")

# carbono
# carbono_xxx_xxx_xxx [%] * 10
data.table::setnames(ctb0051_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0051_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0051_layer[, carbono])
check_empty_layer(ctb0051_layer, "carbono")
# Fill empty layer
ctb0051_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0051_layer, "carbono")

# ph
# ph_h2o_25_eletrodo
data.table::setnames(ctb0051_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0051_layer[, ph := as.numeric(ph)]
summary(ctb0051_layer[, ph])
check_empty_layer(ctb0051_layer, "ph")
# Fill empty layer
ctb0051_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0051_layer, "ph")

# ctc
# ctc_soma_calc [mE/100g]
data.table::setnames(ctb0051_layer, old = "ctc_soma_calc", new = "ctc")
ctb0051_layer[, ctc := as.numeric(ctc)]
summary(ctb0051_layer[, ctc])
check_empty_layer(ctb0051_layer, "ctc")
# Fill empty layer
ctb0051_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0051_layer, "ctc")

# dsi
# dsi is missing. We set it to NA_real_
ctb0051_layer[, dsi := NA_real_]

str(ctb0051_layer)

# Merge ############################################################################################
# events and layers
ctb0051 <- merge(ctb0051_event, ctb0051_layer, all = TRUE)
ctb0051[, dataset_id := "ctb0051"]
# citation
ctb0051 <- merge(ctb0051, ctb0051_citation, by = "dataset_id", all.x = TRUE)
# This work contains many soil profiles from previous works. We will discard them.
ctb0051 <- ctb0051[is.na(observacao_fonte), ]
summary_soildata(ctb0051)
# Layers: 192
# Events: 95
# Georeferenced events: 33

# Plot with mapview
if (FALSE) {
  ctb0051_sf <- sf::st_as_sf(
    ctb0051[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0051_sf["argila"])
}

# Write to disk ####################################################################################
ctb0051 <- select_output_columns(ctb0051)
data.table::fwrite(ctb0051, "ctb0051/ctb0051.csv")
data.table::fwrite(ctb0051_event, "ctb0051/ctb0051_event.csv")
data.table::fwrite(ctb0051_layer, "ctb0051/ctb0051_layer.csv")
