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
# check for duplicated observacao_id
ctb0051_event[, .N, by = observacao_id][N > 1]

# old: observacao_fonte
# new: dado_fonte
data.table::setnames(ctb0051_event, old = "observacao_fonte", new = "dado_fonte")
ctb0051_event[, dado_fonte := as.character(dado_fonte)]
ctb0051_event[, .N, by = dado_fonte]
# NOTE. This dataset contains many soil profiles compiled from previous works. This is indicated
# in the field dado_fonte (old: observacao_fonte). We will discard these profiles later. It is not
# evident if these data sources were already rescued and published in SoilData.

# observacao_data
# data_ano
# Convert Excel date to R Date
data.table::setnames(ctb0051_event, old = "observacao_data", new = "data_ano")
t0 <- "1899-12-30"
ctb0051_event[, data_ano := as.Date(data_ano, origin = t0)]
ctb0051_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0051_event[, .N, by = data_ano]
# There are 199 events with missing date. Several of them are those compiled from previous works.
# For the original events, it should be checked in the source document.

# ano_fonte
# The events with date are those original from this work.
ctb0051_event[!is.na(data_ano), ano_fonte := "original"]
ctb0051_event[, .N, by = ano_fonte]

# Events without date are those compiled from previous works and extra/fertility samples. We assume
# that the later were collected in the same years as the complete soil profiles.
ctb0051_event[is.na(data_ano) & dado_fonte == "Original", data_ano := sample(
  ctb0051_event[!is.na(data_ano), data_ano],
  size = .N,
  replace = TRUE
), by = .I]
ctb0051_event[, .N, by = data_ano]

# ano_fonte
# Update ano_fonte for events with estimated date
ctb0051_event[is.na(ano_fonte) & !is.na(data_ano) & dado_fonte == "Original", ano_fonte := "estimativa"]
ctb0051_event[, .N, by = ano_fonte]

# coord_x
ctb0051_event[, coord_x := as.numeric(coord_x)]
summary(ctb0051_event[, coord_x])
# Events (182) without coordinates are those compiled from previous works and extra/fertility samples.
# We keep it as NA_real_.

# coord_y
ctb0051_event[, coord_y := as.numeric(coord_y)]
summary(ctb0051_event[, coord_y])
# Events (182) without coordinates are those compiled from previous works and extra/fertility samples.
# We keep it as NA_real_.

# coord_datum
# coord_sistema -> coord_datum
data.table::setnames(ctb0051_event, old = "coord_sistema", new = "coord_datum")
# 182 events without coordinates are those compiled from previous works and extra/fertility samples.
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
# estado_id is missing. We set it to NA_character_. This should be filled in the source document.
ctb0051_event[, estado_id := NA_character_]

# municipio_id
# municipio_id is missing. We set it to NA_character_. This should be filled in the source document.
ctb0051_event[, municipio_id := NA_character_]

# amostra_area
# amostra_area is missing. We assume it is 1.0 m^2 (soil profiles)
ctb0051_event[, amostra_area := 1.0]

# taxon_sibcs
# taxon_sibcs_19xx_2
data.table::setnames(ctb0051_event, old = "taxon_sibcs_19xx_2", new = "taxon_sibcs")
ctb0051_event[, taxon_sibcs := as.character(taxon_sibcs)]
# Trim whitespace
ctb0051_event[, taxon_sibcs := trimws(taxon_sibcs)]
# Remove ending period
ctb0051_event[, taxon_sibcs := gsub("\\.$", "", taxon_sibcs)]
ctb0051_event[, .N, by = taxon_sibcs]
# Events without taxon_sibcs are those compiled from previous works and extra/fertility samples.
# We keep it as NA_character_.

# taxon_st
# The Soil Taxonomy classification is specified in the source document in the same field as
# taxon_sibcs for various events. The Soil Taxonomy classification appears after the Brazilian
# Soil Classification System classification, creating a long string of more than 40 characters. The
# two classifications are separated by a period (.). If the string is shorter than 40 characters, it
# appears to contain only the Brazilian Soil Classification System classification. We will process
# these two fields here. However, the source spreadsheet should be changed to have two separate
# fields.
ctb0051_event[, taxon_st := NA_character_]
ctb0051_event[nchar(taxon_sibcs) > 40, taxon_st := sub(".*?\\.\\s*", "", taxon_sibcs)]
ctb0051_event[, taxon_st := trimws(taxon_st)]
ctb0051_event[nchar(taxon_sibcs) > 40, taxon_sibcs := sub("\\..*$", "", taxon_sibcs)]
ctb0051_event[, taxon_sibcs := trimws(taxon_sibcs)]
ctb0051_event[, .N, by = taxon_st]
ctb0051_event[, .N, by = taxon_sibcs]

# pedregosidade
# The source spreadsheet does not have stoniness (pedregosidade) info. We should check the source
# document.
ctb0051_event[, pedregosidade := NA_character_]

# rochosidade
# The source spreadsheet does not have rockiness (rochosidade) info. We should check the source
# document.
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
# The sample ID is aggregated in the field amostra_id. We should dissagregate it in the source
# document.
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
# There are six missing layers in extra samples. This layers could be added in the source
# spreadsheet.
ctb0051_layer <- add_missing_layer(ctb0051_layer)
check_missing_layer(ctb0051_layer)

# mid depth
ctb0051_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0051_layer[, profund_mid])

# terrafina_xxx
# terrafina
# terrafina = terrafina_xxx * 10
data.table::setnames(ctb0051_layer, old = "terrafina_xxx", new = "terrafina")
ctb0051_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0051_layer[, terrafina])
# There are seven layers with missing terrafina. All of those six layers added previously and one
# original R layer (PERFIL-100).
check_empty_layer(ctb0051_layer, "terrafina")
# Fill empty layers, except for the R layer
ctb0051_layer[observacao_id != "PERFIL-100",
  terrafina := fill_empty_layer(y = terrafina, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0051_layer, "terrafina")

# argila
# argila_naoh_xxx * 10
data.table::setnames(ctb0051_layer, old = "argila_naoh_xxx", new = "argila")
ctb0051_layer[, argila := as.numeric(argila) * 10]
summary(ctb0051_layer[, argila])
# There are 11 layers with missing argila. Some of those are layers added previously. Others are
# original layers from AMOSTRA-EXTRA-027.
check_empty_layer(ctb0051_layer, "argila")
# Fill empty layers, except for the R layer
ctb0051_layer[observacao_id != "PERFIL-100",
  argila := fill_empty_layer(y = argila, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0051_layer, "argila")

# silte
# silte_0002mm0050mm_calc * 10
data.table::setnames(ctb0051_layer, old = "silte_0002mm0050mm_calc", new = "silte")
ctb0051_layer[, silte := as.numeric(silte) * 10]
summary(ctb0051_layer[, silte])
# There are 11 layers with missing silte. Some of those are layers added previously. Others are
# original layers from AMOSTRA-EXTRA-027.
check_empty_layer(ctb0051_layer, "silte")
# Fill empty layer except for the R layer
ctb0051_layer[observacao_id != "PERFIL-100",
  silte := fill_empty_layer(y = silte, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0051_layer, "silte")

# areia
# (areia_0200mm2000mm_calc + areia_0050mm0200mm_peneira) * 10
data.table::setnames(ctb0051_layer, old = "areia_0200mm2000mm_calc", new = "areia_grossa")
data.table::setnames(ctb0051_layer, old = "areia_0050mm0200mm_peneira", new = "areia_fina")
ctb0051_layer[, areia := as.numeric(areia_grossa) * 10]
ctb0051_layer[, areia := areia + as.numeric(areia_fina) * 10]
summary(ctb0051_layer[, areia])
# There are 11 layers with missing areia. Some of those are layers added previously. Others are
# original layers from AMOSTRA-EXTRA-027.
check_empty_layer(ctb0051_layer, "areia")
# Fill empty layer except for the R layer
ctb0051_layer[observacao_id != "PERFIL-100",
  areia := fill_empty_layer(y = areia, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0051_layer, "areia")

# carbono
# carbono_xxx_xxx_xxx [%] * 10
data.table::setnames(ctb0051_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0051_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0051_layer[, carbono])
# There are 11 layers with missing carbono. Some of those are layers added previously. Others are
# original layers from AMOSTRA-EXTRA-027.
check_empty_layer(ctb0051_layer, "carbono")
# Fill empty layer except for the R layer
ctb0051_layer[observacao_id != "PERFIL-100",
  carbono := fill_empty_layer(y = carbono, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0051_layer, "carbono")

# ph
# ph_h2o_25_eletrodo
data.table::setnames(ctb0051_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0051_layer[, ph := as.numeric(ph)]
summary(ctb0051_layer[, ph])
# There are 11 layers with missing ph. Some of those are layers added previously. Others are
# original layers from AMOSTRA-EXTRA-027.
check_empty_layer(ctb0051_layer, "ph")
# Fill empty layer, except for the R layer
ctb0051_layer[observacao_id != "PERFIL-100",
  ph := fill_empty_layer(y = ph, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0051_layer, "ph")

# ctc
# ctc_soma_calc [mE/100g]
data.table::setnames(ctb0051_layer, old = "ctc_soma_calc", new = "ctc")
ctb0051_layer[, ctc := as.numeric(ctc)]
summary(ctb0051_layer[, ctc])
# There are 14 layers with missing ctc. Some of those are layers added previously. Others are
# original layers from AMOSTRA-EXTRA-027 and PERFIL-013
check_empty_layer(ctb0051_layer, "ctc")
# Fill empty layer, except for the R layer
ctb0051_layer[observacao_id != "PERFIL-100",
  ctc := fill_empty_layer(y = ctc, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0051_layer, "ctc")

# dsi
# The soil bulk density (dsi) is not specified in the dataset.
ctb0051_layer[, dsi := NA_real_]

str(ctb0051_layer)

# Merge ############################################################################################
# events and layers
ctb0051 <- merge(ctb0051_event, ctb0051_layer, all = TRUE)
ctb0051[, dataset_id := "ctb0051"]

# citation
ctb0051 <- merge(ctb0051, ctb0051_citation, by = "dataset_id", all.x = TRUE)

# This work contains many soil profiles from previous works. We will discard them.
ctb0051 <- ctb0051[dado_fonte == "Original", ]
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
