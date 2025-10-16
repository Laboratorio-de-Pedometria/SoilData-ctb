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

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0028
# Dados de "Levantamento pedológico do campo experimental da Embrapa Uva e Vinho em Bento Gonçalves,
# RS"
# https://drive.google.com/drive/u/0/folders/13FfWFNVY3rEudetbJ3IdUVTIvD43xT7y
gs <- "1PwavEXm8Ow59aF8ZykvKsJwj6THbv_i_3yUmZUuHgkI"
gid_citation <- 851221394
gid_validation <- 1253941506
gid_event <- 398191837
gid_layer <- 1547320225

# validation #######################################################################################
ctb0028_validation <- google_sheet(gs, gid_validation)
str(ctb0028_validation)

# Check for negative validation results
sum(ctb0028_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0028_citation <- google_sheet(gs, gid_citation)
str(ctb0028_citation)

# dataset_titulo
dataset_titulo <- ctb0028_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0028_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0028_citation <- data.table::data.table(
  dataset_id = "ctb0028",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0028_citation)

# event ############################################################################################
ctb0028_event <- google_sheet(gs, gid_event)
str(ctb0028_event)

# Process fields
# observacao_id
data.table::setnames(ctb0028_event, old = "ID do evento", new = "observacao_id")
ctb0028_event[, observacao_id := as.character(observacao_id)]
ctb0028_event[, .N, by = observacao_id][N > 1]

# There is one soil profile that was compiled from a previous study (Klamt et al., 1995)). We
# discard it to avoid duplication
# observacao_id = BG2
ctb0028_event <- ctb0028_event[observacao_id != "BG2"]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0028_event, old = "Ano (coleta)", new = "data_ano")
ctb0028_event[, data_ano := as.integer(data_ano)]
ctb0028_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta dos perfis de solo está especificada no documento de origem dos dados.
ctb0028_event[!is.na(data_ano), ano_fonte := "original"]
ctb0028_event[, .N, by = ano_fonte] 

# coord_x
data.table::setnames(ctb0028_event, old = "Coordenada X [m]", new = "coord_x")
ctb0028_event[, coord_x := as.numeric(coord_x)]
summary(ctb0028_event[, coord_x])

# coord_y
data.table::setnames(ctb0028_event, old = "Coordenada Y [m]", new = "coord_y")
ctb0028_event[, coord_y := as.numeric(coord_y)]
summary(ctb0028_event[, coord_y])

# coord_datum
data.table::setnames(ctb0028_event, old = "Datum (coord)", new = "coord_datum")
ctb0028_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0028_event[, .N, by = coord_datum]

# Transform coordinates to WGS84 (EPSG:4326)
ctb0028_event_sf <- sf::st_as_sf(ctb0028_event[coord_datum == 29182],
  coords = c("coord_x", "coord_y"), crs = 29182
)
ctb0028_event_sf <- sf::st_transform(ctb0028_event_sf, crs = 4326)
ctb0028_event_sf <- sf::st_coordinates(ctb0028_event_sf)
ctb0028_event[coord_datum == 29182, coord_x := ctb0028_event_sf[, 1]]
ctb0028_event[coord_datum == 29182, coord_y := ctb0028_event_sf[, 2]]
ctb0028_event[coord_datum == 29182, coord_datum := 4326]
ctb0028_event[, coord_datum := as.integer(coord_datum)]
rm(ctb0028_event_sf)

# coord_fonte
data.table::setnames(ctb0028_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0028_event[, coord_fonte := as.character(coord_fonte)]
ctb0028_event[, .N, by = coord_fonte]

# coord_precisao
data.table::setnames(ctb0028_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0028_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0028_event[, coord_precisao])

# pais_id
data.table::setnames(ctb0028_event, old = "País", new = "pais_id")
ctb0028_event[, pais_id := as.character(pais_id)]
ctb0028_event[, .N, by = pais_id]

# estado_id
data.table::setnames(ctb0028_event, old = "Estado (UF)", new = "estado_id")
ctb0028_event[, estado_id := as.character(estado_id)]
ctb0028_event[, .N, by = estado_id]

# municipio_id
data.table::setnames(ctb0028_event, old = "Município", new = "municipio_id")
ctb0028_event[, municipio_id := as.character(municipio_id)]
ctb0028_event[, .N, by = municipio_id]

# amostra_area
data.table::setnames(ctb0028_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0028_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0028_event[, amostra_area])

# taxon_sibcs
data.table::setnames(ctb0028_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0028_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0028_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0028_event[, taxon_st := NA_character_]

# pedregosidade
data.table::setnames(ctb0028_event, old = "Pedregosidade", new = "pedregosidade")
ctb0028_event[, pedregosidade := as.character(pedregosidade)]

# rochosidade
data.table::setnames(ctb0028_event, old = "Rochosidade", new = "rochosidade")
ctb0028_event[, rochosidade := as.character(rochosidade)]




str(ctb0028_event)

# layer ############################################################################################
ctb0028_layer <- google_sheet(gs, gid_layer)
str(ctb0028_layer)

# Process fields
# observacao_id
data.table::setnames(ctb0028_layer, old = "ID do evento", new = "observacao_id")
ctb0028_layer[, observacao_id := as.character(observacao_id)]
ctb0028_layer[, .N, by = observacao_id]

# drop the soil profile that was compiled from a previous study
ctb0028_layer <- ctb0028_layer[observacao_id != "BG2"]

# camada_nome
data.table::setnames(ctb0028_layer, old = "ID da camada", new = "camada_nome")
ctb0028_layer[, camada_nome := as.character(camada_nome)]
ctb0028_layer[, .N, by = camada_nome]

# amostra_id
# amostra_id is missing. We assume it is NA
ctb0028_layer[, amostra_id := NA]

# profund_sup
data.table::setnames(ctb0028_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0028_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0028_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0028_layer[, profund_sup])

# profund_inf
data.table::setnames(ctb0028_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0028_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0028_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0028_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0028_layer[, profund_inf])

# check for missing layers
any_missing_layer(ctb0028_layer)

# camada_id
ctb0028_layer <- ctb0028_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0028_layer[, camada_id := 1:.N, by = observacao_id]
ctb0028_layer[, .N, by = camada_id]

# terrafina
# terra fina is missing. We model it as a function of soil texture
# new: morfologia_textura; old: Textura (morfologia)
data.table::setnames(ctb0028_layer, old = "Textura (morfologia)", new = "morfologia_textura")
ctb0028_layer[, morfologia_textura := as.character(morfologia_textura)]
ctb0028_layer[, .N, by = morfologia_textura]
# * cascalhenta: terrafina = 1000 - (150 + 500)/2
# * com cascalho: terrafina = 1000 - (80 + 150)/2
# check if the terms appear in morfologia_textura and attribute the values to terrafina
# else, set to 1000
ctb0028_layer[grep("cascalhenta", morfologia_textura), terrafina := 1000 - (150 + 500) / 2]
ctb0028_layer[grep("com cascalho", morfologia_textura), terrafina := 1000 - (80 + 150) / 2]
ctb0028_layer[is.na(terrafina) & camada_nome != "R", terrafina := 1000]
summary(ctb0028_layer[, terrafina])

# argila
# Some A horizons have missing clay content. We keep them as NA for now
data.table::setnames(ctb0028_layer, old = "Argila [g/kg]", new = "argila")
ctb0028_layer[, argila := as.numeric(argila)]
summary(ctb0028_layer[, argila])
ctb0028_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# silte
# Some A horizons have missing silt content. We keep them as NA for now
data.table::setnames(ctb0028_layer, old = "Silte [g/kg]", new = "silte")
ctb0028_layer[, silte := as.numeric(silte)]
summary(ctb0028_layer[, silte])

# areia
# Areia grossa [g/kg]	+ Areia fina [g/kg]
# Some A horizons have missing sand content. We keep them as NA for now
data.table::setnames(ctb0028_layer, old = "Areia grossa [g/kg]", new = "areia_grossa")
data.table::setnames(ctb0028_layer, old = "Areia fina [g/kg]", new = "areia_fina")
ctb0028_layer[, areia := as.numeric(areia_grossa) + as.numeric(areia_fina)]
summary(ctb0028_layer[, areia])

# carbono
# Matéria orgânica [g/kg]
# Some A horizons have missing carbon content. We keep them as NA for now
data.table::setnames(ctb0028_layer, old = "Matéria orgânica [g/kg]", new = "carbono")
ctb0028_layer[, carbono := as.numeric(carbono) * 0.58]
summary(ctb0028_layer[, carbono])

# ctc
# Some A horizons have missing cec content. We keep them as NA for now
data.table::setnames(ctb0028_layer, old = "Valor T [cmolc/kg]", new = "ctc")
ctb0028_layer[, ctc := as.numeric(ctc)]
summary(ctb0028_layer[, ctc])

# ph
# Some A horizons have missing pH. We keep them as NA for now
data.table::setnames(ctb0028_layer, old = "pH em CaCl2", new = "ph")
ctb0028_layer[, ph := as.numeric(ph)]
summary(ctb0028_layer[, ph])

# dsi
ctb0028_layer[, dsi := NA_real_]

str(ctb0028_layer)

# Merge ############################################################################################
# events and layers
ctb0028 <- merge(ctb0028_event, ctb0028_layer, all = TRUE)
ctb0028[, dataset_id := "ctb0028"]
# citation
ctb0028 <- merge(ctb0028, ctb0028_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0028)
# Layers: 77
# Events: 31
# Georeferenced events: 30

# Plot using mapview
if (FALSE) {
  ctb0028_sf <- sf::st_as_sf(
    ctb0028[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0028_sf["argila"])
}

# Write to disk ####################################################################################
ctb0028 <- select_output_columns(ctb0028)
data.table::fwrite(ctb0028, "ctb0028/ctb0028.csv")
data.table::fwrite(ctb0028_event, "ctb0028/ctb0028_event.csv")
data.table::fwrite(ctb0028_layer, "ctb0028/ctb0028_layer.csv")
