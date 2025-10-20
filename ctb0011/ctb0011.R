# author: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}

# Source helper functions
source("./helper.R") 

# Google Sheet #####################################################################################
# ctb0011
# Mineralogia, morfologia e classificação de Saprolitos e Neossolos derivados de rochas vulcânicas
# no Rio Grande do Sul
# https://drive.google.com/drive/u/0/folders/1-R1Up6oTXTIophop_5AiI72URrd6abz4
gs <- "1ksiT2Sz0kgD6BAve1GyQGLUefrZ1EainxzfofNsJwvk"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0011_validation <- google_sheet(gs, gid_validation)
str(ctb0011_validation)

# Check for negative validation results
sum(ctb0011_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0011_citation <- google_sheet(gs, gid_citation)
str(ctb0011_citation)

# dataset_titulo
dataset_titulo <- ctb0011_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0011_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0011_citation <- data.table::data.table(
  dataset_id = "ctb0011",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0011_citation)

# event ############################################################################################
ctb0011_event <- google_sheet(gs, gid_event)
str(ctb0011_event)

# Process fields

# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0011_event, old = "ID do evento", new = "observacao_id")
ctb0011_event[, observacao_id := as.character(observacao_id)]
# Check if there are duplicated IDs
any(table(ctb0011_event[, observacao_id]) > 1)

# old: Ano (coleta)
# new: data_ano
data.table::setnames(ctb0011_event, old = "Ano (coleta)", new = "data_ano")
ctb0011_event[, data_ano := as.integer(data_ano)]
ctb0011_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta (ano) foi informada no trabalho de origem dos dados.
ctb0011_event[!is.na(data_ano), ano_fonte := "original"]
ctb0011_event[, .N, by = ano_fonte]

# old: Longitude grau
# new: coord_x
# coord_x = (Longitude grau + Longitude minuto / 60) * -1
data.table::setnames(ctb0011_event, old = "Longitude grau", new = "longitude_grau")
ctb0011_event[, longitude_grau := as.numeric(longitude_grau)]
data.table::setnames(ctb0011_event, old = "Longitude minuto", new = "longitude_minuto")
ctb0011_event[, longitude_minuto := as.numeric(longitude_minuto)]
ctb0011_event[, coord_x := (longitude_grau + longitude_minuto / 60) * -1]
summary(ctb0011_event[, coord_x])

# old: Latitude grau
# new: coord_y
# coord_y = (Latitude grau + Latitude minuto / 60) * -1
data.table::setnames(ctb0011_event, old = "Latitude grau", new = "latitude_grau")
ctb0011_event[, latitude_grau := as.numeric(latitude_grau)]
data.table::setnames(ctb0011_event, old = "Latitude minuto", new = "latitude_minuto")
ctb0011_event[, latitude_minuto := as.numeric(latitude_minuto)]
ctb0011_event[, coord_y := (latitude_grau + latitude_minuto / 60) * -1]
summary(ctb0011_event[, coord_y])

# Check for duplicate coordinates
ctb0011_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_datum
# The coordinate reference system (datum) is not informed in the original work.
# Since the coordinates are in degrees and the study was carried out in 2007, we assume it is a
# geographic coordinate system based on the WGS84 datum (EPSG: 4326).
ctb0011_event[!is.na(coord_x) & !is.na(coord_y), coord_datum := 4326]
ctb0011_event[, .N, by = coord_datum]

# coord_fonte
# The source of the coordinates is not informed in the original work. However, we assume that the
# coordinates were collected in the field using a GPS device.
ctb0011_event[!is.na(coord_x) & !is.na(coord_y), coord_fonte := "GPS"]
ctb0011_event[, .N, by = coord_fonte]

# coord_precisao
# The precision of the coordinates is not informed in the original work. As we assumed that the
# coordinates were collected in the field using a GPS device, we assume a precision of 30 m.
ctb0011_event[!is.na(coord_x) & !is.na(coord_y), coord_precisao := 30.0]
summary(ctb0011_event[, coord_precisao])

# old: País
# new: pais_id
data.table::setnames(ctb0011_event, old = "País", new = "pais_id")
ctb0011_event[, pais_id := as.character(pais_id)]
ctb0011_event[, .N, by = pais_id]

# old: Estado (UF)
# new: estado_id
data.table::setnames(ctb0011_event, old = "Estado (UF)", new = "estado_id")
ctb0011_event[, estado_id := as.character(estado_id)]
ctb0011_event[, .N, by = estado_id]

# old: Município
# new: municipio_id
data.table::setnames(ctb0011_event, old = "Município", new = "municipio_id")
ctb0011_event[, municipio_id := as.character(municipio_id)]
ctb0011_event[, .N, by = municipio_id]

# amostra_area
# The area of the sampling unit (amostra_area) is not informed in the original work. Since
# the sampling was done in soil profiles, we assume that the area of the sampling unit is 1.0 m².
ctb0011_event[, amostra_area := 1.0]
summary(ctb0011_event[, amostra_area])

# old: Classificação do solo
# new: taxon_sibcs
data.table::setnames(ctb0011_event, old = "Classificação do solo", new = "taxon_sibcs")
ctb0011_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0011_event[, .N, by = taxon_sibcs]

# taxon_st
# The soil classification according to the USDA Soil Taxonomy (taxon_st) is not informed in the
# original work.
ctb0011_event[, taxon_st := NA_character_]

# old: Pedregosidade
# new: pedregosidade
data.table::setnames(ctb0011_event, old = "Pedregosidade", new = "pedregosidade")
ctb0011_event[, pedregosidade := as.character(pedregosidade)]
ctb0011_event[, .N, by = pedregosidade]

# old: Rochosidade
# new: rochosidade
data.table::setnames(ctb0011_event, old = "Rochosidade", new = "rochosidade")
ctb0011_event[, rochosidade := as.character(rochosidade)]
ctb0011_event[, .N, by = rochosidade]

str(ctb0011_event)

# layer ############################################################################################
ctb0011_layer <- google_sheet(gs, gid_layer)
str(ctb0011_layer)

# Process fields

# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0011_layer, old = "ID do evento", new = "observacao_id")
ctb0011_layer[, observacao_id := as.character(observacao_id)]
ctb0011_layer[, .N, by = observacao_id]

# old: ID da camada
# new: camada_nome
data.table::setnames(ctb0011_layer, old = "ID da camada", new = "camada_nome")
ctb0011_layer[, camada_nome := as.character(camada_nome)]

# Saprolite
# This dataset also contains samples from saprolite (saprolito) found in different parts of the
# soil profiles. We will filter them out.
ctb0011_layer <- ctb0011_layer[Saprolito == "FALSE"]

# amostra_id
ctb0011_layer[, amostra_id := NA]

# old: Limite Superior [cm]
# new: profund_sup
data.table::setnames(ctb0011_layer, old = "Limite Superior [cm]", new = "profund_sup")
ctb0011_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0011_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0011_layer[, profund_sup])

# old: Limite Inferior [cm]
# new: profund_inf
data.table::setnames(ctb0011_layer, old = "Limite Inferior [cm]", new = "profund_inf")
ctb0011_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0011_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0011_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0011_layer[, profund_inf])

# check for equal layer depths
ctb0011_layer[profund_sup == profund_inf, .(observacao_id, camada_nome, profund_sup, profund_inf)]

# check for missing layers
any_missing_layer(ctb0011_layer)

# camada_id
ctb0011_layer <- ctb0011_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0011_layer[, camada_id := 1:.N, by = observacao_id]
ctb0011_layer[, .N, by = camada_id]

# old: Terra fina [g/kg]
# new: terrafina
data.table::setnames(ctb0011_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0011_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0011_layer[, terrafina])
# Terrafina is missing for some layers: R layers.
ctb0011_layer[is.na(terrafina), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]

# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0011_layer, old = "Argila [g/kg]", new = "argila")
ctb0011_layer[, argila := as.numeric(argila)]
summary(ctb0011_layer[, argila])
# Clay is missing for R layers and for RCr and Cr layers with very little fine earth material.
ctb0011_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina, argila)]

# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0011_layer, old = "Silte [g/kg]", new = "silte")
ctb0011_layer[, silte := as.numeric(silte)]
summary(ctb0011_layer[, silte])
# Silte is missing for some layers: R layers. Also, for some layers with very little fine earth
# material (e.g., RCr and Cr layers).
ctb0011_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina, silte)]

# old: Areia grossa [g/kg]
# new: areia_grossa
data.table::setnames(ctb0011_layer, old = "Areia grossa [g/kg]", new = "areia_grossa")
# old: Areia fina [g/kg]
# new: areia_fina
data.table::setnames(ctb0011_layer, old = "Areia fina [g/kg]", new = "areia_fina")
# areia
ctb0011_layer[, areia := as.numeric(areia_grossa) + as.numeric(areia_fina)]
summary(ctb0011_layer[, areia])
# Sand is missing for some layers: R layers. Also, for some layers with very little fine earth
# material (e.g., RCr and Cr layers).
ctb0011_layer[is.na(areia), .(observacao_id, camada_nome, profund_inf, profund_inf, terrafina, areia)]

# old: Corg [g/kg]
# new: carbono
data.table::setnames(ctb0011_layer, old = "Corg [g/kg]", new = "carbono")
ctb0011_layer[, carbono := as.numeric(carbono)]
summary(ctb0011_layer[, carbono])
# Carbon is missing for some layers: R layers. Also, for some layers with very little fine earth
# material (e.g., RCr and Cr layers).
ctb0011_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina, carbono)]

# old: CTC pH 7,0 [cmolc/kg]
# new: ctc
data.table::setnames(ctb0011_layer, old = "CTC pH 7,0 [cmolc/kg]", new = "ctc")
ctb0011_layer[, ctc := as.numeric(ctc)]
summary(ctb0011_layer[, ctc])
# CTC is missing for some layers: R layers. Also, for some layers with very little fine earth
# material (e.g., RCr and Cr layers).
ctb0011_layer[is.na(ctc), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina, ctc)]

# old: pH em H_2O
# new: ph
data.table::setnames(ctb0011_layer, old = "pH em H_2O", new = "ph")
ctb0011_layer[, ph := as.numeric(ph)]
summary(ctb0011_layer[, ph])
# pH is missing for some layers: R layers. Also, for some layers with very little fine earth
# material (e.g., RCr and Cr layers).
ctb0011_layer[is.na(ph), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina, ph)]

# dsi
# The soil bulk density (dsi) is not informed in the original work.
ctb0011_layer[, dsi := NA_real_]

str(ctb0011_layer)

# Merge ############################################################################################
# events and layers
ctb0011 <- merge(ctb0011_event, ctb0011_layer, all = TRUE)
ctb0011[, dataset_id := "ctb0011"]
# citation
ctb0011 <- merge(ctb0011, ctb0011_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0011)
# Layers: 19
# Events: 5
# Georeferenced events: 5

# Plot using mapview
if (FALSE) {
  ctb0011_sf <- sf::st_as_sf(
    ctb0011[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0011_sf["argila"])
}

# Write to disk ####################################################################################
ctb0011 <- select_output_columns(ctb0011)
data.table::fwrite(ctb0011, "ctb0011/ctb0011.csv")
data.table::fwrite(ctb0011_event, "ctb0011/ctb0011_event.csv")
data.table::fwrite(ctb0011_layer, "ctb0011/ctb0011_layer.csv")
