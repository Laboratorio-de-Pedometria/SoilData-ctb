# author: Alessandro Samuel-Rosa
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
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
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

# ID do evento -> observacao_id

data.table::setnames(ctb0011_event, old = "ID do evento", new = "observacao_id")
ctb0011_event[, observacao_id := as.character(observacao_id)]
# Check if there are duplicated IDs
any(table(ctb0011_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_ano
data.table::setnames(ctb0011_event, old = "Ano (coleta)", new = "data_ano")
ctb0011_event[, data_ano := as.integer(data_ano)]
ctb0011_event[, .N, by = data_ano]

# ano_fonte
# A fonte da data de coleta não foi especificada. Assumimos que é "original".
ctb0011_event[!is.na(data_ano), ano_fonte := "original"]
ctb0011_event[, .N, by = ano_fonte]

# coord_x
# Longitude grau -> coord_x
# coord_x = (Longitude grau + Longitude minuto / 60) * -1
data.table::setnames(ctb0011_event, old = "Longitude grau", new = "longitude_grau")
ctb0011_event[, longitude_grau := as.numeric(longitude_grau)]
data.table::setnames(ctb0011_event, old = "Longitude minuto", new = "longitude_minuto")
ctb0011_event[, longitude_minuto := as.numeric(longitude_minuto)]
ctb0011_event[, coord_x := (longitude_grau + longitude_minuto / 60) * -1]
summary(ctb0011_event[, coord_x])

# coord_y
# Latitude grau -> coord_y
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
# coord_datum is missing. We assume it is WGS 84 (EPSG:4326)
ctb0011_event[, coord_datum := 4326]

# coord_precisao
# coord_precisao is missing. We assume it is 30.0
ctb0011_event[, coord_precisao := 30.0]

# coord_fonte
# coord_fonte is missing. We assume it is GPS
ctb0011_event[, coord_fonte := "GPS"]

# pais_id
# old: País
data.table::setnames(ctb0011_event, old = "País", new = "pais_id")
ctb0011_event[, pais_id := as.character(pais_id)]
ctb0011_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0011_event, old = "Estado (UF)", new = "estado_id")
ctb0011_event[, estado_id := as.character(estado_id)]
ctb0011_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0011_event, old = "Município", new = "municipio_id")
ctb0011_event[, municipio_id := as.character(municipio_id)]
ctb0011_event[, .N, by = municipio_id]

# amostra_area
# amostra_area is missing. We assume it is 1.0
ctb0011_event[, amostra_area := 1.0]

# Classificação do solo -> taxon_sibcs
data.table::setnames(ctb0011_event, old = "Classificação do solo", new = "taxon_sibcs")
ctb0011_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0011_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0011_event[, taxon_st := NA_character_]


# Pedregosidade (superficie)
# review the work at another time

ctb0011_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0011_event[, rochosidade := ("Não Rochoso")]

str(ctb0011_event)

# layer ############################################################################################
ctb0011_layer <- google_sheet(gs, gid_layer)
str(ctb0011_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0011_layer, old = "ID do evento", new = "observacao_id")
ctb0011_layer[, observacao_id := as.character(observacao_id)]
ctb0011_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0011_layer, old = "ID da camada", new = "camada_nome")
ctb0011_layer[, camada_nome := as.character(camada_nome)]
# Filter out samples from saprolite
ctb0011_layer <- ctb0011_layer[Saprolito == "FALSE"]

# amostra_id
ctb0011_layer[, amostra_id := NA]

# Limite Superior [cm] -> profund_sup
data.table::setnames(ctb0011_layer, old = "Limite Superior [cm]", new = "profund_sup")
ctb0011_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0011_layer[, profund_sup])

# Limite Inferior [cm] -> profund_inf
data.table::setnames(ctb0011_layer, old = "Limite Inferior [cm]", new = "profund_inf")
ctb0011_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0011_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0011_layer[, profund_inf])

# check for missing layers
any_missing_layer(ctb0011_layer)

# camada_id
ctb0011_layer <- ctb0011_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0011_layer[, camada_id := 1:.N, by = observacao_id]
ctb0011_layer[, .N, by = camada_id]

# terrafina
# Terra fina [g/kg] -> terrafina
data.table::setnames(ctb0011_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0011_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0011_layer[, terrafina])

# Argila [g/kg] -> argila
# Argila is missing for some layers: R layers and RCr layers with very little fine earth material
data.table::setnames(ctb0011_layer, old = "Argila [g/kg]", new = "argila")
ctb0011_layer[, argila := as.numeric(argila)]
summary(ctb0011_layer[, argila])
ctb0011_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# Silte [g/kg] -> silte
data.table::setnames(ctb0011_layer, old = "Silte [g/kg]", new = "silte")
ctb0011_layer[, silte := as.numeric(silte)]
summary(ctb0011_layer[, silte])

# areia
# Areia grossa [g/kg] -> areia_grossa
data.table::setnames(ctb0011_layer, old = "Areia grossa [g/kg]", new = "areia_grossa")
# Areia fina [g/kg] -> areia_fina
data.table::setnames(ctb0011_layer, old = "Areia fina [g/kg]", new = "areia_fina")
# areia
ctb0011_layer[, areia := as.numeric(areia_grossa) + as.numeric(areia_fina)]
summary(ctb0011_layer[, areia])

# Corg [g/kg] -> carbono
data.table::setnames(ctb0011_layer, old = "Corg [g/kg]", new = "carbono")
ctb0011_layer[, carbono := as.numeric(carbono)]
summary(ctb0011_layer[, carbono])

# CTC pH 7,0 [cmolc/kg] -> ctc
data.table::setnames(ctb0011_layer, old = "CTC pH 7,0 [cmolc/kg]", new = "ctc")
ctb0011_layer[, ctc := as.numeric(ctc)]
summary(ctb0011_layer[, ctc])

# pH em H_2O -> ph
data.table::setnames(ctb0011_layer, old = "pH em H_2O", new = "ph")
ctb0011_layer[, ph := as.numeric(ph)]
summary(ctb0011_layer[, ph])

# dsi
ctb0011_layer[, dsi := NA_real_]

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
