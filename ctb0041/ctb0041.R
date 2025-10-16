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
if (!require("mapview")) {
  install.packages("mapview")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0041
# Dados de "MODELOS PREDITIVOS PARA CARBONO ORGÂNICO POR ESPECTRORRADIOMETRIA EM SOLOS DA CAATINGA"
# https://drive.google.com/drive/folders/1WPZCfbYvZF4TbX1VLw7CbXBcRyBnUuaS
gs <- "1Pu92I-RGEg6_sMwGkrgwTu4tUXzi7Metwi7DEkMCRbw"
gid_citation <- 0
gid_validation <- 88779986
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0041_validation <- google_sheet(gs, gid_validation)
str(ctb0041_validation)

# Check for negative validation results
sum(ctb0041_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0041_citation <- google_sheet(gs, gid_citation)
str(ctb0041_citation)

# dataset_titulo
dataset_titulo <- ctb0041_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0041_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0041_citation <- data.table::data.table(
  dataset_id = "ctb0041",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0041_citation)

# event ############################################################################################
ctb0041_event <- google_sheet(gs, gid_event)
str(ctb0041_event)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0041_event, old = "ID do evento", new = "observacao_id")
ctb0041_event[, observacao_id := as.character(observacao_id)]
ctb0041_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0041_event, old = "Ano (coleta)", new = "data_ano")
ctb0041_event[, data_ano := as.integer(data_ano)]
ctb0041_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0041_event[!is.na(data_ano), ano_fonte := "original"]
ctb0041_event[, .N, by = ano_fonte]

# coord_x
# (Long (grau)	+ Long (min) / 60 + Long (seg) / 3600) * -1 -> coord_x
data.table::setnames(ctb0041_event, old = "Long (grau)", new = "coord_x_grau")
data.table::setnames(ctb0041_event, old = "Long (min)", new = "coord_x_min")
data.table::setnames(ctb0041_event, old = "Long (seg)", new = "coord_x_seg")
ctb0041_event[, coord_x := coord_x_grau + coord_x_min / 60 + coord_x_seg / 3600]
ctb0041_event[, coord_x := coord_x * -1]
summary(ctb0041_event[, coord_x])

# coord_y
# (Lat (grau) + Lat (min) / 60 + Lat (seg) / 3600) * -1 -> coord_y
data.table::setnames(ctb0041_event, old = "Lat (grau)", new = "coord_y_grau")
data.table::setnames(ctb0041_event, old = "Lat (min)", new = "coord_y_min")
data.table::setnames(ctb0041_event, old = "Lat (seg)", new = "coord_y_seg")
ctb0041_event[, coord_y := coord_y_grau + coord_y_min / 60 + coord_y_seg / 3600]
ctb0041_event[, coord_y := coord_y * -1]
summary(ctb0041_event[, coord_y])

# Datum (coord) -> coord_datum
# SIRGAS 2000
data.table::setnames(ctb0041_event, old = "Datum (coord)", new = "coord_datum")
ctb0041_event[coord_datum == "SIRGAS 2000", coord_datum := 4674]
ctb0041_event[, coord_datum := as.integer(coord_datum)]
ctb0041_event[, .N, by = coord_datum]

# Transform coordinates to WGS84
ctb0041_event_sf <- sf::st_as_sf(
  ctb0041_event[coord_datum == 4674],
  coords = c("coord_x", "coord_y"), crs = 4674
)
ctb0041_event_sf <- sf::st_transform(ctb0041_event_sf, 4326)
ctb0041_event_sf <- sf::st_coordinates(ctb0041_event_sf)
ctb0041_event[coord_datum == 4674, coord_x := ctb0041_event_sf[, 1]]
ctb0041_event[coord_datum == 4674, coord_y := ctb0041_event_sf[, 2]]
ctb0041_event[coord_datum == 4674, coord_datum := 4326]
rm(ctb0041_event_sf)
summary(ctb0041_event[, .(coord_datum, coord_x, coord_y)])

# Check for duplicated coordinates
ctb0041_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Fonte (coord) -> coord_fonte
# The source of the coordinates is missing. We set it to NA_character_
data.table::setnames(ctb0041_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0041_event[, coord_fonte := as.character(coord_fonte)]
ctb0041_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# As the source of the coordinates is GPS, we set the precision to 30 m.
data.table::setnames(ctb0041_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0041_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0041_event[, coord_precisao := set_gps_precision()]
summary(ctb0041_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0041_event, old = "País", new = "pais_id")
ctb0041_event[, pais_id := as.character(pais_id)]
ctb0041_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0041_event, old = "Estado (UF)", new = "estado_id")
ctb0041_event[, estado_id := as.character(estado_id)]
ctb0041_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0041_event, old = "Município", new = "municipio_id")
ctb0041_event[, municipio_id := as.character(municipio_id)]
ctb0041_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0041_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0041_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0041_event[, amostra_area])

# SiBCS 2013 <- taxon_sibcs
data.table::setnames(ctb0041_event, old = "SiBCS 2013", new = "taxon_sibcs")
ctb0041_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0041_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0041_event[, taxon_st := NA_character_]


# Pedregosidade (superficie)
# this document don't  have pedregosidade info

ctb0041_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# this document don't  have rochosidade info

ctb0041_event[, rochosidade := NA_character_]

str(ctb0041_event)

# layer ############################################################################################
ctb0041_layer <- google_sheet(gs, gid_layer)
str(ctb0041_layer)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0041_layer, old = "ID do evento", new = "observacao_id")
ctb0041_layer[, observacao_id := as.character(observacao_id)]
ctb0041_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0041_layer, old = "ID da camada", new = "camada_nome")
ctb0041_layer[, camada_nome := as.character(camada_nome)]
ctb0041_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0041_layer, old = "ID da amostra", new = "amostra_id")
ctb0041_layer[, amostra_id := as.character(amostra_id)]
ctb0041_layer[, .N, by = amostra_id]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0041_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0041_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0041_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0041_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0041_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0041_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0041_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0041_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0041_layer[, profund_inf])

# Check for missing layers
check_missing_layer(ctb0041_layer)

# Check for repeated layers
check_repeated_layer(ctb0041_layer)

# Compute mid depth
ctb0041_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0041_layer[, profund_mid])

# camada_id
ctb0041_layer <- ctb0041_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0041_layer[, camada_id := 1:.N, by = observacao_id]
ctb0041_layer[, .N, by = camada_id]

# Fração fina (< 2mm) [g/kg] -> terrafina
data.table::setnames(ctb0041_layer, old = "Fração fina (< 2mm) [g/kg]", new = "terrafina")
ctb0041_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0041_layer[, terrafina])

# Argila [g/kg] -> argila
data.table::setnames(ctb0041_layer, old = "Argila [g/kg]", new = "argila")
ctb0041_layer[, argila := as.numeric(argila)]
summary(ctb0041_layer[, argila])

# Silte [g/kg] -> silte
data.table::setnames(ctb0041_layer, old = "Silte [g/kg]", new = "silte")
ctb0041_layer[, silte := as.numeric(silte)]
summary(ctb0041_layer[, silte])

# Areia [g/kg] -> areia
data.table::setnames(ctb0041_layer, old = "Areia [g/kg]", new = "areia")
ctb0041_layer[, areia := as.numeric(areia)]
summary(ctb0041_layer[, areia])

# Check the particle size distribution
ctb0041_layer[, psd := argila + silte + areia]
ctb0041_layer[abs(psd - 1000) <= 50, argila := round(argila / psd * 1000)]
ctb0041_layer[abs(psd - 1000) <= 50, areia := round(areia / psd * 1000)]
ctb0041_layer[abs(psd - 1000) <= 50, silte := 1000 - argila - areia]
ctb0041_layer[, psd := argila + silte + areia]
ctb0041_layer[psd != 1000, ]

# C [g/kg] -> carbono
data.table::setnames(ctb0041_layer, old = "C [g/kg]", new = "carbono")
ctb0041_layer[, carbono := as.numeric(carbono)]
summary(ctb0041_layer[, carbono])

# pH em água -> ph
data.table::setnames(ctb0041_layer, old = "pH em água", new = "ph")
ctb0041_layer[, ph := as.numeric(ph)]
summary(ctb0041_layer[, ph])

# CTC [cmolc/dm^3] -> ctc
data.table::setnames(ctb0041_layer, old = "CTC pH 7,0 [cmolc/dm^3]", new = "ctc")
ctb0041_layer[, ctc := as.numeric(ctc)]
summary(ctb0041_layer[, ctc])

# Ds aparente [g/cm^3] -> dsi
data.table::setnames(ctb0041_layer, old = "Ds aparente [g/cm^3]", new = "dsi")
ctb0041_layer[, dsi := as.numeric(dsi)]
summary(ctb0041_layer[, dsi])

str(ctb0041_layer)

# Merge ############################################################################################
# events and layers
ctb0041 <- merge(ctb0041_event, ctb0041_layer, all = TRUE)
ctb0041[, dataset_id := "ctb0041"]
# citation
ctb0041 <- merge(ctb0041, ctb0041_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0041)
# Layers: 54
# Events: 54
# Georeferenced events: 54

# Plot using mapview
if (FALSE) {
  ctb0041_sf <- sf::st_as_sf(
    ctb0041[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0041_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0041 <- select_output_columns(ctb0041)
data.table::fwrite(ctb0041, "ctb0041/ctb0041.csv")
data.table::fwrite(ctb0041_event, "ctb0041/ctb0041_event.csv")
data.table::fwrite(ctb0041_layer, "ctb0041/ctb0041_layer.csv")
