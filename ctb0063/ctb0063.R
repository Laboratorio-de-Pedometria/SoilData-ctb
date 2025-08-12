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
# ctb0063
# Dados de "Levantamento de reconhecimento de alta intensidade dos solos da Apa de Cafuringa-DF,
# escala 1:100.000"
# https://drive.google.com/drive/folders/1xCfTSFF5fk5izBf7kjtkuJqHss-jddLf
gs <- "1ANGyAqMCzT-iWZPziHCUUilSGDTfQCfl"
gid_validation <- 1974932169
gid_citation <- 923063661
gid_event <- 1636375983
gid_layer <- 2135436033

# validation #######################################################################################
ctb0063_validation <- google_sheet(gs, gid_validation)
str(ctb0063_validation)

# Check for negative validation results
sum(ctb0063_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0063_citation <- google_sheet(gs, gid_citation)
str(ctb0063_citation)

# dataset_titulo
dataset_titulo <- ctb0063_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0063_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0063_citation <- data.table::data.table(
  dataset_id = "ctb0063",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0063_citation)

# event ############################################################################################
ctb0063_event <- google_sheet(gs, gid_event)
str(ctb0063_event)

# Process fields
# Fonte dos dados -> dados_fonte
data.table::setnames(ctb0063_event, old = "Fonte dos dados", new = "dados_fonte")
ctb0063_event[, dados_fonte := as.character(dados_fonte)]
ctb0063_event[, .N, by = dados_fonte]

# ID do evento -> observacao_id
data.table::setnames(ctb0063_event, old = "ID do evento", new = "observacao_id")
ctb0063_event[, observacao_id := as.character(observacao_id)]
ctb0063_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
# 2025-05-25 The year of collection is missing. Analysis of the existing documents suggests that
# the data was collected between 1998 and 2002. As the soil survey report was published in 2002,
# we will set the year of collection to 2000.
data.table::setnames(ctb0063_event, old = "Ano (coleta)", new = "data_ano")
ctb0063_event[, data_ano := as.integer(data_ano)]
ctb0063_event[, data_ano := 2000]
ctb0063_event[, data_fonte := "estimativa"]
ctb0063_event[, .N, by = data_ano]

# Longitude -> coord_x
# 2025-05-25 The coordinates are missing
data.table::setnames(ctb0063_event, old = "Longitude", new = "coord_x")
ctb0063_event[, coord_x := as.numeric(coord_x)]
summary(ctb0063_event[, coord_x])

# Latitude -> coord_y
# 2025-05-25 The coordinates are missing
data.table::setnames(ctb0063_event, old = "Latitude", new = "coord_y")
ctb0063_event[, coord_y := as.numeric(coord_y)]
summary(ctb0063_event[, coord_y])

# Datum (coord) -> coord_datum
# 2025-05-25 The datum of the coordinates is missing
data.table::setnames(ctb0063_event, old = "Datum (coord)", new = "coord_datum")
ctb0063_event[, coord_datum := as.character(coord_datum)]
ctb0063_event[, .N, by = coord_datum]

# Check for duplicated coordinates
# 2025-05-25 The coordinates are missing
ctb0063_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Fonte (coord) -> coord_fonte
# 2025-05-25 The source of the coordinates is missing
data.table::setnames(ctb0063_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0063_event[, coord_fonte := as.character(coord_fonte)]
ctb0063_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# 2025-05-25 The precision of the coordinates is missing
data.table::setnames(ctb0063_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0063_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0063_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0063_event, old = "País", new = "pais_id")
ctb0063_event[, pais_id := as.character(pais_id)]
ctb0063_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0063_event, old = "Estado (UF)", new = "estado_id")
ctb0063_event[, estado_id := as.character(estado_id)]
ctb0063_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0063_event, old = "Município", new = "municipio_id")
ctb0063_event[, municipio_id := as.character(municipio_id)]
ctb0063_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0063_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0063_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0063_event[, amostra_area])

# SiBCS 1999 -> taxon_sibcs
data.table::setnames(ctb0063_event, old = "SiBCS (1999)", new = "taxon_sibcs")
ctb0063_event[, taxon_sibcs := as.character(taxon_sibcs)]
# 2025-05-25 There is an error in the source document. Lisiane will check.
ctb0063_event[observacao_id == "64-65-66", taxon_sibcs := "Latossolo Vermelho distrófico"]
ctb0063_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo pelo Soil Taxonomy não está disponível neste dataset.
ctb0063_event[, taxon_st := NA_character_]

str(ctb0063_event)

# layer ############################################################################################
ctb0063_layer <- google_sheet(gs, gid_layer)
str(ctb0063_layer)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0063_layer, old = "ID do evento", new = "observacao_id")
ctb0063_layer[, observacao_id := as.character(observacao_id)]
ctb0063_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
# Samples were taken from the A and B horizons. So there is an empty layer between them.
data.table::setnames(ctb0063_layer, old = "ID da camada", new = "camada_nome")
ctb0063_layer[, camada_nome := as.character(camada_nome)]
ctb0063_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0063_layer, old = "ID da amostra", new = "amostra_id")
ctb0063_layer[, amostra_id := as.character(amostra_id)]
ctb0063_layer[, .N, by = amostra_id]

# Profundidade inicial [cm] -> profund_sup
# 2024-12-20 The presence of negative values indicates that there are organic layers on the surface.
data.table::setnames(ctb0063_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0063_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0063_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0063_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0063_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0063_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0063_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0063_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0063_layer[, profund_inf])

# Check for missing layers
check_missing_layer(ctb0063_layer)

# Compute mid depth
ctb0063_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0063_layer[, profund_mid])

# camada_id
ctb0063_layer <- ctb0063_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0063_layer[, camada_id := 1:.N, by = observacao_id]
ctb0063_layer[, .N, by = camada_id]

# Terra fina [%] * 10 -> terrafina
# 2024-12-20 Missing. Lisiane will check.
data.table::setnames(ctb0063_layer, old = "Terra fina [%]", new = "terrafina")
ctb0063_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0063_layer[, terrafina])

# Argila [%] * 10 -> argila
data.table::setnames(ctb0063_layer, old = "Argila [%]", new = "argila")
ctb0063_layer[, argila := as.numeric(argila) * 10]
summary(ctb0063_layer[, argila])

# silte
# 2024-12-20 Missing. We set it to NA. Lisiane will check.
ctb0063_layer[, silte := NA_real_]

# areia
# 2024-12-20 Missing. we set it to NA. Lisiane will check.
ctb0063_layer[, areia := NA_real_]

# C [g/kg] -> carbono
data.table::setnames(ctb0063_layer, old = "C [g/kg]", new = "carbono")
ctb0063_layer[, carbono := as.numeric(carbono)]
summary(ctb0063_layer[, carbono])

# ph em H2O -> ph
data.table::setnames(ctb0063_layer, old = "ph em H2O", new = "ph")
ctb0063_layer[, ph := as.numeric(ph)]
summary(ctb0063_layer[, ph])

# T [cmolc/dm^3] -> ctc
data.table::setnames(ctb0063_layer, old = "T [cmolc/dm^3]", new = "ctc")
ctb0063_layer[, ctc := as.numeric(ctc)]
summary(ctb0063_layer[, ctc])

# densidade
# 2024-12-20 Missing. We set it to NA. Lisiane will check.
ctb0063_layer[, dsi := NA_real_]

str(ctb0063_layer)

# Merge ############################################################################################
# events and layers
ctb0063 <- merge(ctb0063_event, ctb0063_layer, all = TRUE)
ctb0063[, dataset_id := "ctb0063"]
# citation
ctb0063 <- merge(ctb0063, ctb0063_citation, by = "dataset_id", all.x = TRUE)

# Keep only rows where the source of the data is "Original"
ctb0063 <- ctb0063[dados_fonte == "Original"]

summary_soildata(ctb0063)
# Layers: 40
# Events: 15
# Georeferenced events: 0

# Plot using mapview
if (FALSE) {
  ctb0063_sf <- sf::st_as_sf(
    ctb0063[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0063_sf, zcol = "argila")
}

# Write to disk ####################################################################################
ctb0063 <- select_output_columns(ctb0063)
data.table::fwrite(ctb0063, "ctb0063/ctb0063.csv")
data.table::fwrite(ctb0063_event, "ctb0063/ctb0063_event.csv")
data.table::fwrite(ctb0063_layer, "ctb0063/ctb0063_layer.csv")
