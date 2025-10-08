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
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0021
# Dados de "Guia de campo da excursão pedológica da rocha à garrafa: geologia, solos e vinhos"
# https://drive.google.com/drive/u/0/folders/1iBLW28Ul8o3pb5GFOmuZgjWIroavgf11
gs <- "1HHKvkFLS5Mu7NBEmxW1jca902PzOWD2Vj4EdMM-DQhQ"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0021_validation <- google_sheet(gs, gid_validation)
str(ctb0021_validation)

# Check for negative validation results
sum(ctb0021_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0021_citation <- google_sheet(gs, gid_citation)
str(ctb0021_citation)

# dataset_titulo
dataset_titulo <- ctb0021_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0021_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0021_citation <- data.table::data.table(
  dataset_id = "ctb0021",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0021_citation)

# event ############################################################################################
ctb0021_event <- google_sheet(gs, gid_event)
str(ctb0021_event)

# Process fields

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0021_event, old = "ID do evento", new = "observacao_id")
ctb0021_event[, observacao_id := as.character(observacao_id)]
ctb0021_event[, .N, by = observacao_id][N > 1]

# Ano (coleta) -> data_ano
data.table::setnames(ctb0021_event, old = "Ano (coleta)", new = "data_ano")
ctb0021_event[, data_ano := as.integer(data_ano)]
ctb0021_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta está especificada no documento de origem dos dados.
ctb0021_event[, ano_fonte := "original"]
ctb0021_event[, .N, by = ano_fonte]

# coord_x
# (Long (grau) + Long (min) / 60 + Long (seg) / 3600) * -1
data.table::setnames(ctb0021_event, old = "Long (grau)", new = "coord_x_grau")
data.table::setnames(ctb0021_event, old = "Long (min)", new = "coord_x_min")
data.table::setnames(ctb0021_event, old = "Long (seg)", new = "coord_x_seg")
ctb0021_event[, coord_x := coord_x_grau + coord_x_min / 60 + coord_x_seg / 3600]
ctb0021_event[, coord_x := coord_x * -1]
summary(ctb0021_event[, coord_x])

# coord_y
# (Lat (grau) + Lat (min) / 60 + Lat (seg) / 3600) * -1
data.table::setnames(ctb0021_event, old = "Lat (grau)", new = "coord_y_grau")
data.table::setnames(ctb0021_event, old = "Lat (min)", new = "coord_y_min")
data.table::setnames(ctb0021_event, old = "Lat (seg)", new = "coord_y_seg")
ctb0021_event[, coord_y := coord_y_grau + coord_y_min / 60 + coord_y_seg / 3600]
ctb0021_event[, coord_y := coord_y * -1]
summary(ctb0021_event[, coord_y])

# coord_datum
# SIRGAS 2000
data.table::setnames(ctb0021_event, old = "Datum (coord)", new = "coord_datum")
ctb0021_event[, coord_datum := 4674]
ctb0021_event_sf <- sf::st_as_sf(ctb0021_event, coords = c("coord_x", "coord_y"), crs = 4674)
ctb0021_event_sf <- sf::st_transform(ctb0021_event_sf, crs = 4326)
ctb0021_event_sf <- sf::st_coordinates(ctb0021_event_sf)
ctb0021_event[, coord_x := ctb0021_event_sf[, 1]]
ctb0021_event[, coord_y := ctb0021_event_sf[, 2]]
ctb0021_event[, coord_datum := 4326]
rm(ctb0021_event_sf)

# Check for duplicate coordinates
ctb0021_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_fonte
# old: Fonte (coord)
# new: coord_fonte
data.table::setnames(ctb0021_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0021_event[, coord_fonte := as.character(coord_fonte)]
ctb0021_event[, .N, by = coord_fonte]

# coord_precisao
# old: Precisão (coord) [m]
# new: coord_precisao
# coord_precisao is missing. Because coord_fonte is "GPS", we assume it is 30 meters.
data.table::setnames(ctb0021_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0021_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0021_event[coord_fonte == "GPS", coord_precisao := 30]
summary(ctb0021_event[, coord_precisao])

# pais_id
# old: País
# new: pais_id
data.table::setnames(ctb0021_event, old = "País", new = "pais_id")
ctb0021_event[, pais_id := as.character(pais_id)]
ctb0021_event[, .N, by = pais_id]

# estado_id
# old: Estado (UF)
# new: estado_id
data.table::setnames(ctb0021_event, old = "Estado (UF)", new = "estado_id")
ctb0021_event[, estado_id := as.character(estado_id)]
ctb0021_event[, .N, by = estado_id]

# municipio_id
# old: Município
# new: municipio_id
data.table::setnames(ctb0021_event, old = "Município", new = "municipio_id")
ctb0021_event[, municipio_id := as.character(municipio_id)]
ctb0021_event[, .N, by = municipio_id]

# amostra_area
# old: Área amostrada [m^2]
# new: amostra_area
data.table::setnames(ctb0021_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0021_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0021_event[, amostra_area])

# taxon_sibcs
# old: SiBCS
# new: taxon_sibcs
data.table::setnames(ctb0021_event, old = "SiBCS", new = "taxon_sibcs")
ctb0021_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0021_event[, .N, by = taxon_sibcs]

# Soil Taxonomy -> taxon_st
data.table::setnames(ctb0021_event, old = "Soil Taxonomy", new = "taxon_st")
ctb0021_event[, taxon_st := as.character(taxon_st)]
ctb0021_event[, .N, by = taxon_st]


# Pedregosidade (superficie)
data.table::setnames(ctb0021_event, old = "Pedregosidade", new = "pedregosidade")
ctb0021_event[, pedregosidade := as.character(pedregosidade)]
ctb0021_event[, .N, by = pedregosidade]

# Rochosidade (superficie)
data.table::setnames(ctb0021_event, old = "rochosidade", new = "rochosidade")
ctb0021_event[, rochosidade := as.character(rochosidade)]
ctb0021_event[, .N, by = rochosidade]

str(ctb0021_event)

# layer ############################################################################################
ctb0021_layer <- google_sheet(gs, gid_layer)
str(ctb0021_layer)

# Process fields

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0021_layer, old = "ID do evento", new = "observacao_id")
ctb0021_layer[, observacao_id := as.character(observacao_id)]
ctb0021_layer[, .N, by = observacao_id]

# camada_nome
# old: ID da camada
# new: camada_nome
data.table::setnames(ctb0021_layer, old = "ID da camada", new = "camada_nome")
ctb0021_layer[, camada_nome := as.character(camada_nome)]
ctb0021_layer[, .N, by = camada_nome]

# amostra_id
# old: ID da amostra
# new: amostra_id
data.table::setnames(ctb0021_layer, old = "ID da amostra", new = "amostra_id")
ctb0021_layer[, amostra_id := as.character(amostra_id)]
ctb0021_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0021_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0021_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0021_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0021_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0021_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0021_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0021_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0021_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0021_layer[, profund_inf])

# camada_id
ctb0021_layer <- ctb0021_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0021_layer[, camada_id := 1:.N, by = observacao_id]
ctb0021_layer[, .N, by = camada_id]

# Check for missing layers
check_missing_layer(ctb0021_layer)

# terrafina
# The fine earth fraction is available for most layers. For some layers, it is recorder as
# terrafina = >900 g/kg. In this case, we simply remove the > sign and convert it to numeric.
# For other layers such as those with camada_nome = Cr or C, terrafina is missing. We set it to NA.
# For layers with camada_nome = R, we set terrafina to 0.
# old: Terra fina [g/kg]
# new: terrafina
data.table::setnames(ctb0021_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0021_layer[, terrafina := as.character(terrafina)]
ctb0021_layer[grepl(">", terrafina), terrafina := gsub(">", "", terrafina)]
ctb0021_layer[, terrafina := as.numeric(terrafina)]
ctb0021_layer[camada_nome %in% c("Cr", "C"), terrafina := NA_real_]
ctb0021_layer[camada_nome == "R" & is.na(terrafina), terrafina := 0]
summary(ctb0021_layer[, terrafina])
check_empty_layer(ctb0021_layer, "terrafina")

# argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0021_layer, old = "Argila [g/kg]", new = "argila")
ctb0021_layer[, argila := as.numeric(argila)]
summary(ctb0021_layer[, argila])
check_empty_layer(ctb0021_layer, "argila")

# silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0021_layer, old = "Silte [g/kg]", new = "silte")
ctb0021_layer[, silte := as.numeric(silte)]
summary(ctb0021_layer[, silte])
check_empty_layer(ctb0021_layer, "silte")

# areia
# old: Areia [g/kg]
# new: areia
data.table::setnames(ctb0021_layer, old = "Areia [g/kg]", new = "areia")
ctb0021_layer[, areia := as.numeric(areia)]
summary(ctb0021_layer[, areia])
check_empty_layer(ctb0021_layer, "areia")

# carbono
# old: C-org [g/kg]
# new: carbono
data.table::setnames(ctb0021_layer, old = "C-org [g/kg]", new = "carbono")
ctb0021_layer[, carbono := as.numeric(carbono)]
summary(ctb0021_layer[, carbono])
check_empty_layer(ctb0021_layer, "carbono")

# ctc
# old: CTC pH7 [cmolc/kg]
# new: ctc
data.table::setnames(ctb0021_layer, old = "CTC pH7 [cmolc/kg]", new = "ctc")
ctb0021_layer[, ctc := as.numeric(ctc)]
summary(ctb0021_layer[, ctc])
check_empty_layer(ctb0021_layer, "ctc")

# ph
# old: pH H2O
# new: ph
data.table::setnames(ctb0021_layer, old = "pH H2O", new = "ph")
ctb0021_layer[, ph := as.numeric(ph)]
summary(ctb0021_layer[, ph])
check_empty_layer(ctb0021_layer, "ph")

# dsi
# dsi is missing. We assume it is NA
ctb0021_layer[, dsi := NA_real_]

str(ctb0021_layer)

# Merge ############################################################################################
# events and layers
ctb0021 <- merge(ctb0021_event, ctb0021_layer, all = TRUE)
ctb0021[, dataset_id := "ctb0021"]
# citation
ctb0021 <- merge(ctb0021, ctb0021_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0021)
# Layers: 42
# Events: 8
# Georeferenced events: 8

# Plot using mapview
if (FALSE) {
  ctb0021_sf <- sf::st_as_sf(
    ctb0021[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0021_sf["argila"])
}

# Write to disk ####################################################################################
ctb0021 <- select_output_columns(ctb0021)
data.table::fwrite(ctb0021, "ctb0021/ctb0021.csv")
data.table::fwrite(ctb0021_event, "ctb0021/ctb0021_event.csv")
data.table::fwrite(ctb0021_layer, "ctb0021/ctb0021_layer.csv")
