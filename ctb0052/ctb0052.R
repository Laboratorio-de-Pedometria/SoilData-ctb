# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}
if (!requireNamespace("ranger")) {
     
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0052
# Dados de "Variação de parâmetros dendrométricos de Pinus taeda L. e a distribuição espacial de
# atributos do solo por técnicas de mapeamento digital de solos"
# 
# Google Drive: https://drive.google.com/drive/u/1/folders/1o1y45aiXr4n5ou3A3wpTQ2Beb0DNCGob
# NotebookLM: https://notebooklm.google.com/notebook/f4c92e6c-f44a-4abe-8a12-1abc0ece53cd

# There are some events without layers. Perhaps we can fill them using near by events or other 
# techniques.

# citation #########################################################################################
ctb0052_citation <- data.table::data.table(
  dataset_id = "ctb0052",
  dataset_titulo = "Variação de parâmetros dendrométricos de Pinus taeda L. e a distribuição espacial de atributos do solo por técnicas de mapeamento digital de solos",
  dataset_licenca = "CC-BY"
)

# event ############################################################################################
# Temporarily read from local CSV: this needs to go to Google Drive
ctb0052_event <- data.table::fread("ctb0052/ctb0052_event.csv")
str(ctb0052_event)

# Process fields

# observacao_id
# OLD: id_ponto
# NEW: observacao_id
data.table::setnames(ctb0052_event, old = "id_ponto", new = "observacao_id")
ctb0052_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
ctb0052_event[, .N, by = observacao_id][N > 1]

# data_ano
# OLD: data
# NEW: data_ano
data.table::setnames(ctb0052_event, old = "data", new = "data_ano")
ctb0052_event[, data_ano := as.integer(data_ano)]
ctb0052_event[, .N, by = data_ano]

# ano_fonte = original
ctb0052_event[, ano_fonte := "original"]

# Longitude
# coord_x
# Longitude is given in UTM coordinates (EPSG:31982)
ctb0052_event[, coord_x := as.numeric(coord_x)]
summary(ctb0052_event[, coord_x])

# Latitude
# coord_y
# Latitude is given in UTM coordinates (EPSG:31982)
ctb0052_event[, coord_y := as.numeric(coord_y)]
summary(ctb0052_event[, coord_y])

# Coordinate reference system
# old: datum
# new: coord_datum
# The coordinates are given in UTM zone 22S (EPSG:31982). We convert them to WGS84 (EPSG:4326).
data.table::setnames(ctb0052_event, old = "datum", new = "coord_datum")
ctb0052_event[, coord_datum := gsub("SIRGAS 2000 22 S", 31982, coord_datum)]
ctb0052_event[, coord_datum := as.integer(coord_datum)]
ctb0052_event[, .N, by = coord_datum]
# transform coordinates from EPSG:31982 to EPSG:4326
ctb0052_event_sf <- sf::st_as_sf(
  ctb0052_event[coord_datum == 31982],
  coords = c("coord_x", "coord_y"), crs = 31982
)
ctb0052_event_sf <- sf::st_transform(ctb0052_event_sf, 4326)
ctb0052_event_sf <- sf::st_coordinates(ctb0052_event_sf)
ctb0052_event[coord_datum == 31982, coord_x := ctb0052_event_sf[, 1]]
ctb0052_event[coord_datum == 31982, coord_y := ctb0052_event_sf[, 2]]
ctb0052_event[coord_datum == 31982, coord_datum := 4326]
rm(ctb0052_event_sf)
ctb0052_event[, .N, by = coord_datum]
summary(ctb0052_event[, .(coord_x, coord_y)])

# check for duplicated coordinates
ctb0052_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0052_event[coord_duplicated == TRUE & !is.na(coord_x), .(observacao_id, coord_x, coord_y)]
ctb0052_event[, coord_duplicated := NULL]

# Source of coordinates
# old: Fonte das coordenadas
# new: coord_fonte
data.table::setnames(ctb0052_event, old = "Fonte das coordenadas", new = "coord_fonte")
ctb0052_event[, coord_fonte := as.character(coord_fonte)]
ctb0052_event[, .N, by = coord_fonte]

# coord_precisao
# The precision of the coordinates is missing. As the source of the coordinates is GPS, we set
# it to 30 m.
ctb0052_event[, coord_precisao := 30]
summary(ctb0052_event[, coord_precisao])

# pais_id
# old: País
# new: pais_id
data.table::setnames(ctb0052_event, old = "País", new = "pais_id")
ctb0052_event[, pais_id := as.character(pais_id)]
ctb0052_event[, .N, by = pais_id]

# estado_id
# old: Estado
# new: estado_id
data.table::setnames(ctb0052_event, old = "Estado", new = "estado_id")
ctb0052_event[, estado_id := as.character(estado_id)]
ctb0052_event[, .N, by = estado_id]

# municipio_id
# old: Município
# new: municipio_id
data.table::setnames(ctb0052_event, old = "Município", new = "municipio_id")
ctb0052_event[, municipio_id := as.character(municipio_id)]
ctb0052_event[, .N, by = municipio_id]

# amostra_area
# old: Área amostrada [m2]
# new: amostra_area
data.table::setnames(ctb0052_event, old = "Área amostrada [m2]", new = "amostra_area")
ctb0052_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0052_event[, amostra_area])

# taxon_sibcs
# old: SIBCS_classificacao OR classe
# new: taxon_sibcs
data.table::setnames(ctb0052_event, old = "SIBCS_classificacao", new = "taxon_sibcs")
ctb0052_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0052_event[is.na(taxon_sibcs), taxon_sibcs := as.character(classe)]
ctb0052_event[, .N, by = taxon_sibcs]
# Two soil profiles have two soil classifications, possibly indicating that the authors were not
# sure about the classification. We keep only the first in the sequence, separated by "/".
ctb0052_event[, taxon_sibcs := sub("/.*", "", taxon_sibcs)]
ctb0052_event[, .N, by = taxon_sibcs]

# taxon_st
# US Soil Taxonomy classification is missing. We set it to NA_character_
ctb0052_event[, taxon_st := NA_character_]

# pedregosidade
# Pedregosidade -> pedregosidade
data.table::setnames(ctb0052_event, old = "Pedregosidade", new = "pedregosidade")
ctb0052_event[, pedregosidade := as.character(pedregosidade)]
ctb0052_event[, .N, by = pedregosidade]

# rochosidade
# Rochosidade -> rochosidade
data.table::setnames(ctb0052_event, old = "Rochosidade", new = "rochosidade")
ctb0052_event[, rochosidade := as.character(rochosidade)]
ctb0052_event[, .N, by = rochosidade]

str(ctb0052_event)

# layer ############################################################################################
# Temporarily read from local CSV: this needs to go to Google Drive
ctb0052_layer <- data.table::fread("ctb0052/ctb0052_layer.csv")
str(ctb0052_layer)

# Process fields
# observacao_id
# old: id_ponto
# new: observacao_id
data.table::setnames(ctb0052_layer, old = "id_ponto", new = "observacao_id")
ctb0052_layer[, observacao_id := as.character(observacao_id)]
ctb0052_layer[, .N, by = observacao_id]

# camada_nome
# old: camada
# new: camada_nome
data.table::setnames(ctb0052_layer, old = "camada", new = "camada_nome")
ctb0052_layer[, camada_nome := as.character(camada_nome)]
ctb0052_layer[, .N, by = camada_nome]

# amostra_id
ctb0052_layer[, amostra_id := as.character(amostra_id)]
ctb0052_layer[, .N, by = amostra_id]

# profund_sup
ctb0052_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0052_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0052_layer[, profund_sup])
# There are 125 layers missing "profund_sup". These are laboratory replicates for which the authors did not
# report the depth, but are consistently identifies by "observacao_id" and "camada_id". 

# profund_inf
ctb0052_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0052_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0052_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0052_layer[, profund_inf])
# There are 125 layers missing "profund_inf". These are laboratory replicates for which the authors did not
# report the depth, but are consistently identifies by "observacao_id" and "camada_id". 

# Compute average over layers
# We compute the average of all replicates for each layer (camada_nome) in each observation
# (observacao_id)
str(ctb0052_layer)
ctb0052_layer[, amostra_id := NULL]
ctb0052_layer <- ctb0052_layer[,
  lapply(.SD, mean, na.rm = TRUE),
  by = .(observacao_id, camada_nome)
]
str(ctb0052_layer)

# camada_id
ctb0052_layer <- ctb0052_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0052_layer[, camada_id := 1:.N, by = observacao_id]
ctb0052_layer[, .N, by = camada_id]

# amostra_id
ctb0052_layer[, amostra_id := 1:.N, by = observacao_id]

# check for missing layers
check_missing_layer(ctb0052_layer)

# compute mid depth
ctb0052_layer[, profund_mid := (profund_sup + profund_inf) / 2]

# check for equal depths
ctb0052_layer[profund_sup == profund_inf, .(observacao_id, camada_nome, profund_sup, profund_inf)]

# terrafina
# terra fina(%) -> terrafina
# The content of fine earth was missing in this dataset and the authors made an educated guess
# based on the soil profile descriptions and pictures.
data.table::setnames(ctb0052_layer, old = "terra fina(%)", new = "terrafina")
ctb0052_layer[, terrafina := as.numeric(terrafina) * 10]
# round
ctb0052_layer[, terrafina := round(terrafina)]
summary(ctb0052_layer[, terrafina])
# There are 361 layers with missing "terrafina".
check_empty_layer(ctb0052_layer, "terrafina")
# These are mostly auger samples and Cr or R horizon samples from soil profiles for which the authors do
# not have any information. Some of them are samples not collected by the authors, but reported in the dataset
# for consistency. We keep them as NA.

# argila
# old: argila(%) * 10
# new: argila
data.table::setnames(ctb0052_layer, old = "argila(%)", new = "argila")
ctb0052_layer[, argila := as.numeric(argila) * 10]
summary(ctb0052_layer[, argila])
# There are 83 layers with missing "argila". Most of these are Cr, R/Cr, and R layers from soil
# profiles and auger samples. Some of them are layers not sampled at all. We will try to fill some of these
# using interpolation, except for R layers.
check_empty_layer(ctb0052_layer, "argila")
# fill empty layers
ctb0052_layer[!grepl("R", camada_nome, ignore.case = TRUE),
  argila := fill_empty_layer(y = argila, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Interpolation was applied to only 3 layers.
check_empty_layer(ctb0052_layer, "argila")

# silte
# old: silte(%) * 10
# new: silte
data.table::setnames(ctb0052_layer, old = "silte(%)", new = "silte")
ctb0052_layer[, silte := as.numeric(silte) * 10]
summary(ctb0052_layer[, silte])
# There are 83 layers with missing "silte". Most of these are Cr, R/Cr, and R layers from soil
# profiles and auger samples. Some of them are layers not sampled at all. We will try to fill some of these
# using interpolation, except for R layers.
check_empty_layer(ctb0052_layer, "silte")
# fill empty layers
ctb0052_layer[!grepl("R", camada_nome, ignore.case = TRUE),
  silte := fill_empty_layer(y = silte, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Interpolation was applied to only 3 layers.
check_empty_layer(ctb0052_layer, "silte")

# areia
# old: areia(%) * 10
# new: areia
data.table::setnames(ctb0052_layer, old = "areia(%)", new = "areia")
ctb0052_layer[, areia := as.numeric(areia) * 10]
summary(ctb0052_layer[, areia])
# There are 83 layers with missing "areia". Most of these are Cr, R/Cr, and R layers from soil
# profiles and auger samples. Some of them are layers not sampled at all. We will try to fill some of these
# using interpolation, except for R layers.
check_empty_layer(ctb0052_layer, "areia")
# fill empty layers
ctb0052_layer[!grepl("R", camada_nome, ignore.case = TRUE),
  areia := fill_empty_layer(y = areia, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
# Interpolation was applied to only 3 layers.
check_empty_layer(ctb0052_layer, "areia")

# Check if sand + silt + clay = 1000 g/kg
# limit: 10%
# round to avoid numerical issues
ctb0052_layer[, argila := round(argila)]
ctb0052_layer[, silte := round(silte)]
ctb0052_layer[, areia := round(areia)]
ctb0052_layer[, psd_sum := argila + silte + areia]
ctb0052_layer[, psd_check := abs(psd_sum - 1000) <= 10]
ctb0052_layer[psd_check == FALSE, .(observacao_id, camada_nome, argila, silte, areia, psd_sum)]
ctb0052_layer[, psd_sum := NULL]
ctb0052_layer[, psd_check := NULL]

# ph
# old: pH_agua
# new: ph
data.table::setnames(ctb0052_layer, old = "pH_agua", new = "ph")
ctb0052_layer[, ph := as.numeric(ph)]
summary(ctb0052_layer[, ph])
# There are 75 layers missing "ph", most of them Cr and R layers. Some of them are layers not sampled at all.
# We will try to fill some of these using interpolation, except for R layers.
check_empty_layer(ctb0052_layer, "ph")
# fill empty layers, except for R layers.
ctb0052_layer[!grepl("R", camada_nome, ignore.case = TRUE),
  ph := fill_empty_layer(y = ph, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0052_layer, "ph")
# interpolation worked for 2 layers.

# ctc
# Ca(cmolc.kg) + Mg(cmolc.kg) + H+Al(cmolc.kg) + K (cmolc/kg) + Na (cmolc/kg)
# old: Ca(cmolc.kg)
# new: ca
data.table::setnames(ctb0052_layer, old = "Ca(cmolc.kg)", new = "ca")
ctb0052_layer[, ca := as.numeric(ca)]
summary(ctb0052_layer[, ca])
# old: Mg(cmolc.kg)
# new: mg
data.table::setnames(ctb0052_layer, old = "Mg(cmolc.kg)", new = "mg")
ctb0052_layer[, mg := as.numeric(mg)]
summary(ctb0052_layer[, mg])
# old: H+Al(cmolc.kg)
# new: hal
data.table::setnames(ctb0052_layer, old = "H+Al(cmolc.kg)", new = "hal")
ctb0052_layer[, hal := as.numeric(hal)]
summary(ctb0052_layer[, hal])
# old: K (cmolc/kg)
# new: k
data.table::setnames(ctb0052_layer, old = "K (cmolc/kg)", new = "k")
ctb0052_layer[, k := as.numeric(k)]
summary(ctb0052_layer[, k])
# old: Na (cmolc/kg)
# new: na
data.table::setnames(ctb0052_layer, old = "Na (cmolc/kg)", new = "na")
ctb0052_layer[, na := as.numeric(na)]
summary(ctb0052_layer[, na])
# ctc
ctb0052_layer[, ctc := ca + mg + hal + k + na]
summary(ctb0052_layer[, ctc])
# there are 362 layers missing ctc. Some of them are layers not sampled at all. We will try to fill some of
# these using interpolation, except for R layers.
check_empty_layer(ctb0052_layer, "ctc")
# fill empty layers, except for R layers.
ctb0052_layer[!grepl("R", camada_nome, ignore.case = TRUE),
  ctc := fill_empty_layer(y = ctc, x = profund_mid),
  by = observacao_id
]
check_empty_layer(ctb0052_layer, "ctc")
# interpolation did not solve any missingness.

# carbono
# Soil organic carbon content is given in three different columns:
# cot_DC(%) - dry combustion
# cos_WCc(%) - wet colorimetric
# cos_WCt(%) - wet total
# We first convert all to g/kg (i.e., multiply by 10). Then, we use linear models to fill missing
# values in cot_DC(%) using cos_WCt(%) and cos_WCc(%). Finally, we fill remaining missing
# values using interpolation.
# cot_DC(%) * 10 -> carbono
data.table::setnames(ctb0052_layer, old = "cot_DC(%)", new = "carbono")
ctb0052_layer[, carbono := as.numeric(carbono) * 10]
ctb0052_layer[, has_carbono := !is.na(carbono)]
summary(ctb0052_layer[, carbono]) # 289 NAs
# cos_WCc(%) * 10 -> carbono_color
data.table::setnames(ctb0052_layer, old = "cos_WCc(%)", new = "carbono_color")
ctb0052_layer[, carbono_color := as.numeric(carbono_color) * 10]
summary(ctb0052_layer[, carbono_color]) # 263 NAs
# cos_WCt(%) * 10 -> carbono_wet
data.table::setnames(ctb0052_layer, old = "cos_WCt(%)", new = "carbono_wet")
ctb0052_layer[, carbono_wet := as.numeric(carbono_wet) * 10]
summary(ctb0052_layer[, carbono_wet]) # 158 NAs
# linear model: carbono ~ carbono_wet
lm_carbono <- lm(carbono ~ carbono_wet + I(carbono_wet^2), data = ctb0052_layer)
summary(lm_carbono) # 0.9881
ctb0052_layer[is.na(carbono) & !is.na(carbono_wet), carbono := predict(lm_carbono, .SD)]
summary(ctb0052_layer[, carbono]) # 139 NAs
# linear model: carbono ~ carbono_color + I(carbono_color^2)
# lm_carbono <- lm(carbono ~ carbono_color + I(carbono_color^2),
#   data = ctb0052_layer[has_carbono == TRUE]
# )
# summary(lm_carbono) # 0.3432
# ctb0052_layer[is.na(carbono) & !is.na(carbono_color), carbono := predict(lm_carbono, .SD)]
set.seed(369)
rf_carbono <- ranger::ranger(
  carbono ~ carbono_color + argila + silte + areia + terrafina + profund_inf + profund_sup + ph + ctc,
  data = ctb0052_layer[has_carbono == TRUE]
)
print(rf_carbono) # 0.4337614
ctb0052_layer[
  is.na(carbono) & !is.na(carbono_color),
  carbono := predict(rf_carbono, .SD)$predictions
]
summary(ctb0052_layer[, carbono])
# There are 94 layers missing "carbono". Most of them are Cr, R layers. Some of them were not sampled at all
# by the authors, but included in the dataset for consistency.
check_empty_layer(ctb0052_layer, "carbono")
# fill empty layers, except for R layers.
ctb0052_layer[!grepl("R", camada_nome, ignore.case = TRUE),
  carbono := fill_empty_layer(y = carbono, x = profund_mid, ylim = c(0, 1000)),
  by = observacao_id
]
check_empty_layer(ctb0052_layer, "carbono")
# Interpolation worked for 2 layers.

# dsi
# old: Ds(g.cm3)
# new: dsi
data.table::setnames(ctb0052_layer, old = "Ds(g.cm3)", new = "dsi")
ctb0052_layer[, dsi := as.numeric(dsi)]
summary(ctb0052_layer[, dsi])
# There are 366 layers missing bulk density.
check_empty_layer(ctb0052_layer, "dsi")
# fill empty layers, except for R layers.
ctb0052_layer[!grepl("R", camada_nome, ignore.case = TRUE),
  dsi := fill_empty_layer(y = dsi, x = profund_mid, ylim = c(0, 2.6)),
  by = observacao_id
]
check_empty_layer(ctb0052_layer, "dsi")
# interpolation did not solve any missingness.

str(ctb0052_layer)

# Merge ############################################################################################
# Merge events and layers
ctb0052 <- merge(ctb0052_event, ctb0052_layer, all = TRUE)
ctb0052[, dataset_id := "ctb0052"]

# citation
ctb0052 <- merge(ctb0052, ctb0052_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0052)
# Layers: 407
# Events: 137
# Georeferenced events: 137

# Plot using mapview
if (FALSE) {
  ctb0052_sf <- sf::st_as_sf(
    ctb0052[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0052_sf["argila"])
}

# Write to disk ####################################################################################
ctb0052 <- select_output_columns(ctb0052)
data.table::fwrite(ctb0052, "ctb0052/ctb0052.csv")
