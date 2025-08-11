# author: Alessandro Samuel-Rosa
# data: 2024 CC-BY
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
# ctb0052
# Dados de "Variação de parâmetros dendrométricos de Pinus taeda L. e a distribuição espacial de
# atributos do solo por técnicas de mapeamento digital de solos"
# https://drive.google.com/drive/u/1/folders/1o1y45aiXr4n5ou3A3wpTQ2Beb0DNCGob

# citation #########################################################################################
ctb0052_citation <- data.table::data.table(
  dataset_id = "ctb0052",
  dataset_titulo = "Variação de parâmetros dendrométricos de Pinus taeda L. e a distribuição espacial de atributos do solo por técnicas de mapeamento digital de solos",
  dataset_licenca = "CC-BY"
)

# MERGE ORIGINAL DATA
# event ############################################################################################
# PROFILE
gs_perfil <- "12XVwsh74gafTLJicCydx_rbcnM89HmH0FFTNVvXeUGY"
gid_perfil <- "0"
perfil <- google_sheet(gs_perfil, gid_perfil)
# POINTS
gs_pontos <- "1swA6UoU7MJrldXu92PZe3CY8ajIVf0oLrRRvJAxlkT4"
gid_pontos <- "0"
pontos <- google_sheet(gs_pontos, gid_pontos)
# MERGE
ctb0052_event <- rbindlist(list(perfil, pontos), fill = TRUE)
str(ctb0052_event)
data.table::fwrite(ctb0052_event, "ctb0052/ctb0052_event.csv")

# layer ############################################################################################
# PROFILE
# Profile: exchangeable aluminum content
gs_perfil_al <- "1cWVlkn_Dx-WxcFZS8eUoEQPK5N8-AnxzmqLz1w-9rWA"
gid_perfil_al <- "0"
perfil_al <- google_sheet(gs_perfil_al, gid_perfil_al)
perfil_al[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: carbon and organic matter content
gs_perfil_cos <- "1k3tRJ7V7flf0HahlM96VQ_WGQMzmAVW7Y3K9H0Lo260"
gid_perfil_cos <- "0"
perfil_cos <- google_sheet(gs_perfil_cos, gid_perfil_cos)
perfil_cos[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: water-dispersed clay content
gs_perfil_arg_h2o <- "1ehiNHqmSxiCc23pUHIfGJtujMTAbEGHsJTEq9jBbqrM"
gid_perfil_arg_h2o <- "504220634"
perfil_arg_h2o <- google_sheet(gs_perfil_arg_h2o, gid_perfil_arg_h2o)
perfil_arg_h2o[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: exchangeable calcium and magnesium content
gs_perfil_ca_mg <- "1uxgw4_fEJ-rFvpCopYe10JOKB86Bu9ZLfgrIavaCiSg"
gid_perfil_ca_mg <- "0"
perfil_ca_mg <- google_sheet(gs_perfil_ca_mg, gid_perfil_ca_mg)
perfil_ca_mg[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: soil bulk density
gs_perfil_ds <- "1w-ohEcAyJnpZLSCMQekfWzYup9poNpKwnpkMW23Y6K8"
gid_perfil_ds <- "0"
perfil_ds <- google_sheet(gs_perfil_ds, gid_perfil_ds)
perfil_ds[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: particle size distribution
gs_perfil_tex <- "1NEa7N2l0a44GqTtCP1wWLw-3w-y-gl7awVaiwdLuCZ4"
gid_perfil_tex <- "0"
perfil_tex <- google_sheet(gs_perfil_tex, gid_perfil_tex)
perfil_tex[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: acidity
gs_perfil_HAl <- "1p1O5w1_Zi1VgDaqVfkmnB4Gsgcomp4I0uywo43VHIUk"
gid_perfil_HAl <- "0"
perfil_HAl <- google_sheet(gs_perfil_HAl, gid_perfil_HAl)
perfil_HAl[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: saturated hydraulic conductivity
gs_perfil_Ksat <- "1mxuaXRUNd-BJ2oFENSkZQn7Cy9izSBGp7twFoIJe0Vk"
gid_perfil_Ksat <- "0"
perfil_Ksat <- google_sheet(gs_perfil_Ksat, gid_perfil_Ksat)
perfil_Ksat[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: exchangeable potassium and sodium content
gs_perfil_k_na <- "16cgzzRtlSh3M_uwQFEliTTvTOlpfG5bmN54jB6UIuZI"
gid_perfil_k_na <- "0"
perfil_k_na <- google_sheet(gs_perfil_k_na, gid_perfil_k_na)
perfil_k_na[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: soil pH in water
gs_perfil_ph <- "1R4VmTWPG5qfeU5urB_VnjmGYKb8utGYYqt9WkEJE2z0"
gid_perfil_ph <- "0"
perfil_ph <- google_sheet(gs_perfil_ph, gid_perfil_ph)
perfil_ph[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: porosity
gs_perfil_poro <- "1k-wK_qhBgs2L6esANSABJrU881PhpZsUa1PHhxHclnk"
gid_perfil_poro <- "0"
perfil_poro <- google_sheet(gs_perfil_poro, gid_perfil_poro)
perfil_poro[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# Profile: horizon depth
gs_perfil_hz <- "1WDRQr2GlBe9vtm3R1S-Bfvzzcew4aVDYkFdyVkD6Ypc"
gid_perfil_hz <- "2141949106"
perfil_hz <- google_sheet(gs_perfil_hz, gid_perfil_hz)
perfil_hz[, amostra_id := 1:.N, by = .(id_ponto, camada)]
# MERGE
# Merge profiles by "id_ponto", "camada", and "amostra_id"
ctb0052_profile <- merge(
  perfil_al, perfil_cos,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_arg_h2o,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_ca_mg,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_ds,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_tex,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_HAl,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_Ksat,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_k_na,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_ph,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_poro,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_profile <- merge(
  ctb0052_profile, perfil_hz,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
str(ctb0052_profile)

# POINTS
# Point: soil organic carbon content
gs_ponto_cos <- "13GToSvWcCr3T-1Jwdl_idDzccEdwpdXsvc6w0Sv8kEw"
gid_ponto_cos <- "1368324009"
ponto_cos <- google_sheet(gs_ponto_cos, gid_ponto_cos)
ponto_cos[, amostra_id := 1:.N, by = .(id_ponto, camada)]

# Point: particle size distribution
gs_ponto_tex <- "1UN_4SFR4eaBBUnMxWeQEexAEjrx1dUZr3KMHT9Mu3pU"
gid_ponto_tex <- "0"
ponto_tex <- google_sheet(gs_ponto_tex, gid_ponto_tex)
ponto_tex[, amostra_id := 1:.N, by = .(id_ponto, camada)]

# Point: soil pH in water
gs_ponto_ph <- "1EnqRvq_6wFNfRhDA6I9Aby-fJgYcPXCoJOmhH45yfB0"
gid_ponto_ph <- "0"
ponto_ph <- google_sheet(gs_ponto_ph, gid_ponto_ph)
ponto_ph[, amostra_id := 1:.N, by = .(id_ponto, camada)]

# cbind points by "id_ponto", "camada", and "amostra_id"
ctb0052_points <- merge(
  ponto_tex, ponto_ph, by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
ctb0052_points <- merge(
  ctb0052_points, ponto_cos,
  by = c("id_ponto", "camada", "amostra_id"), all = TRUE
)
# # Depth from "camada" (eg. 0-20) to "profund_sup" and "profund_inf"
# ctb0052_points[, profund_sup := strsplit(camada, "-")[[1]][1], by = .I]
# ctb0052_points[, profund_sup := as.numeric(profund_sup)]
# ctb0052_points[, profund_inf := strsplit(camada, "-")[[1]][2], by = .I]
# ctb0052_points[, profund_inf := as.numeric(profund_inf)]
# str(ctb0052_points)

# MERGE
# Merge profiles and points by "id_ponto", "camada", and "amostra_id"
colnames(ctb0052_points) %in% colnames(ctb0052_profile)
ctb0052_layer <- rbindlist(list(ctb0052_profile, ctb0052_points), fill = TRUE)
ctb0052_layer <- merge(
  ctb0052_layer,
  ctb0052_event[, c("id_ponto", "espessura_solum (cm)", "camada_impedimento (cm)")]
)
str(ctb0052_layer)

# Write to disk
data.table::fwrite(ctb0052_layer, "ctb0052/ctb0052_layer.csv")

# event ############################################################################################
# Process fields
# observacao_id
# OLD: id_ponto
# NEW: observacao_id
data.table::setnames(ctb0052_event, old = "id_ponto", new = "observacao_id")
ctb0052_event[, observacao_id := as.character(observacao_id)]
ctb0052_event[, .N, by = observacao_id][N > 1]

# data_ano
# OLD: data
# NEW: data_ano
data.table::setnames(ctb0052_event, old = "data", new = "data_ano")
ctb0052_event[, data_ano := as.integer(data_ano)]
ctb0052_event[, .N, by = data_ano]

# data_fonte = original
ctb0052_event[, data_fonte := "original"]

# coord_x
ctb0052_event[, coord_x := as.numeric(coord_x)]
summary(ctb0052_event[, coord_x])

# coord_y
ctb0052_event[, coord_y := as.numeric(coord_y)]
summary(ctb0052_event[, coord_y])

# coord_datum
# OLD: datum
# NEW: coord_datum
# SIRGAS 2000 22 S -> EPSG:31982
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

# check for duplicated coordinates
ctb0052_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0052_event[coord_duplicated == TRUE & !is.na(coord_x), .(observacao_id, coord_x, coord_y)]
ctb0052_event[, coord_duplicated := NULL]

# coord_fonte
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
# RL -> NEOSSOLO LITÓLICO
ctb0052_event[taxon_sibcs == "RL", taxon_sibcs := "NEOSSOLO LITÓLICO"]
# CX -> CAMBISSOLO HÁPLICO
ctb0052_event[taxon_sibcs == "CX", taxon_sibcs := "CAMBISSOLO HÁPLICO"]
# GX -> GLEISSOLO HÁPLICO
ctb0052_event[taxon_sibcs == "GX", taxon_sibcs := "GLEISSOLO HÁPLICO"]
# RR -> NEOSSOLO REGOLÍTICO
ctb0052_event[taxon_sibcs == "RR", taxon_sibcs := "NEOSSOLO REGOLÍTICO"]
# GM -> GLEISSOLO MELÂNICO
ctb0052_event[taxon_sibcs == "GM", taxon_sibcs := "GLEISSOLO MELÂNICO"]
# OX -> ORGANOSSOLO HÁPLICO
ctb0052_event[taxon_sibcs == "OX", taxon_sibcs := "ORGANOSSOLO HÁPLICO"]
# RL/RR -> NEOSSOLO LITÓLICO
ctb0052_event[taxon_sibcs == "RL/RR", taxon_sibcs := "NEOSSOLO LITÓLICO"]
# RR/GM -> NEOSSOLO REGOLÍTICO
ctb0052_event[taxon_sibcs == "RR/GM", taxon_sibcs := "NEOSSOLO REGOLÍTICO"]
ctb0052_event[, .N, by = taxon_sibcs]

# taxon_st
# US Soil Taxonomy classification is missing. We set it to NA_character_
ctb0052_event[, taxon_st := NA_character_]

str(ctb0052_event)

# layer ############################################################################################

# Compute average over layers
ctb0052_layer[, amostra_id := NULL]
ctb0052_layer <- ctb0052_layer[, lapply(.SD, mean, na.rm = TRUE), by = .(id_ponto, camada)]
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
# amostra_id is missing. We set it to NA_character_
ctb0052_layer[, amostra_id := NA_character_]

# profund_sup
ctb0052_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0052_layer[, profund_sup])

# profund_inf
ctb0052_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0052_layer[, profund_inf])

# check for missing layers
check_missing_layer(ctb0052_layer)
ctb0052_layer[observacao_id == "P11" & camada_nome == "Cg2", profund_inf := profund_inf + 20]

# compute mid depth
ctb0052_layer[, profund_mid := (profund_sup + profund_inf) / 2]

# camada_id
ctb0052_layer <- ctb0052_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0052_layer[, camada_id := 1:.N, by = observacao_id]
ctb0052_layer[, .N, by = camada_id]

# terrafina
# terra fina is missing. We assume it is NA
ctb0052_layer[, terrafina := NA_real_]

# argila
# old: argila(%) * 10
# new: argila
data.table::setnames(ctb0052_layer, old = "argila(%)", new = "argila")
ctb0052_layer[, argila := as.numeric(argila) * 10]
summary(ctb0052_layer[, argila])
check_empty_layer(ctb0052_layer, "argila")
# fill empty layers
ctb0052_layer[, argila := fill_empty_layer(y = argila, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0052_layer, "argila")

# silte
# old: silte(%) * 10
# new: silte
data.table::setnames(ctb0052_layer, old = "silte(%)", new = "silte")
ctb0052_layer[, silte := as.numeric(silte) * 10]
summary(ctb0052_layer[, silte])
check_empty_layer(ctb0052_layer, "silte")
# fill empty layers
ctb0052_layer[, silte := fill_empty_layer(y = silte, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0052_layer, "silte")

# areia
# old: areia(%) * 10
# new: areia
data.table::setnames(ctb0052_layer, old = "areia(%)", new = "areia")
ctb0052_layer[, areia := as.numeric(areia) * 10]
summary(ctb0052_layer[, areia])
check_empty_layer(ctb0052_layer, "areia")
# fill empty layers
ctb0052_layer[, areia := fill_empty_layer(y = areia, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0052_layer, "areia")

# carbono
# cot_DC(%) * 10 -> carbono
data.table::setnames(ctb0052_layer, old = "cot_DC(%)", new = "carbono")
ctb0052_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0052_layer[, carbono])
# cos_WCc(%) * 10 -> carbono_color
data.table::setnames(ctb0052_layer, old = "cos_WCc(%)", new = "carbono_color")
ctb0052_layer[, carbono_color := as.numeric(carbono_color) * 10]
summary(ctb0052_layer[, carbono_color])
# cos_WCt(%) * 10 -> carbono_wet
data.table::setnames(ctb0052_layer, old = "cos_WCt(%)", new = "carbono_wet")
ctb0052_layer[, carbono_wet := as.numeric(carbono_wet) * 10]
summary(ctb0052_layer[, carbono_wet])
# linear model: carbono ~ carbono_wet
lm_carbono <- lm(carbono ~ carbono_wet, data = ctb0052_layer)
summary(lm_carbono)
ctb0052_layer[is.na(carbono) & !is.na(carbono_wet), carbono := predict(lm_carbono, .SD)]
summary(ctb0052_layer[, carbono])
# linear model: carbono ~ carbono_color
# lm_carbono <- lm(carbono ~ carbono_color, data = ctb0052_layer)
# summary(lm_carbono)
# ctb0052_layer[is.na(carbono) & !is.na(carbono_color), carbono := predict(lm_carbono, .SD)]
check_empty_layer(ctb0052_layer, "carbono")
# fill empty layers
ctb0052_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0052_layer, "carbono")

# ph
# old: pH_agua
# new: ph
data.table::setnames(ctb0052_layer, old = "pH_agua", new = "ph")
ctb0052_layer[, ph := as.numeric(ph)]
summary(ctb0052_layer[, ph])
check_empty_layer(ctb0052_layer, "ph")
# fill empty layers
ctb0052_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0052_layer, "ph")

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
check_empty_layer(ctb0052_layer, "ctc")
# fill empty layers
ctb0052_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0052_layer, "ctc")

# dsi
# old: Ds(g.cm3)
# new: dsi
data.table::setnames(ctb0052_layer, old = "Ds(g.cm3)", new = "dsi")
ctb0052_layer[, dsi := as.numeric(dsi)]
summary(ctb0052_layer[, dsi])
check_empty_layer(ctb0052_layer, "dsi")
# fill empty layers
ctb0052_layer[, dsi := fill_empty_layer(y = dsi, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0052_layer, "dsi")

str(ctb0052_layer)

# Merge ############################################################################################
# Merge events and layers
ctb0052 <- merge(ctb0052_event, ctb0052_layer, all = TRUE)
ctb0052[, dataset_id := "ctb0052"]
summary_soildata(ctb0052)
# Layers: 467
# Events: 137
# Georeferenced events: 137 (ponto 13 ???)

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
data.table::fwrite(ctb0052_event, "ctb0052/ctb0052_event.csv")
data.table::fwrite(ctb0052_layer, "ctb0052/ctb0052_layer.csv")
