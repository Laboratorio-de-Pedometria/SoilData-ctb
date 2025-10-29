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
# ctb0052
# Dados de "Variação de parâmetros dendrométricos de Pinus taeda L. e a distribuição espacial de
# atributos do solo por técnicas de mapeamento digital de solos"
# 
# Google Drive: https://drive.google.com/drive/u/1/folders/1o1y45aiXr4n5ou3A3wpTQ2Beb0DNCGob
# NotebookLM: https://notebooklm.google.com/notebook/f4c92e6c-f44a-4abe-8a12-1abc0ece53cd

# This script downloads and preprocesses the raw data from Google Sheets and saves them as CSV
# files. These CSV files will be used to create a consolidated SoilData database in GoogleDrive
# and then used in the post processing scripts.

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
str(perfil)

# POINTS
gs_pontos <- "1swA6UoU7MJrldXu92PZe3CY8ajIVf0oLrRRvJAxlkT4"
gid_pontos <- "0"
pontos <- google_sheet(gs_pontos, gid_pontos)
str(pontos)

# MERGE
ctb0052_event <- rbindlist(list(perfil, pontos), fill = TRUE)
str(ctb0052_event)

# Fill SIBCS_classificacao with classe where missing
ctb0052_event[is.na(SIBCS_classificacao), SIBCS_classificacao := classe]
# RL -> NEOSSOLO LITÓLICO
ctb0052_event[SIBCS_classificacao == "RL", SIBCS_classificacao := "NEOSSOLO LITÓLICO"]
# CX -> CAMBISSOLO HÁPLICO
ctb0052_event[SIBCS_classificacao == "CX", SIBCS_classificacao := "CAMBISSOLO HÁPLICO"]
# GX -> GLEISSOLO HÁPLICO
ctb0052_event[SIBCS_classificacao == "GX", SIBCS_classificacao := "GLEISSOLO HÁPLICO"]
# RR -> NEOSSOLO REGOLÍTICO
ctb0052_event[SIBCS_classificacao == "RR", SIBCS_classificacao := "NEOSSOLO REGOLÍTICO"]
# GM -> GLEISSOLO MELÂNICO
ctb0052_event[SIBCS_classificacao == "GM", SIBCS_classificacao := "GLEISSOLO MELÂNICO"]
# OX -> ORGANOSSOLO HÁPLICO
ctb0052_event[SIBCS_classificacao == "OX", SIBCS_classificacao := "ORGANOSSOLO HÁPLICO"]
# RL/RR -> NEOSSOLO LITÓLICO
ctb0052_event[
  SIBCS_classificacao == "RL/RR",
  SIBCS_classificacao := "NEOSSOLO LITÓLICO/NEOSSOLO REGOLÍTICO"
]
# RR/GM -> NEOSSOLO REGOLÍTICO
ctb0052_event[
  SIBCS_classificacao == "RR/GM",
  SIBCS_classificacao := "NEOSSOLO REGOLÍTICO/GLEISSOLO MELÂNICO"
]
ctb0052_event[, .N, by = SIBCS_classificacao]

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
# Depth from "camada" (eg. 0-20) to "profund_sup" and "profund_inf"
ctb0052_points[, profund_sup := strsplit(camada, "-")[[1]][1], by = .I]
ctb0052_points[, profund_inf := strsplit(camada, "-")[[1]][2], by = .I]
str(ctb0052_points)

# MERGE
# Merge profiles and points by "id_ponto", "camada", and "amostra_id"
colnames(ctb0052_points) %in% colnames(ctb0052_profile)
ctb0052_layer <- rbindlist(list(ctb0052_profile, ctb0052_points), fill = TRUE)
ctb0052_layer <- merge(
  ctb0052_layer,
  ctb0052_event[, c("id_ponto", "classe", "espessura solum (cm)", "Contato inferior")]
)
str(ctb0052_layer)

# Deal with lithic contact information  ############################################################

# prof_limitante
# This column contains the depth of the root limiting layer (cm)
ctb0052_layer[, prof_limitante := `espessura solum (cm)`]
# substitute ">" with "" in prof_limitante
ctb0052_layer[grepl(">", prof_limitante), prof_limitante := Inf]
# set as numeric
ctb0052_layer[, prof_limitante := as.numeric(prof_limitante)]
summary(ctb0052_layer[, prof_limitante])

# hz_limitante
# This column contains the symbol of the root limiting layer
ctb0052_layer[, hz_limitante := `Contato inferior`]
ctb0052_layer[, .N, by = hz_limitante]

# lower_depth
# Create lower_depth from profund_inf to keep the original data intact
ctb0052_layer[, lower_depth := profund_inf]
ctb0052_layer[, lower_depth := depth_slash(lower_depth), by = .I]
ctb0052_layer[, lower_depth := depth_plus(lower_depth), by = .I]
ctb0052_layer[, lower_depth := as.numeric(lower_depth)]
summary(ctb0052_layer[, lower_depth])

# upper_depth
# Create upper_depth from profund_sup to keep the original data intact
ctb0052_layer[, upper_depth := profund_sup]
ctb0052_layer[, upper_depth := depth_slash(upper_depth), by = .I]
ctb0052_layer[, upper_depth := as.numeric(upper_depth)]
summary(ctb0052_layer[, upper_depth])

# Identify auger holes
# The ID (id_ponto) of auger holes and soil profiles are different by the fact that auger holes
# contains only numbers, while soil profiles contains letters and numbers
ctb0052_layer[, is_auger := grepl("^[0-9]+$", id_ponto)]
# Check auger holes
ctb0052_layer[is_auger == TRUE, id_ponto]

# Identify layers in witch the limiting depth (prof_limitante) is between the upper (upper_depth)
# and lower (lower_depth) depths of the layer
ctb0052_layer[,
  within_solum := ifelse(
    upper_depth < prof_limitante & lower_depth >= prof_limitante,
    TRUE,
    FALSE
  ),
  by = .I
]
if (FALSE) {
  View(ctb0052_layer[
    within_solum == TRUE & is_auger == TRUE,
    .(id_ponto, classe, camada, upper_depth, prof_limitante, lower_depth, hz_limitante)
  ])
}

# When this happens, and hz_limitante is not NA, set profund_inf = prof_limitante
ctb0052_layer[
  within_solum == TRUE & is_auger == TRUE & !is.na(hz_limitante), profund_inf := prof_limitante
]
ctb0052_layer[
  within_solum == TRUE & is_auger == TRUE & !is.na(hz_limitante),
  .(id_ponto, classe, camada, lower_depth, prof_limitante, hz_limitante)
]
# For each event (id_ponto), get a single occurrence of within_solum == TRUE & is_auger == TRUE
ctb0052_layer_within <- ctb0052_layer[
  within_solum == TRUE & is_auger == TRUE & !is.na(hz_limitante),
  .SD[1],
  by = id_ponto,
  .SDcols = c("id_ponto", "camada", "upper_depth", "lower_depth", "prof_limitante", "hz_limitante")
]
# Set camada = hz_limitante, profund_sup = prof_limitante, profund_inf = prof_limitante + 10)
ctb0052_layer_within[
  ,
  `:=` (
    camada = hz_limitante,
    profund_sup = prof_limitante,
    profund_inf = prof_limitante + 10
  )
]

# rbind the modified layers to the original data
ctb0052_layer <- rbindlist(list(ctb0052_layer, ctb0052_layer_within), fill = TRUE)
# sort by id_ponto and upper_depth
data.table::setorder(ctb0052_layer, id_ponto, profund_sup)
rm(ctb0052_layer_within)
str(ctb0052_layer)

# For other cases, when camada_obs is not NA, set camada = camada_obs
ctb0052_layer[!is.na(camada_obs), camada := camada_obs]

# Clean
ctb0052_layer[
  ,
  c("is_auger", "within_solum", "upper_depth", "lower_depth", "hz_limitante",
    "camada_obs", "prof_limitante", "Contato inferior", "espessura solum (cm)", "classe") := NULL
]

# Write to disk
data.table::fwrite(ctb0052_layer, "ctb0052/ctb0052_layer.csv")
