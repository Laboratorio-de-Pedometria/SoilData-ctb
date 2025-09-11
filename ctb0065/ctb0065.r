# autor: Felipe Brun Vergani
# data: 2025

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
if (!require("parzer")) {
  install.packages("parzer")
  library("parzer")
}

# Source helper functions
source("./helper.R")


# Google Sheet #####################################################################################
# ctb0065
# Dados de "Solos de três áreas de  restinga. 1. Morfologia, caracterização e classificação"
# 
# https://drive.google.com/drive/folders/1N90u15zgwHxxOU__vQJKfivjubk5_gb0


ctb0065_ids <- soildata_catalog("ctb0065")

# validation #####################################################################################

ctb0065_validation <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_validation)
str(ctb0065_validation)

# Check for negative validation results
sum(ctb0065_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0065_citation <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_citation)
str(ctb0065_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0065_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0065_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0065_citation <- data.table::data.table(
  dataset_id = "ctb0065",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0065_citation)

# event #####################################################################################
ctb0065_event <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_event)
str(ctb0065_event)

#PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0065_event, old = "ID do evento", new = "observacao_id")
ctb0065_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0065_event[, observacao_id]) > 1)


#The year of data collection is missing, the author started 
#the master's degree in 1992, so we estimate that he obtained the data collection in 1993.
#It is necessary to contact the author to find out the actual year of collection.
# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0065_event, old = "Ano (coleta)", new = "data_ano")
ctb0065_event[, data_ano := as.integer(data_ano)]
ctb0065_event[, data_ano := 1993]
# data_fonte
ctb0065_event[, data_fonte := "estimativa"]
ctb0065_event[, .N, by = data_ano]




# coord_datum
# Datum (coord) -> coord_datum
data.table::setnames(ctb0065_event, old = "Datum (coord)", new = "coord_datum")
ctb0065_event[, coord_datum := NA_real_]


# Missing seconds precision on Longitude / Latitude
# The parzer library was added to convert the characters into values appropriate for the coordinate table.
# Longitude -> coord_x
data.table::setnames(ctb0065_event, old = "Longitude", new = "coord_x")
ctb0065_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0065_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0065_event, old = "Latitude", new = "coord_y")
ctb0065_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0065_event[, coord_y])

# Check for duplicate coordinates
any(ctb0065_event[, .N, by = .(coord_x, coord_y)][, N] > 1)

# Precisão (coord) [m] -> coord_precisao
# Coordinates were attributed with little knowledge of the precision. We set it to NA_real_
data.table::setnames(ctb0065_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0065_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0065_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0065_event[, coord_fonte := NA_real_]

# País -> pais_id
data.table::setnames(ctb0065_event, old = "País", new = "pais_id")
ctb0065_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0065_event, old = "Estado (UF)", new = "estado_id")
ctb0065_event[, estado_id := "RJ"]

# Município -> municipio_id
data.table::setnames(ctb0065_event, old = "Município", new = "municipio_id")
ctb0065_event[, municipio_id := as.character(municipio_id)]
ctb0065_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0065_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0065_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0065_event[, amostra_area])


# Classificação de solo (1988) <- taxon_sibcs
data.table::setnames(ctb0065_event, old = "Classificação do Solo (1988)", new = "taxon_sibcs")
ctb0065_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0065_event[, .N, by = taxon_sibcs]

# taxon_st_1999

ctb0065_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# Stoneness is missing in this document...

ctb0065_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# Roughness is missing in this document, however, based on the estimated analysis, 
# there is no roughness in this work.

ctb0065_event[, rochosidade := ("Não Rochoso")]


str(ctb0065_event)

# layers ###########################################################################################
ctb0065_layer <- google_sheet(ctb0065_ids$gs_id, ctb0065_ids$gid_layer)
str(ctb0065_layer)

# PROCESS FIELDS

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0065_layer, old = "ID do evento", new = "observacao_id")
ctb0065_layer[, observacao_id := as.character(observacao_id)]
ctb0065_layer[, .N, by = observacao_id]

# camada_nome
# old: ID da camada
# new: camada_nome
data.table::setnames(ctb0065_layer, old = "ID da camada", new = "camada_nome")
ctb0065_layer[, camada_nome := as.character(camada_nome)]
ctb0065_layer[, .N, by = camada_nome]

# amostra_id
# old: ID da amostra
# new: amostra_id
data.table::setnames(ctb0065_layer, old = "ID da amostra", new = "amostra_id")
ctb0065_layer[, amostra_id := NA_real_]

# perfil_id
# old: Perfil
# new: perfil_id
data.table::setnames(ctb0065_layer, old = "Perfil", new = "perfil_id")
ctb0065_layer[, perfil_id := as.character(perfil_id)]
ctb0065_layer[, .N, by = perfil_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0065_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0065_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0065_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0065_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0065_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0065_layer[, profund_inf])

# camada_id
ctb0065_layer <- ctb0065_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0065_layer[, camada_id := 1:.N, by = observacao_id]
ctb0065_layer[, .N, by = camada_id]
summary(ctb0065_layer[, camada_id])

# terrafina
# old: Fração fina [< 2mm~] (%)
# new: terrafina
#terrafina is missing for some -13, -10 and -10, -5 layers.
data.table::setnames(ctb0065_layer, old = "Fração fina [< 2mm~] (%)", new = "terrafina")
ctb0065_layer[, terrafina := as.numeric(terrafina) * 10]
ctb0065_layer[is.na(terrafina), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]

# areia_grossa
# old: Areia grossa (%)
# new: areia_grossa
#Areia_grossa is missing for some -13, -10 and -10, -5 layers.
data.table::setnames(ctb0065_layer, old = "Areia grossa (%)", new = "areia_grossa")
ctb0065_layer[, areia_grossa := as.numeric(areia_grossa) * 10]
ctb0065_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: Areia fina (%)
# new: areia_fina
#areia_fina is missing for some -13, -10 and -10, -5 layers.
data.table::setnames(ctb0065_layer, old = "Areia fina (%)", new = "areia_fina")
ctb0065_layer[, areia_fina := as.numeric(areia_fina) * 10]
ctb0065_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

# areia
# criação da coluna areia 
ctb0065_layer[, areia:= areia_grossa+areia_fina]

# silte
# old: Silte (%)
# new: silte
#Silte is missing for some -13, -10 and -10, -5 layers.
data.table::setnames(ctb0065_layer, old = "Silte (%)", new = "silte")
ctb0065_layer[, silte := as.numeric(silte) * 10]
ctb0065_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila (%)
# new: argila
#Argila is missing for some -13, -10 and -10, -5 layers.
data.table::setnames(ctb0065_layer, old = "Argila (%)", new = "argila")
ctb0065_layer[, argila := as.numeric(argila) * 10]
ctb0065_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0065_layer[, psd := round(argila + silte + areia_fina + areia_grossa)]
psd_lims <- 900:1100
# Check the limits
ctb0065_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0065_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C (orgânico) (g/kg)
# new: carbono
data.table::setnames(ctb0065_layer, old = "C (orgânico) (g/kg)", new = "carbono")
ctb0065_layer[, carbono := as.numeric(carbono)]
summary(ctb0065_layer[, carbono])
check_empty_layer(ctb0065_layer, "carbono")

# ctc
# old: CTC pH 7,0 (cmolc/kg)
# new: ctc
data.table::setnames(ctb0065_layer, old = "CTC pH 7,0 (cmolc/kg)", new = "ctc")
ctb0065_layer[, ctc := as.numeric(ctc)]
summary(ctb0065_layer[, ctc])
check_empty_layer(ctb0065_layer, "ctc")

# ph
# old: H2O (pH)
# new: ph
data.table::setnames(ctb0065_layer, old = "H2O (pH)", new = "ph")
ctb0065_layer[, ph := as.numeric(ph)]
summary(ctb0065_layer[, ph])
check_empty_layer(ctb0065_layer, "ph")

# dsi
# old: Densidade do solo (g/cm^3)
# new: dsi
data.table::setnames(ctb0065_layer, old = "Densidade do solo (g/cm^3)", new = "dsi")
ctb0065_layer[, dsi := as.numeric(dsi)]
summary(ctb0065_layer[, dsi])

str(ctb0065_layer)

# Merge ############################################################################################
# events and layers
ctb0065 <- merge(ctb0065_event, ctb0065_layer, all = TRUE)
ctb0065[, dataset_id := "ctb0065"]
# citation
ctb0065 <- merge(ctb0065, ctb0065_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0065)
#Layers: 36
#Events: 8
#Georeferenced events: 7

# Plot using mapview
if (FALSE) {
  ctb0065_sf <- sf::st_as_sf(
    ctb0065[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0065_sf["argila"])
}

# Write to disk ####################################################################################
ctb0065 <- select_output_columns(ctb0065)
data.table::fwrite(ctb0065, "ctb0065/ctb0065.csv")
data.table::fwrite(ctb0065_event, "ctb0065/ctb0065_event.csv")
data.table::fwrite(ctb0065_layer, "ctb0065/ctb0065_layer.csv")