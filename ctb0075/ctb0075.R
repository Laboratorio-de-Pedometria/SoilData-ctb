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
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0075
# Modelagem de Dados de Sensoriamento Remoto para o Mapeamento Digital 
# de Solos e Índice Relativo de Umidade dos Solos do Distrito Federal
# 
# 
# https://docs.google.com/spreadsheets/d/1p1ED_uCfW3b39TTbxsoK9wdBJ2rwttswQWUnQuaJwBM/edit?usp=sharing


ctb0075_ids <- soildata_catalog("ctb0075")

# validation #####################################################################################

ctb0075_validation <- google_sheet(ctb0075_ids$gs_id, ctb0075_ids$gid_validation)
str(ctb0075_validation)

# Check for negative validation results
sum(ctb0075_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0075_citation <- google_sheet(ctb0075_ids$gs_id, ctb0075_ids$gid_citation)
str(ctb0075_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0075_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0075_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0075_citation <- data.table::data.table(
  dataset_id = "ctb0075",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0075_citation)

# event #####################################################################################
ctb0075_event <- google_sheet(ctb0075_ids$gs_id, ctb0075_ids$gid_event)
str(ctb0075_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0075_event, old = "ID do evento", new = "observacao_id")
ctb0075_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0075_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0075_event, old = "Ano (coleta)", new = "data_ano")
ctb0075_event[, data_ano := as.integer(data_ano)]
ctb0075_event[, .N, by = data_ano]

# ano_fonte
ctb0075_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0075_event[, .N, by = ano_fonte]


# coord_x
# Longitude -> coord_x
data.table::setnames(ctb0075_event, old = "Longitude", new = "coord_x")
ctb0075_event[, coord_x := as.numeric(coord_x)]
summary(ctb0075_event[, coord_x])

# coord_y
# Latitude-> coord_y
data.table::setnames(ctb0075_event, old = "Latitude", new = "coord_y")
ctb0075_event[, coord_y := as.numeric(coord_y)]
summary(ctb0075_event[, coord_y])

# Check for duplicate coordinates
ctb0075_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# old: Datum (coord)
# new: coord_datum
data.table::setnames(ctb0075_event, old = "Datum (coord)", new = "coord_datum")
ctb0075_event[, coord_datum := as.character(coord_datum)]
summary(ctb0075_event[, coord_datum])


# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0075_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0075_event[, coord_precisao := NA_real_]
summary(ctb0075_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0075_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0075_event[, coord_fonte := NA_real_]
summary(ctb0075_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0075_event, old = "País", new = "pais_id")
ctb0075_event[, pais_id := "BR"]

# #Mapeamento dos estados para sigla se necessário utilizar a função 'recode'
# mapa_siglas <- c(
#   "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
#   "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
#   "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
#   "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
#   "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
#   "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
#   "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
#   "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
#   "Tocantins" = "TO"
# )


# Estado (UF) -> estado_id
data.table::setnames(ctb0075_event, old = "Estado (UF)", new = "estado_id")
ctb0075_event[, estado_id := as.character(estado_id)]
ctb0075_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0075_event, old = "Município", new = "municipio_id")
ctb0075_event[, municipio_id := as.character(municipio_id)]
ctb0075_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# is missing on main document
ctb0075_event[, amostra_area := NA_real_]
summary(ctb0075_event[, amostra_area])

# SiBCS (2018) -> taxon_sibcs
data.table::setnames(ctb0075_event, old = "SiBCS (2018)", new = "taxon_sibcs")
ctb0075_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0075_event[, .N, by = taxon_sibcs]

# taxon_st_1999

ctb0075_event[, taxon_st := NA_character_]

# Pedregosidade (superficie) 
# pedregosidade is missing in this document

ctb0075_event[, pedregosidade := NA_character_]


# Rochosidade (superficie)
# rochosidade is missing in this document

ctb0075_event[, rochosidade := NA_character_]


str(ctb0075_event)


# layers ###########################################################################################
ctb0075_layer <- google_sheet(ctb0075_ids$gs_id, ctb0075_ids$gid_layer)
str(ctb0075_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0075_layer, old = "ID do evento", new = "observacao_id")
ctb0075_layer[, observacao_id := as.character(observacao_id)]
ctb0075_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
# camada_nome is missing in this document
ctb0075_layer[, camada_nome := NA_character_]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0075_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0075_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0075_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0075_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0075_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0075_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0075_layer[, profund_inf])


# areia
# old: Areia [g/kg]
# new: areia
# areia is missing for some layers...
data.table::setnames(ctb0075_layer, old = "Areia [g/kg]", new = "areia")
ctb0075_layer[, areia := as.numeric(areia)]
ctb0075_layer[is.na(areia), .(observacao_id, camada_nome, profund_sup, profund_inf, areia)]


# silte
# old: Silte [g/kg]
# new: silte
# silte is missing for some layers...
data.table::setnames(ctb0075_layer, old = "Silte [g/kg]", new = "silte")
ctb0075_layer[, silte := as.numeric(silte)]
ctb0075_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila [g/kg]
# new: argila
# argila is missing for some layers...
data.table::setnames(ctb0075_layer, old = "Argila [g/kg]", new = "argila")
ctb0075_layer[, argila := as.numeric(argila)]
ctb0075_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# Não existe fração fina na tabela apenas fração grossa portanto esta N/A
ctb0075_layer[, terrafina := NA_character_]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0075_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0075_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0075_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C [g/kg]
# new: carbono
data.table::setnames(ctb0075_layer, old = "C [g/kg]", new = "carbono")
ctb0075_layer[, carbono := as.numeric(carbono)]
summary(ctb0075_layer[, carbono])
check_empty_layer(ctb0075_layer, "carbono")

# ctc
# old: T [cmolc/dm3]
# new: ctc
data.table::setnames(ctb0075_layer, old = "T [cmolc/dm3]", new = "ctc")
ctb0075_layer[, ctc := as.numeric(ctc)]
summary(ctb0075_layer[, ctc])
check_empty_layer(ctb0075_layer, "ctc")

# ph
# old: pH H_2O
# new: ph
data.table::setnames(ctb0075_layer, old = "pH H_2O", new = "ph")
ctb0075_layer[, ph := as.numeric(ph)]
summary(ctb0075_layer[, ph])
check_empty_layer(ctb0075_layer, "ph")

# dsi
# dsi is missing in this document 
ctb0075_layer[, dsi := NA_real_]

str(ctb0075_layer)

# Merge ############################################################################################
# events and layers
ctb0075 <- merge(ctb0075_event, ctb0075_layer, all = TRUE)
ctb0075[, dataset_id := "ctb0075"]
# citation
ctb0075 <- merge(ctb0075, ctb0075_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0075)

# Layers: 110
# Events: 40
# Georeferenced events: 40


# Plot using mapview
if (FALSE) {
  ctb0075_sf <- sf::st_as_sf(
    ctb0075[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0075_sf["argila"])
}

# Write to disk ####################################################################################
ctb0075 <- select_output_columns(ctb0075)
data.table::fwrite(ctb0075, "ctb0075/ctb0075.csv")
data.table::fwrite(ctb0075_event, "ctb0075/ctb0075_event.csv")
data.table::fwrite(ctb0075_layer, "ctb0075/ctb0075_layer.csv")
