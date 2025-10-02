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
# ctb0080
# Dados de "Variabilidade de atributos do solo em um transecto entre os biomas Pantanal Mato-grossense e Cerrado "
# 
# 
# https://docs.google.com/spreadsheets/d/1Qxw0rFw9MY_SWtAvbn6YjHPO3X2gtOH9N38xS5V-oCM/edit?usp=sharing


ctb0080_ids <- soildata_catalog("ctb0080")

# validation #####################################################################################

ctb0080_validation <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_validation)
str(ctb0080_validation)

# Check for negative validation results
sum(ctb0080_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0080_citation <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_citation)
str(ctb0080_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0080_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0080_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0080_citation <- data.table::data.table(
  dataset_id = "ctb0080",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0080_citation)

# event #####################################################################################
ctb0080_event <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_event)
str(ctb0080_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0080_event, old = "ID do evento", new = "observacao_id")
ctb0080_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0080_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0080_event, old = "Ano (coleta)", new = "data_ano")
ctb0080_event[, data_ano := as.integer(data_ano)]
ctb0080_event[, .N, by = data_ano]

# ano_fonte
ctb0080_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0080_event[, .N, by = ano_fonte]


# coord_x
# X -> coord_x
#
data.table::setnames(ctb0080_event, old = "X", new = "coord_x")
ctb0080_event[, coord_x := as.numeric(coord_x)]
summary(ctb0080_event[, coord_x])

# coord_y
# Y -> coord_y
# 
data.table::setnames(ctb0080_event, old = "Y", new = "coord_y")
ctb0080_event[, coord_y := as.numeric(coord_y)]
summary(ctb0080_event[, coord_y])

# Check for duplicate coordinates
ctb0080_event[, .N, by = .(coord_x, coord_y)][N > 1]

# DATUM -> coord_datum
data.table::setnames(ctb0080_event, old = "DATUM", new = "coord_datum")
ctb0080_event[, coord_datum := as.character(coord_datum)]

# Precisão (coord) -> coord_precisao
# We set it to NA_real_
ctb0080_event[, coord_precisao := NA_real_]
summary(ctb0080_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0080_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0080_event[, coord_fonte := as.character(coord_fonte)]
summary(ctb0080_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0080_event, old = "País", new = "pais_id")
ctb0080_event[, pais_id := "BR"]

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
data.table::setnames(ctb0080_event, old = "Estado", new = "estado_id")
ctb0080_event[, estado_id := as.character(estado_id)]
ctb0080_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0080_event, old = "Município", new = "municipio_id")
ctb0080_event[, municipio_id := as.character(municipio_id)]
ctb0080_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# is missing on main document
ctb0080_event[, amostra_area := NA_real_]
summary(ctb0080_event[, amostra_area])

# SiBCS (2009) -> taxon_sibcs
data.table::setnames(ctb0080_event, old = "SiBCS (2009)", new = "taxon_sibcs")
ctb0080_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0080_event[, .N, by = taxon_sibcs]

# taxon_st_1999
# missing taxon_st_1999 so, we set N/A.
ctb0080_event[, taxon_st := NA_character_]

# Pedregosidade (superficie) 
data.table::setnames(ctb0080_event, old = "Pedregosidade", new = "pedregosidade")
ctb0080_event[, pedregosidade := as.character(pedregosidade)]
ctb0080_event[, .N, by = pedregosidade]

# Rochosidade (superficie)

data.table::setnames(ctb0080_event, old = "Rochosidade", new = "rochosidade")
ctb0080_event[, rochosidade := as.character(rochosidade)]
ctb0080_event[, .N, by = rochosidade]



str(ctb0080_event)



# layers ###########################################################################################
ctb0080_layer <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_layer)
str(ctb0080_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0080_layer, old = "ID do evento", new = "observacao_id")
ctb0080_layer[, observacao_id := as.character(observacao_id)]
ctb0080_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0080_layer, old = "ID da camada", new = "camada_nome")
ctb0080_layer[, camada_nome := as.character(camada_nome)]
ctb0080_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0080_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0080_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0080_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0080_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0080_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0080_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0080_layer[, profund_inf])

# Sand in this document is separated into
# Coarse sand, Fine sand

# areia_grossa
# old: "Areia grossa [g/kg]"
# new: areia_grossa
# 
data.table::setnames(ctb0080_layer, old = "Areia grossa [g/kg]", new = "areia_grossa")
ctb0080_layer[, areia_grossa := as.numeric(areia_grossa)]
ctb0080_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: "Areia fina [g/kg]"
# new: areia_fina
# 
data.table::setnames(ctb0080_layer, old = "Areia fina [g/kg]", new = "areia_fina")
ctb0080_layer[, areia_fina := as.numeric(areia_fina)]
ctb0080_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

              
# areia
# criação da coluna areia 
cols_areia <- c("areia_grossa", "areia_fina")

# Criação da coluna areia, somando as frações e tratando NAs como 0
# na.rm = TRUE (NA remove) remove os NAs antes de somar
ctb0080_layer[, areia := rowSums(.SD, na.rm = TRUE), .SDcols = cols_areia]


# silte
# old: Silte [g/kg]
# new: silte
# 
data.table::setnames(ctb0080_layer, old = "Silte [g/kg]", new = "silte")
ctb0080_layer[, silte := as.numeric(silte)]
ctb0080_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila [g/kg]
# new: argila
#
data.table::setnames(ctb0080_layer, old = "Argila [g/kg]", new = "argila")
ctb0080_layer[, argila := as.numeric(argila)]
ctb0080_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# old: Terra fina [g/kg]
# new: terrafina
data.table::setnames(ctb0080_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0080_layer[, terrafina := as.numeric(terrafina)]
ctb0080_layer[is.na(terrafina), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0080_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0080_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0080_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C[orgânico] [g/kg]
# new: carbono
data.table::setnames(ctb0080_layer, old = "C[orgânico] [g/kg]", new = "carbono")
ctb0080_layer[, carbono := as.numeric(carbono)]
summary(ctb0080_layer[, carbono])
check_empty_layer(ctb0080_layer, "carbono")

# ctc
# old: T [cmolc/kg]
# new: ctc
data.table::setnames(ctb0080_layer, old = "T [cmolc/kg]", new = "ctc")
ctb0080_layer[, ctc := as.numeric(ctc)]
summary(ctb0080_layer[, ctc])
check_empty_layer(ctb0080_layer, "ctc")

# ph
# old: pH [H_2O]
# new: ph
data.table::setnames(ctb0080_layer, old = "pH [H_2O]", new = "ph")
ctb0080_layer[, ph := as.numeric(ph)]
summary(ctb0080_layer[, ph])
check_empty_layer(ctb0080_layer, "ph")

# dsi
# old: Densidade Solo [kg/dm^3]
# new: dsi
data.table::setnames(ctb0080_layer, old = "Densidade Solo [kg/dm^3]", new = "dsi")
ctb0080_layer[, dsi := as.numeric(dsi)]
summary(ctb0080_layer[, dsi])
check_empty_layer(ctb0080_layer, "dsi")

str(ctb0080_layer)

# Merge ############################################################################################
# events and layers
ctb0080 <- merge(ctb0080_event, ctb0080_layer, all = TRUE)
ctb0080[, dataset_id := "ctb0080"]
# citation
ctb0080 <- merge(ctb0080, ctb0080_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0080)

#Layers: 1780
#Events: 1415
#Georeferenced events: 1415


# Plot using mapview
if (FALSE) {
  ctb0080_sf <- sf::st_as_sf(
    ctb0080[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0080_sf["argila"])
}

# Write to disk ####################################################################################
ctb0080 <- select_output_columns(ctb0080)
data.table::fwrite(ctb0080, "ctb0080/ctb0080.csv")
data.table::fwrite(ctb0080_event, "ctb0080/ctb0080_event.csv")
data.table::fwrite(ctb0080_layer, "ctb0080/ctb0080_layer.csv")
