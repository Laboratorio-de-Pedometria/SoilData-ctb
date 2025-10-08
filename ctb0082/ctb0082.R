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
if (!require("parzer")) {
  install.packages("parzer")
  library("parzer")
}

# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0082
# Dados de "Atributos Físicos e Matéria Orgânica de Organossolos Háplicos em Distintos Ambientes no Brasil"
# e "Atributos químicos, carbono orgânico e substâncias húmicas em Organossolos Háplicos de várias regiões do Brasil"
# 
# 
# https://docs.google.com/spreadsheets/d/17c4-EtKjNuDjbd53OJutySYfp4lOR3b_8p_Qh-pRyJU/edit?usp=sharing


ctb0082_ids <- soildata_catalog("ctb0082")

# validation #####################################################################################

ctb0082_validation <- google_sheet(ctb0082_ids$gs_id, ctb0082_ids$gid_validation)
str(ctb0082_validation)

# Check for negative validation results
sum(ctb0082_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0082_citation <- google_sheet(ctb0082_ids$gs_id, ctb0082_ids$gid_citation)
str(ctb0082_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0082_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0082_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0082_citation <- data.table::data.table(
  dataset_id = "ctb0082",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0082_citation)

# event #####################################################################################
ctb0082_event <- google_sheet(ctb0082_ids$gs_id, ctb0082_ids$gid_event)
str(ctb0082_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0082_event, old = "ID do evento", new = "observacao_id")
ctb0082_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0082_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0082_event, old = "Ano (coleta)", new = "data_ano")
ctb0082_event[, data_ano := as.integer(data_ano)]
ctb0082_event[, .N, by = data_ano]

# ano_fonte
ctb0082_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0082_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0082_event, old = "Longitude", new = "coord_x")
ctb0082_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0082_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0082_event, old = "Latitude", new = "coord_y")
ctb0082_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0082_event[, coord_y])

# Check for duplicate coordinates
ctb0082_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# coord_datum is missing in this document.
data.table::setnames(ctb0082_event, old = "Datum (coord)", new = "coord_datum")
ctb0082_event[, coord_datum := NA_real_]

# Precisão (coord) -> coord_precisao
# We set it to NA_real_
ctb0082_event[, coord_precisao := NA_real_]
summary(ctb0082_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0082_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0082_event[, coord_fonte := as.character(coord_fonte)]
summary(ctb0082_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0082_event, old = "País", new = "pais_id")
ctb0082_event[, pais_id := "BR"]

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
data.table::setnames(ctb0082_event, old = "Estado (UF)", new = "estado_id")
ctb0082_event[, estado_id := as.character(estado_id)]
ctb0082_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0082_event, old = "Município", new = "municipio_id")
ctb0082_event[, municipio_id := as.character(municipio_id)]
ctb0082_event[, .N, by = municipio_id]

# Área do evento [m] -> amostra_area
#
data.table::setnames(ctb0082_event, old = "Área do evento [m]", new = "amostra_area")
ctb0082_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0082_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0082_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0082_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0082_event[, .N, by = taxon_sibcs]

# taxon_st_1999
# missing taxon_st_1999 so, we set N/A.
ctb0082_event[, taxon_st := NA_character_]


# Pedregosidade (superficie) 
# missing in this document.
ctb0082_event[, pedregosidade := NA_character_]


# Rochosidade (superficie)
# missing in this document.
ctb0082_event[, rochosidade := NA_character_]


str(ctb0082_event)

# layers ###########################################################################################
ctb0082_layer <- google_sheet(ctb0082_ids$gs_id, ctb0082_ids$gid_layer)
str(ctb0082_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0082_layer, old = "ID do evento", new = "observacao_id")
ctb0082_layer[, observacao_id := as.character(observacao_id)]
ctb0082_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0082_layer, old = "ID da camada", new = "camada_nome")
ctb0082_layer[, camada_nome := as.character(camada_nome)]
ctb0082_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0082_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0082_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0082_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0082_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0082_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0082_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0082_layer[, profund_inf])

#We dont have Areia/Silte/Argila informations in this document. So we set as NA_real_ (N/A)

#areia
ctb0082_layer[, areia := NA_real_]
#silte
ctb0082_layer[, silte := NA_real_]
#argila
ctb0082_layer[, argila := NA_real_]


#terrafina
#terrafina is missing in this document, so we set N/A.

ctb0082_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0082_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0082_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0082_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: Carbono total [g/kg]
# new: carbono
data.table::setnames(ctb0082_layer, old = "Carbono total [g/kg]", new = "carbono")
ctb0082_layer[, carbono := as.numeric(carbono)]
ctb0082_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0082_layer[, carbono])

# ctc
# old: T [cmolc/dm3]
# new: ctc
data.table::setnames(ctb0082_layer, old = "T [cmolc​/dm3]", new = "ctc")
ctb0082_layer[, ctc := as.numeric(ctc)]
summary(ctb0082_layer[, ctc])
check_empty_layer(ctb0082_layer, "ctc")

# ph
# old: pH H2O
# new: ph
data.table::setnames(ctb0082_layer, old = "pH H2​O", new = "ph")
ctb0082_layer[, ph := as.numeric(ph)]
summary(ctb0082_layer[, ph])
check_empty_layer(ctb0082_layer, "ph")

# dsi
# old: Densidade do solo [mg/m3]
# new: dsi
data.table::setnames(ctb0082_layer, old = "Densidade do solo [mg/m3]", new = "dsi")
ctb0082_layer[, dsi := as.numeric(dsi)]
summary(ctb0082_layer[, dsi])
check_empty_layer(ctb0082_layer, "dsi")

str(ctb0082_layer)

# Merge ############################################################################################
# events and layers
ctb0082 <- merge(ctb0082_event, ctb0082_layer, all = TRUE)
ctb0082[, dataset_id := "ctb0082"]
# citation
ctb0082 <- merge(ctb0082, ctb0082_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0082)

#Layers: 38
#Events: 8
#Georeferenced events: 8


# Plot using mapview
if (FALSE) {
  ctb0082_sf <- sf::st_as_sf(
    ctb0082[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0082_sf["argila"])
}

# Write to disk ####################################################################################
ctb0082 <- select_output_columns(ctb0082)
data.table::fwrite(ctb0082, "ctb0082/ctb0082.csv")
data.table::fwrite(ctb0082_event, "ctb0082/ctb0082_event.csv")
data.table::fwrite(ctb0082_layer, "ctb0082/ctb0082_layer.csv")
