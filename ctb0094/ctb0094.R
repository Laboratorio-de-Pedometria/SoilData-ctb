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
# ctb0094
# Dados de "Atributos Químicos de Solos Hidromórficos em Área de Preservação Permanente no Sudoeste do Paraná"
# 
# https://docs.google.com/spreadsheets/d/1hEaWnYXayvDMTkfDcosTiW8zablNQ45FUYNMMydWXz4/edit?usp=sharing


ctb0094_ids <- soildata_catalog("ctb0094")

# validation #####################################################################################

ctb0094_validation <- google_sheet(ctb0094_ids$gs_id, ctb0094_ids$gid_validation)
str(ctb0094_validation)

# Check for negative validation results
sum(ctb0094_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0094_citation <- google_sheet(ctb0094_ids$gs_id, ctb0094_ids$gid_citation)
str(ctb0094_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0094_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0094_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0094_citation <- data.table::data.table(
  dataset_id = "ctb0094",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0094_citation)

# event #####################################################################################
ctb0094_event <- google_sheet(ctb0094_ids$gs_id, ctb0094_ids$gid_event)
str(ctb0094_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0094_event, old = "ID do evento", new = "observacao_id")
ctb0094_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0094_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0094_event, old = "Ano (coleta)", new = "data_ano")
ctb0094_event[, data_ano := as.integer(data_ano)]
ctb0094_event[, .N, by = data_ano]

# ano_fonte
ctb0094_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0094_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0094_event, old = "x", new = "coord_x")
ctb0094_event[, coord_x := as.numeric(coord_x)]
summary(ctb0094_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0094_event, old = "y", new = "coord_y")
ctb0094_event[, coord_y := as.numeric(coord_y)]
summary(ctb0094_event[, coord_y])

# Check for duplicate coordinates
ctb0094_event[, .N, by = .(coord_x, coord_y)][N > 1]

# DATUM -> coord_datum
data.table::setnames(ctb0094_event, old = "Datum (coord)", new = "coord_datum")

# Identifica as linhas que possuem coordenadas válidas para transformação
idx_transform <- which(!is.na(ctb0094_event$coord_x) & !is.na(ctb0094_event$coord_y))

if (length(idx_transform) > 0) {

  # Cria um objeto espacial (sf) a partir das coordenadas UTM.
  # A área de estudo (Sudoeste do Paraná) está na zona UTM 22S.
  # O datum original é SIRGAS 2000. O código EPSG correspondente é 31982.
  utm_sirgas_sf <- sf::st_as_sf(
    ctb0094_event[idx_transform, ],
    coords = c("coord_x", "coord_y"),
    crs = 31982, # CRS de Origem: SIRGAS 2000 / UTM zone 22S
    remove = FALSE
  )
  
  # Reprojeta (transforma) as coordenadas para WGS84 (EPSG:4326)
  wgs84_sf <- sf::st_transform(utm_sirgas_sf, crs = 4326)
  
  # Extrai as novas coordenadas geográficas (Longitude, Latitude)
  transformed_coords <- sf::st_coordinates(wgs84_sf)
  
  # Atualiza a tabela original com as coordenadas convertidas e o novo datum
  ctb0094_event[idx_transform, `:=`(
    coord_x = transformed_coords[, "X"],
    coord_y = transformed_coords[, "Y"],
    coord_datum = 4326
  )]
}
summary(ctb0094_event[, .(coord_x, coord_y)])

# Precisão (coord) -> coord_precisao
# We set it to NA_real_ (missing)
data.table::setnames(ctb0094_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0094_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
# Google Earth
data.table::setnames(ctb0094_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0094_event[, coord_fonte := as.character(coord_fonte)]


# País -> pais_id
data.table::setnames(ctb0094_event, old = "País", new = "pais_id")
ctb0094_event[, pais_id := "BR"]

# #Mapeamento dos estados para sigla se necessário utilizar a função 'recode'
 mapa_siglas <- c(
   "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
   "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
   "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
   "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
   "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
   "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
   "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
   "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
   "Tocantins" = "TO"
  )


# Estado (UF) -> estado_id
data.table::setnames(ctb0094_event, old = "Estado (UF)", new = "estado_id")
ctb0094_event[, estado_id := as.character(estado_id)]
ctb0094_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0094_event, old = "Município", new = "municipio_id")
ctb0094_event[, municipio_id := as.character(municipio_id)]
ctb0094_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0094_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0094_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0094_event[, amostra_area])

# SiBCS  -> taxon_sibcs
# is missing in this document.
ctb0094_event[, taxon_sibcs := NA_character_]

# taxon_st 
# missing this soil taxonomy on document
ctb0094_event[, taxon_st := NA_character_]
ctb0094_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
# missing in this document.

ctb0094_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# missing in this document.

ctb0094_event[, rochosidade := NA_character_]



str(ctb0094_event)

# layers ###########################################################################################
ctb0094_layer <- google_sheet(ctb0094_ids$gs_id, ctb0094_ids$gid_layer)
str(ctb0094_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0094_layer, old = "ID do evento", new = "observacao_id")
ctb0094_layer[, observacao_id := as.character(observacao_id)]
ctb0094_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0094_layer, old = "ID da camada", new = "camada_nome")
ctb0094_layer[, camada_nome := as.character(camada_nome)]
ctb0094_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0094_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0094_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0094_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0094_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0094_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0094_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0094_layer[, profund_inf])

#areia 
# old: Areia [%]
# new: areia
data.table::setnames(ctb0094_layer, old = "Areia [%]", new = "areia")
ctb0094_layer[, areia := as.numeric((areia)/100)*1000]
summary(ctb0094_layer[, areia])

#silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0094_layer, old = "Silte [%]", new = "silte")
ctb0094_layer[, silte := as.numeric((silte)/100)*1000]
summary(ctb0094_layer[, silte])

#argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0094_layer, old = "Argila [%]", new = "argila")
ctb0094_layer[, argila := as.numeric((argila)/100)*1000]
summary(ctb0094_layer[, argila])


#terrafina
data.table::setnames(ctb0094_layer, old = "Fração terra fina", new = "terrafina")
ctb0094_layer[, terrafina := as.character(terrafina)]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0094_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0094_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0094_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: Matéria orgânica  [%]
# new: carbono
# (as.numeric(carbono)/100)*1000 --> g/kg
data.table::setnames(ctb0094_layer, old = "Matéria orgânica  [%]", new = "carbono")
ctb0094_layer[, carbono := ((as.numeric(carbono)/100)*1000)/1.724]
ctb0094_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0094_layer[, carbono])

# ctc
# old: CTC efetiva  [cmolc/kg]
# new: ctc
data.table::setnames(ctb0094_layer, old = "CTC efetiva  [cmolc/kg]", new = "ctc")
ctb0094_layer[, ctc := as.numeric(ctc)]
summary(ctb0094_layer[, ctc])
check_empty_layer(ctb0094_layer, "ctc")

# ph
# old: pH  [%]
# new: ph
data.table::setnames(ctb0094_layer, old = "pH  [%]", new = "ph")
ctb0094_layer[, ph := as.numeric(ph)]
summary(ctb0094_layer[, ph])
check_empty_layer(ctb0094_layer, "ph")

# dsi 
#missing in this document.
ctb0094_layer[, dsi := NA_real_]




str(ctb0094_layer)

# Merge ############################################################################################
# events and layers
ctb0094 <- merge(ctb0094_event, ctb0094_layer, all = TRUE)
ctb0094[, dataset_id := "ctb0094"]
# citation
ctb0094 <- merge(ctb0094, ctb0094_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0094)

#Layers: 10
#Events: 9
#Georeferenced events: 8


# Plot using mapview
if (FALSE) {
  ctb0094_sf <- sf::st_as_sf(
    ctb0094[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0094_sf["argila"])
}

# Write to disk ####################################################################################
ctb0094 <- select_output_columns(ctb0094)
data.table::fwrite(ctb0094, "ctb0094/ctb0094.csv")
data.table::fwrite(ctb0094_event, "ctb0094/ctb0094_event.csv")
data.table::fwrite(ctb0094_layer, "ctb0094/ctb0094_layer.csv")
