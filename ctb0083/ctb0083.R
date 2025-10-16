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
# ctb0083
# Dados de "APLICAÇÃO E ANÁLISE DA LEGISLAÇÃO PARANAENSE RELATIVA ÀS ÁREAS ÚMIDAS,
# EM UMA BACIA EXPERIMENTAL SITUADA EM ANTONINA (PR)"
# 
# 
# https://docs.google.com/spreadsheets/d/1IKjQNVDVYhVNewPDCpTcD8ONMoKqBFSqb6gQN3YUT4E/edit?usp=sharing


ctb0083_ids <- soildata_catalog("ctb0083")

# validation #####################################################################################

ctb0083_validation <- google_sheet(ctb0083_ids$gs_id, ctb0083_ids$gid_validation)
str(ctb0083_validation)

# Check for negative validation results
sum(ctb0083_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0083_citation <- google_sheet(ctb0083_ids$gs_id, ctb0083_ids$gid_citation)
str(ctb0083_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0083_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0083_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0083_citation <- data.table::data.table(
  dataset_id = "ctb0083",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0083_citation)

# event #####################################################################################
ctb0083_event <- google_sheet(ctb0083_ids$gs_id, ctb0083_ids$gid_event)
str(ctb0083_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0083_event, old = "ID do evento", new = "observacao_id")
ctb0083_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0083_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0083_event, old = "Ano (coleta)", new = "data_ano")
ctb0083_event[, data_ano := as.integer(data_ano)]
ctb0083_event[, .N, by = data_ano]

# ano_fonte
ctb0083_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0083_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0083_event, old = "Coordenadas UTM X", new = "coord_x")
ctb0083_event[, coord_x := as.numeric(coord_x)]

# Latitude -> coord_y
data.table::setnames(ctb0083_event, old = "Coordenadas UTM Y", new = "coord_y")
ctb0083_event[, coord_y := as.numeric(coord_y)]

# Datum (coord) -> coord_datum
data.table::setnames(ctb0083_event, old = "Datum (coord)", new = "coord_datum")
ctb0083_event[coord_datum == "SAD-69", coord_datum := 29192]
ctb0083_event[, coord_datum := as.integer(coord_datum)]

# --- MODIFICAÇÃO PRINCIPAL AQUI ---
# Remove as linhas com NA e sobrescreve o data.table original
ctb0083_event <- na.omit(ctb0083_event, cols = c("coord_x", "coord_y"))

# Converte o data.table para um objeto espacial (sf)
# Agora usamos o 'ctb0083_event' original, pois ele já foi limpo
ctb0083_event_sf <- sf::st_as_sf(
  ctb0083_event,
  coords = c("coord_x", "coord_y"),
  crs = 29192 # Define o CRS de origem como SAD 69 / UTM Zone 22S
)

# Transforma as coordenadas para WGS84 (EPSG: 4326)
ctb0083_event_sf_wgs84 <- sf::st_transform(ctb0083_event_sf, 4326)

# Extrai as novas coordenadas do objeto sf
new_coords <- sf::st_coordinates(ctb0083_event_sf_wgs84)

# Atualiza a tabela original com as coordenadas convertidas e o novo datum
ctb0083_event[, coord_x := new_coords[, 1]] # Novas longitudes
ctb0083_event[, coord_y := new_coords[, 2]] # Novas latitudes
ctb0083_event[, coord_datum := 4326L]       # Novo datum: WGS84

# Verifica o resultado final
summary(ctb0083_event[, .(coord_datum, coord_x, coord_y)])


# Precisão (coord) -> coord_precisao
#
data.table::setnames(ctb0083_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0083_event[, coord_precisao := as.character(coord_precisao)]
summary(ctb0083_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0083_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0083_event[, coord_fonte := as.character(coord_fonte)]
summary(ctb0083_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0083_event, old = "País", new = "pais_id")
ctb0083_event[, pais_id := "BR"]

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
data.table::setnames(ctb0083_event, old = "Estado (UF)", new = "estado_id")
ctb0083_event[, estado_id := as.character(estado_id)]
ctb0083_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0083_event, old = "Município", new = "municipio_id")
ctb0083_event[, municipio_id := as.character(municipio_id)]
ctb0083_event[, .N, by = municipio_id]

# Área do evento [m] -> amostra_area
#
data.table::setnames(ctb0083_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0083_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0083_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0083_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0083_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0083_event[, .N, by = taxon_sibcs]

# taxon_st_1999
# missing taxon_st_1999 so, we set N/A.
ctb0083_event[, taxon_st := NA_character_]


# Pedregosidade (superficie) 
# missing in this document.
ctb0083_event[, pedregosidade := NA_character_]


# Rochosidade (superficie)
# missing in this document.
ctb0083_event[, rochosidade := NA_character_]


str(ctb0083_event)

# layers ###########################################################################################
ctb0083_layer <- google_sheet(ctb0083_ids$gs_id, ctb0083_ids$gid_layer)
str(ctb0083_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0083_layer, old = "ID do evento", new = "observacao_id")
ctb0083_layer[, observacao_id := as.character(observacao_id)]
ctb0083_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0083_layer, old = "ID da camada", new = "camada_nome")
ctb0083_layer[, camada_nome := as.character(camada_nome)]
ctb0083_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0083_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0083_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0083_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0083_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0083_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0083_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0083_layer[, profund_inf])

#areia
data.table::setnames(ctb0083_layer, old = "Areia [g/kg]", new = "areia")
ctb0083_layer[, areia := as.numeric(areia)]
summary(ctb0083_layer[, areia])
#silte
data.table::setnames(ctb0083_layer, old = "Silte [g/kg]", new = "silte")
ctb0083_layer[, silte := as.numeric(silte)]
summary(ctb0083_layer[, silte])
#argila
data.table::setnames(ctb0083_layer, old = "Argila [g/kg]", new = "argila")
ctb0083_layer[, argila := as.numeric(argila)]
summary(ctb0083_layer[, argila])


#terrafina
#terrafina is missing in this document, so we set N/A.

ctb0083_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0083_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0083_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0083_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C [g/dm]
# new: carbono
data.table::setnames(ctb0083_layer, old = "C [g/dm]", new = "carbono")
ctb0083_layer[, carbono := as.numeric(carbono)]
ctb0083_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0083_layer[, carbono])

# ctc
# missing CTC in this document.

ctb0083_layer[, ctc := NA_real_]


# ph
# missing ph in this document.
ctb0083_layer[, ph := NA_real_]


# dsi
# because we have two soil density variables, we average the two for the dsi variable
# new: dsi
data.table::setnames(ctb0083_layer, old = "Densidade do solo A [g/cm3]", new = "Solo_A")
data.table::setnames(ctb0083_layer, old = "Densidade do solo B [g/cm3]", new = "Solo_B")

ctb0083_layer[, dsi := rowMeans(.SD, na.rm = TRUE), .SDcols = c("Solo_A", "Solo_B")]

summary(ctb0083_layer[, dsi])
check_empty_layer(ctb0083_layer, "dsi")

str(ctb0083_layer)

# Merge ############################################################################################
# events and layers
ctb0083 <- merge(ctb0083_event, ctb0083_layer, all = TRUE)
ctb0083[, dataset_id := "ctb0083"]
# citation
ctb0083 <- merge(ctb0083, ctb0083_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0083)

#Layers: 80
#Events: 27
#Georeferenced events: 15


# Plot using mapview
if (FALSE) {
  ctb0083_sf <- sf::st_as_sf(
    ctb0083[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0083_sf["argila"])
}

# Write to disk ####################################################################################
ctb0083 <- select_output_columns(ctb0083)
data.table::fwrite(ctb0083, "ctb0083/ctb0083.csv")
data.table::fwrite(ctb0083_event, "ctb0083/ctb0083_event.csv")
data.table::fwrite(ctb0083_layer, "ctb0083/ctb0083_layer.csv")
