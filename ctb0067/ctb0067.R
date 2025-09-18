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
# ctb0067
# Dados de "Caracterização e gênese de Espodossolos da planície costeira do Estado de São Paulo"
# 
# https://docs.google.com/spreadsheets/d/1Nbl5kY9qX-VYINdK04bTjmcdyvc_ME64vKbBSz2oLR4/edit?usp=sharing


ctb0067_ids <- soildata_catalog("ctb0067")

# validation #####################################################################################

ctb0067_validation <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_validation)
str(ctb0067_validation)

# Check for negative validation results
sum(ctb0067_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0067_citation <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_citation)
str(ctb0067_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0067_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0067_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0067_citation <- data.table::data.table(
  dataset_id = "ctb0067",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0067_citation)

# event #####################################################################################
ctb0067_event <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_event)
str(ctb0067_event)

#PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0067_event, old = "ID do evento", new = "observacao_id")
ctb0067_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0067_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0067_event, old = "Ano (coleta)", new = "data_ano")
ctb0067_event[, data_ano := as.integer(data_ano)]
ctb0067_event[, .N, by = data_ano]

# ano_fonte
ctb0067_event[!is.na(data_ano), ano_fonte := "original"]
ctb0067_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0067_event, old = "Longitude (m)", new = "coord_x")
ctb0067_event[, coord_x := as.numeric(coord_x)]
summary(ctb0067_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0067_event, old = "Latitude (m)", new = "coord_y")
ctb0067_event[, coord_y := as.numeric(coord_y)]
summary(ctb0067_event[, coord_y])


# Datum (coord) -> coord_datum
# SIRGAS 2000 / UTM zone 23S
data.table::setnames(ctb0067_event, old = "Datum (coord)", new = "coord_datum")
ctb0067_event[coord_datum == "UTM zona 23S", coord_datum := 32723]
ctb0067_event[, coord_datum := as.integer(coord_datum)]
ctb0067_event[, .N, by = coord_datum]

# Transform coordinates to WGS84
ctb0067_event_sf <- sf::st_as_sf(
  ctb0067_event[coord_datum == 32723],
  coords = c("coord_x", "coord_y"), crs = 32723
)
ctb0067_event_sf <- sf::st_transform(ctb0067_event_sf, 4326)
ctb0067_event_sf <- sf::st_coordinates(ctb0067_event_sf)
ctb0067_event[coord_datum == 32723, coord_x := ctb0067_event_sf[, 1]]
ctb0067_event[coord_datum == 32723, coord_y := ctb0067_event_sf[, 2]]
ctb0067_event[coord_datum == 32723, coord_datum := 4326]
rm(ctb0067_event_sf)
summary(ctb0067_event[, .(coord_datum, coord_x, coord_y)])

# Check for duplicated coordinates
ctb0067_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Precisão (coord) [m] -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0067_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0067_event[, coord_precisao := NA_real_]
summary(ctb0067_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0067_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0067_event[, coord_fonte := NA_real_]
summary(ctb0067_event[, coord_precisao])


# País -> pais_id
data.table::setnames(ctb0067_event, old = "País", new = "pais_id")
ctb0067_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0067_event, old = "Estado (UF)", new = "estado_id")
ctb0067_event[, estado_id := "SP"]

# Município -> municipio_id
data.table::setnames(ctb0067_event, old = "Município", new = "municipio_id")
ctb0067_event[, municipio_id := as.character(municipio_id)]
ctb0067_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0067_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0067_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0067_event[, amostra_area])

# taxon_sibcs
ctb0067_event[, taxon_sibcs := NA_character_]

# taxon_st_1999

ctb0067_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# review the work at another time

ctb0067_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0067_event[, rochosidade := ("Não Rochoso")]


str(ctb0067_event)

# layers ###########################################################################################
ctb0067_layer <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_layer)
str(ctb0067_layer)

# PROCESS FIELDS

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0067_layer, old = "ID do evento", new = "observacao_id")
ctb0067_layer[, observacao_id := as.character(observacao_id)]
ctb0067_layer[, .N, by = observacao_id]

# camada_nome
# old: ID da camada
# new: camada_nome
data.table::setnames(ctb0067_layer, old = "ID da camada", new = "camada_nome")
ctb0067_layer[, camada_nome := as.character(camada_nome)]
ctb0067_layer[, .N, by = camada_nome]

# amostra_id
# old: ID da amostra
# new: amostra_id
data.table::setnames(ctb0067_layer, old = "ID da amostra", new = "amostra_id")
ctb0067_layer[, amostra_id := NA_character_]

# perfil_id
# old: Perfil
# new: perfil_id
data.table::setnames(ctb0067_layer, old = "Perfil", new = "perfil_id")
ctb0067_layer[, perfil_id := as.character(perfil_id)]
ctb0067_layer[, .N, by = perfil_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0067_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0067_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0067_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0067_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0067_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0067_layer[, profund_inf])