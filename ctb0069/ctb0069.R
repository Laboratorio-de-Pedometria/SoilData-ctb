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
# ctb0069
# Diagnóstico agroambiental do entorno do Parque Nacional das Emas"
# 
# TRANSFORMAR EM PUBLICO PARA ADICIONAR O CAMINHO DO DOCUMENTO ABAIXO
# https://docs.google.com/spreadsheets/????????????????????????????


ctb0069_ids <- soildata_catalog("ctb0069")

# validation #####################################################################################

ctb0069_validation <- google_sheet(ctb0069_ids$gs_id, ctb0069_ids$gid_validation)
str(ctb0069_validation)

# Check for negative validation results
sum(ctb0069_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0069_citation <- google_sheet(ctb0069_ids$gs_id, ctb0069_ids$gid_citation)
str(ctb0069_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0069_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0069_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0069_citation <- data.table::data.table(
  dataset_id = "ctb0069",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0069_citation)

# event #####################################################################################
ctb0069_event <- google_sheet(ctb0069_ids$gs_id, ctb0069_ids$gid_event)
str(ctb0069_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0069_event, old = "ID do evento", new = "observacao_id")
ctb0069_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0069_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0069_event, old = "Ano (coleta)", new = "data_ano")
ctb0069_event[, data_ano := as.integer(data_ano)]
ctb0069_event[, .N, by = data_ano]

# ano_fonte
ctb0069_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0069_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0069_event, old = "Longitude", new = "coord_x")
ctb0069_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0069_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0069_event, old = "Latitude", new = "coord_y")
ctb0069_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0069_event[, coord_y])

# Datum (coord) -> coord_datum
ctb0069_event[, coord_datum := NA_character_]


# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0069_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0069_event[, coord_precisao := NA_real_]
summary(ctb0069_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0069_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0069_event[, coord_fonte := NA_real_]
summary(ctb0069_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0069_event, old = "País", new = "pais_id")
ctb0069_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0069_event, old = "Estado (UF)", new = "estado_id")
ctb0069_event[, estado_id := as.character(estado_id)]
ctb0069_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0069_event, old = "Município", new = "municipio_id")
ctb0069_event[, municipio_id := as.character(municipio_id)]
ctb0069_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0069_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0069_event[, amostra_area := NA_character_]
summary(ctb0069_event[, amostra_area])

# Classificação -> taxon_sibcs
data.table::setnames(ctb0069_event, old = "Classificação", new = "taxon_sibcs")
ctb0069_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0069_event[, .N, by = taxon_sibcs]

# taxon_st_1999

ctb0069_event[, taxon_st := NA_character_]

# Pedregosidade (superficie) 
data.table::setnames(ctb0069_event, old = "Pedregosidade", new = "pedregosidade")
ctb0069_event[, pedregosidade := as.character(pedregosidade)]
ctb0069_event[, .N, by = pedregosidade]

# Rochosidade (superficie)

data.table::setnames(ctb0069_event, old = "Rochosidade", new = "rochosidade")
ctb0069_event[, rochosidade := as.character(rochosidade)]
ctb0069_event[, .N, by = rochosidade]



str(ctb0069_event)


