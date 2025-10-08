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
# ctb0097
# Dados de "Efeito do fogo nos atributos do solo de duas unidades fitofisionômicas do Pantanal"
# 
# 
# https://docs.google.com/spreadsheets/d/1EXKa0JaJsqftiG5sdZmDp5wTHwODJIEZBXXxnO0IxKE/edit?usp=sharing


ctb0097_ids <- soildata_catalog("ctb0097")

# validation #####################################################################################

ctb0097_validation <- google_sheet(ctb0097_ids$gs_id, ctb0097_ids$gid_validation)
str(ctb0097_validation)

# Check for negative validation results
sum(ctb0097_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0097_citation <- google_sheet(ctb0097_ids$gs_id, ctb0097_ids$gid_citation)
str(ctb0097_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0097_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0097_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0097_citation <- data.table::data.table(
  dataset_id = "ctb0097",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0097_citation)

# event #####################################################################################
ctb0097_event <- google_sheet(ctb0097_ids$gs_id, ctb0097_ids$gid_event)
str(ctb0097_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0097_event, old = "ID do evento", new = "observacao_id")
ctb0097_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0097_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0097_event, old = "Ano (coleta)", new = "data_ano")
ctb0097_event[, data_ano := as.integer(data_ano)]
ctb0097_event[, .N, by = data_ano]

# ano_fonte
ctb0097_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0097_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0097_event, old = "Longitude", new = "coord_x")
ctb0097_event[, coord_x := as.numeric(coord_x)]
summary(ctb0097_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0097_event, old = "Latitude", new = "coord_y")
ctb0097_event[, coord_y := as.numeric(coord_y)]
summary(ctb0097_event[, coord_y])

# Check for duplicate coordinates
ctb0097_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# DATUM == WGS84 already.
data.table::setnames(ctb0097_event, old = "Datum (coord)", new = "coord_datum")
ctb0097_event[, coord_datum := as.character(coord_datum)]


# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0097_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0097_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0097_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0097_event[, coord_fonte := as.character(coord_fonte)]
summary(ctb0097_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0097_event, old = "País", new = "pais_id")
ctb0097_event[, pais_id := "BR"]

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
data.table::setnames(ctb0097_event, old = "Estado (UF)", new = "estado_id")
ctb0097_event[, estado_id := as.character(estado_id)]
ctb0097_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0097_event, old = "Município", new = "municipio_id")
ctb0097_event[, municipio_id := as.character(municipio_id)]
ctb0097_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0097_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0097_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0097_event[, amostra_area])

# SiBCS (edição?) -> taxon_sibcs
data.table::setnames(ctb0097_event, old = "SiBCS (edição?)", new = "taxon_sibcs")
ctb0097_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0097_event[, .N, by = taxon_sibcs]

# taxon_st <- Soil Taxonomy (edição?)
# missing edition of this soil taxonomy
data.table::setnames(ctb0097_event, old = "Soil Taxonomy (edição?)", new = "taxon_st")
ctb0097_event[, taxon_st := as.character(taxon_st)]
ctb0097_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
data.table::setnames(ctb0097_event, old = "Pedregosidade", new = "pedregosidade")
ctb0097_event[, pedregosidade := as.character(pedregosidade)]
ctb0097_event[, .N, by = pedregosidade]

# Rochosidade (superficie)

data.table::setnames(ctb0097_event, old = "Rochosidade", new = "rochosidade")
ctb0097_event[, rochosidade := as.character(rochosidade)]
ctb0097_event[, .N, by = rochosidade]


str(ctb0097_event)

# layers ###########################################################################################
ctb0097_layer <- google_sheet(ctb0097_ids$gs_id, ctb0097_ids$gid_layer)
str(ctb0097_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0097_layer, old = "ID do evento", new = "observacao_id")
ctb0097_layer[, observacao_id := as.character(observacao_id)]
ctb0097_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0097_layer, old = "ID da camada", new = "camada_nome")
ctb0097_layer[, camada_nome := as.character(camada_nome)]
ctb0097_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0097_layer, old = "ID da amostra", new = "amostra_id")
ctb0097_layer[, amostra_id := as.character(amostra_id)]
ctb0097_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0097_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0097_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0097_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0097_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0097_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0097_layer[, profund_inf])

#areia grossa
# old: Areia Grossa 2-0,2 mm [g/kg]
# new: areia_grossa
data.table::setnames(ctb0097_layer, old = "Areia Grossa 2-0,2 mm [g/kg]", new = "areia_grossa")
ctb0097_layer[, areia_grossa := as.numeric(areia_grossa)]
summary(ctb0097_layer[, areia_grossa])

#areia
ctb0097_layer[, areia := areia_grossa]
summary(ctb0097_layer[, areia])


#silte
# old: Silte 0,005-0,002 mm [g/kg]
# new: silte
data.table::setnames(ctb0097_layer, old = "Silte 0,005-0,002 mm [g/kg]", new = "silte")
ctb0097_layer[, silte := as.numeric(silte)]
summary(ctb0097_layer[, silte])


#argila
# old: Argila > 0,002 mm [g/kg]
# new: argila
data.table::setnames(ctb0097_layer, old = "Argila > 0,002 mm [g/kg]", new = "argila")
ctb0097_layer[, argila := as.numeric(argila)]
summary(ctb0097_layer[, argila])


#terrafina
# is missing in this document.
ctb0097_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0097_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0097_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0097_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: MO [g/kg]
# new: carbono
data.table::setnames(ctb0097_layer, old = "MO [g/kg]", new = "carbono")
ctb0097_layer[, carbono := as.numeric(carbono)/1.724]
ctb0097_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0097_layer[, carbono])

# ctc
# old: T [cmolc/kg]
# new: ctc
data.table::setnames(ctb0097_layer, old = "T [cmolc/kg]", new = "ctc")
ctb0097_layer[, ctc := as.numeric(ctc)]
summary(ctb0097_layer[, ctc])
check_empty_layer(ctb0097_layer, "ctc")

# ph
# old: pH em H_2O
# new: ph
data.table::setnames(ctb0097_layer, old = "pH em H_2O", new = "ph")
ctb0097_layer[, ph := as.numeric(ph)]
summary(ctb0097_layer[, ph])
check_empty_layer(ctb0097_layer, "ph")

# dsi
# dsi is missing in this document.
ctb0097_layer[, dsi := NA_real_]


str(ctb0097_layer)

# Merge ############################################################################################
# events and layers
ctb0097 <- merge(ctb0097_event, ctb0097_layer, all = TRUE)
ctb0097[, dataset_id := "ctb0097"]
# citation
ctb0097 <- merge(ctb0097, ctb0097_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0097)


#Layers: 231
#Events: 174
#Georeferenced events: 174


# Plot using mapview
if (FALSE) {
  ctb0097_sf <- sf::st_as_sf(
    ctb0097[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0097_sf["argila"])
}

# Write to disk ####################################################################################
ctb0097 <- select_output_columns(ctb0097)
data.table::fwrite(ctb0097, "ctb0097/ctb0097.csv")
data.table::fwrite(ctb0097_event, "ctb0097/ctb0097_event.csv")
data.table::fwrite(ctb0097_layer, "ctb0097/ctb0097_layer.csv")
