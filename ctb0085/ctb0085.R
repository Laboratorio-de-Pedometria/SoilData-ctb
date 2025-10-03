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
# ctb0085
# Dados de "Levantamento de Reconhecimento dos Solos do Município de Bandeirantes - MS"
# 
# 
# https://docs.google.com/spreadsheets/d/1UTqOJDhMevj6LFECp126BEHOGPjBo0lAmDdzRjunMmM/edit?usp=sharing


ctb0085_ids <- soildata_catalog("ctb0085")

# validation #####################################################################################

ctb0085_validation <- google_sheet(ctb0085_ids$gs_id, ctb0085_ids$gid_validation)
str(ctb0085_validation)

# Check for negative validation results
sum(ctb0085_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0085_citation <- google_sheet(ctb0085_ids$gs_id, ctb0085_ids$gid_citation)
str(ctb0085_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0085_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0085_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0085_citation <- data.table::data.table(
  dataset_id = "ctb0085",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0085_citation)

# event #####################################################################################
ctb0085_event <- google_sheet(ctb0085_ids$gs_id, ctb0085_ids$gid_event)
str(ctb0085_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0085_event, old = "ID do evento", new = "observacao_id")
ctb0085_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0085_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0085_event, old = "Ano (coleta)", new = "data_ano")
ctb0085_event[, data_ano := as.integer(data_ano)]
ctb0085_event[, .N, by = data_ano]

# ano_fonte
ctb0085_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0085_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0085_event, old = "Longitude (m)", new = "coord_x")
ctb0085_event[, coord_x := as.numeric(coord_x)]
summary(ctb0085_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0085_event, old = "Latitude (m)", new = "coord_y")
ctb0085_event[, coord_y := as.numeric(coord_y)]
summary(ctb0085_event[, coord_y])

# Check for duplicate coordinates
ctb0085_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# coord_datum is missing in this document.
data.table::setnames(ctb0085_event, old = "Datum (coord)", new = "coord_datum")
ctb0085_event[, coord_datum := NA_real_]

# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0085_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0085_event[, coord_precisao := as.character(coord_precisao)]
summary(ctb0085_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0085_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0085_event[, coord_fonte := as.character(coord_fonte)]
summary(ctb0085_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0085_event, old = "País", new = "pais_id")
ctb0085_event[, pais_id := "BR"]

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
data.table::setnames(ctb0085_event, old = "Estado (UF)", new = "estado_id")
ctb0085_event[, estado_id := recode(estado_id, !!!mapa_siglas)]
ctb0085_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0085_event, old = "Município", new = "municipio_id")
ctb0085_event[, municipio_id := as.character(municipio_id)]
ctb0085_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0085_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0085_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0085_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0085_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0085_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0085_event[, .N, by = taxon_sibcs]

# taxon_st_1999
# missing taxon_st_1999 so, we set N/A.
ctb0085_event[, taxon_st := NA_character_]


# Pedregosidade (superficie) 
data.table::setnames(ctb0085_event, old = "Pedregosidade", new = "pedregosidade")
ctb0085_event[, pedregosidade := as.character(pedregosidade)]
ctb0085_event[, .N, by = pedregosidade]

# Rochosidade (superficie)

data.table::setnames(ctb0085_event, old = "Rochosidade", new = "rochosidade")
ctb0085_event[, rochosidade := as.character(rochosidade)]
ctb0085_event[, .N, by = rochosidade]


str(ctb0085_event)

# layers ###########################################################################################
ctb0085_layer <- google_sheet(ctb0085_ids$gs_id, ctb0085_ids$gid_layer)
str(ctb0085_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0085_layer, old = "ID do evento", new = "observacao_id")
ctb0085_layer[, observacao_id := as.character(observacao_id)]
ctb0085_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0085_layer, old = "ID da camada", new = "camada_nome")
ctb0085_layer[, camada_nome := as.character(camada_nome)]
ctb0085_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0085_layer, old = "ID da amostra", new = "amostra_id")
ctb0085_layer[, amostra_id := as.character(amostra_id)]
ctb0085_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0085_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0085_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0085_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0085_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0085_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0085_layer[, profund_inf])

#areia grossa
# old: Areia grossa 2-0,20 mm (g/kg)
# new: areia_grossa
data.table::setnames(ctb0085_layer, old = "Areia grossa 2-0,20 mm (g/kg)", new = "areia_grossa")
ctb0085_layer[, areia_grossa := as.numeric(areia_grossa)]
summary(ctb0085_layer[, areia_grossa])

#areia fina
# old: Areia fina 0,20-0,05 mm (g/kg)
# new: areia_fina
data.table::setnames(ctb0085_layer, old = "Areia fina 0,20-0,05 mm (g/kg)", new = "areia_fina")
ctb0085_layer[, areia_fina := as.numeric(areia_fina)]
summary(ctb0085_layer[, areia_fina])

#areia
ctb0085_layer[, areia := areia_grossa + areia_fina]
summary(ctb0085_layer[, areia])


#silte
# old: Silte 0,05-0,002 mm (g/kg)
# new: silte
data.table::setnames(ctb0085_layer, old = "Silte 0,05-0,002 mm (g/kg)", new = "silte")
ctb0085_layer[, silte := as.numeric(silte)]
summary(ctb0085_layer[, silte])


#argila
# old: Argila < 0,002 mm (g/kg)
# new: argila
data.table::setnames(ctb0085_layer, old = "Argila < 0,002 mm (g/kg)", new = "argila")
ctb0085_layer[, argila := as.numeric(argila)]
summary(ctb0085_layer[, argila])


#terrafina
# old: Terra fina < 2 mm (g/kg)
# new: terrafina
data.table::setnames(ctb0085_layer, old = "Terra fina < 2 mm (g/kg)", new = "terrafina")
ctb0085_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0085_layer[, terrafina])

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0085_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0085_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0085_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C (g/kg)
# new: carbono
data.table::setnames(ctb0085_layer, old = "C (g/kg)", new = "carbono")
ctb0085_layer[, carbono := as.numeric(carbono)]
ctb0085_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0085_layer[, carbono])

# ctc
# old: Valor T (cmol_c/kg)
# new: ctc
data.table::setnames(ctb0085_layer, old = "Valor T (cmol_c/kg)", new = "ctc")
ctb0085_layer[, ctc := as.numeric(ctc)]
summary(ctb0085_layer[, ctc])
check_empty_layer(ctb0085_layer, "ctc")

# ph
# old: pH em Água
# new: ph
data.table::setnames(ctb0085_layer, old = "pH em Água", new = "ph")
ctb0085_layer[, ph := as.numeric(ph)]
summary(ctb0085_layer[, ph])
check_empty_layer(ctb0085_layer, "ph")

# dsi
# old: Densidade do solo [mg/m3]
# new: dsi
data.table::setnames(ctb0085_layer, old = "Densidade do Solo (g/cm^3)", new = "dsi")
ctb0085_layer[, dsi := as.numeric(dsi)]
summary(ctb0085_layer[, dsi])
check_empty_layer(ctb0085_layer, "dsi")

str(ctb0085_layer)

# Merge ############################################################################################
# events and layers
ctb0085 <- merge(ctb0085_event, ctb0085_layer, all = TRUE)
ctb0085[, dataset_id := "ctb0085"]
# citation
ctb0085 <- merge(ctb0085, ctb0085_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0085)


#Layers: 103
#Events: 27
#Georeferenced events: 21


# Plot using mapview
if (FALSE) {
  ctb0085_sf <- sf::st_as_sf(
    ctb0085[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0085_sf["argila"])
}

# Write to disk ####################################################################################
ctb0085 <- select_output_columns(ctb0085)
data.table::fwrite(ctb0085, "ctb0085/ctb0085.csv")
data.table::fwrite(ctb0085_event, "ctb0085/ctb0085_event.csv")
data.table::fwrite(ctb0085_layer, "ctb0085/ctb0085_layer.csv")
