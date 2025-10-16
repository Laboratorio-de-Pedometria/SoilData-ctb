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
# https://docs.google.com/spreadsheets/d/1o0sFLwLcJtGhfhdxAI1z3AJTBuAjptSGBYj51NgIeV0/edit?usp=sharing


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
# Define o datum de origem como SIRGAS 2000 (EPSG: 4674)
# O "L" garante que o número seja tratado como um inteiro (integer).
data.table::setnames(ctb0069_event, old = "Datum (coord)", new = "coord_datum")
ctb0069_event[, coord_datum := NULL]
ctb0069_event[, coord_datum := "N/A"]
ctb0069_event[coord_datum == "N/A", coord_datum := 4674L]
ctb0069_event[, coord_datum := as.integer(coord_datum)]

#  Converte o data.table para um objeto espacial (sf)
# Informamos que o sistema de coordenadas (CRS) original é 4674
ctb0069_event_sf <- sf::st_as_sf(
  ctb0069_event,
  coords = c("coord_x", "coord_y"),
  crs = 4674 # Define o CRS de origem como SIRGAS 2000
)

#  Transforma as coordenadas para WGS84 (EPSG: 4326)
ctb0069_event_sf_wgs84 <- sf::st_transform(ctb0069_event_sf, 4326)

#  Extrai as novas coordenadas (já em WGS84) do objeto sf
new_coords <- sf::st_coordinates(ctb0069_event_sf_wgs84)

#  Atualiza a tabela original com as coordenadas convertidas e o novo datum
ctb0069_event[, coord_x := new_coords[, 1]] # Novas longitudes
ctb0069_event[, coord_y := new_coords[, 2]] # Novas latitudes
ctb0069_event[, coord_datum := 4326L]      # Novo datum: WGS84

summary(ctb0069_event[, .(coord_datum, coord_x, coord_y)])


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



#using the 'recode' function because in the original document the states are outside of UF
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
ctb0069_event[, amostra_area := NA_real_]
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



# layers ###########################################################################################
ctb0069_layer <- google_sheet(ctb0069_ids$gs_id, ctb0069_ids$gid_layer)
str(ctb0069_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0069_layer, old = "ID do evento", new = "observacao_id")
ctb0069_layer[, observacao_id := as.character(observacao_id)]
ctb0069_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0069_layer, old = "ID da camada", new = "camada_nome")
ctb0069_layer[, camada_nome := as.character(camada_nome)]
ctb0069_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0069_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0069_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0069_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0069_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0069_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0069_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0069_layer[, profund_inf])

# areia_grossa
# old: "Areia grossa 2 - 0,2 mm (g/kg)"
# new: areia_grossa
# areia_grossa is missing for some layers...
data.table::setnames(ctb0069_layer, old = "Areia grossa 2 - 0,2 mm (g/kg)", new = "areia_grossa")
ctb0069_layer[, areia_grossa := as.numeric(areia_grossa)]
ctb0069_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: "Areia fina 0,2 - 0,05 mm (g/kg)"
# new: areia_fina
# areia_fina is missing for some layers...
data.table::setnames(ctb0069_layer, old = "Areia fina 0,2 - 0,05 mm (g/kg)", new = "areia_fina")
ctb0069_layer[, areia_fina := as.numeric(areia_fina)]
ctb0069_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

# areia
# criação da coluna areia 
ctb0069_layer[, areia:= areia_grossa+areia_fina]

# silte
# old: Silte 0,05 - 0,002 mm (g/kg)
# new: silte
# silte is missing for some layers...
data.table::setnames(ctb0069_layer, old = "Silte 0,05 - 0,002 mm (g/kg)", new = "silte")
ctb0069_layer[, silte := as.numeric(silte)]
ctb0069_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila > 0,002 mm (g/kg)
# new: argila
# argila is missing for some layers...
data.table::setnames(ctb0069_layer, old = "Argila > 0,002 mm (g/kg)", new = "argila")
ctb0069_layer[, argila := as.numeric(argila)]
ctb0069_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# old: T F S A (g/kg)
# new: terrafina
data.table::setnames(ctb0069_layer, old = "T F S A (g/kg)", new = "terrafina")
ctb0069_layer[, terrafina := as.numeric(terrafina)]
ctb0069_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0069_layer[, psd := round(argila + silte + areia_fina + areia_grossa)]
psd_lims <- 900:1100
# Check the limits
ctb0069_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0069_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C (g/kg)
# new: carbono
data.table::setnames(ctb0069_layer, old = "C (g/kg)", new = "carbono")
ctb0069_layer[, carbono := as.numeric(carbono)]
summary(ctb0069_layer[, carbono])
check_empty_layer(ctb0069_layer, "carbono")

# ctc
# old: Valor T (soma) (cmolc/kg)
# new: ctc
data.table::setnames(ctb0069_layer, old = "Valor T (soma) (cmolc/kg)", new = "ctc")
ctb0069_layer[, ctc := as.numeric(ctc)]
summary(ctb0069_layer[, ctc])
check_empty_layer(ctb0069_layer, "ctc")

# ph
# old: pH Água
# new: ph
data.table::setnames(ctb0069_layer, old = "pH Água", new = "ph")
ctb0069_layer[, ph := as.numeric(ph)]
summary(ctb0069_layer[, ph])
check_empty_layer(ctb0069_layer, "ph")

# dsi
# dsi is missing in this document 
ctb0069_layer[, dsi := NA_real_]

str(ctb0069_layer)

# Merge ############################################################################################
# events and layers
ctb0069 <- merge(ctb0069_event, ctb0069_layer, all = TRUE)
ctb0069[, dataset_id := "ctb0069"]
# citation
ctb0069 <- merge(ctb0069, ctb0069_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0069)

#Layers: 56
#Events: 15
#Georeferenced events: 15

# Plot using mapview
if (FALSE) {
  ctb0069_sf <- sf::st_as_sf(
    ctb0069[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0069_sf["argila"])
}

# Write to disk ####################################################################################
ctb0069 <- select_output_columns(ctb0069)
data.table::fwrite(ctb0069, "ctb0069/ctb0069.csv")
data.table::fwrite(ctb0069_event, "ctb0069/ctb0069_event.csv")
data.table::fwrite(ctb0069_layer, "ctb0069/ctb0069_layer.csv")
