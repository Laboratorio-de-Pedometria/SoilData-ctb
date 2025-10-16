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
# ctb0064
# Dados de "Caracterização, Gênese, Classificação e Aptidão Agrícola de
# uma Sequência de Solos do Terciário na Região de Campos, RJ"
# 
# https://docs.google.com/spreadsheets/d/1WIrft56We5JloJFaQeWrGCx-LrfodyYpU_7AvCvoRDU/edit?usp=sharing


ctb0064_ids <- soildata_catalog("ctb0064")

# validation #####################################################################################

ctb0064_validation <- google_sheet(ctb0064_ids$gs_id, ctb0064_ids$gid_validation)
str(ctb0064_validation)

# Check for negative validation results
sum(ctb0064_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0064_citation <- google_sheet(ctb0064_ids$gs_id, ctb0064_ids$gid_citation)
str(ctb0064_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0064_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0064_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0064_citation <- data.table::data.table(
  dataset_id = "ctb0064",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0064_citation)

# event #####################################################################################
ctb0064_event <- google_sheet(ctb0064_ids$gs_id, ctb0064_ids$gid_event)
str(ctb0064_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0064_event, old = "ID do evento", new = "observacao_id")
ctb0064_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0064_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0064_event, old = "Ano (coleta)", new = "data_ano")
ctb0064_event[, data_ano := as.integer(data_ano)]
ctb0064_event[, .N, by = data_ano]

# ano_fonte
ctb0064_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0064_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0064_event, old = "Longitude", new = "coord_x")
ctb0064_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0064_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0064_event, old = "Latitude", new = "coord_y")
ctb0064_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0064_event[, coord_y])

# Datum (coord) -> coord_datum
# Córrego Alegre
data.table::setnames(ctb0064_event, old = "Datum (coord)", new = "coord_datum")
ctb0064_event[, coord_datum := NULL]
ctb0064_event[, coord_datum := 4618]
ctb0064_event[, coord_datum := as.integer(coord_datum)]

#  Converte o data.table para um objeto espacial (sf)
# Informamos que o sistema de coordenadas (CRS) original é 4618
ctb0064_event_sf <- sf::st_as_sf(
  ctb0064_event,
  coords = c("coord_x", "coord_y"),
  crs = 4618
)

#  Transforma as coordenadas para WGS84 (EPSG: 4326)
ctb0064_event_sf_wgs84 <- sf::st_transform(ctb0064_event_sf, 4618)

#  Extrai as novas coordenadas (já em WGS84) do objeto sf
new_coords <- sf::st_coordinates(ctb0064_event_sf_wgs84)

#  Atualiza a tabela original com as coordenadas convertidas e o novo datum
ctb0064_event[, coord_x := new_coords[, 1]] # Novas longitudes
ctb0064_event[, coord_y := new_coords[, 2]] # Novas latitudes
ctb0064_event[, coord_datum := 4326]      # Novo datum: WGS84

summary(ctb0064_event[, .(coord_datum, coord_x, coord_y)])


# Precisão (coord) -> coord_precisao

data.table::setnames(ctb0064_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0064_event[, coord_precisao := as.character(coord_precisao)]
summary(ctb0064_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0064_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0064_event[, coord_fonte := as.character(coord_precisao)]
summary(ctb0064_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0064_event, old = "País", new = "pais_id")
ctb0064_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0064_event, old = "Estado (UF)", new = "estado_id")
ctb0064_event[, estado_id := as.character(estado_id)]
ctb0064_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0064_event, old = "Município", new = "municipio_id")
ctb0064_event[, municipio_id := as.character(municipio_id)]
ctb0064_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0064_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0064_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0064_event[, amostra_area])

# SiBCS (em elaboração) -> taxon_sibcs
data.table::setnames(ctb0064_event, old = "SiBCS (em elaboração)", new = "taxon_sibcs")
ctb0064_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0064_event[, .N, by = taxon_sibcs]

# SoilTaxonomy (1975) -> taxon_st
data.table::setnames(ctb0064_event, old = "SoilTaxonomy (1975)", new = "taxon_st")
ctb0064_event[, taxon_st := as.character(taxon_st)]
ctb0064_event[, .N, by = taxon_st]


#no information on stoniness and rockiness in this work###############
# Pedregosidade (superficie) 

ctb0064_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)

ctb0064_event[, rochosidade := NA_character_]

######################################################################


str(ctb0064_event)



# layers ###########################################################################################
ctb0064_layer <- google_sheet(ctb0064_ids$gs_id, ctb0064_ids$gid_layer)
str(ctb0064_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0064_layer, old = "ID do evento", new = "observacao_id")
ctb0064_layer[, observacao_id := as.character(observacao_id)]
ctb0064_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0064_layer, old = "ID da camada", new = "camada_nome")
ctb0064_layer[, camada_nome := as.character(camada_nome)]
ctb0064_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0064_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0064_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0064_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0064_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0064_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0064_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0064_layer[, profund_inf])

# areia_grossa
# old: "Areia grossa  [%]"
# new: areia_grossa
data.table::setnames(ctb0064_layer, old = "Areia grossa  [%]", new = "areia_grossa")
ctb0064_layer[, areia_grossa := as.numeric(areia_grossa)*10]
ctb0064_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: "Areia fina [%]"
# new: areia_fina
data.table::setnames(ctb0064_layer, old = "Areia fina [%]", new = "areia_fina")
ctb0064_layer[, areia_fina := as.numeric(areia_fina)*10]
ctb0064_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

# areia
# criação da coluna areia 
ctb0064_layer[, areia:= areia_grossa+areia_fina]

# silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0064_layer, old = "Silte [%]", new = "silte")
ctb0064_layer[, silte := as.numeric(silte)*10]
ctb0064_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila < 0,002 mm [%]
# new: argila
data.table::setnames(ctb0064_layer, old = "Argila <0,002 mm [%]", new = "argila")
ctb0064_layer[, argila := as.numeric(argila)*10]
ctb0064_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# old: Terra fina <2 mm [%]
# new: terrafina
data.table::setnames(ctb0064_layer, old = "Terra fina <2 mm [%]", new = "terrafina")
ctb0064_layer[, terrafina := as.numeric(terrafina)*10]
ctb0064_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0064_layer[, psd := round(argila + silte + areia_fina + areia_grossa)]
psd_lims <- 900:1100
# Check the limits
ctb0064_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0064_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C orgânico [%]
# new: carbono
data.table::setnames(ctb0064_layer, old = "C orgânico [%]", new = "carbono")
ctb0064_layer[, carbono := as.numeric(carbono)*10]
summary(ctb0064_layer[, carbono])
check_empty_layer(ctb0064_layer, "carbono")

# ctc
# old: Valor T [meq/100g]
# new: ctc
data.table::setnames(ctb0064_layer, old = "Valor T [meq/100g]", new = "ctc")
ctb0064_layer[, ctc := as.numeric(ctc)]
summary(ctb0064_layer[, ctc])
check_empty_layer(ctb0064_layer, "ctc")

# ph
# old: pH em água
# new: ph
data.table::setnames(ctb0064_layer, old = "pH em água", new = "ph")
ctb0064_layer[, ph := as.numeric(ph)]
summary(ctb0064_layer[, ph])
check_empty_layer(ctb0064_layer, "ph")

# dsi
# old: Densidade aparente [g/cm^3]
# new: dsi
data.table::setnames(ctb0064_layer, old = "Densidade aparente [g/cm^3]", new = "dsi")
ctb0064_layer[, dsi := as.numeric(dsi)]
summary(ctb0064_layer[, dsi])

str(ctb0064_layer)

# Merge ############################################################################################
# events and layers
ctb0064 <- merge(ctb0064_event, ctb0064_layer, all = TRUE)
ctb0064[, dataset_id := "ctb0064"]
# citation
ctb0064 <- merge(ctb0064, ctb0064_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0064)

#Layers: 39
#Events: 5
#Georeferenced events: 5


# Plot using mapview
if (FALSE) {
  ctb0064_sf <- sf::st_as_sf(
    ctb0064[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0064_sf["argila"])
}

# Write to disk ####################################################################################
ctb0064 <- select_output_columns(ctb0064)
data.table::fwrite(ctb0064, "ctb0064/ctb0064.csv")
data.table::fwrite(ctb0064_event, "ctb0064/ctb0064_event.csv")
data.table::fwrite(ctb0064_layer, "ctb0064/ctb0064_layer.csv")
