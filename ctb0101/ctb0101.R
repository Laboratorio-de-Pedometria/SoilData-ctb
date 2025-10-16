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
# ctb0101
# Dados de "Sistema pedológico Planossolo-Plintossolo no Pantanal de Barão de Melgaço-MT"
# 
# https://docs.google.com/spreadsheets/d/1WsAluqYkfOBMRr-fajh1UOwaKbYhLusUtTzrn0oSlZs/edit?usp=sharing


ctb0101_ids <- soildata_catalog("ctb0101")

# validation #####################################################################################

ctb0101_validation <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_validation)
str(ctb0101_validation)

# Check for negative validation results
sum(ctb0101_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0101_citation <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_citation)
str(ctb0101_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0101_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0101_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0101_citation <- data.table::data.table(
  dataset_id = "ctb0101",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0101_citation)

# event #####################################################################################
ctb0101_event <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_event)
str(ctb0101_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0101_event, old = "ID do evento", new = "observacao_id")
ctb0101_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0101_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0101_event, old = "Ano (coleta)", new = "data_ano")
ctb0101_event[, data_ano := as.integer(data_ano)]
ctb0101_event[, .N, by = data_ano]

# ano_fonte
ctb0101_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0101_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0101_event, old = "Longitude", new = "coord_x")
ctb0101_event[, coord_x := as.numeric(coord_x)]
summary(ctb0101_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0101_event, old = "Latitude", new = "coord_y")
ctb0101_event[, coord_y := as.numeric(coord_y)]
summary(ctb0101_event[, coord_y])

# Check for duplicate coordinates
ctb0101_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# UTM SAD 69 Zona 21s : EPSG == 29191
data.table::setnames(ctb0101_event, old = "Datum (coord)", new = "coord_datum")
ctb0101_event[, coord_datum := NULL]
ctb0101_event[, coord_datum := 29191]


# Cria um objeto 'sf' (simple features) com os dados a serem transformados
ctb0101_event_sf <- sf::st_as_sf(
  ctb0101_event[coord_datum == 29191],
  coords = c("coord_x", "coord_y"),
  crs = 29191 # Informa o sistema de coordenadas de origem
)

# Transforma as coordenadas para WGS84 (padrão GPS, EPSG: 4326)
ctb0101_event_sf <- sf::st_transform(ctb0101_event_sf, 4326)

# Extrai as novas coordenadas (Longitude e Latitude) do objeto 'sf'
new_coords <- sf::st_coordinates(ctb0101_event_sf)

# Atualiza o data.table original com as novas coordenadas e o novo datum
ctb0101_event[coord_datum == 29191, coord_x := new_coords[, 1]] # Longitude
ctb0101_event[coord_datum == 29191, coord_y := new_coords[, 2]] # Latitude
ctb0101_event[coord_datum == 29191, coord_datum := 4326]       # Novo datum: WGS84



# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0101_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0101_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0101_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0101_event[, coord_fonte := as.character(coord_fonte)]


# País -> pais_id
data.table::setnames(ctb0101_event, old = "País", new = "pais_id")
ctb0101_event[, pais_id := "BR"]


# Estado -> estado_id
data.table::setnames(ctb0101_event, old = "Estado", new = "estado_id")
ctb0101_event[, estado_id := as.character(estado_id)]
ctb0101_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0101_event, old = "Município", new = "municipio_id")
ctb0101_event[, municipio_id := as.character(municipio_id)]
ctb0101_event[, .N, by = municipio_id]

# Área do evento [m2] -> amostra_area
#
data.table::setnames(ctb0101_event, old = "Área do evento [m2]", new = "amostra_area")
ctb0101_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0101_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0101_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0101_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0101_event[, .N, by = taxon_sibcs]

# taxon_st 
# missing this soil taxonomy on document
ctb0101_event[, taxon_st := NA_character_]
ctb0101_event[, .N, by = taxon_st]


### no rockiness and stoniness data in this work #########
# Pedregosidade (superficie) 

ctb0101_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)

ctb0101_event[, rochosidade := NA_character_]

###########################################################

str(ctb0101_event)

# layers ###########################################################################################
ctb0101_layer <- google_sheet(ctb0101_ids$gs_id, ctb0101_ids$gid_layer)
str(ctb0101_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0101_layer, old = "ID do evento", new = "observacao_id")
ctb0101_layer[, observacao_id := as.character(observacao_id)]
ctb0101_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0101_layer, old = "ID da camada", new = "camada_nome")
ctb0101_layer[, camada_nome := as.character(camada_nome)]
ctb0101_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0101_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0101_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0101_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0101_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0101_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0101_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0101_layer[, profund_inf])

#This work is separated from Very Coarse Sand, Coarse Sand, Medium Sand, Fine Sand, Very Fine Sand

#areia grossa
# old: Areia Muito Grossa [%]
# new: areia_grossa
data.table::setnames(ctb0101_layer, old = "Areia Muito Grossa [%]", new = "areia_muito_grossa")
ctb0101_layer[, areia_muito_grossa := as.numeric(areia_muito_grossa)*10]
summary(ctb0101_layer[, areia_muito_grossa])

#areia grossa
# old: Areia Grossa [%]
# new: areia_grossa
data.table::setnames(ctb0101_layer, old = "Areia Grossa [%]", new = "areia_grossa")
ctb0101_layer[, areia_grossa := as.numeric(areia_grossa)*10]
summary(ctb0101_layer[, areia_grossa])

#areia media
# old: Areia Média [%]
# new: areia_media
data.table::setnames(ctb0101_layer, old = "Areia Média [%]", new = "areia_media")
ctb0101_layer[, areia_media := as.numeric(areia_media)*10]
summary(ctb0101_layer[, areia_media])


#areia fina
# old: Areia Fina [%]
# new: areia_fina
data.table::setnames(ctb0101_layer, old = "Areia Fina [%]", new = "areia_fina")
ctb0101_layer[, areia_fina := as.numeric(areia_fina)*10]
summary(ctb0101_layer[, areia_fina])

#areia muito fina
# old: Areia Muito Fina [%]
# new: areia_muito_fina
data.table::setnames(ctb0101_layer, old = "Areia Muito Fina [%]", new = "areia_muito_fina")
ctb0101_layer[, areia_muito_fina := as.numeric(areia_muito_fina)*10]
summary(ctb0101_layer[, areia_muito_fina])

#areia 
ctb0101_layer[, areia := areia_muito_grossa + areia_grossa + areia_media + areia_fina + areia_muito_fina]
summary(ctb0101_layer[, areia])


#silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0101_layer, old = "Silte [%]", new = "silte")
ctb0101_layer[, silte := as.numeric(silte)*10]
summary(ctb0101_layer[, silte])


#argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0101_layer, old = "Argila [%]", new = "argila")
ctb0101_layer[, argila := as.numeric(argila)*10]
summary(ctb0101_layer[, argila])


#terrafina
# is missing in this document.
ctb0101_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0101_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0101_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0101_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: Corg [g/kg]
# new: carbono
# multiply by 10 to convert to g/kg
data.table::setnames(ctb0101_layer, old = "Corg [g/kg]", new = "carbono")
ctb0101_layer[, carbono := as.numeric(carbono)]
ctb0101_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0101_layer[, carbono])

# ctc
# old: T [mmolc/kg]
# new: ctc
data.table::setnames(ctb0101_layer, old = "T [mmolc/kg]", new = "ctc")
ctb0101_layer[, ctc := as.numeric(ctc)]
summary(ctb0101_layer[, ctc])
check_empty_layer(ctb0101_layer, "ctc")

# ph
# old: pH H_2O
# new: ph
data.table::setnames(ctb0101_layer, old = "pH H_2O", new = "ph")
ctb0101_layer[, ph := as.numeric(ph)]
summary(ctb0101_layer[, ph])
check_empty_layer(ctb0101_layer, "ph")

# dsi 
# no dsi information in this work
ctb0101_layer[, dsi := NA_real_]
summary(ctb0101_layer[, dsi])



str(ctb0101_layer)

# Merge ############################################################################################
# events and layers
ctb0101 <- merge(ctb0101_event, ctb0101_layer, all = TRUE)
ctb0101[, dataset_id := "ctb0101"]
# citation
ctb0101 <- merge(ctb0101, ctb0101_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0101)

#Layers: 68
#Events: 8
#Georeferenced events: 8


# Plot using mapview
if (FALSE) {
  ctb0101_sf <- sf::st_as_sf(
    ctb0101[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0101_sf["argila"])
}

# Write to disk ####################################################################################
ctb0101 <- select_output_columns(ctb0101)
data.table::fwrite(ctb0101, "ctb0101/ctb0101.csv")
data.table::fwrite(ctb0101_event, "ctb0101/ctb0101_event.csv")
data.table::fwrite(ctb0101_layer, "ctb0101/ctb0101_layer.csv")
