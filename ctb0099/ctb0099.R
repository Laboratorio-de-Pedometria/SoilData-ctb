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
# ctb0099
# Dados de "Soils surrounding saline-alkaline lakes of Nhecolândia, Pantanal, Brazil: 
# Toposequences, mineralogy and chemistry"
# 
# 
# 
# https://docs.google.com/spreadsheets/d/1k7HG1_hsAG1Wmaek-p8uGoCekmwrs8nXSiKLw2XKnaI/edit?usp=sharing


ctb0099_ids <- soildata_catalog("ctb0099")

# validation #####################################################################################

ctb0099_validation <- google_sheet(ctb0099_ids$gs_id, ctb0099_ids$gid_validation)
str(ctb0099_validation)

# Check for negative validation results
sum(ctb0099_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0099_citation <- google_sheet(ctb0099_ids$gs_id, ctb0099_ids$gid_citation)
str(ctb0099_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0099_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0099_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0099_citation <- data.table::data.table(
  dataset_id = "ctb0099",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0099_citation)

# event #####################################################################################
ctb0099_event <- google_sheet(ctb0099_ids$gs_id, ctb0099_ids$gid_event)
str(ctb0099_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0099_event, old = "ID do evento", new = "observacao_id")
ctb0099_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0099_event[, observacao_id]) > 1)

# data_ano
# Ano [coleta] -> data_coleta_ano
data.table::setnames(ctb0099_event, old = "Ano (coleta)", new = "data_ano")
ctb0099_event[, data_ano := as.integer(data_ano)]
ctb0099_event[, .N, by = data_ano]

# ano_fonte
ctb0099_event[, ano_fonte := "Original"]
ctb0099_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0099_event, old = "Longitude", new = "coord_x")
ctb0099_event[, coord_x := as.character(coord_x)]
summary(ctb0099_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0099_event, old = "Latitude", new = "coord_y")
ctb0099_event[, coord_y := as.character(coord_y)]
summary(ctb0099_event[, coord_y])


# Datum (coord) -> coord_datum
# SAD69 zona 21s
data.table::setnames(ctb0099_event, old = "Datum (coord)", new = "coord_datum")
ctb0099_event[, coord_datum := NULL]
ctb0099_event[, coord_datum := 29191]


# Cria um objeto 'sf' (simple features) com os dados a serem transformados
ctb0099_event_sf <- sf::st_as_sf(
  ctb0099_event[coord_datum == 29191],
  coords = c("coord_x", "coord_y"),
  crs = 29191 # Informa o sistema de coordenadas de origem
)

# Transforma as coordenadas para WGS84 (padrão GPS, EPSG: 4326)
ctb0099_event_sf <- sf::st_transform(ctb0099_event_sf, 4326)

# Extrai as novas coordenadas (Longitude e Latitude) do objeto 'sf'
new_coords <- sf::st_coordinates(ctb0099_event_sf)

# Atualiza o data.table original com as novas coordenadas e o novo datum
ctb0099_event[coord_datum == 29191, coord_x := new_coords[, 1]] # Longitude
ctb0099_event[coord_datum == 29191, coord_y := new_coords[, 2]] # Latitude
ctb0099_event[coord_datum == 29191, coord_datum := 4326]       # Novo datum: WGS84


#No Accuracy and Source Information
# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0099_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0099_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0099_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0099_event[, coord_fonte := NA_real_]
summary(ctb0099_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0099_event, old = "País", new = "pais_id")
ctb0099_event[, pais_id := "BR"]


# Estado (UF) -> estado_id
data.table::setnames(ctb0099_event, old = "Estado (UF)", new = "estado_id")
ctb0099_event[, estado_id := as.character(estado_id)]
ctb0099_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0099_event, old = "Município", new = "municipio_id")
ctb0099_event[, municipio_id := as.character(municipio_id)]
ctb0099_event[, .N, by = municipio_id]

# Área do evento [m] -> amostra_area
#
data.table::setnames(ctb0099_event, old = "Área do evento [m]", new = "amostra_area")
ctb0099_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0099_event[, amostra_area])

#taxon_sibcs
ctb0099_event[, taxon_sibcs := NA_character_]


# taxon_st 
# missing edition of this soil taxonomy
ctb0099_event[, taxon_st := NA_character_]


# Pedregosidade (superficie) 
#no stoneness data in this work
ctb0099_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
#no rockiness data in this work

ctb0099_event[, rochosidade := NA_character_]



str(ctb0099_event)

# layers ###########################################################################################
ctb0099_layer <- google_sheet(ctb0099_ids$gs_id, ctb0099_ids$gid_layer)
str(ctb0099_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0099_layer, old = "ID do evento", new = "observacao_id")
ctb0099_layer[, observacao_id := as.character(observacao_id)]
ctb0099_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0099_layer, old = "ID da camada", new = "camada_nome")
ctb0099_layer[, camada_nome := as.character(camada_nome)]
ctb0099_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0099_layer, old = "ID da amostra", new = "amostra_id")
ctb0099_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0099_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0099_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0099_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0099_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0099_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0099_layer[, profund_inf])

#areia fina
# old: Areia fina [%]
# new: areia_fina
data.table::setnames(ctb0099_layer, old = "Areia fina [%]", new = "areia_fina")
ctb0099_layer[, areia_fina := as.numeric(areia_fina)*10]
summary(ctb0099_layer[, areia_fina])


#areia grossa
# old: Areia grossa [%]
# new: areia_grossa
data.table::setnames(ctb0099_layer, old = "Areia grossa [%]", new = "areia_grossa")
ctb0099_layer[, areia_grossa := as.numeric(areia_grossa)*10]
summary(ctb0099_layer[, areia_grossa])

ctb0099_layer[, areia := areia_fina + areia_grossa]

#silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0099_layer, old = "Silte [%]", new = "silte")
ctb0099_layer[, silte := as.numeric(silte)*10]
summary(ctb0099_layer[, silte])

#argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0099_layer, old = "Argila [%]", new = "argila")
ctb0099_layer[, argila := as.numeric(argila)*10]
summary(ctb0099_layer[, argila])


#terrafina
# is missing in this document.
ctb0099_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0099_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0099_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0099_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# no carbono information in this document.
ctb0099_layer[, carbono := NA_real_]


# ctc
# no ctc information in this document.
ctb0099_layer[, ctc := NA_real_]


# ph
# old: pH em H_2O
# new: ph
data.table::setnames(ctb0099_layer, old = "pH em H_2O", new = "ph")
ctb0099_layer[, ph := as.numeric(ph)]
summary(ctb0099_layer[, ph])
check_empty_layer(ctb0099_layer, "ph")

# dsi
# dsi is missing in this document.
ctb0099_layer[, dsi := NA_real_]


str(ctb0099_layer)

# Merge ############################################################################################
# events and layers
ctb0099 <- merge(ctb0099_event, ctb0099_layer, all = TRUE)
ctb0099[, dataset_id := "ctb0099"]
# citation
ctb0099 <- merge(ctb0099, ctb0099_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0099)


#Layers: 344
#Events: 26
#Georeferenced events: 26


# Plot using mapview
if (FALSE) {
  ctb0099_sf <- sf::st_as_sf(
    ctb0099[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0099_sf["argila"])
}

# Write to disk ####################################################################################
ctb0099 <- select_output_columns(ctb0099)
data.table::fwrite(ctb0099, "ctb0099/ctb0099.csv")
data.table::fwrite(ctb0099_event, "ctb0099/ctb0099_event.csv")
data.table::fwrite(ctb0099_layer, "ctb0099/ctb0099_layer.csv")
