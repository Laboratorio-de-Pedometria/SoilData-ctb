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
# ctb0066
# Dados de "ASSOCIAÇÕES ENTRE OS SOLOS, OS AMBIENTES SEDIMENTARES QUATERNÁRIOS E AS FITOFISIONOMIAS DE
# PLANÍCIE COSTEIRA E BAIXA ENCOSTA NAS BACIAS DOS RIOS ITAGUARÉ E GUARATUBA (BERTIOGA-SP)"
# 
# 
# https://docs.google.com/spreadsheets/d/1MOb43dAvvpbnwQ2ykbwAvkNt5YZYiX5AZjtvqeSyKrw/edit?usp=sharing


ctb0066_ids <- soildata_catalog("ctb0066")

# validation #####################################################################################

ctb0066_validation <- google_sheet(ctb0066_ids$gs_id, ctb0066_ids$gid_validation)
str(ctb0066_validation)

# Check for negative validation results
sum(ctb0066_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0066_citation <- google_sheet(ctb0066_ids$gs_id, ctb0066_ids$gid_citation)
str(ctb0066_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0066_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0066_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0066_citation <- data.table::data.table(
  dataset_id = "ctb0066",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0066_citation)

# event #####################################################################################
ctb0066_event <- google_sheet(ctb0066_ids$gs_id, ctb0066_ids$gid_event)
str(ctb0066_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0066_event, old = "ID do evento", new = "observacao_id")
ctb0066_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0066_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0066_event, old = "Ano (coleta)", new = "data_ano")
ctb0066_event[, data_ano := as.integer(data_ano)]
ctb0066_event[, .N, by = data_ano]

# ano_fonte
ctb0066_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0066_event[, .N, by = ano_fonte]


# coord_x
# Longitude -> coord_x
#
data.table::setnames(ctb0066_event, old = "Longitude", new = "coord_x")
ctb0066_event[, coord_x := as.numeric(coord_x)]
summary(ctb0066_event[, coord_x])

# coord_y
# Latitude -> coord_y
# 
data.table::setnames(ctb0066_event, old = "Latitude", new = "coord_y")
ctb0066_event[, coord_y := as.numeric(coord_y)]
summary(ctb0066_event[, coord_y])

# Check for duplicate coordinates
ctb0066_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum(coord) -> coord_datum
data.table::setnames(ctb0066_event, old = "Datum (coord)", new = "coord_datum")
#reset the coord_datum variable to NULL as it is coming as a result of a function
ctb0066_event[, coord_datum := NULL]
#we estimate it to be 29193 due to the collection date and location
ctb0066_event[, coord_datum := 29193]



# Cria um objeto 'sf' (simple features) com os dados a serem transformados
ctb0066_event_sf <- sf::st_as_sf(
  ctb0066_event[coord_datum == 29193],
  coords = c("coord_x", "coord_y"),
  crs = 29193 # Informa o sistema de coordenadas de origem
)

# Transforma as coordenadas para WGS84 (padrão GPS, EPSG: 4326)
ctb0066_event_sf <- sf::st_transform(ctb0066_event_sf, 4326)

# Extrai as novas coordenadas (Longitude e Latitude) do objeto 'sf'
new_coords <- sf::st_coordinates(ctb0066_event_sf)

# Atualiza o data.table original com as novas coordenadas e o novo datum
ctb0066_event[coord_datum == 29193, coord_x := new_coords[, 1]] # Longitude
ctb0066_event[coord_datum == 29193, coord_y := new_coords[, 2]] # Latitude
ctb0066_event[coord_datum == 29193, coord_datum := 4326]       # Novo datum: WGS84



# Precisão (coord) -> coord_precisao
# We set it to NA_real_
ctb0066_event[, coord_precisao := NA_real_]
summary(ctb0066_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0066_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0066_event[, coord_fonte := as.character(coord_fonte)]
summary(ctb0066_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0066_event, old = "País", new = "pais_id")
ctb0066_event[, pais_id := "BR"]


# Estado (UF) -> estado_id
data.table::setnames(ctb0066_event, old = "Estado (UF)", new = "estado_id")
ctb0066_event[, estado_id := as.character(estado_id)]
ctb0066_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0066_event, old = "Município", new = "municipio_id")
ctb0066_event[, municipio_id := as.character(municipio_id)]
ctb0066_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# is missing on main document
ctb0066_event[, amostra_area := NA_real_]
summary(ctb0066_event[, amostra_area])

# taxon_sibcs
# missing taxon bso, we set N/A.
ctb0066_event[, taxon_sibcs := NA_character_]

# taxon_st_1999
# missing taxon_st_1999 so, we set N/A.
ctb0066_event[, taxon_st := NA_character_]

# Pedregosidade (superficie) 
# missing pedregosidade so, we set N/A;

ctb0066_event[, pedregosidade := NA_character_]


# Rochosidade (superficie)
# missing rochosidade so, we set N/A;

ctb0066_event[, rochosidade := NA_character_]




str(ctb0066_event)



# layers ###########################################################################################
ctb0066_layer <- google_sheet(ctb0066_ids$gs_id, ctb0066_ids$gid_layer)
str(ctb0066_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0066_layer, old = "ID do evento", new = "observacao_id")
ctb0066_layer[, observacao_id := as.character(observacao_id)]
ctb0066_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0066_layer, old = "ID da camada", new = "camada_nome")
ctb0066_layer[, camada_nome := as.character(camada_nome)]
ctb0066_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0066_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0066_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0066_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0066_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0066_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0066_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0066_layer[, profund_inf])


# areia
# old: "Areia total (%)"
# new: areia
# 
data.table::setnames(ctb0066_layer, old = "Areia total (%)", new = "areia")
ctb0066_layer[, areia := ((as.numeric(areia)/100)*1000)]
ctb0066_layer[is.na(areia), .(observacao_id, camada_nome, profund_sup, profund_inf, areia)]


# silte
# old: Silte (%)
# new: silte
# 
data.table::setnames(ctb0066_layer, old = "Silte (%)", new = "silte")
ctb0066_layer[, silte := ((as.numeric(silte)/100)*1000)]
ctb0066_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila (%)
# new: argila
#
data.table::setnames(ctb0066_layer, old = "Argila (%)", new = "argila")
ctb0066_layer[, argila := ((as.numeric(argila)/100)*100)]
ctb0066_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# is missing...
ctb0066_layer[, terrafina := NA_real_]



# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0066_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0066_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0066_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C (%)
# new: carbono
data.table::setnames(ctb0066_layer, old = "C (%)", new = "carbono")
#divide by 100 to remove the % and then multiply by 1000 to enter as g/kg
ctb0066_layer[, carbono := ((as.numeric(carbono)/100)*1000)] 

summary(ctb0066_layer[, carbono])
check_empty_layer(ctb0066_layer, "carbono")

# ctc
# old: T (mmolc/dm^3)
# new: ctc
data.table::setnames(ctb0066_layer, old = "T (mmolc/dm^3)", new = "ctc")
ctb0066_layer[, ctc := as.numeric(ctc)]
summary(ctb0066_layer[, ctc])
check_empty_layer(ctb0066_layer, "ctc")

# ph
# ph is missing on the document.
ctb0066_layer[, ph := NA_real_]


# dsi
# dsi is missing on the main document.
ctb0066_layer[, dsi := NA_real_]


str(ctb0066_layer)

# Merge ############################################################################################
# events and layers
ctb0066 <- merge(ctb0066_event, ctb0066_layer, all = TRUE)
ctb0066[, dataset_id := "ctb0066"]
# citation
ctb0066 <- merge(ctb0066, ctb0066_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0066)

#Layers: 199
#Events: 31
#Georeferenced events: 31


# Plot using mapview
if (FALSE) {
  ctb0066_sf <- sf::st_as_sf(
    ctb0066[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0066_sf["argila"])
}

# Write to disk ####################################################################################
ctb0066 <- select_output_columns(ctb0066)
data.table::fwrite(ctb0066, "ctb0066/ctb0066.csv")
data.table::fwrite(ctb0066_event, "ctb0066/ctb0066_event.csv")
data.table::fwrite(ctb0066_layer, "ctb0066/ctb0066_layer.csv")
