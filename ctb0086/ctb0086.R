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


# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0086
# Dados de "Solos e avaliação do potencial agrossilvipastoril das microrregiões Paracatu e Unaí - Minas Gerais"
# 
# 
# https://docs.google.com/spreadsheets/d/1j_qaBIcvWwYd5feIcqwG2dOJI7KGln7ia_3hc95ha4Y/edit?usp=sharing


ctb0086_ids <- soildata_catalog("ctb0086")

# validation #####################################################################################

ctb0086_validation <- google_sheet(ctb0086_ids$gs_id, ctb0086_ids$gid_validation)
str(ctb0086_validation)

# Check for negative validation results
sum(ctb0086_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0086_citation <- google_sheet(ctb0086_ids$gs_id, ctb0086_ids$gid_citation)
str(ctb0086_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0086_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0086_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0086_citation <- data.table::data.table(
  dataset_id = "ctb0086",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0086_citation)

# event #####################################################################################
ctb0086_event <- google_sheet(ctb0086_ids$gs_id, ctb0086_ids$gid_event)
str(ctb0086_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0086_event, old = "ID do evento", new = "observacao_id")
ctb0086_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0086_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
# Ano coleta is missing in this docuument so, we set N/A.
data.table::setnames(ctb0086_event, old = "Ano (coleta)", new = "data_ano")
ctb0086_event[, data_ano := NA_real_]


# ano_fonte
ctb0086_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0086_event[, .N, by = ano_fonte]




#prepare the data to be merged into just one variable
data.table::setnames(ctb0086_event, old = "Lat (grau)", new = "lat_graus")
data.table::setnames(ctb0086_event, old = "Lat (min)", new = "lat_minutos")
data.table::setnames(ctb0086_event, old = "Lat (seg)", new = "lat_segundos")
data.table::setnames(ctb0086_event, old = "Long (graus)", new = "lon_graus")
data.table::setnames(ctb0086_event, old = "Long (min)", new = "lon_minutos")
data.table::setnames(ctb0086_event, old = "Long (seg)", new = "lon_segundos")

# coord_y
ctb0086_event[, coord_y := paste(lon_graus, lon_minutos, lon_segundos, "W")]
ctb0086_event[, coord_y := parzer::parse_lon(coord_y)]
summary(ctb0086_event[, coord_y])

# coord_x
ctb0086_event[, coord_x := paste(lat_graus, lat_minutos, lat_segundos, "S")]
ctb0086_event[, coord_x := parzer::parse_lat(coord_x)]
summary(ctb0086_event[, coord_x])


# Datum (coord)
# is missing so we estimate is SAD69
data.table::setnames(ctb0086_event, old = "Datum (coord)", new = "coord_datum")
ctb0086_event[, coord_datum := NULL]
ctb0086_event[, coord_datum := 4618]

#  Converte o data.table para um objeto espacial (sf)
# Informamos que o sistema de coordenadas (CRS) original é 4618
ctb0086_event_sf <- sf::st_as_sf(
  ctb0086_event,
  coords = c("coord_x", "coord_y"),
  crs = 4618
)

#  Transforma as coordenadas para WGS84 (EPSG: 4326)
ctb0086_event_sf_wgs84 <- sf::st_transform(ctb0086_event_sf, 4326)

#  Extrai as novas coordenadas (já em WGS84) do objeto sf
new_coords <- sf::st_coordinates(ctb0086_event_sf_wgs84)

#  Atualiza a tabela original com as coordenadas convertidas e o novo datum
ctb0086_event[, coord_x := new_coords[, 1]] # Novas longitudes
ctb0086_event[, coord_y := new_coords[, 2]] # Novas latitudes
ctb0086_event[, coord_datum := 4326]      # Novo datum: WGS84


# Precisão (coord) -> coord_precisao
data.table::setnames(ctb0086_event, old = "Precisão (coord)", new = "coord_precisao", skip_absent = TRUE)
ctb0086_event[, coord_precisao := as.character(coord_precisao)]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0086_event, old = "Fonte (coord)", new = "coord_fonte", skip_absent = TRUE)
ctb0086_event[, coord_fonte := as.character(coord_fonte)]



# País -> pais_id
data.table::setnames(ctb0086_event, old = "País", new = "pais_id")
ctb0086_event[, pais_id := "BR"]


# Estado (UF) -> estado_id
data.table::setnames(ctb0086_event, old = "Estado (UF)", new = "estado_id")
ctb0086_event[, estado_id := as.character(estado_id)]
ctb0086_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0086_event, old = "Município", new = "municipio_id")
ctb0086_event[, municipio_id := as.character(municipio_id)]
ctb0086_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0086_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0086_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0086_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0086_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0086_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0086_event[, .N, by = taxon_sibcs]

# taxon_st_1999
# missing taxon_st_1999 so, we set N/A.
ctb0086_event[, taxon_st := NA_character_]

#stoniness and rockiness are missing from this document#########
# Pedregosidade (superficie) 

ctb0086_event[, pedregosidade := NA_character_]


# Rochosidade (superficie)


ctb0086_event[, rochosidade := NA_character_]

###############################################################

str(ctb0086_event)

# layers ###########################################################################################
ctb0086_layer <- google_sheet(ctb0086_ids$gs_id, ctb0086_ids$gid_layer)
str(ctb0086_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0086_layer, old = "ID do evento", new = "observacao_id")
ctb0086_layer[, observacao_id := as.character(observacao_id)]
ctb0086_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0086_layer, old = "ID da camada", new = "camada_nome")
ctb0086_layer[, camada_nome := as.character(camada_nome)]
ctb0086_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0086_layer, old = "ID da amostra", new = "amostra_id")
ctb0086_layer[, amostra_id := as.character(amostra_id)]
ctb0086_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0086_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0086_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0086_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0086_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0086_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0086_layer[, profund_inf])

#areia grossa
# old: Areia grossa 2 - 0,2 (g/kg)
# new: areia_grossa
data.table::setnames(ctb0086_layer, old = "Areia grossa 2 - 0,2 (g/kg)", new = "areia_grossa")
ctb0086_layer[, areia_grossa := as.numeric(areia_grossa)]
summary(ctb0086_layer[, areia_grossa])

#areia fina
# old: Areia fina 0,2 - 0,05 (g/kg)
# new: areia_fina
data.table::setnames(ctb0086_layer, old = "Areia fina 0,2 - 0,05 (g/kg)", new = "areia_fina")
ctb0086_layer[, areia_fina := as.numeric(areia_fina)]
summary(ctb0086_layer[, areia_fina])

#areia
ctb0086_layer[, areia := areia_grossa + areia_fina]
summary(ctb0086_layer[, areia])


#silte
# old: Silte 0,05 - 0,002 (g/kg)
# new: silte
data.table::setnames(ctb0086_layer, old = "Silte 0,05 - 0,002 (g/kg)", new = "silte")
ctb0086_layer[, silte := as.numeric(silte)]
summary(ctb0086_layer[, silte])


#argila
# old: Argila < 0,002 (g/kg)
# new: argila
data.table::setnames(ctb0086_layer, old = "Argila < 0,002 (g/kg)", new = "argila")
ctb0086_layer[, argila := as.numeric(argila)]
summary(ctb0086_layer[, argila])


#terrafina
# old: Terra fina <2 (g/kg)
# new: terrafina
data.table::setnames(ctb0086_layer, old = "Terra fina <2 (g/kg)", new = "terrafina")
ctb0086_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0086_layer[, terrafina])

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0086_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0086_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0086_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C (g/kg)
# new: carbono
data.table::setnames(ctb0086_layer, old = "C (g/kg)", new = "carbono")
ctb0086_layer[, carbono := as.numeric(carbono)]
ctb0086_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0086_layer[, carbono])

# ctc
# old: T (cmolc/kg)
# new: ctc
data.table::setnames(ctb0086_layer, old = "T (cmolc/kg)", new = "ctc")
ctb0086_layer[, ctc := as.numeric(ctc)]
summary(ctb0086_layer[, ctc])
check_empty_layer(ctb0086_layer, "ctc")

# ph
# old: pH H_2O
# new: ph
data.table::setnames(ctb0086_layer, old = "pH H_2O", new = "ph")
ctb0086_layer[, ph := as.numeric(ph)]
summary(ctb0086_layer[, ph])
check_empty_layer(ctb0086_layer, "ph")

# dsi
# old: Densidade aparente (g/cm^3)
# new: dsi
data.table::setnames(ctb0086_layer, old = "Densidade aparente (g/cm^3)", new = "dsi")
ctb0086_layer[, dsi := as.numeric(dsi)]
summary(ctb0086_layer[, dsi])
check_empty_layer(ctb0086_layer, "dsi")

str(ctb0086_layer)

# Merge ############################################################################################
# events and layers
ctb0086 <- merge(ctb0086_event, ctb0086_layer, all = TRUE)
ctb0086[, dataset_id := "ctb0086"]
# citation
ctb0086 <- merge(ctb0086, ctb0086_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0086)

#Layers: 256
#Events: 91
#Georeferenced events: 91


# Plot using mapview
if (TRUE) {
  ctb0086_sf <- sf::st_as_sf(
    ctb0086[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0086_sf["argila"])
}

# Write to disk ####################################################################################
ctb0086 <- select_output_columns(ctb0086)
data.table::fwrite(ctb0086, "ctb0086/ctb0086.csv")
data.table::fwrite(ctb0086_event, "ctb0086/ctb0086_event.csv")
data.table::fwrite(ctb0086_layer, "ctb0086/ctb0086_layer.csv")
