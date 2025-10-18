# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}
if (!requireNamespace("parzer")) {
  install.packages("parzer")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0073
# Dados de "Relações pedologia, geomorfologia e sedimentologia no Pantanal Norte"
#
# Google Drive: https://drive.google.com/drive/u/1/folders/1-ZuzR5Ma_kdWIpSnOGg_MlYRvhE7BRfe
ctb0073_ids <- soildata_catalog("ctb0073")

# validation #####################################################################################
ctb0073_validation <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_validation)
check_sheet_validation(ctb0073_validation)

# citation #####################################################################################
ctb0073_citation <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_citation)
str(ctb0073_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0073_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0073_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0073_citation <- data.table::data.table(
  dataset_id = "ctb0073",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0073_citation)

# event #####################################################################################
ctb0073_event <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_event)
str(ctb0073_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0073_event, old = "ID do evento", new = "observacao_id")
ctb0073_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0073_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0073_event, old = "Ano (coleta)", new = "data_ano")
ctb0073_event[, data_ano := as.integer(data_ano)]
ctb0073_event[, .N, by = data_ano]

# ano_fonte
ctb0073_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0073_event[, .N, by = ano_fonte]


# coord_x
# Longitude -> coord_x
# coord_x = (Longitude grau + Longitude minuto / 60 + longitude segundo / 3600) * -1
data.table::setnames(ctb0073_event, old = "Longitude grau", new = "longitude_grau")
ctb0073_event[, longitude_grau := as.numeric(longitude_grau)]
data.table::setnames(ctb0073_event, old = "Longitude minuto", new = "longitude_minuto")
ctb0073_event[, longitude_minuto := as.numeric(longitude_minuto)]
data.table::setnames(ctb0073_event, old = "Longitude segundo", new = "longitude_segundo")
ctb0073_event[, longitude_segundo := as.numeric(longitude_segundo)]
ctb0073_event[, coord_x := (longitude_grau + (longitude_minuto/60) + (longitude_segundo/3600)) * -1]
summary(ctb0073_event[, coord_x])

# coord_y
# Latitude -> coord_y
# coord_y = (Latitude grau + Latitude minuto / 60 + latitude segundo / 3600) * -1
data.table::setnames(ctb0073_event, old = "Latitude grau", new = "latitude_grau")
ctb0073_event[, latitude_grau := as.numeric(latitude_grau)]
data.table::setnames(ctb0073_event, old = "Latitude minuto", new = "latitude_minuto")
ctb0073_event[, latitude_minuto := as.numeric(latitude_minuto)]
data.table::setnames(ctb0073_event, old = "Latitude segundo", new = "latitude_segundo")
ctb0073_event[, latitude_segundo := as.numeric(latitude_segundo)]
ctb0073_event[, coord_y := (latitude_grau + (latitude_minuto/60) + (latitude_segundo/3600)) * -1]
summary(ctb0073_event[, coord_y])

# Check for duplicate coordinates
ctb0073_event[, .N, by = .(coord_x, coord_y)][N > 1]
# Remove todas as linhas onde 'coord_x' ou 'coord_y' sejam NA
ctb0073_event <- na.omit(ctb0073_event, cols = c("coord_x", "coord_y"))

# Datum (coord) -> coord_datum
# Define o datum de origem como SIRGAS 2000 (EPSG: 4674)
# O "L" garante que o número seja tratado como um inteiro (integer).
data.table::setnames(ctb0073_event, old = "Datum (coord)", new = "coord_datum")
ctb0073_event[, coord_datum := NULL]
ctb0073_event[, coord_datum := "N/A"]
ctb0073_event[coord_datum == "N/A", coord_datum := 4674L]
ctb0073_event[, coord_datum := as.integer(coord_datum)]

#  Converte o data.table para um objeto espacial (sf)
# Informamos que o sistema de coordenadas (CRS) original é 4674
ctb0073_event_sf <- sf::st_as_sf(
  ctb0073_event,
  coords = c("coord_x", "coord_y"),
  crs = 4674 # Define o CRS de origem como SIRGAS 2000
)

#  Transforma as coordenadas para WGS84 (EPSG: 4326)
ctb0073_event_sf_wgs84 <- sf::st_transform(ctb0073_event_sf, 4326)

#  Extrai as novas coordenadas (já em WGS84) do objeto sf
new_coords <- sf::st_coordinates(ctb0073_event_sf_wgs84)

#  Atualiza a tabela original com as coordenadas convertidas e o novo datum
ctb0073_event[, coord_x := new_coords[, 1]] # Novas longitudes
ctb0073_event[, coord_y := new_coords[, 2]] # Novas latitudes
ctb0073_event[, coord_datum := 4326L]      # Novo datum: WGS84

summary(ctb0073_event[, .(coord_datum, coord_x, coord_y)])


# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0073_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0073_event[, coord_precisao := NA_real_]
summary(ctb0073_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0073_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0073_event[, coord_fonte := NA_real_]
summary(ctb0073_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0073_event, old = "País", new = "pais_id")
ctb0073_event[, pais_id := "BR"]

# #Mapeamento dos estados para sigla se necessário utilizar a função 'recode'
# mapa_siglas <- c(
#   "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
#   "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
#   "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
#   "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
#   "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
#   "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
#   "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
#   "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
#   "Tocantins" = "TO"
# )


# Estado (UF) -> estado_id
data.table::setnames(ctb0073_event, old = "Estado (UF)", new = "estado_id")
ctb0073_event[, estado_id := as.character(estado_id)]
ctb0073_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0073_event, old = "Município", new = "municipio_id")
ctb0073_event[, municipio_id := as.character(municipio_id)]
ctb0073_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# is missing on main document
ctb0073_event[, amostra_area := NA_real_]
summary(ctb0073_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0073_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0073_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0073_event[, .N, by = taxon_sibcs]

# taxon_st_1999

ctb0073_event[, taxon_st := NA_character_]

# Pedregosidade (superficie) 
data.table::setnames(ctb0073_event, old = "Pedregosidade", new = "pedregosidade")
ctb0073_event[, pedregosidade := as.character(pedregosidade)]
ctb0073_event[, .N, by = pedregosidade]

# Rochosidade (superficie)

data.table::setnames(ctb0073_event, old = "Rochosidade", new = "rochosidade")
ctb0073_event[, rochosidade := as.character(rochosidade)]
ctb0073_event[, .N, by = rochosidade]



str(ctb0073_event)



# layers ###########################################################################################
ctb0073_layer <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_layer)
str(ctb0073_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0073_layer, old = "ID do evento", new = "observacao_id")
ctb0073_layer[, observacao_id := as.character(observacao_id)]
ctb0073_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0073_layer, old = "ID da camada", new = "camada_nome")
ctb0073_layer[, camada_nome := as.character(camada_nome)]
ctb0073_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0073_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0073_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0073_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0073_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0073_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0073_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0073_layer[, profund_inf])

# Sand in this document is separated into
# Very coarse sand, Coarse sand, Medium sand, Fine sand, and Very fine sand.

#areia_muito_grossa
# old: Areia muito grossa (2-1 mm) [g/Kg]
# new: areia_muito_grossa
data.table::setnames(ctb0073_layer, old = "Areia muito grossa (2-1 mm) [g/Kg]", new = "areia_muito_grossa")
ctb0073_layer[, areia_muito_grossa := as.numeric(areia_muito_grossa)]
ctb0073_layer[is.na(areia_muito_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_muito_grossa)]


# areia_grossa
# old: "Areia grossa (1-0,5 mm) [g/Kg]"
# new: areia_grossa
# areia_grossa is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia grossa (1-0,5 mm) [g/Kg]", new = "areia_grossa")
ctb0073_layer[, areia_grossa := as.numeric(areia_grossa)]
ctb0073_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_media
# old: "Areia média (0,5-0,25 mm) [g/Kg]"
# new: areia_media
# areia_media is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia média (0,5-0,25 mm) [g/Kg]", new = "areia_media")
ctb0073_layer[, areia_media := as.numeric(areia_media)]
ctb0073_layer[is.na(areia_media), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_media)]


# areia_fina
# old: "Areia fina (0,25-0,1 mm) [g/Kg]"
# new: areia_fina
# areia_fina is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia fina (0,25-0,1 mm) [g/Kg]", new = "areia_fina")
ctb0073_layer[, areia_fina := as.numeric(areia_fina)]
ctb0073_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

# areia_muito_fina
# old: "Areia muito fina (0,1-0,05 mm) [g/Kg]"
# new: areia_muito_fina
# areia_muito_fina is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia muito fina (0,1-0,05 mm) [g/Kg]", new = "areia_muito_fina")
ctb0073_layer[, areia_muito_fina := as.numeric(areia_muito_fina)]
ctb0073_layer[is.na(areia_muito_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_muito_fina)]



# areia
# criação da coluna areia 
cols_areia <- c("areia_muito_grossa", "areia_grossa", "areia_media", "areia_fina", "areia_muito_fina")

# Criação da coluna areia, somando as frações e tratando NAs como 0
# na.rm = TRUE (NA remove) remove os NAs antes de somar
ctb0073_layer[, areia := rowSums(.SD, na.rm = TRUE), .SDcols = cols_areia]


# silte
# old: Silte (0,05-0,002 mm) [g/Kg]
# new: silte
# silte is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Silte (0,05-0,002 mm) [g/Kg]", new = "silte")
ctb0073_layer[, silte := as.numeric(silte)]
ctb0073_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
#clay in this document is present as clay and clay dispersed in water, only clay was used
# old: Argila > 0,002 mm (g/kg)
# new: argila
# argila is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Argila (<0,002) [g/Kg]", new = "argila")
ctb0073_layer[, argila := as.numeric(argila)]
ctb0073_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# Não existe fração fina na tabela apenas fração grossa portanto esta N/A
ctb0073_layer[, terrafina := NA_character_]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0073_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0073_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0073_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C [g/Kg]
# new: carbono
data.table::setnames(ctb0073_layer, old = "C [g/Kg]", new = "carbono")
ctb0073_layer[, carbono := as.numeric(carbono)]
summary(ctb0073_layer[, carbono])
check_empty_layer(ctb0073_layer, "carbono")

# ctc
# old: T [cmolc/Kg]
# new: ctc
data.table::setnames(ctb0073_layer, old = "T [cmolc/Kg]", new = "ctc")
ctb0073_layer[, ctc := as.numeric(ctc)]
summary(ctb0073_layer[, ctc])
check_empty_layer(ctb0073_layer, "ctc")

# ph
# old: pH 1:2,5 em H_2O
# new: ph
data.table::setnames(ctb0073_layer, old = "pH 1:2,5 em H_2O", new = "ph")
ctb0073_layer[, ph := as.numeric(ph)]
summary(ctb0073_layer[, ph])
check_empty_layer(ctb0073_layer, "ph")

# dsi
# dsi is missing in this document 
ctb0073_layer[, dsi := NA_real_]

str(ctb0073_layer)

# Merge ############################################################################################
# events and layers
ctb0073 <- merge(ctb0073_event, ctb0073_layer, all = TRUE)
ctb0073[, dataset_id := "ctb0073"]
# citation
ctb0073 <- merge(ctb0073, ctb0073_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0073)


# Layers: 322
# Events: 33
# Georeferenced events: 12


# Plot using mapview
if (FALSE) {
  ctb0073_sf <- sf::st_as_sf(
    ctb0073[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0073_sf["argila"])
}

# Write to disk ####################################################################################
ctb0073 <- select_output_columns(ctb0073)
data.table::fwrite(ctb0073, "ctb0073/ctb0073.csv")
data.table::fwrite(ctb0073_event, "ctb0073/ctb0073_event.csv")
data.table::fwrite(ctb0073_layer, "ctb0073/ctb0073_layer.csv")
