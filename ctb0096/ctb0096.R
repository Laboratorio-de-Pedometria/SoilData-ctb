# autor: Felipe Brun Vergani
# data: 2026


# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0096
# Dados de "Diagnóstico de atributos físico-hídricos dos solos
# de textura arenosa em áreas de intensificação agrícola no bioma Cerrado"
# 
# https://docs.google.com/spreadsheets/d/1aYky46K1H_kPtWgILNBaVDv-QTbLdxwfMEBRdZR85V0/edit?usp=sharing


ctb0096_ids <- soildata_catalog("ctb0096")

# validation #####################################################################################

ctb0096_validation <- google_sheet(ctb0096_ids$gs_id, ctb0096_ids$gid_validation)
str(ctb0096_validation)

# Check for negative validation results
sum(ctb0096_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0096_citation <- google_sheet(ctb0096_ids$gs_id, ctb0096_ids$gid_citation)
str(ctb0096_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0096_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0096_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0096_citation <- data.table::data.table(
  dataset_id = "ctb0096",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0096_citation)

# event #####################################################################################
ctb0096_event <- google_sheet(ctb0096_ids$gs_id, ctb0096_ids$gid_event)
str(ctb0096_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0096_event, old = "ID do evento", new = "observacao_id")
ctb0096_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0096_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0096_event, old = "Ano (coleta)", new = "data_ano")
ctb0096_event[, data_ano := as.integer(data_ano)]
ctb0096_event[, .N, by = data_ano]

# ano_fonte
ctb0096_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0096_event[, .N, by = ano_fonte]


# coord_x
# old: Longitude
# new: coord_x
data.table::setnames(ctb0096_event, old = "Longitude", new = "coord_x")
ctb0096_event[coord_x == "#N/A", coord_x := NA_character_ ]
ctb0096_event[, coord_x := parzer::parse_lon(coord_x)]
# Aplicar sinal negativo (Oeste)
ctb0096_event[!is.na(coord_x) & coord_x > 0, coord_x := -coord_x]
summary(ctb0096_event[, coord_x])

# coord_y
# old: Latitude
# new: coord_y
data.table::setnames(ctb0096_event, old = "Latitude", new = "coord_y")
ctb0096_event[coord_y == "#N/A", coord_y := NA_character_ ]
ctb0096_event[, coord_y := parzer::parse_lat(coord_y)]
# Aplicar sinal negativo (Sul)
ctb0096_event[!is.na(coord_y) & coord_y > 0, coord_y := -coord_y]
summary(ctb0096_event[, coord_y])

# Check for duplicate coordinates
# Varias coordenadas duplicadas devido o trabalho apenas reportar as coordenadas da propriedade rural onde as amostras foram coletadas.
# Existem imagens de satélite mostrando os pontos, portanto é possível inferir onde estão esses pontos.
ctb0096_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_datum
# O sistema de coordenadas não é reportado, mas por ter imagens de satélite do google maps, é possível induzir que o coord_datum é 4326 (WGS-84)
ctb0096_event[!is.na(coord_x) & !is.na(coord_y), coord_datum := 4326L]

check_equal_coordinates(ctb0096_event)

set.seed(12345)
amount <- 500  # metros
ctb0096_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
# Filtrando somente linhas duplicadas COM coordenadas válidas
idx_jitter <- ctb0096_event[, which(coord_duplicated == TRUE & 
                                      !is.na(coord_x) & 
                                      !is.na(coord_y))]
ctb0096_event_sf <- sf::st_as_sf(
  ctb0096_event[idx_jitter],
  coords = c("coord_x", "coord_y"), crs = 4326
)
ctb0096_event_sf <- sf::st_transform(ctb0096_event_sf, crs = 31983)
ctb0096_event_sf <- sf::st_jitter(ctb0096_event_sf, amount = amount)
ctb0096_event_sf <- sf::st_transform(ctb0096_event_sf, crs = 4326)

ctb0096_event[idx_jitter, coord_x := sf::st_coordinates(ctb0096_event_sf)[, 1]]
ctb0096_event[idx_jitter, coord_y := sf::st_coordinates(ctb0096_event_sf)[, 2]]
rm(ctb0096_event_sf)

# coord_precisao
# We set it to NA_real_ (missing)
ctb0096_event[, coord_precisao := NA_real_]

# coord_fonte
# É possível deduzir que foi utilizado imagens de satélite do Google Maps
ctb0096_event[!is.na(coord_datum), coord_fonte := "Google Maps"]


# País -> pais_id
data.table::setnames(ctb0096_event, old = "País", new = "pais_id")
ctb0096_event[, pais_id := "BR"]


# Estado (UF) -> estado_id
data.table::setnames(ctb0096_event, old = "Estado (UF)", new = "estado_id")
ctb0096_event[, estado_id := as.character(estado_id)]
ctb0096_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0096_event, old = "Município", new = "municipio_id")
ctb0096_event[, municipio_id := as.character(municipio_id)]
ctb0096_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0096_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0096_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0096_event[, amostra_area])

# SiBCS  -> taxon_sibcs
# is missing in this document.
ctb0096_event[, taxon_sibcs := NA_character_]

# taxon_st 
# missing this soil taxonomy on document
ctb0096_event[, taxon_st := NA_character_]
ctb0096_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
# missing in this document.

ctb0096_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# missing in this document.

ctb0096_event[, rochosidade := NA_character_]

# cobertura
# Concatenates one or more source columns (e.g. situacao, uso_atual, cobertura) into a single
# field. Adjust the vector below with the names of the already-renamed source columns.
data.table::setnames(ctb0096_event, old = "Cobertura Vegetal", new = "cobertura")
cobertura_cols <- c("cobertura")
concat_columns(ctb0096_event, target = "cobertura", sources = cobertura_cols)
ctb0096_event[, .N, by = cobertura]

#vegetacao
#In this work, there is a reference to vegetation cover, 
#therefore assign this variable to cover, leaving the vegetation variable empty.
ctb0096_event[, vegetacao := NA_character_]
ctb0096_event[, .N, by = vegetacao]

# erosao
ctb0096_event[,erosao := NA_character_]
ctb0096_event[, .N, by = erosao]




str(ctb0096_event)

# layers ###########################################################################################
ctb0096_layer <- google_sheet(ctb0096_ids$gs_id, ctb0096_ids$gid_layer)
str(ctb0096_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0096_layer, old = "ID do evento", new = "observacao_id")
ctb0096_layer[, observacao_id := as.character(observacao_id)]
ctb0096_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0096_layer, old = "ID da camada", new = "camada_nome")
ctb0096_layer[, camada_nome := as.character(camada_nome)]
ctb0096_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0096_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0096_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0096_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0096_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0096_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0096_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0096_layer[, profund_inf])

# profund_mid
ctb0096_layer[, profund_mid := (profund_sup + profund_inf)/2]

# camada_id
data.table::setorder(ctb0096_layer, observacao_id, profund_mid)
ctb0096_layer[, camada_id := seq_len(.N), by = observacao_id]
ctb0096_layer[, .N, by = camada_id]

#areia 
# old: Areia total [g kg-1]
# new: areia
data.table::setnames(ctb0096_layer, old = "Areia total [g/kg]", new = "areia")
ctb0096_layer[, areia := as.numeric(areia)]
summary(ctb0096_layer[, areia])


#argila
# old: Argila        [g kg-1]
# new: argila
data.table::setnames(ctb0096_layer, old = "Argila [g/kg]", new = "argila")
ctb0096_layer[, argila := as.numeric(argila)]
summary(ctb0096_layer[, argila])


#silte
# Silte is missing in this sheets. 
# Assume que para fechar a granulometria é necessário realizar 1000 menos a soma de areia e argila.
ctb0096_layer[, silte := 1000 - (areia + argila)]
summary(ctb0096_layer[, silte])


#terrafina
#There is no information in the spreadsheets for this variable in this study.
ctb0096_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0096_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0096_layer[!psd %in% psd_lims & !is.na(psd), .N]
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0096_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# There is no information in the spreadsheets for this variable in this study.
ctb0096_layer[, carbono := NA_real_]

# ctc
# There is no information in the spreadsheets for this variable in this study.
ctb0096_layer[, ctc := NA_real_]


#ph
# There is no information in the spreadsheets for this variable in this study.
ctb0096_layer[, ph := NA_real_]

# dsi 
# old: Densidade aparente do solo [Ds] [g cm-3]
# new: dsi
data.table::setnames(ctb0096_layer,
                     old = "Densidade aparente do solo (Ds) [g/cm^3]",
                     new = "dsi")
ctb0096_layer[, dsi := as.numeric(dsi)]
summary(ctb0096_layer[, dsi])
check_empty_layer(ctb0096_layer, "dsi")



str(ctb0096_layer)

# Merge ############################################################################################
# events and layers
ctb0096 <- merge(ctb0096_event, ctb0096_layer, all = TRUE)
ctb0096[, dataset_id := "ctb0096"]
# citation
ctb0096 <- merge(ctb0096, ctb0096_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0096)

#Layers: 343
#Events: 49
#Georeferenced events: 49


# Plot using mapview
if (FALSE) {
  ctb0096_sf <- sf::st_as_sf(
    ctb0096[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  ctb0096_areia <- ctb0096_sf["areia"]
  mapview::mapview(ctb0096_areia)
}

# Write to disk ####################################################################################
ctb0096 <- select_output_columns(ctb0096)
data.table::fwrite(ctb0096, "ctb0096/ctb0096.csv")
data.table::fwrite(ctb0096_event, "ctb0096/ctb0096_event.csv")
data.table::fwrite(ctb0096_layer, "ctb0096/ctb0096_layer.csv")
