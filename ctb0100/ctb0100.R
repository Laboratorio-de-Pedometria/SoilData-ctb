# autor: Felipe Brun Vergani
# data: 2026

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0100
# Dados de "Processos pedogenéticos atuais e pretéritos em solos alcalino-sódicos do  Pantanal Norte"
# 
# 
# 
# https://docs.google.com/spreadsheets/d/19SpKVvUP04n2g8j4nlfVgC58uPkqETycvOsjV2WM41s/edit?usp=sharing


ctb0100_ids <- soildata_catalog("ctb0100")

# validation #####################################################################################

ctb0100_validation <- google_sheet(ctb0100_ids$gs_id, ctb0100_ids$gid_validation)
str(ctb0100_validation)

# Check for negative validation results
check_sheet_validation(ctb0100_validation)

# citation #####################################################################################
ctb0100_citation <- google_sheet(ctb0100_ids$gs_id, ctb0100_ids$gid_citation)
str(ctb0100_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0100_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0100_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0100_citation <- data.table::data.table(
  dataset_id = "ctb0100",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0100_citation)

# event #####################################################################################
ctb0100_event <- google_sheet(ctb0100_ids$gs_id, ctb0100_ids$gid_event)
str(ctb0100_event)

#PROCESS FIELDS


# observacao_id
# old:  ID do evento
# new: observacao_id
data.table::setnames(ctb0100_event, old = "ID do evento", new = "observacao_id")
ctb0100_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0100_event[, observacao_id]) > 1)

# data_ano
# old: Ano de coleta
# new: data_ano
# This study did not provide the data collection date. 
data.table::setnames(ctb0100_event, old = "Ano de coleta", new = "data_ano")
ctb0100_event[, data_ano := as.integer(data_ano)]
ctb0100_event[, .N, by = data_ano]

# ano_fonte
ctb0100_event[, ano_fonte := "Original"]
ctb0100_event[, .N, by = ano_fonte]


# coord_x
# old: X
# new: coord_x
data.table::setnames(ctb0100_event, old = "X", new = "coord_x")
ctb0100_event[, coord_x := as.numeric(coord_x)]
summary(ctb0100_event[, coord_x])

# coord_y
# old: Y
# new: coord_y
data.table::setnames(ctb0100_event, old = "Y", new = "coord_y")
ctb0100_event[, coord_y := as.numeric(coord_y)]
summary(ctb0100_event[, coord_y])


# Sistema de coordenadas -> coord_datum
# UTM-SAD 69 Zona 21s
data.table::setnames(ctb0100_event, old = "Sistema de coordenadas", new = "coord_datum")
ctb0100_event[, coord_datum := NULL]
ctb0100_event[, coord_datum := 29191]


# Cria um objeto 'sf' (simple features) com os dados a serem transformados
# Filter out rows with missing coordinates before creating the sf object
ctb0100_event_sf <- sf::st_as_sf(
  ctb0100_event[coord_datum == 29191 & !is.na(coord_x) & !is.na(coord_y)],
  coords = c("coord_x", "coord_y"),
  crs = 29191 # Informa o sistema de coordenadas de origem
)

# Transforma as coordenadas para WGS84 (padrão GPS, EPSG: 4326)
ctb0100_event_sf <- sf::st_transform(ctb0100_event_sf, 4326)

# Extrai as novas coordenadas (Longitude e Latitude) do objeto 'sf'
new_coords <- sf::st_coordinates(ctb0100_event_sf)

# Atualiza o data.table original com as novas coordenadas e o novo datum
ctb0100_event[coord_datum == 29191 & !is.na(coord_x) & !is.na(coord_y), coord_x := new_coords[, 1]] # Longitude
ctb0100_event[coord_datum == 29191 & !is.na(coord_x) & !is.na(coord_y), coord_y := new_coords[, 2]] # Latitude
ctb0100_event[coord_datum == 29191 & !is.na(coord_x) & !is.na(coord_y), coord_datum := 4326]        # Novo datum: WGS84



# No Accuracy and Source Information
# Precisão (coord) -> coord_precisao
# We set it to NA_real_
ctb0100_event[, coord_precisao := NA_real_]

# No fonte(coord) information.
# Fonte (coord) -> coord_fonte
ctb0100_event[, coord_fonte := NA_real_]
summary(ctb0100_event[, coord_fonte])

# pais_id

ctb0100_event[, pais_id := "BR"]

# estado_id
# old: Estado
# new: estado_id
data.table::setnames(ctb0100_event, old = "Estado", new = "estado_id")
ctb0100_event[, estado_id := as.character(estado_id)]
ctb0100_event[, .N, by = estado_id]


# municipio_id
# old: Município
# new: municipio_id
data.table::setnames(ctb0100_event, old = "Município", new = "municipio_id")
ctb0100_event[, municipio_id := as.character(municipio_id)]
ctb0100_event[, .N, by = municipio_id]

# amostra_area
# old: Área do evento [m2]
# new: amostra_area
data.table::setnames(ctb0100_event, old = "Área do evento [m2]", new = "amostra_area")
ctb0100_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0100_event[, amostra_area])

# taxon_sibcs
# old: SiBCS[2013]
# new: taxon_sibcs
data.table::setnames(ctb0100_event, old = "SiBCS [2013]", new = "taxon_sibcs")
ctb0100_event[, taxon_sibcs := as.character(taxon_sibcs)]


# taxon_st 
# old: Classificação do solo Soil Taxonomy
# new: taxon_st
data.table::setnames(ctb0100_event, old = "Classificação do solo Soil Taxonomy", new = "taxon_st")
ctb0100_event[, taxon_st := as.character(taxon_st)]


# Pedregosidade (superficie)
data.table::setnames(ctb0100_event, old = "Pedregosidade", new = "pedregosidade")
ctb0100_event[, pedregosidade := as.character(pedregosidade)]

# Rochosidade (superficie)
data.table::setnames(ctb0100_event, old = "Rochosidade", new = "rochosidade")
ctb0100_event[, rochosidade := as.character(rochosidade)]


str(ctb0100_event)

# layers ###########################################################################################
ctb0100_layer <- google_sheet(ctb0100_ids$gs_id, ctb0100_ids$gid_layer)
str(ctb0100_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0100_layer, old = "ID do evento", new = "observacao_id")
ctb0100_layer[, observacao_id := as.character(observacao_id)]
ctb0100_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0100_layer, old = "ID da camada", new = "camada_nome")
ctb0100_layer[, camada_nome := as.character(camada_nome)]
ctb0100_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
ctb0100_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0100_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0100_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0100_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0100_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0100_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0100_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0100_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0100_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0100_layer[, profund_inf])

# camada_id
ctb0100_layer[, mid_depth := (profund_sup + profund_inf)/2]
ctb0100_layer <- ctb0100_layer[order(observacao_id, mid_depth)]
ctb0100_layer[, camada_id := 1: .N, by = observacao_id]
ctb0100_layer[, .N, by = camada_id]

# areia
# old: Areia Total [g/kg]
# new: areia
data.table::setnames(ctb0100_layer, old = "Areia Total [g/kg]", new = "areia")
ctb0100_layer[, areia := as.numeric(areia)]
summary(ctb0100_layer[, areia])


# silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0100_layer, old = "Silte [g/kg]", new = "silte")
ctb0100_layer[, silte := as.numeric(silte)]
summary(ctb0100_layer[, silte])

# argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0100_layer, old = "Argila [g/kg]", new = "argila")
ctb0100_layer[, argila := as.numeric(argila)]
summary(ctb0100_layer[, argila])


#terrafina
# is missing in this document.
ctb0100_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0100_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0100_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0100_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C.org [g/kg]
# new: carbono
data.table::setnames(ctb0100_layer, old = "C.org [g/kg]", new = "carbono")
ctb0100_layer[, carbono := as.numeric(carbono)]
summary(ctb0100_layer[, carbono])
check_empty_layer(ctb0100_layer, "carbono")


# ctc
# old: CTC [cmolc/kg]
# new: ctc
data.table::setnames(ctb0100_layer, old = "CTC [cmolc/kg]", new = "ctc")
ctb0100_layer[, ctc := as.numeric(ctc)]
ctb0100_layer[observacao_id %in% c("P2", "P5-Setor-A"), ctc := NA_real_]
summary(ctb0100_layer[, ctc])
check_empty_layer(ctb0100_layer, "ctc")


# ph
# old: pH H_2O
# new: ph
data.table::setnames(ctb0100_layer, old = "pH H_2O", new = "ph")
ctb0100_layer[, ph := as.numeric(ph)]
summary(ctb0100_layer[, ph])
check_empty_layer(ctb0100_layer, "ph")

# dsi
# dsi is missing in this document.
ctb0100_layer[, dsi := NA_real_]


str(ctb0100_layer)

# Merge ############################################################################################
# events and layers
ctb0100 <- merge(ctb0100_event, ctb0100_layer, all = TRUE)
ctb0100[, dataset_id := "ctb0100"]
# citation
ctb0100 <- merge(ctb0100, ctb0100_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0100)


#Layers: 34
#Events: 7
#Georeferenced events: 5


# Plot using mapview
if (FALSE) {
  ctb0100_sf <- sf::st_as_sf(
    ctb0100[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0100_sf["areia"], layer.name ="areia")
}

# Write to disk ####################################################################################
ctb0100 <- select_output_columns(ctb0100)
data.table::fwrite(ctb0100, "ctb0100/ctb0100.csv")
data.table::fwrite(ctb0100_event, "ctb0100/ctb0100_event.csv")
data.table::fwrite(ctb0100_layer, "ctb0100/ctb0100_layer.csv")
