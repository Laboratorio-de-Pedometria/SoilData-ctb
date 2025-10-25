# autor: Felipe Brun Vergani
# data: 2025

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0080
# Dados de "Variabilidade de atributos do solo em um transecto entre os biomas Pantanal
# Mato-Grossense e Cerrado"
# 
# Google Drive: https://drive.google.com/drive/u/1/folders/1n87B-QaQnTRNSpVDPpdtKEv5dbkOfbp6
# NotebookLM: https://notebooklm.google.com/notebook/d8e16cd4-a3e8-4575-bf39-ac51f561a9de
ctb0080_ids <- soildata_catalog("ctb0080")

# validation #####################################################################################
# Load validation sheet and check
ctb0080_validation <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_validation)
check_sheet_validation(ctb0080_validation)

# citation #####################################################################################
ctb0080_citation <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_citation)
str(ctb0080_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0080_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0080_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0080_citation <- data.table::data.table(
  dataset_id = "ctb0080",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0080_citation)

# event #####################################################################################
ctb0080_event <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_event)
str(ctb0080_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0080_event, old = "ID do evento", new = "observacao_id")
ctb0080_event[, observacao_id := as.character(observacao_id)]
# Check for duplicate IDs
any(table(ctb0080_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0080_event, old = "Ano (coleta)", new = "data_ano")
ctb0080_event[, data_ano := as.integer(data_ano)]
ctb0080_event[, .N, by = data_ano]
# There are two soil profiles with the sampling year 2008. These could be soil profiles obtained
# from the work of Raphael Beirigo (2008) in the Pantanal region. We need to check this with the
# authors.

# ano_fonte
# There are 1405 soil profiles missing the year of collection in the field. These are auger
# holes. We will contact the authors to check if they can provide this information. For now, we
# will attribute a random year between the interval of years reported for other soil profiles.
ctb0080_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0080_event[, .N, by = ano_fonte]

# data_ano - fill missing values
year_range <- ctb0080_event[!is.na(data_ano), range(data_ano)]
ctb0080_event[is.na(data_ano), data_ano := sample(year_range[1]:year_range[2], .N, replace = TRUE)]
ctb0080_event[is.na(ano_fonte), ano_fonte := "Estimativa"]
ctb0080_event[, .N, by = data_ano]
ctb0080_event[, .N, by = ano_fonte]

# coord_x
# X -> coord_x
data.table::setnames(ctb0080_event, old = "X", new = "coord_x")
ctb0080_event[, coord_x := as.numeric(coord_x)]
summary(ctb0080_event[, coord_x])

# coord_y
# Y -> coord_y
data.table::setnames(ctb0080_event, old = "Y", new = "coord_y")
ctb0080_event[, coord_y := as.numeric(coord_y)]
summary(ctb0080_event[, coord_y])

# Check for duplicate coordinates
check_duplicated_coordinates(ctb0080_event)
# There are 90 cases of duplicated coordinates. These are auger holes. After a quick check of the
# source document and also analythical data, we conclude that these are indeed the same locations
# and thus we can drop the duplicates. However, as we also need to drop their respective layers, we
# will not drop them now, but later after merging events and layers.

# DATUM -> coord_datum
data.table::setnames(ctb0080_event, old = "DATUM", new = "coord_datum")
ctb0080_event[, coord_datum := as.character(coord_datum)]
ctb0080_event[, .N, by = coord_datum]
# All coordinates are in "SAD69 / UTM zona 21S" (EPSG: 29191).
ctb0080_event[coord_datum == "SAD69 / UTM zona 21S", coord_datum := 29191]
ctb0080_event[, coord_datum := as.integer(coord_datum)]
ctb0080_event[, .N, by = coord_datum]
# Trasnform coordinates to WGS84 (EPSG: 4326)
# Cria um objeto 'sf' (simple features) com os dados a serem transformados
ctb0080_event_sf <- sf::st_as_sf(
  ctb0080_event[coord_datum == 29191],
  coords = c("coord_x", "coord_y"),
  crs = 29191 # Informa o sistema de coordenadas de origem
)
# Transforma as coordenadas para WGS84 (padrão GPS, EPSG: 4326)
ctb0080_event_sf <- sf::st_transform(ctb0080_event_sf, 4326)
# Atualiza o data.table original com as novas coordenadas e o novo datum
ctb0080_event[coord_datum == 29191, coord_x := sf::st_coordinates(ctb0080_event_sf)[, 1]] # Longitude
ctb0080_event[coord_datum == 29191, coord_y := sf::st_coordinates(ctb0080_event_sf)[, 2]] # Latitude
ctb0080_event[coord_datum == 29191, coord_datum := 4326] # Novo datum: WGS84
summary(ctb0080_event[, .(coord_x, coord_y)])
ctb0080_event[, .N, by = coord_datum]
rm(ctb0080_event_sf)

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0080_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0080_event[, coord_fonte := as.character(coord_fonte)]
ctb0080_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The coordinates were obtained using GPS equipment, but the sources do not explicitly state the
# technical precision (e.g., error margin in meters or centimeters) of the GPS device used. So we
# will assume a precision of 10 meters.
data.table::setnames(ctb0080_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0080_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0080_event[, coord_precisao])
ctb0080_event[is.na(coord_precisao), coord_precisao := 10]
summary(ctb0080_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0080_event, old = "País", new = "pais_id")
ctb0080_event[, pais_id := as.character(pais_id)]
ctb0080_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0080_event, old = "Estado", new = "estado_id")
ctb0080_event[, estado_id := as.character(estado_id)]
ctb0080_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0080_event, old = "Município", new = "municipio_id")
ctb0080_event[, municipio_id := as.character(municipio_id)]
ctb0080_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# The area of the sampling event is not reported in the source documents. However, it can be
# inferred from the type of sampling. This will be done in the source spreadsheet in the future.
data.table::setnames(ctb0080_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0080_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0080_event[, amostra_area])

# SiBCS (2009) -> taxon_sibcs
data.table::setnames(ctb0080_event, old = "SiBCS (2009)", new = "taxon_sibcs")
ctb0080_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0080_event[, .N, by = taxon_sibcs]

# taxon_st
# Soil classification according to US Soil Taxonomy is missing in this document.
ctb0080_event[, taxon_st := NA_character_]

# pedregosidade
# Pedregosidade -> pedregosidade
data.table::setnames(ctb0080_event, old = "Pedregosidade", new = "pedregosidade")
ctb0080_event[, pedregosidade := as.character(pedregosidade)]
ctb0080_event[, .N, by = pedregosidade]

# rochosidade
# Rochosidade -> rochosidade
data.table::setnames(ctb0080_event, old = "Rochosidade", new = "rochosidade")
ctb0080_event[, rochosidade := as.character(rochosidade)]
ctb0080_event[, .N, by = rochosidade]

str(ctb0080_event)

# layers ###########################################################################################
ctb0080_layer <- google_sheet(ctb0080_ids$gs_id, ctb0080_ids$gid_layer)
str(ctb0080_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0080_layer, old = "ID do evento", new = "observacao_id")
ctb0080_layer[, observacao_id := as.character(observacao_id)]
ctb0080_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0080_layer, old = "ID da camada", new = "camada_nome")
ctb0080_layer[, camada_nome := as.character(camada_nome)]
ctb0080_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0080_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0080_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0080_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0080_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0080_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0080_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0080_layer[, profund_inf])

# Sand in this document is separated into
# Coarse sand, Fine sand

# areia_grossa
# old: "Areia grossa [g/kg]"
# new: areia_grossa
# 
data.table::setnames(ctb0080_layer, old = "Areia grossa [g/kg]", new = "areia_grossa")
ctb0080_layer[, areia_grossa := as.numeric(areia_grossa)]
ctb0080_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: "Areia fina [g/kg]"
# new: areia_fina
# 
data.table::setnames(ctb0080_layer, old = "Areia fina [g/kg]", new = "areia_fina")
ctb0080_layer[, areia_fina := as.numeric(areia_fina)]
ctb0080_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

              
# areia
data.table::setnames(ctb0080_layer, old = "Areia [g/kg]", new = "areia")
# Utilizo NULL para limpar os "#N/A" factors.
ctb0080_layer[, areia := NULL]
ctb0080_layer[, areia := (areia_fina + areia_grossa)]


# silte
# old: Silte [g/kg]
# new: silte
# 
data.table::setnames(ctb0080_layer, old = "Silte [g/kg]", new = "silte")
ctb0080_layer[, silte := as.numeric(silte)]
ctb0080_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila [g/kg]
# new: argila
#
data.table::setnames(ctb0080_layer, old = "Argila [g/kg]", new = "argila")
ctb0080_layer[, argila := as.numeric(argila)]
ctb0080_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# old: Terra fina [g/kg]
# new: terrafina
data.table::setnames(ctb0080_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0080_layer[, terrafina := as.numeric(terrafina)]
ctb0080_layer[is.na(terrafina), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0080_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0080_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0080_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C[orgânico] [g/kg]
# new: carbono
data.table::setnames(ctb0080_layer, old = "C[orgânico] [g/kg]", new = "carbono")
ctb0080_layer[, carbono := as.numeric(carbono)]
summary(ctb0080_layer[, carbono])
check_empty_layer(ctb0080_layer, "carbono")

# ctc
# old: T [cmolc/kg]
# new: ctc
data.table::setnames(ctb0080_layer, old = "T [cmolc/kg]", new = "ctc")
ctb0080_layer[, ctc := as.numeric(ctc)]
summary(ctb0080_layer[, ctc])
check_empty_layer(ctb0080_layer, "ctc")

# ph
# old: pH [H_2O]
# new: ph
data.table::setnames(ctb0080_layer, old = "pH [H_2O]", new = "ph")
ctb0080_layer[, ph := as.numeric(ph)]
summary(ctb0080_layer[, ph])
check_empty_layer(ctb0080_layer, "ph")

# dsi
# old: Densidade Solo [kg/dm^3]
# new: dsi
data.table::setnames(ctb0080_layer, old = "Densidade Solo [kg/dm^3]", new = "dsi")
ctb0080_layer[, dsi := as.numeric(dsi)]
summary(ctb0080_layer[, dsi])
check_empty_layer(ctb0080_layer, "dsi")

str(ctb0080_layer)

# Merge ############################################################################################
# events and layers
ctb0080 <- merge(ctb0080_event, ctb0080_layer, all = TRUE)
ctb0080[, dataset_id := "ctb0080"]
# citation
ctb0080 <- merge(ctb0080, ctb0080_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0080)

#Layers: 1780
#Events: 1415
#Georeferenced events: 1415


# Plot using mapview
if (FALSE) {
  ctb0080_sf <- sf::st_as_sf(
    ctb0080[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0080_sf["argila"])
}

# Write to disk ####################################################################################
ctb0080 <- select_output_columns(ctb0080)
data.table::fwrite(ctb0080, "ctb0080/ctb0080.csv")
data.table::fwrite(ctb0080_event, "ctb0080/ctb0080_event.csv")
data.table::fwrite(ctb0080_layer, "ctb0080/ctb0080_layer.csv")
