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
# Soil layers from augerings are named "A", "B", and "C" to indicate the order "0-10", "10-20", and
# "20-30", respectively. We will replace these names with the actual depth intervals. We identify
# these layers by checking which soil layers do not have "Perfil" in "observacao_id".
ctb0080_layer[!grepl("Perfil", observacao_id) & camada_nome == "A", camada_nome := "0-10"]
ctb0080_layer[!grepl("Perfil", observacao_id) & camada_nome == "B", camada_nome := "10-20"]
ctb0080_layer[!grepl("Perfil", observacao_id) & camada_nome == "C", camada_nome := "20-30"]
ctb0080_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# The laboratory sample ID is not reported in the source document.
ctb0080_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0080_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
# Resolve irregular depth intervals
ctb0080_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0080_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0080_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0080_layer, old = "Profundidade final [cm]", new = "profund_inf")
# Resolve irregular depth intervals
ctb0080_layer[, profund_inf := depth_slash(profund_inf), by = .I]
# Resolve censored depths
ctb0080_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0080_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0080_layer[, profund_inf])

# Check for duplicated layers
check_duplicated_layer(ctb0080_layer)

# Check for layers with equal top and bottom depths
check_equal_depths(ctb0080_layer)

# Check for negative layer depths
check_depth_inversion(ctb0080_layer)
# There are six layers with inverted depths, all of them indicating organic layers (Oo). We will
# correct these depths, moving the top depth to 0 cm. The correction will be propagated to the
# depths of the other layers as well.
ctb0080_layer[profund_sup > profund_inf & grepl("Oo", camada_nome), `:=`(
  is_organic = TRUE,
  thickness_to_add = max(abs(profund_sup), na.rm = TRUE)
), by = .I]
ctb0080_layer[, thickness_to_add := max(thickness_to_add, na.rm = TRUE), by = observacao_id]
ctb0080_layer[thickness_to_add > 0, .(observacao_id, camada_nome, profund_sup, profund_inf, thickness_to_add, is_organic)]
# If is_organic is TRUE, subtract thickness_to_add from profund_sup and add thickness_to_add to
# profund_inf. Else, add 
# thickness_to_add to profund_sup and profund_inf.
ctb0080_layer[is_organic == TRUE, `:=`(
  profund_sup = profund_sup - thickness_to_add,
  profund_inf = profund_inf + thickness_to_add
)]
ctb0080_layer[is.na(is_organic) & thickness_to_add > 0, `:=`(
  profund_sup = profund_sup + thickness_to_add,
  profund_inf = profund_inf + thickness_to_add
)]
ctb0080_layer[thickness_to_add > 0, .(observacao_id, camada_nome, profund_sup, profund_inf, thickness_to_add, is_organic)]

# Check for missing layers
check_missing_layer(ctb0080_layer)
# There are no missing layers.

# terrafina
# old: Terra fina [g/kg]
# new: terrafina
data.table::setnames(ctb0080_layer, old = "Terra fina [g/kg]", new = "terrafina")
ctb0080_layer[, terrafina := as.numeric(terrafina)]
summary(ctb0080_layer[, terrafina])
# There are 1740 layers with missing "terrafina" values. These include organic layers and auger
# holes. We will keep these missing values as NA for now. 
check_empty_layer(ctb0080_layer, "terrafina")

# areia
# Areia total [g/kg] -> areia
data.table::setnames(ctb0080_layer, old = "Areia total [g/kg]", new = "areia")
ctb0080_layer[, areia := as.numeric(areia)]
summary(ctb0080_layer[, areia])
# There are seven layers with missing "areia" values. These include organic layers and an R layer,
# which is as expected.
check_empty_layer(ctb0080_layer, "areia")

# silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0080_layer, old = "Silte [g/kg]", new = "silte")
ctb0080_layer[, silte := as.numeric(silte)]
summary(ctb0080_layer[, silte])
# There are seven layers with missing "silte" values. These include organic layers and an R layer,
# which is as expected.
check_empty_layer(ctb0080_layer, "silte")

# clay
# old: Clay [g/kg]
# new: clay
data.table::setnames(ctb0080_layer, old = "Clay [g/kg]", new = "clay")
ctb0080_layer[, clay := as.numeric(clay)]
summary(ctb0080_layer[, clay])
# There are seven layers with missing "clay" values. These include organic layers and an R layer,
# which is as expected.
check_empty_layer(ctb0080_layer, "clay")

# argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0080_layer, old = "Argila [g/kg]", new = "argila")
ctb0080_layer[, argila := as.numeric(argila)]
summary(ctb0080_layer[, argila])
# There are seven layers with missing "argila" values. These include organic layers and an R layer,
# which is as expected.
check_empty_layer(ctb0080_layer, "argila")

# Check the particle size distribution
ctb0080_layer[, argila := round(argila)]
ctb0080_layer[, silte := round(silte)]
ctb0080_layer[, areia := round(areia)]
# The sum of argila, silte and areia should be 1000 g/kg. We accept a tolerance of +/- 100 g/kg.
ctb0080_layer[, psd_sum := argila + silte + areia]
ctb0080_layer[!is.na(psd_sum), psd_check := psd_sum %in% 900:1100]
# Check the limits
ctb0080_layer[!psd_check & !is.na(psd_check), .(observacao_id, camada_nome, argila, silte, areia, psd_sum)]
# There are no layers with sum of the particle size distribution outside the limits.

# carbono
# old: C[orgânico] [g/kg]
# new: carbono
data.table::setnames(ctb0080_layer, old = "C[orgânico] [g/kg]", new = "carbono")
ctb0080_layer[, carbono := as.numeric(carbono)]
summary(ctb0080_layer[, carbono])
# There are 11 layers with missing "carbono" values. These include organic layers and an R layer,
# which is as expected.
check_empty_layer(ctb0080_layer, "carbono")

# ctc
# old: T [cmolc/kg]
# new: ctc
data.table::setnames(ctb0080_layer, old = "T [cmolc/kg]", new = "ctc")
ctb0080_layer[, ctc := as.numeric(ctc)]
summary(ctb0080_layer[, ctc])
# There are 1740 layers with missing "ctc" values. These include organic layers, an R layer, and the
# auger holes. This is in accordance with the documentation.
check_empty_layer(ctb0080_layer, "ctc")

# ph
# old: pH [H_2O]
# new: ph
data.table::setnames(ctb0080_layer, old = "pH [H_2O]", new = "ph")
ctb0080_layer[, ph := as.numeric(ph)]
summary(ctb0080_layer[, ph])
# There are 1740 layers with missing "ph" values. These include organic layers, an R layer, and the
# auger holes. This is in accordance with the documentation.
check_empty_layer(ctb0080_layer, "ph")

# dsi
# old: Densidade Solo [kg/dm^3]
# new: dsi
data.table::setnames(ctb0080_layer, old = "Densidade Solo [kg/dm^3]", new = "dsi")
ctb0080_layer[, dsi := as.numeric(dsi)]
summary(ctb0080_layer[, dsi])
# There are 16 layers with missing soil bulk density "dsi" values. These include organic layers and
# an R layer, which is as expected. It also includes mineral layers from the Perfil-2 and Perfil-4.
# The document does not provide bulk density data for these profiles.
check_empty_layer(ctb0080_layer, "dsi")

str(ctb0080_layer)

# Merge ############################################################################################
# events and layers
ctb0080 <- merge(ctb0080_event, ctb0080_layer, all = TRUE)
ctb0080[, dataset_id := "ctb0080"]

# Drop duplicated coordinates and their respective layers
# Check for duplicated coordinates
ctb0080[, duplicated_coordinates := duplicated(.SD), .SDcols = c("coord_x", "coord_y")]
ctb0080[duplicated_coordinates == TRUE, .(observacao_id, coord_x, coord_y)]
# Filter out the duplicated coordinates
ctb0080 <- ctb0080[duplicated_coordinates == FALSE]
ctb0080[, duplicated_coordinates := NULL]

# citation
ctb0080 <- merge(ctb0080, ctb0080_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0080)
# Layers: 1325
# Events: 1325
# Georeferenced events: 1325

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
