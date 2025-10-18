# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
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
# Google Drive: https://drive.google.com/drive/u/0/folders/1HsBKA6yMB0zu4omBKpb6PkiVeBBoVAJW
# NotebookLM: https://notebooklm.google.com/notebook/04228dea-024b-4176-95f5-22cd4a6281d4
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

# event ############################################################################################
ctb0066_event <- google_sheet(ctb0066_ids$gs_id, ctb0066_ids$gid_event)
str(ctb0066_event)

# PROCESS FIELDS

# NOTE. This study contains more data in the annex of the original publication. The data must be
# digitized and added to the dataset in the future.

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0066_event, old = "ID do evento", new = "observacao_id")
ctb0066_event[, observacao_id := as.character(observacao_id)]
ctb0066_event[, .N, by = observacao_id]
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
# UTM coordinates
data.table::setnames(ctb0066_event, old = "Longitude", new = "coord_x")
ctb0066_event[, coord_x := as.numeric(coord_x)]
summary(ctb0066_event[, coord_x])

# coord_y
# Latitude -> coord_y
# UTM coordinates
data.table::setnames(ctb0066_event, old = "Latitude", new = "coord_y")
ctb0066_event[, coord_y := as.numeric(coord_y)]
summary(ctb0066_event[, coord_y])

# Check for duplicate coordinates
ctb0066_event[, coord_duplicated := .N > 1, by = .(coord_x, coord_y)]
ctb0066_event[coord_duplicated == TRUE, .(observacao_id, coord_x, coord_y)]
# There are no duplicated coordinates

# Datum(coord) -> coord_datum
# The coordinate reference system used for the geographical data in the study is the Universal
# Transverse Mercator (UTM) projection with Horizontal Datum: SAD 69. This information is displayed
# explicitly on the maps and figures related to the study area, such as the Map of Unidades
# Geológicas Quaternárias (UQ) (Figure 5) and the map showing the location of the points of
# investigation (Figure 11)
data.table::setnames(ctb0066_event, old = "Datum (coord)", new = "coord_datum")
ctb0066_event[coord_datum == "SAD69 / UTM zone 23S", coord_datum := 29193]
ctb0066_event[, coord_datum := as.integer(coord_datum)]
ctb0066_event[, .N, by = coord_datum]

# Transform coordinates from SAD69 / UTM zone 23S (EPSG: 29193) to WGS84 (EPSG: 4326)
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

# Remove temporary objects
rm(ctb0066_event_sf, new_coords)

# Precisão (coord) -> coord_precisao
# The original document does not provide information about the coordinate precision. Considering
# the reliance on standard topographic maps (1:50,000), large-scale aerial photographs (1:35,000),
# and civilian-grade GPS technology (Garmin12/XL), a realistic estimate of the absolute positional
# error for the recorded coordinates is likely in the range of 5 to 10 meters.
data.table::setnames(ctb0066_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0066_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0066_event[is.na(coord_precisao) & !is.na(coord_x) & !is.na(coord_y), coord_precisao := 10]
summary(ctb0066_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0066_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0066_event[, coord_fonte := as.character(coord_fonte)]
ctb0066_event[, .N, by = coord_fonte]

# País -> pais_id
data.table::setnames(ctb0066_event, old = "País", new = "pais_id")
ctb0066_event[, pais_id := as.character(pais_id)]
ctb0066_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0066_event, old = "Estado (UF)", new = "estado_id")
ctb0066_event[, estado_id := as.character(estado_id)]
ctb0066_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0066_event, old = "Município", new = "municipio_id")
ctb0066_event[, municipio_id := as.character(municipio_id)]
ctb0066_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# The study opted for the collection of simple samples (amostras simples) at specific depth
# intervals, collected from trenches (trincheiras) and auger holes (tradagens). We will assume
# an area of 1 m² for each sample.
data.table::setnames(ctb0066_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0066_event[, amostra_area := as.numeric(amostra_area)]
ctb0066_event[is.na(amostra_area), amostra_area := 1]
summary(ctb0066_event[, amostra_area])

# taxon_sibcs
# The soil classification is based on the Brazilian Soil Classification System (SiBCS). It is
# available in the original publication, but not in the Google Sheets document. So, we set N/A.
ctb0066_event[, taxon_sibcs := NA_character_]

# taxon_st
# The US Soil Taxonomy classification is not available in the original publication, so we set N/A.
ctb0066_event[, taxon_st := NA_character_]

# pedregosidade
# There is no mention to stoniness in the original publication, so we set N/A.
ctb0066_event[, pedregosidade := NA_character_]

# rochosidade
# There is no mention to rockiness in the original publication, so we set N/A.
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

# terrafina
# The terrafina (fine earth fraction) is not available in the original publication. However, this
# information could be infered from other features. This will be possible when the complete data 
# from the annex of the original publication is digitized and added to the dataset. For now, we
# set NA.
ctb0066_layer[, terrafina := NA_real_]

# areia
# old: "Areia total (%)"
# new: areia
# Only 21 samples of soil (all taken at the 0−20 cm depth) were selected for textural analysis
# (sand, silt, and clay). Organossols and Melanic Gleissols were generally excluded from routine
# granulometric analysis.
data.table::setnames(ctb0066_layer, old = "Areia total (%)", new = "areia")
ctb0066_layer[, areia := areia * 10]
summary(ctb0066_layer[, areia])
# There are 183 layers missing sand content
ctb0066_layer[!is.na(areia), .N]
check_empty_layer(ctb0066_layer, "areia")

# silte
# old: Silte (%)
# new: silte
data.table::setnames(ctb0066_layer, old = "Silte (%)", new = "silte")
ctb0066_layer[, silte := silte * 10]
summary(ctb0066_layer[, silte])
# there are 183 layers missing silt content
ctb0066_layer[!is.na(silte), .N]
check_empty_layer(ctb0066_layer, "silte")

# argila
# old: Argila (%)
# new: argila
data.table::setnames(ctb0066_layer, old = "Argila (%)", new = "argila")
ctb0066_layer[, argila := argila * 10]
summary(ctb0066_layer[, argila])
# there are 184 layers missing clay content
ctb0066_layer[!is.na(argila), .N]
check_empty_layer(ctb0066_layer, "argila")

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0066_layer[, psd := argila + silte + areia]
psd_lims <- 900:1100
# Check the limits
ctb0066_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.

# carbono
# old: C (%)
# new: carbono
data.table::setnames(ctb0066_layer, old = "C (%)", new = "carbono")
ctb0066_layer[, carbono := carbono * 10]
summary(ctb0066_layer[, carbono])
# There are 3 layers missing carbon content: P49A, P49B, and P48. All of the 0-20 cm depth.
# This data is available in the document in the form of figures, so it can be digitized in the
# future.
check_empty_layer(ctb0066_layer, "carbono")

# ctc
# old: T (mmolc/dm^3)
# new: ctc
data.table::setnames(ctb0066_layer, old = "T (mmolc/dm^3)", new = "ctc")
ctb0066_layer[, ctc := as.numeric(ctc)]
summary(ctb0066_layer[, ctc])
# There are 3 layers missing ctc content. The same layers missing carbon content.
check_empty_layer(ctb0066_layer, "ctc")

# ph
# old: pH em CaCl_2
# new: ph
data.table::setnames(ctb0066_layer, old = "pH em CaCl_2", new = "ph")
ctb0066_layer[, ph := as.numeric(ph)]
summary(ctb0066_layer[, ph])
# There are 3 layers missing pH content. The same layers missing carbon content.
check_empty_layer(ctb0066_layer, "ph")

# dsi
# There is no explicit mention or reported measurement of soil bulk density.
ctb0066_layer[, dsi := NA_real_]

str(ctb0066_layer)

# Merge ############################################################################################
# events and layers
ctb0066 <- merge(ctb0066_event, ctb0066_layer, all = TRUE)
ctb0066[, dataset_id := "ctb0066"]

# citation
ctb0066 <- merge(ctb0066, ctb0066_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0066)
# Layers: 199
# Events: 31
# Georeferenced events: 31

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
