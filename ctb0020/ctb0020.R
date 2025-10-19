# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("openxlsx")) {
  install.packages("openxlsx")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0020
# Atributos físico-hídricos do solo sob diferentes períodos de palhada no Cerrado da Região
# Sudoeste do Estado do Piauí, Brasil
file_path <- "~/ownCloud/febr-repo/processamento/ctb0020/2021-01-15-ctb0020-embargado.xlsx"
xlsx <- path.expand(file_path)

# citation #########################################################################################
ctb0020_citation <- openxlsx::read.xlsx(xlsx, sheet = "identificacao")
ctb0020_citation <- data.table::as.data.table(ctb0020_citation)
str(ctb0020_citation)

# dataset_titulo
dataset_titulo <- ctb0020_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0020_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0020_citation <- data.table::data.table(
  dataset_id = "ctb0020",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0020_citation)

# event ############################################################################################
ctb0020_event <- openxlsx::read.xlsx(xlsx, sheet = "observacao")
ctb0020_event <- data.table::as.data.table(ctb0020_event)
str(ctb0020_event)

# Process fields

# observacao_id
ctb0020_event[, observacao_id := as.character(observacao_id)]
# Check if there are duplicated IDs
ctb0020_event[, .N, by = observacao_id][N > 1]

# old: observacao_data
# new: data_ano
data.table::setnames(ctb0020_event, old = "observacao_data", new = "data_ano")
ctb0020_event[, .N, by = data_ano]
# Convert Excel date to R Date and then extract the year
t0 <- "1899-12-30"
ctb0020_event[, data_ano := as.Date(data_ano, origin = t0, format = "%Y-%m-%d")]
ctb0020_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0020_event[, .N, by = data_ano]

# ano_fonte
# The year of sampling is informed in the publication.
ctb0020_event[, ano_fonte := "original"]
ctb0020_event[, .N, by = ano_fonte]

# coord_x
# Longitude is recorded as character using the following format: "45°14'04\"
# Get the degrees, minutes and seconds using regular expressions.
# Then convert to decimal degrees.
ctb0020_event[, coord_x := gsub(pattern = "°", replacement = " + ", x = coord_x)]
ctb0020_event[, coord_x := gsub(pattern = "'", replacement = "/60 + ", x = coord_x)]
ctb0020_event[, coord_x := gsub(pattern = "\"", replacement = "/3600", x = coord_x)]
# Now evaluate the expression for each row
ctb0020_event[, coord_x := eval(parse(text = coord_x)) * -1, by = .I]
ctb0020_event[, coord_x := as.numeric(coord_x)]
summary(ctb0020_event[, coord_x])

# coord_y
# Latitude is recorded as character using the following format: "9°23'32\""
# Get the degrees, minutes and seconds using regular expressions.
# Then convert to decimal degrees.
ctb0020_event[, coord_y := gsub(pattern = "°", replacement = " + ", x = coord_y)]
ctb0020_event[, coord_y := gsub(pattern = "'", replacement = "/60 + ", x = coord_y)]
ctb0020_event[, coord_y := gsub(pattern = "\"", replacement = "/3600", x = coord_y)]
# Now evaluate the expression for each row
ctb0020_event[, coord_y := eval(parse(text = coord_y)) * -1, by = .I]
ctb0020_event[, coord_y := as.numeric(coord_y)]
summary(ctb0020_event[, coord_y])

# old: coord_sistema
# new: coord_datum
data.table::setnames(ctb0020_event, old = "coord_sistema", new = "coord_datum")
ctb0020_event[, coord_datum := gsub("EPSG:", "", x = coord_datum)]
ctb0020_event[, coord_datum := as.integer(coord_datum)]
ctb0020_event[, .N, by = coord_datum]

# Check for duplicate coordinates
ctb0020_event[, .N, by = .(coord_x, coord_y)][N > 1]
# Two fields were sampled in each farm/treatment. They have the same coordinates.
# We will jitter the coordinates up to 30 m to make them unique. But first we need to transform
# to a projected coordinate system (SIRGAS 2000 / UTM zone 24S - EPSG:32724), then jitter, and
# finally transform back to WGS 84 (EPSG:4326).
id_duplicated <- ctb0020_event[, duplicated(.SD, by = c("coord_x", "coord_y"))]
ctb0020_event_sf <- sf::st_as_sf(
  ctb0020_event[id_duplicated],
  coords = c("coord_x", "coord_y"), crs = 4326
)
ctb0020_event_sf <- sf::st_transform(ctb0020_event_sf, crs = 32724)
set.seed(1984)
amount <- 30 # meters
# Jitter the coordinates up to 'amount' meters
ctb0020_event_sf <- sf::st_jitter(ctb0020_event_sf, amount = amount)
ctb0020_event_sf <- sf::st_transform(ctb0020_event_sf, crs = 4326)
ctb0020_event_sf <- sf::st_coordinates(ctb0020_event_sf)
ctb0020_event[id_duplicated, coord_x := ctb0020_event_sf[, 1]]
ctb0020_event[id_duplicated, coord_y := ctb0020_event_sf[, 2]]
# Check for duplicate coordinates
ctb0020_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_fonte
# The coordinates were obtained using a GPS device.
ctb0020_event[, coord_fonte := as.character(coord_fonte)]
ctb0020_event[, .N, by = coord_fonte]

# coord_precisao
ctb0020_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0020_event[, coord_precisao])
# The precision of the coordinates is informed in the publication as being 10 m. We add 30 m to it,
# due to the jittering process above.
ctb0020_event[!is.na(coord_precisao), coord_precisao := coord_precisao + amount]
summary(ctb0020_event[, coord_precisao])

# pais_id
ctb0020_event[, pais_id := as.character(pais_id)]
ctb0020_event[, .N, by = pais_id]

# estado_id
ctb0020_event[, estado_id := as.character(estado_id)]
ctb0020_event[, .N, by = estado_id]

# municipio_id
ctb0020_event[, municipio_id := as.character(municipio_id)]
ctb0020_event[, .N, by = municipio_id]

# amostra_area
# The area of each sample is not informed in the publication.
ctb0020_event[, amostra_area := NA_real_]

# taxon_sibcs
# old: SiBCS
# new: taxon_sibcs
# Soil classification apparently was infered by the authors using little information, possibly
# guessing from previous knowledge of the region. The clay content (see below) is not enough to
# classify the soil as Latossolo for various profiles.
data.table::setnames(ctb0020_event, old = "SiBCS", new = "taxon_sibcs")
ctb0020_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0020_event[, .N, by = taxon_sibcs]

# taxon_st
# The soil classification according to the Soil Taxonomy is not available in this dataset.
ctb0020_event[, taxon_st := NA_character_]

# pedregosidade
# The study did not inform the presence of stones in the soil profiles. However, we can guess from
# the soil texture, the soil class and the region that there are no stones in the profiles.
# Therefore, we assume that there are no stones in the profiles.
ctb0020_event[, pedregosidade := "ausente"]

# rochosidade
# The study did not inform the presence of bedrock in the soil profiles. However, we can guess
# from the soil texture, the soil class and the region that there is no bedrock in the profiles.
# Therefore, we assume that there is no bedrock in the profiles.
ctb0020_event[, rochosidade := "ausente"]

str(ctb0020_event)

# layer ############################################################################################
ctb0020_layer <- openxlsx::read.xlsx(xlsx, sheet = "camada")
ctb0020_layer <- data.table::as.data.table(ctb0020_layer)
str(ctb0020_layer)

# Process fields

# observacao_id
# Each sampling point has four layers.
ctb0020_layer[, observacao_id := as.character(observacao_id)]
ctb0020_layer[, .N, by = observacao_id] # should be 4

# camada_nome
ctb0020_layer[, camada_nome := as.character(camada_nome)]
ctb0020_layer[, .N, by = camada_nome]

# old: ID da amostra
# new: amostra_id
# amostra_id in this dataset indicates the field ID in a farm/treatment
# Two fields were sampled in each farm/treatment.
ctb0020_layer[, amostra_id := as.character(amostra_id)]
ctb0020_layer[, .N, by = amostra_id]

# profund_sup
ctb0020_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0020_layer[, profund_sup])

# profund_inf
ctb0020_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0020_layer[, profund_inf])

# Check for missing layers
any_missing_layer(ctb0020_layer)

# camada_id
ctb0020_layer <- ctb0020_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0020_layer[, camada_id := 1:.N, by = observacao_id]
ctb0020_layer[, .N, by = camada_id]

# terrafina
# The fine earth fraction in not informed in the dataset and the publication is not available.
# Based on the soil texture and class, we assume that the fine earth fraction is 100%.
ctb0020_layer[, terrafina := 1000]

# argila
ctb0020_layer[, argila := as.numeric(argila)]
summary(ctb0020_layer[, argila])

# silte
ctb0020_layer[, silte := as.numeric(silte)]
summary(ctb0020_layer[, silte])

# old: areia_total
# new: areia
data.table::setnames(ctb0020_layer, old = "areia_total", new = "areia")
ctb0020_layer[, areia := as.numeric(areia)]
summary(ctb0020_layer[, areia])

# old: carbono_cromo_xxx_xxx
# new: carbono
data.table::setnames(ctb0020_layer, old = "carbono_cromo_xxx_xxx", new = "carbono")
ctb0020_layer[, carbono := as.numeric(carbono)]
summary(ctb0020_layer[, carbono])

# ctc
# The cation exchange capacity is not informed in the dataset and the publication is not available.
ctb0020_layer[, ctc := NA_real_]

# ph
# The pH is not informed in the dataset and the publication is not available.
ctb0020_layer[, ph := NA_real_]

# dsi_cilindro -> dsi
data.table::setnames(ctb0020_layer, old = "dsi_cilindro", new = "dsi")
ctb0020_layer[, dsi := as.numeric(dsi)]
summary(ctb0020_layer[, dsi])

str(ctb0020_layer)

# Merge ############################################################################################
# events and layers
ctb0020 <- merge(ctb0020_event, ctb0020_layer, by = "observacao_id", all = TRUE)
ctb0020[, dataset_id := "ctb0020"]
# citation
ctb0020 <- merge(ctb0020, ctb0020_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0020)
# Layers: 136
# Events: 34
# Georeferenced events: 34

# Check soil classification with respect to the clay content
# For each soil profile (event), get the maximum clay and sand contents for() all layers. If the
# maximum clay content is greater than 150 g/kg and the maximum sand content is less than 700 g/kg,
# the soil classification remains Latossolo. Otherwise, it is changed to Neossolo Quartzarênico.
ctb0020[, max_clay := max(argila, na.rm = TRUE), by = observacao_id]
ctb0020[, max_sand := max(areia, na.rm = TRUE), by = observacao_id]
unique(ctb0020[, .(max_clay, max_sand, taxon_sibcs), by = observacao_id])
# Check if the maximum clay content is greater than 150 g/kg and the maximum sand content is less
# than 700 g/kg
ctb0020[, taxon_sibcs :=
  ifelse(max_clay > 150 & max_sand < 700, "Latossolo", "Neossolo Quartzarênico")]
ctb0020[, .N, by = .(taxon_sibcs)]
# remove soil_class
ctb0020[, max_clay := NULL]

# Plot using mapview
if (FALSE) {
  ctb0020_sf <- sf::st_as_sf(
    ctb0020[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0020_sf["argila"])
}

# Write to disk ####################################################################################
ctb0020 <- select_output_columns(ctb0020)
data.table::fwrite(ctb0020, file = "ctb0020/ctb0020.csv")
data.table::fwrite(ctb0020_event, file = "ctb0020/ctb0020_event.csv")
data.table::fwrite(ctb0020_layer, file = "ctb0020/ctb0020_layer.csv")
