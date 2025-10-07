# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("mapview")) {
  install.packages("mapview")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0054
# Dados de "Solos da Reserva Particular do Patrimônio Natural SESC Pantanal"
# https://drive.google.com/drive/u/0/folders/1IH0D6uQrOD7FHWVDl-AEy0XKZZedqevi
gs <- "1nmJOv7Au9EmQRgNUVptXCQTRa1J2JXzlbN0mjckO3hY"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0054_validation <- google_sheet(gs, gid_validation)
str(ctb0054_validation)

# Check for negative validation results
sum(ctb0054_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0054_citation <- google_sheet(gs, gid_citation)
str(ctb0054_citation)

# dataset_titulo
dataset_titulo <- ctb0054_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0054_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0054_citation <- data.table::data.table(
  dataset_id = "ctb0054",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0054_citation)

# event ############################################################################################
ctb0054_event <- google_sheet(gs, gid_event)
str(ctb0054_event)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0054_event, old = "ID do evento", new = "observacao_id")
ctb0054_event[, observacao_id := as.character(observacao_id)]
ctb0054_event[, .N, by = observacao_id][N > 1]

# Ano de coleta -> data_ano
data.table::setnames(ctb0054_event, old = "Ano de coleta", new = "data_ano")
ctb0054_event[, data_ano := as.integer(data_ano)]
ctb0054_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta é informada no trabalho de origem dos dados.
ctb0054_event[!is.na(data_ano), ano_fonte := "original"]
ctb0054_event[, .N, by = ano_fonte]

# X (m) -> coord_x
data.table::setnames(ctb0054_event, old = "X (m)", new = "coord_x")
ctb0054_event[, coord_x := as.numeric(coord_x)]
summary(ctb0054_event[, coord_x])

# Y (m) -> coord_y
data.table::setnames(ctb0054_event, old = "Y (m)", new = "coord_y")
ctb0054_event[, coord_y := as.numeric(coord_y)]
summary(ctb0054_event[, coord_y])

# EPSG -> coord_datum
data.table::setnames(ctb0054_event, old = "EPSG", new = "coord_datum")
ctb0054_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0054_event[, coord_datum := as.integer(coord_datum)]
ctb0054_event[, .N, by = coord_datum]

# transform coordinates from EPSG:29191 to EPSG:4326
ctb0054_event_sf <- sf::st_as_sf(ctb0054_event, coords = c("coord_x", "coord_y"), crs = 29191)
ctb0054_event_sf <- sf::st_transform(ctb0054_event_sf, 4326)
ctb0054_event_sf <- sf::st_coordinates(ctb0054_event_sf)
ctb0054_event[, coord_x := ctb0054_event_sf[, 1]]
ctb0054_event[, coord_y := ctb0054_event_sf[, 2]]
ctb0054_event[, coord_datum := 4326]
rm(ctb0054_event_sf)
ctb0054_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0054_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Fonte das coordenadas -> coord_fonte
data.table::setnames(ctb0054_event, old = "Fonte das coordenadas", new = "coord_fonte")
ctb0054_event[, coord_fonte := as.character(coord_fonte)]
ctb0054_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is missing. As the source of the coordinates is GPS and Google
# My Maps, we set it to 30 m.
data.table::setnames(ctb0054_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0054_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0054_event[is.na(coord_precisao), coord_precisao := 30]
summary(ctb0054_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0054_event, old = "País", new = "pais_id")
ctb0054_event[, pais_id := as.character(pais_id)]
ctb0054_event[, .N, by = pais_id]

# Estado -> estado_id
data.table::setnames(ctb0054_event, old = "Estado", new = "estado_id")
ctb0054_event[, estado_id := as.character(estado_id)]
ctb0054_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0054_event, old = "Município", new = "municipio_id")
ctb0054_event[, municipio_id := as.character(municipio_id)]
ctb0054_event[, .N, by = municipio_id]

# amostra_area
# The area of the event is missing. We set it to 1 m^2 because all events are soil profiles.
ctb0054_event[, amostra_area := 1]

# taxon_sibcs
# old: Classificação do solo
# new: taxon_sibcs
data.table::setnames(ctb0054_event, old = "Classificação do solo", new = "taxon_sibcs")
ctb0054_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0054_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo pelo Soil Taxonomy não está disponível neste dataset.
ctb0054_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# review the work at another time

ctb0054_event[, pedregosidade := ("Não Pedregoso")]

# Rochosidade (superficie)
# review the work at another time

ctb0054_event[, rochosidade := ("Não Rochoso")]

str(ctb0054_event)

# layer ############################################################################################
ctb0054_layer <- google_sheet(gs, gid_layer)
str(ctb0054_layer)

# Process fields
# ID do evento -> observacao_id
data.table::setnames(ctb0054_layer, old = "ID do evento", new = "observacao_id")
ctb0054_layer[, observacao_id := as.character(observacao_id)]
ctb0054_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0054_layer, old = "ID da camada", new = "camada_nome")
ctb0054_layer[, camada_nome := as.character(camada_nome)]
ctb0054_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0054_layer[, amostra_id := NA_character_]

# Profundidade superior [cm] -> profund_sup
# Some layers have missing values for the upper depth. These are not entire layers, but parts of
# layers. We will remove them.
data.table::setnames(ctb0054_layer, old = "Profundidade superior [cm]", new = "profund_sup")
ctb0054_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0054_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0054_layer[, profund_sup])
ctb0054_layer <- ctb0054_layer[!is.na(profund_sup), ]

# Profundidade inferior [cm] -> profund_inf
data.table::setnames(ctb0054_layer, old = "Profundidade inferior [cm]", new = "profund_inf")
ctb0054_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0054_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0054_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0054_layer[, profund_inf])

# check for missing layers
check_missing_layer(ctb0054_layer)

# compute mid depth
ctb0054_layer[, profund_mid := (profund_sup + profund_inf) / 2]
summary(ctb0054_layer[, profund_mid])

# camada_id
ctb0054_layer <- ctb0054_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0054_layer[, camada_id := 1:.N, by = observacao_id]
ctb0054_layer[, .N, by = camada_id]

# terrafina
# terra fina is missing. We assume it is NA
ctb0054_layer[, terrafina := NA_real_]

# Argila total [%] * 10 -> argila
data.table::setnames(ctb0054_layer, old = "Argila total [%]", new = "argila")
ctb0054_layer[, argila := as.numeric(argila) * 10]
summary(ctb0054_layer[, argila])

# Silte [%] * 10 -> silte
data.table::setnames(ctb0054_layer, old = "Silte [%]", new = "silte")
ctb0054_layer[, silte := as.numeric(silte) * 10]
summary(ctb0054_layer[, silte])

# Areia total [%] * 10 -> areia
data.table::setnames(ctb0054_layer, old = "Areia total [%]", new = "areia")
ctb0054_layer[, areia := as.numeric(areia) * 10]
summary(ctb0054_layer[, areia])

# Check the particle size distribution
# The sum of the particle size distribution should be 1000.
ctb0054_layer[, psd := argila + silte + areia]
psd_lims <- 950:1050
ctb0054_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 3 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd not in the range
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0054_layer[!psd %in% psd_lims & !is.na(psd), ..cols]
# These are simple rounding errors. We will correct them by distributing the error evenly.
ctb0054_layer[psd != 1000, argila := round(argila / psd * 1000)]
ctb0054_layer[psd != 1000, silte := round(silte / psd * 1000)]
ctb0054_layer[psd != 1000, areia := round(areia / psd * 1000)]
ctb0054_layer[, psd := argila + silte + areia]
ctb0054_layer[psd != 1000, ]
ctb0054_layer[, psd := NULL]

# Carbono [g/kg] -> carbono
data.table::setnames(ctb0054_layer, old = "Carbono [g/kg]", new = "carbono")
ctb0054_layer[, carbono := as.numeric(carbono)]
summary(ctb0054_layer[, carbono])
check_empty_layer(ctb0054_layer, "carbono")
# fill empty layers
ctb0054_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0054_layer, "carbono")

# Organic layers
# Organic layers have upper depth (profund_sup) lower than 0.
ctb0054_layer[profund_sup < 0, .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]

# ph
# old: pH em H2O
# new: ph
data.table::setnames(ctb0054_layer, old = "pH em H2O", new = "ph")
ctb0054_layer[, ph := as.numeric(ph)]
summary(ctb0054_layer[, ph])
check_empty_layer(ctb0054_layer, "ph")
# fill empty layers
ctb0054_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0054_layer, "ph")

# ctc
# old: CTC [mmolc/kg]
# new: ctc [cmolc/kg]
data.table::setnames(ctb0054_layer, old = "CTC [mmolc/kg]", new = "ctc")
ctb0054_layer[, ctc := as.numeric(ctc) / 10]
summary(ctb0054_layer[, ctc])
check_empty_layer(ctb0054_layer, "ctc")
# fill empty layers
ctb0054_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0054_layer, "ctc")

# dsi
# soil bulk density is missing. We set it to NA_real_
ctb0054_layer[, dsi := NA_real_]

str(ctb0054_layer)

# Merge ############################################################################################
# events and layers
ctb0054 <- merge(ctb0054_event, ctb0054_layer, all = TRUE)
ctb0054[, dataset_id := "ctb0054"]
# citation
ctb0054 <- merge(ctb0054, ctb0054_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0054)
# Layers: 279
# Events: 47
# Georeferenced events: 47

# Plot using mapview
if (FALSE) {
  ctb0054_sf <- sf::st_as_sf(
    ctb0054[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0054_sf["argila"])
}

# Write to disk ####################################################################################
ctb0054 <- select_output_columns(ctb0054)
data.table::fwrite(ctb0054, "ctb0054/ctb0054.csv")
data.table::fwrite(ctb0054_event, "ctb0054/ctb0054_event.csv")
data.table::fwrite(ctb0054_layer, "ctb0054/ctb0054_layer.csv")
