# autor: Alessandro Samuel-Rosa
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0017
# Dados de 'Solos da Bacia Hidrográfica do Rio Uberaba'
# https://drive.google.com/drive/folders/1CtxewFfjgRQuWTFO6fjZ_M3-w-m2Glds?usp=drive_link
gs <- "1WBaSoLQDucp8_wXv9hMs8sT0ZEUOLfC_2fM-9Pr5OwE"
gid_validation <- 88779986
gid_citation <- 0
gid_event <- 1628657862
gid_layer <- 771766248

# validation #######################################################################################
ctb0017_validation <- google_sheet(gs, gid_validation)
str(ctb0017_validation)

# Check for negative validation results
sum(ctb0017_validation == FALSE, na.rm = TRUE)

# citation #########################################################################################
ctb0017_citation <- google_sheet(gs, gid_citation)
str(ctb0017_citation)

# dataset_titulo
dataset_titulo <- ctb0017_citation[campo == "Título", valor]

# dataset_licenca
dataset_licenca <- ctb0017_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0017_citation <- data.table::data.table(
  dataset_id = "ctb0017",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0017_citation)

# event ############################################################################################
ctb0017_event <- google_sheet(gs, gid_event)
str(ctb0017_event)

# Process fields

# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0017_event, old = "ID do evento", new = "observacao_id")
ctb0017_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0017_event[, observacao_id]) > 1)

# old: Ano (coleta)
# new: data_ano
# The document reports that the data was collected in 2009 and 2010. We will assume that all data
# was collected in 2009.
data.table::setnames(ctb0017_event, old = "Ano (coleta)", new = "data_ano")
ctb0017_event[, data_ano := as.integer(data_ano)]
ctb0017_event[, data_ano := 2009]

# ano_fonte = "estimativa"
# As per the original document, the data was collected in 2009 and 2010. We assumed all data was
# collected in 2009. However, since the exact collection date was not specified, we made an estimate.
ctb0017_event[!is.na(data_ano) , ano_fonte := "estimativa"]

# old: Longitude [grau]
# new: coord_x
data.table::setnames(ctb0017_event, old = "Longitude [grau]", new = "coord_x")
ctb0017_event[, coord_x := as.numeric(coord_x)]
summary(ctb0017_event[, coord_x])

# old: Latitude [grau]
# new: coord_y
data.table::setnames(ctb0017_event, old = "Latitude [grau]", new = "coord_y")
ctb0017_event[, coord_y := as.numeric(coord_y)]
summary(ctb0017_event[, coord_y])

# Check for duplicate coordinates
ctb0017_event[, .N, by = .(coord_x, coord_y)][N > 1]

# old: Datum (coord)
# new: coord_datum
data.table::setnames(ctb0017_event, old = "Datum (coord)", new = "coord_datum")
ctb0017_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0017_event[, coord_datum := as.integer(coord_datum)]
ctb0017_event[, .N, by = coord_datum]

# old: Precisão (coord) [m]
# new: coord_precisao
data.table::setnames(ctb0017_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0017_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0017_event[, coord_precisao])

# old: Fonte (coord)
# new: coord_fonte
data.table::setnames(ctb0017_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0017_event[, coord_fonte := as.character(coord_fonte)]
ctb0017_event[, .N, by = coord_fonte]

# old: País
# new: pais_id
data.table::setnames(ctb0017_event, old = "País", new = "pais_id")
ctb0017_event[, pais_id := as.character(pais_id)]
ctb0017_event[, .N, by = pais_id]

# old: Estado (UF)
# new: estado_id
data.table::setnames(ctb0017_event, old = "Estado (UF)", new = "estado_id")
ctb0017_event[, estado_id := as.character(estado_id)]
ctb0017_event[, .N, by = estado_id]

# old: Município
# new: municipio_id
data.table::setnames(ctb0017_event, old = "Município", new = "municipio_id")
ctb0017_event[, municipio_id := as.character(municipio_id)]
ctb0017_event[, .N, by = municipio_id]

# old: Área amostrada [m^2]
# new: amostra_area
data.table::setnames(ctb0017_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0017_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0017_event[, amostra_area])

# old: Classificação de solo
# new: taxon_sibcs
data.table::setnames(ctb0017_event, old = "Classificação de solo", new = "taxon_sibcs")
ctb0017_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0017_event[, .N, by = taxon_sibcs]

# taxon_st
# Soil Taxonomy classification not available in this dataset.
ctb0017_event[, taxon_st := NA_character_]

# old: Pedregosidade
# new: pedregosidade
data.table::setnames(ctb0017_event, old = "Pedregosidade", new = "pedregosidade")
ctb0017_event[, pedregosidade := as.character(pedregosidade)]
ctb0017_event[, .N, by = pedregosidade]

# old: Rochosidade
# new: rochosidade
data.table::setnames(ctb0017_event, old = "Rochosidade", new = "rochosidade")
ctb0017_event[, rochosidade := as.character(rochosidade)]
ctb0017_event[, .N, by = rochosidade]

str(ctb0017_event)

# layer ############################################################################################
ctb0017_layer <- google_sheet(gs, gid_layer)
str(ctb0017_layer)

# Process fields

# ID do evento -> observacao_id
# Each event has only two layers
data.table::setnames(ctb0017_layer, old = "ID do evento", new = "observacao_id")
ctb0017_layer[, observacao_id := as.character(observacao_id)]
ctb0017_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
# Sampled layers are named "A" and "B"
data.table::setnames(ctb0017_layer, old = "ID da camada", new = "camada_nome")
ctb0017_layer[, camada_nome := as.character(camada_nome)]
ctb0017_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0017_layer, old = "ID da amostra", new = "amostra_id")
ctb0017_layer[, amostra_id := as.character(amostra_id)]
ctb0017_layer[, .N, by = amostra_id]

# Profundidade inicial [cm] -> profund_sup
data.table::setnames(ctb0017_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0017_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0017_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0017_layer[, profund_sup])

# Profundidade final [cm] -> profund_inf
data.table::setnames(ctb0017_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0017_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0017_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0017_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0017_layer[, profund_inf])

# camada_id
ctb0017_layer <- ctb0017_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0017_layer[, camada_id := 1:.N, by = observacao_id]
ctb0017_layer[, .N, by = camada_id]

# Check for missing layers
any_missing_layer(ctb0017_layer)

# old: Terra fina (mm) [%]
# new: terrafina
data.table::setnames(ctb0017_layer, old = "Terra fina (mm) [%]", new = "terrafina")
ctb0017_layer[, terrafina := as.numeric(terrafina) * 10]
# WARNING: One A layer and two B layers have 0 values for terrafina. This is unlikely to be correct.
# We will temporarily replace these values with 1000 (maximum value observed in the dataset).
ctb0017_layer[terrafina == 0, .(observacao_id, camada_nome, terrafina)]
ctb0017_layer[camada_nome == "A|B" & terrafina == 0, terrafina := 1000]
summary(ctb0017_layer[, terrafina])
# The only layer missing data on fine earth is one R layer. 
ctb0017_layer[is.na(terrafina), .(observacao_id, camada_nome, profund_sup, profund_inf, terrafina)]

# Argila (mm) [%] -> argila
data.table::setnames(ctb0017_layer, old = "Argila (mm) [%]", new = "argila")
ctb0017_layer[, argila := as.numeric(argila) * 10]
summary(ctb0017_layer[, argila])
# The only layer missing data on clay is one R layer.
ctb0017_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# Silte (mm) [%] -> silte
data.table::setnames(ctb0017_layer, old = "Silte (mm) [%]", new = "silte")
ctb0017_layer[, silte := as.numeric(silte) * 10]
summary(ctb0017_layer[, silte])
# The only layer missing data on silt is one R layer.
ctb0017_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# Areia (mm) [%] -> areia
data.table::setnames(ctb0017_layer, old = "Areia (mm) [%]", new = "areia")
ctb0017_layer[, areia := as.numeric(areia) * 10]
summary(ctb0017_layer[, areia])
# The only layer missing data on sand is one R layer.
ctb0017_layer[is.na(areia), .(observacao_id, camada_nome, profund_sup, profund_inf, areia)]

# C [dag/kg^1] -> carbono
data.table::setnames(ctb0017_layer, old = "C [dag/kg^1]", new = "carbono")
ctb0017_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0017_layer[, carbono])
# The only layer missing data on carbon is one R layer.
ctb0017_layer[is.na(carbono), .(observacao_id, camada_id, camada_nome, profund_sup, profund_inf, carbono)]

# T [cmolc/dm^3] -> ctc
data.table::setnames(ctb0017_layer, old = "T [cmolc/dm^3]", new = "ctc")
ctb0017_layer[, ctc := as.numeric(ctc)]
summary(ctb0017_layer[, ctc])
# The only layer missing data on cation exchange capacity is one R layer.
ctb0017_layer[is.na(ctc), .(observacao_id, camada_id, camada_nome, profund_sup, profund_inf, ctc)]

# pH em Água -> ph
data.table::setnames(ctb0017_layer, old = "pH em Água", new = "ph")
ctb0017_layer[, ph := as.numeric(ph)]
summary(ctb0017_layer[, ph])
# The only layer missing data on pH is one R layer.
ctb0017_layer[is.na(ph), .(observacao_id, camada_id, camada_nome, profund_sup, profund_inf, ph)]

# Densidade do solo [g/cm^3] -> dsi
data.table::setnames(ctb0017_layer, old = "Densidade do solo [g/cm^3]", new = "dsi")
ctb0017_layer[, dsi := as.numeric(dsi)]
summary(ctb0017_layer[, dsi])
# The only layer missing data on soil density is one R layer.
ctb0017_layer[is.na(dsi), .(observacao_id, camada_id, camada_nome, profund_sup, profund_inf, dsi)]

str(ctb0017_layer)

# Merge ############################################################################################
# events and layers
ctb0017 <- merge(ctb0017_event, ctb0017_layer, all = TRUE)
ctb0017[, dataset_id := "ctb0017"]
# citation
ctb0017 <- merge(ctb0017, ctb0017_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0017)
# Layers: 167
# Events: 83
# Georeferenced events: 83

# Plot using mapview
if (FALSE) {
  ctb0017_sf <- sf::st_as_sf(
    ctb0017[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0017_sf["argila"])
}

# Write to disk ####################################################################################
ctb0017 <- select_output_columns(ctb0017)
data.table::fwrite(ctb0017, "ctb0017/ctb0017.csv")
data.table::fwrite(ctb0017_event, "ctb0017/ctb0017_event.csv")
data.table::fwrite(ctb0017_layer, "ctb0017/ctb0017_layer.csv")
