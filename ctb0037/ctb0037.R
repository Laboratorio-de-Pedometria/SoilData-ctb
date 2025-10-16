# autor: Alessandro Samuel-Rosa
# data: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library("data.table")
}
if (!require("sf")) {
  install.packages("sf")
  library("sf")
}
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library("openxlsx")
}
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0037
# Solo da Sub-Bacia do Rio Giruazinho, Giruá, RS
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0037/2022-01-31-ctb0037.xlsx")

# citation #########################################################################################
ctb0037_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0037_citation <- data.table::as.data.table(ctb0037_citation)
str(ctb0037_citation)

# dataset_titulo
dataset_titulo <- ctb0037_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0037_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0037_citation <- data.table::data.table(
  dataset_id = "ctb0037",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0037_citation)

# event ############################################################################################
ctb0037_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0037_event <- data.table::as.data.table(ctb0037_event)
str(ctb0037_event)

# Process fields
# observacao_id
ctb0037_event[, observacao_id := as.character(observacao_id)]
ctb0037_event[, .N, by = observacao_id][N > 1]

# data_ano
# observacao_data
data.table::setnames(ctb0037_event, old = "observacao_data", new = "data_ano")
t0 <- "1899-12-30"
ctb0037_event[, data_ano := as.Date(data_ano, origin = t0, format = "%Y-%m-%d")]
ctb0037_event[, data_ano := as.numeric(format(data_ano, "%Y"))]
ctb0037_event[, .N, by = data_ano]

# ano_fonte
# A data de coleta no campo está especificada no documento de origem dos dados
ctb0037_event[!is.na(data_ano), ano_fonte := "original"]
ctb0037_event[, .N, by = ano_fonte]

# coord_x
ctb0037_event[, coord_x := as.numeric(coord_x)]
summary(ctb0037_event[, coord_x])

# coord_y
ctb0037_event[, coord_y := as.numeric(coord_y)]
summary(ctb0037_event[, coord_y])

# coord_datum
# coord_sistema -> coord_datum
data.table::setnames(ctb0037_event, old = "coord_sistema", new = "coord_datum")
ctb0037_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0037_event[, coord_datum := as.integer(coord_datum)]
ctb0037_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0037_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_precisao
ctb0037_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0037_event[, coord_precisao])

# coord_fonte
ctb0037_event[, coord_fonte := as.character(coord_fonte)]
ctb0037_event[, .N, by = coord_fonte]

# pais_id
ctb0037_event[, pais_id := as.character(pais_id)]
ctb0037_event[, .N, by = pais_id]

# estado_id
ctb0037_event[, estado_id := as.character(estado_id)]
ctb0037_event[, .N, by = estado_id]

# municipio_id
ctb0037_event[, municipio_id := as.character(municipio_id)]
ctb0037_event[, .N, by = municipio_id]

# amostra_area
ctb0037_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0037_event[, amostra_area])

# taxon_sibcs
# taxon_sibcs_2018 -> taxon_sibcs
data.table::setnames(ctb0037_event, old = "taxon_sibcs_2018", new = "taxon_sibcs")
ctb0037_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0037_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset.
ctb0037_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# Não tenho acesso a este trabalho após a inserção das variaveis pedregosidade e rochosidade
# Logo, irei colocar NA_character_ para as variaveis.

ctb0037_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
ctb0037_event[, rochosidade := ("Não Rochoso")]

str(ctb0037_event)

# layer ############################################################################################
ctb0037_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0037_layer <- data.table::as.data.table(ctb0037_layer)
str(ctb0037_layer)

# Process fields
# observacao_id
ctb0037_layer[, observacao_id := as.character(observacao_id)]
ctb0037_layer[, .N, by = observacao_id]

# camada_nome
ctb0037_layer[, camada_nome := as.character(camada_nome)]
ctb0037_layer[, .N, by = camada_nome]

# amostra_id
ctb0037_layer[, amostra_id := as.character(amostra_id)]
ctb0037_layer[, .N, by = amostra_id]

# profund_sup
ctb0037_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0037_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0037_layer[, profund_sup])

# profund_inf
ctb0037_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0037_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0037_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0037_layer[, profund_inf])

# check for missing layers
any_missing_layer(ctb0037_layer)

# camada_id
ctb0037_layer <- ctb0037_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0037_layer[, camada_id := 1:.N, by = observacao_id]
ctb0037_layer[, .N, by = camada_id]

# terrafina
# terra fina is missing. We assume it is 1000 g/kg
ctb0037_layer[, terrafina := 1000]

# areia_naoh_peneira -> areia
data.table::setnames(ctb0037_layer, old = "areia_naoh_peneira", new = "areia")
ctb0037_layer[, areia := as.numeric(areia)]
summary(ctb0037_layer[, areia])

# argila_naoh_pipeta -> argila
data.table::setnames(ctb0037_layer, old = "argila_naoh_pipeta", new = "argila")
ctb0037_layer[, argila := as.numeric(argila)]
summary(ctb0037_layer[, argila])

# silte = 1000 - areia - argila
ctb0037_layer[, silte := 1000 - areia - argila]
summary(ctb0037_layer[, silte])

# carbono_forno_1min950_cgdct -> carbono_seco
# carbono_cromo_30min150_mohr -> carbono_umido
data.table::setnames(ctb0037_layer, old = "carbono_forno_1min950_cgdct", new = "carbono")
data.table::setnames(ctb0037_layer, old = "carbono_cromo_30min150_mohr", new = "carbono_umido")
ctb0037_layer[, carbono := as.numeric(carbono)]
ctb0037_layer[, carbono_umido := as.numeric(carbono_umido)]
carbono_lm <- lm(carbono ~ carbono_umido, data = ctb0037_layer)
summary(carbono_lm)
ctb0037_layer[
  is.na(carbono),
  carbono := predict(carbono_lm, newdata = ctb0037_layer[is.na(carbono), .(carbono_umido)])
]
summary(ctb0037_layer[, carbono])

# ph_h2o_25_eletrodo -> ph
data.table::setnames(ctb0037_layer, old = "ph_h2o_25_eletrodo", new = "ph")
ctb0037_layer[, ph := as.numeric(ph)]
summary(ctb0037_layer[, ph])

# ctc = calcio_kcl_eaa + magnesio_kcl_eaa + potassio_mehlich1_eeac + sodio_mehlich1_eeac +
#       acidez_caoac2ph7_naoh
ctb0037_layer[, ctc := as.numeric(calcio_kcl_eaa) + as.numeric(magnesio_kcl_eaa)]
ctb0037_layer[, ctc := ctc + as.numeric(potassio_mehlich1_eeac) + as.numeric(sodio_mehlich1_eeac)]
ctb0037_layer[, ctc := ctc + as.numeric(acidez_caoac2ph7_naoh)]
summary(ctb0037_layer[, ctc])

# dsi_cilindro -> dsi
data.table::setnames(ctb0037_layer, old = "dsi_cilindro", new = "dsi")
ctb0037_layer[, dsi := as.numeric(dsi)]
summary(ctb0037_layer[, dsi])

str(ctb0037_layer)

# Merge ############################################################################################
# events and layers
ctb0037 <- merge(ctb0037_event, ctb0037_layer, all = TRUE)
ctb0037[, dataset_id := "ctb0037"]
# citation
ctb0037 <- merge(ctb0037, ctb0037_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0037)
# Layers: 849
# Events: 263
# Georeferenced events: 263

# Plot with mapview
if (FALSE) {
  sf_ctb0037 <- sf::st_as_sf(
    ctb0037[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(sf_ctb0037["argila"])
}

# Write to disk ####################################################################################
ctb0037 <- select_output_columns(ctb0037)
data.table::fwrite(ctb0037, "ctb0037/ctb0037.csv")
data.table::fwrite(ctb0037_event, "ctb0037/ctb0037_event.csv")
data.table::fwrite(ctb0037_layer, "ctb0037/ctb0037_layer.csv")
