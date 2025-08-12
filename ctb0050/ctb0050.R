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
if (!require("openxlsx")) {
  install.packages("openxlsx")
}
if (!require("mapview")) {
  install.packages("mapview")
}

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0050
# Projeto RADAMBRASIL. Folha SG.22 Curitiba, parte da folha SG.21 Asunción e folha SG.23 Iguape
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0050/2022-06-06-ctb0050.xlsx")

# citation #########################################################################################
ctb0050_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0050_citation <- data.table::as.data.table(ctb0050_citation)
str(ctb0050_citation)

# dataset_titulo
dataset_titulo <- ctb0050_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0050_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0050_citation <- data.table::data.table(
  dataset_id = "ctb0050",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0050_citation)

# event ############################################################################################
ctb0050_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0050_event <- data.table::as.data.table(ctb0050_event)
str(ctb0050_event)

# Process fields
# observacao_id
ctb0050_event[, observacao_id := as.character(observacao_id)]
ctb0050_event[, .N, by = observacao_id][N > 1]

# data_ano
# observacao_data
data.table::setnames(ctb0050_event, old = "observacao_data", new = "data_ano")
ctb0050_event[, .N, by = data_ano]
t0 <- "1899-12-30"
ctb0050_event[, data_ano := as.Date(data_ano, origin = t0)]
ctb0050_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0050_event[, .N, by = data_ano]
# Many events do not have a date. We will process them later.

# data_fonte
# Para os eventos com data, a data de coleta no campo está especificada no documento de origem dos
# dados
ctb0050_event[!is.na(data_ano), data_fonte := "original"]
ctb0050_event[, .N, by = data_fonte]

# coord_x
ctb0050_event[, coord_x := as.numeric(coord_x)]
summary(ctb0050_event[, coord_x])

# coord_y
ctb0050_event[, coord_y := as.numeric(coord_y)]
summary(ctb0050_event[, coord_y])

# coord_datum
# coord_sistema -> coord_datum
data.table::setnames(ctb0050_event, old = "coord_sistema", new = "coord_datum")
ctb0050_event[, .N, by = coord_datum]
ctb0050_event[coord_datum == "SAD69", coord_datum := 4618]
ctb0050_event[, coord_datum := as.integer(coord_datum)]
ctb0050_event[, .N, by = coord_datum]
# 4618 -> 4326
ctb0050_event_sf <- sf::st_as_sf(ctb0050_event[coord_datum == 4618],
  coords = c("coord_x", "coord_y"), crs = 4618
)
ctb0050_event_sf <- sf::st_transform(ctb0050_event_sf, crs = 4326)
ctb0050_event_sf <- sf::st_coordinates(ctb0050_event_sf)
ctb0050_event[coord_datum == 4618, coord_x := ctb0050_event_sf[, 1]]
ctb0050_event[coord_datum == 4618, coord_y := ctb0050_event_sf[, 2]]
ctb0050_event[coord_datum == 4618, coord_datum := 4326]
rm(ctb0050_event_sf)
ctb0050_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0050_event[, coord_duplicated := .N > 1, by = .(coord_y, coord_x)]
ctb0050_event[coord_duplicated == TRUE & !is.na(coord_x), .(observacao_id, coord_x, coord_y)]

# coord_precisao
# coord_precisao is missing. We assume it is 1 minute, which corresponds to 1.852 km at the equator
ctb0050_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0050_event[!is.na(coord_fonte), coord_precisao := 1852]
summary(ctb0050_event[, coord_precisao])

# coord_fonte
ctb0050_event[, coord_fonte := as.character(coord_fonte)]
ctb0050_event[, .N, by = coord_fonte]

# pais_id
# pais_id is missing. We set it to "BR"
ctb0050_event[, pais_id := "BR"]

# estado_id
# estado_id is missing. We set it to NA_character_
ctb0050_event[, estado_id := NA_character_]

# municipio_id
# municipio_id is missing. We set it to NA_character_
ctb0050_event[, municipio_id := NA_character_]

# amostra_area
# amostra_area is missing. We assume it is 1.0 m^2 (soil profiles)
ctb0050_event[, amostra_area := 1.0]

# taxon_sibcs
# taxon_sibcs_19xx_2
data.table::setnames(ctb0050_event, old = "taxon_sibcs_19xx_2", new = "taxon_sibcs")
ctb0050_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0050_event[, .N, by = taxon_sibcs]

# taxon_st
# Classificação do solo segundo o Soil Taxonomy não está disponível neste dataset
ctb0050_event[, taxon_st := NA_character_]

str(ctb0050_event)

# layer ############################################################################################
ctb0050_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0050_layer <- data.table::as.data.table(ctb0050_layer)
str(ctb0050_layer)

# Process fields
# observacao_id
ctb0050_layer[, observacao_id := as.character(observacao_id)]
ctb0050_layer[, .N, by = observacao_id]

# camada_nome
ctb0050_layer[, camada_nome := as.character(camada_nome)]
ctb0050_layer[, .N, by = camada_nome]

# amostra_id
ctb0050_layer[, amostra_id := as.character(amostra_id)]
ctb0050_layer[, .N, by = amostra_id]

# profund_sup
ctb0050_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0050_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0050_layer[, profund_sup])

# profund_inf
ctb0050_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0050_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0050_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0050_layer[, profund_inf])

# camada_id
ctb0050_layer <- ctb0050_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0050_layer[, camada_id := 1:.N, by = observacao_id]
ctb0050_layer[, .N, by = camada_id]

# check for missing layers
any_missing_layer(ctb0050_layer)
# There is one missing layer in observacao_id = PERFIL-089 between camada_nome = B22 (140-200 cm)
# and camada_nome = B23 (330-450 cm). Given the names of the adjacent layers and their depths, the
# missing layer could be the result of a typo. So we will discard layer B23 for now.
ctb0050_layer <- ctb0050_layer[!(observacao_id == "PERFIL-089" & camada_nome == "B23"), ]
any_missing_layer(ctb0050_layer)

# mid depth
ctb0050_layer[, profund_mid := (profund_sup + profund_inf) / 2]

# terrafina
# terrafina_xxx * 10
data.table::setnames(ctb0050_layer, old = "terrafina_xxx", new = "terrafina")
ctb0050_layer[, terrafina := as.numeric(terrafina) * 10]
summary(ctb0050_layer[, terrafina])

# argila
# argila_sodio_xxx * 10
data.table::setnames(ctb0050_layer, old = "argila_sodio_xxx", new = "argila")
ctb0050_layer[, argila := as.numeric(argila) * 10]
summary(ctb0050_layer[, argila])

# silte
# Silte.-.0,05-0,002.mm * 10
data.table::setnames(ctb0050_layer, old = "Silte.-.0,05-0,002.mm", new = "silte")
ctb0050_layer[, silte := as.numeric(silte) * 10]
summary(ctb0050_layer[, silte])

# areia
# (Areia.grossa.2.-.0,2.mm + Areia.fina.0,2-0,05.mm) * 10
data.table::setnames(ctb0050_layer, old = "Areia.grossa.2.-.0,2.mm", new = "areia_grossa")
data.table::setnames(ctb0050_layer, old = "Areia.fina.0,2-0,05.mm", new = "areia_fina")
ctb0050_layer[, areia := as.numeric(areia_grossa) * 10]
ctb0050_layer[, areia := areia + as.numeric(areia_fina) * 10]
summary(ctb0050_layer[, areia])

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0050_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0050_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0050_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# carbono_xxx_xxx_xxx [%] * 10
data.table::setnames(ctb0050_layer, old = "carbono_xxx_xxx_xxx", new = "carbono")
ctb0050_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0050_layer[, carbono])
check_empty_layer(ctb0050_layer, "carbono")
# Fill empty layer
ctb0050_layer[, carbono := fill_empty_layer(y = carbono, x = profund_mid), by = observacao_id]
check_empty_layer(ctb0050_layer, "carbono")

# ph
# old: ph_h2o_xxx_xxx
data.table::setnames(ctb0050_layer, old = "ph_h2o_xxx_xxx", new = "ph")
# old: PH.(1:2.5).-.H2O
data.table::setnames(ctb0050_layer, old = "PH.(1:2.5).-.H2O", new = "ph2")
ctb0050_layer[, ph := as.numeric(ph)]
ctb0050_layer[is.na(ph), ph := as.numeric(ph2)]
ctb0050_layer[, ph2 := NULL]
summary(ctb0050_layer[, ph])
find_empty_layer(ctb0050_layer, "ph")
# Fill empty layer
ctb0050_layer[, ph := fill_empty_layer(y = ph, x = profund_mid), by = observacao_id]
find_empty_layer(ctb0050_layer, "ph")

# ctc
# T.-.NH4.OAc.pH.7.(CTC) [mE/100g] OR Valor.T.(soma).CTC.mE/100g [mE/100g]
data.table::setnames(ctb0050_layer, old = "T.-.NH4.OAc.pH.7.(CTC)", new = "ctc")
data.table::setnames(ctb0050_layer, old = "Valor.T.(soma).CTC.mE/100g", new = "ctc_soma_calc")
ctb0050_layer[, ctc := as.numeric(ctc)]
ctb0050_layer[is.na(ctc), ctc := as.numeric(ctc_soma_calc)]
ctb0050_layer[, ctc_soma_calc := NULL]
summary(ctb0050_layer[, ctc])
find_empty_layer(ctb0050_layer, "ctc")
# Fill empty layer
ctb0050_layer[, ctc := fill_empty_layer(y = ctc, x = profund_mid), by = observacao_id]
find_empty_layer(ctb0050_layer, "ctc")

# dsi
# dsi is missing. We set it to NA_real_
ctb0050_layer[, dsi := NA_real_]

str(ctb0050_layer)

# Merge ############################################################################################
# events and layers
ctb0050 <- merge(ctb0050_event, ctb0050_layer, all = TRUE)
ctb0050[, dataset_id := "ctb0050"]
# citation
ctb0050 <- merge(ctb0050, ctb0050_citation, by = "dataset_id", all.x = TRUE)

# This work contains many soil profiles from previous works. We will discard them.
# Wi discard them here to guarantee that the respective layers are also discarded too.
ctb0050 <- ctb0050[is.na(observacao_fonte), ]

# Now we will process the events without a date
ctb0050[, .N, by = data_ano]
# For each event (observacao_id) without a date, we will set the date to a value sampled from
# the range of existing dates.
ctb0050[, na_year := is.na(data_ano)]
year_range <- ctb0050[na_year == FALSE, .(min = min(data_ano), max = max(data_ano))]
ctb0050[
  na_year == TRUE, data_ano := sample(year_range$min:year_range$max, size = 1),
  by = observacao_id
]
ctb0050[na_year == TRUE, .(observacao_id, data_ano)]
ctb0050[is.na(data_fonte), data_fonte := "estimativa"]

summary_soildata(ctb0050)
# Layers: 507
# Events: 267
# Georeferenced events: 64

# Plot with mapview
if (FALSE) {
  ctb0050_sf <- sf::st_as_sf(
    ctb0050[coord_datum == 4326, ],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0050_sf["argila"])
}

# Write to disk ####################################################################################
ctb0050 <- select_output_columns(ctb0050)
data.table::fwrite(ctb0050, "ctb0050/ctb0050.csv")
data.table::fwrite(ctb0050_event, "ctb0050/ctb0050_event.csv")
data.table::fwrite(ctb0050_layer, "ctb0050/ctb0050_layer.csv")
