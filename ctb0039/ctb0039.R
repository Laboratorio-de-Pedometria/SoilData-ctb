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

# Source helper functions
source("./helper.R")

# ownCloud #########################################################################################
# ctb0039
# As vertentes características e os sistemas pedológicos como instrumentos de análise para a
# identificação das fragilidades e potencialidades ambientais na Bacia Hidrográfica do Paraná 3
file_path <- path.expand("~/ownCloud/febr-repo/processamento/ctb0039/2021-08-09-ctb0039.xlsx")

# The study includes auger holes and soil profiles. However, only the soil profiles have been
# included in this dataset so far

# citation #########################################################################################
ctb0039_citation <- openxlsx::read.xlsx(file_path, sheet = "identificacao")
ctb0039_citation <- data.table::as.data.table(ctb0039_citation)
str(ctb0039_citation)

# dataset_titulo
dataset_titulo <- ctb0039_citation[campo == "dados_titulo", valor]

# dataset_licenca
dataset_licenca <- ctb0039_citation[campo == "dados_licenca", valor]

# Refactor data.table
ctb0039_citation <- data.table::data.table(
  dataset_id = "ctb0039",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0039_citation)

# event ############################################################################################
ctb0039_event <- openxlsx::read.xlsx(file_path, sheet = "observacao")
ctb0039_event <- data.table::as.data.table(ctb0039_event)
str(ctb0039_event)

# Process fields
# observacao_id
ctb0039_event[, observacao_id := as.character(observacao_id)]
# check for duplicated ids
ctb0039_event[, .N, by = observacao_id][N > 1]

# observacao_data
# data_ano
# The date is in Excel format (days since 1899-12-30)
data.table::setnames(ctb0039_event, old = "observacao_data", new = "data_ano")
t0 <- "1899-12-30"
ctb0039_event[, data_ano := as.Date(data_ano, origin = t0, format = "%Y-%m-%d")]
ctb0039_event[, data_ano := as.integer(format(data_ano, "%Y"))]
ctb0039_event[, .N, by = data_ano]

# ano_fonte
# The date of collection in the field is specified in the original data document
ctb0039_event[!is.na(data_ano), ano_fonte := "original"]
ctb0039_event[, .N, by = ano_fonte]

# coord_x
ctb0039_event[, coord_x := as.numeric(coord_x)]
summary(ctb0039_event[, coord_x])

# coord_y
ctb0039_event[, coord_y := as.numeric(coord_y)]
summary(ctb0039_event[, coord_y])

# coord_datum
# coord_sistema -> coord_datum
data.table::setnames(ctb0039_event, old = "coord_sistema", new = "coord_datum")
ctb0039_event[, coord_datum := gsub("EPSG:", "", coord_datum)]
ctb0039_event[, coord_datum := as.integer(coord_datum)]
ctb0039_event[, .N, by = coord_datum]

# check for duplicated coordinates
ctb0039_event[, .N, by = .(coord_x, coord_y)][N > 1]

# coord_precisao
ctb0039_event[, coord_precisao := as.numeric(coord_precisao)]
summary(ctb0039_event[, coord_precisao])

# coord_fonte
ctb0039_event[, coord_fonte := as.character(coord_fonte)]
ctb0039_event[, .N, by = coord_fonte]

# pais_id
ctb0039_event[, pais_id := as.character(pais_id)]
ctb0039_event[, .N, by = pais_id]

# estado_id
ctb0039_event[, estado_id := as.character(estado_id)]
ctb0039_event[, .N, by = estado_id]

# municipio_id
ctb0039_event[, municipio_id := as.character(municipio_id)]
ctb0039_event[, .N, by = municipio_id]

# amostra_area
ctb0039_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0039_event[, amostra_area])

# taxon_sibcs_2013
# taxon_sibcs
data.table::setnames(ctb0039_event, old = "taxon_sibcs_2013", new = "taxon_sibcs")
ctb0039_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0039_event[, .N, by = taxon_sibcs]

# taxon_st
# The soil classification according to Soil Taxonomy is not available in this dataset.
ctb0039_event[, taxon_st := NA_character_]

# pedregosidade
# This variable is not available in this dataset. However, for two soil classes (Latossolo and
# Nitossolo), we can assume no stone content.
ctb0039_event[, pedregosidade := NA_character_]
ctb0039_event[
  grepl("Latossolo|Nitossolo", taxon_sibcs),
  pedregosidade := "ausente"
]
ctb0039_event[, .N, by = pedregosidade]

# rochosidade
# This variable is not available in this dataset. However, for two soil classes (Latossolo and
# Nitossolo), we can assume no rock content.
ctb0039_event[, rochosidade := NA_character_]
ctb0039_event[
  grepl("Latossolo|Nitossolo", taxon_sibcs),
  rochosidade := "ausente"
]
ctb0039_event[, .N, by = rochosidade]

str(ctb0039_event)

# layer ############################################################################################
ctb0039_layer <- openxlsx::read.xlsx(file_path, sheet = "camada")
ctb0039_layer <- data.table::as.data.table(ctb0039_layer)
str(ctb0039_layer)

# Process fields
# observacao_id
ctb0039_layer[, observacao_id := as.character(observacao_id)]
ctb0039_layer[, .N, by = observacao_id]

# camada_nome
ctb0039_layer[, camada_nome := as.character(camada_nome)]
ctb0039_layer[, .N, by = camada_nome]
ctb0039_layer[, camada_nome := gsub("B nit 1", "Bt1", camada_nome)]
ctb0039_layer[, camada_nome := gsub("B nit 2", "Bt2", camada_nome)]
ctb0039_layer[, camada_nome := gsub("B nitico", "Bt", camada_nome)]
ctb0039_layer[, .N, by = camada_nome]

# amostra_id
ctb0039_layer[, amostra_id := as.integer(amostra_id)]
ctb0039_layer[, .N, by = amostra_id]

# profund_sup
ctb0039_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0039_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0039_layer[, profund_sup])

# profund_inf
ctb0039_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0039_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0039_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0039_layer[, profund_inf])

# check for missing layers
any_missing_layer(ctb0039_layer)

# camada_id
ctb0039_layer <- ctb0039_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0039_layer[, camada_id := 1:.N, by = observacao_id]
ctb0039_layer[, .N, by = camada_id]

# terrafina
# The fine earth fraction is not available in this dataset. However, for some soil classes
# (Latossolo and Nitossolo), we can assume no gravel content, i.e., terrafina = 1000 g/kg. We will
# set this after we merge events and layers.
ctb0039_layer[, terrafina := NA_real_]

# argila_naoh_pipeta
# argila
data.table::setnames(ctb0039_layer, old = "argila_naoh_pipeta", new = "argila")
ctb0039_layer[, argila := as.numeric(argila)]
summary(ctb0039_layer[, argila])
check_empty_layer(ctb0039_layer, "argila")

# silte_naoh_calc
# silte
data.table::setnames(ctb0039_layer, old = "silte_naoh_calc", new = "silte")
ctb0039_layer[, silte := as.numeric(silte)]
summary(ctb0039_layer[, silte])
check_empty_layer(ctb0039_layer, "silte")

# areia_naoh_peneira
# areia
data.table::setnames(ctb0039_layer, old = "areia_naoh_peneira", new = "areia")
ctb0039_layer[, areia := as.numeric(areia)]
summary(ctb0039_layer[, areia])
check_empty_layer(ctb0039_layer, "areia")

# Check that sand + silt + clay = 1000
ctb0039_layer[, psd := argila + silte + areia]
psd_limits <- c(900, 1100)
# Check that the particle size distribution is within the limits
ctb0039_layer[, psd_check := ifelse(psd < psd_limits[1], "below", ifelse(psd > psd_limits[2], "above", "within"))]
ctb0039_layer[, .N, by = psd_check]

# old: matorg_xxx_xxx_xxx
# new: carbono
# carbono = matorg_xxx_xxx_xxx (g/dm^3) * 0.58
# Note that the original data is in g/dm³, i.e. volume basis.
data.table::setnames(ctb0039_layer, old = "matorg_xxx_xxx_xxx", new = "carbono")
ctb0039_layer[, carbono := as.numeric(carbono) * 0.58]
summary(ctb0039_layer[, carbono])
check_empty_layer(ctb0039_layer, "carbono")

# ph_cacl2_001mol10_eletrodo
# ph
data.table::setnames(ctb0039_layer, old = "ph_cacl2_001mol10_eletrodo", new = "ph")
ctb0039_layer[, ph := as.numeric(ph)]
summary(ctb0039_layer[, ph])
check_empty_layer(ctb0039_layer, "ph")

# ctc_soma_calc
# ctc
data.table::setnames(ctb0039_layer, old = "ctc_soma_calc", new = "ctc")
ctb0039_layer[, ctc := as.numeric(ctc)]
summary(ctb0039_layer[, ctc])
check_empty_layer(ctb0039_layer, "ctc")

# dsi_cilindro
# dsi
data.table::setnames(ctb0039_layer, old = "dsi_cilindro", new = "dsi")
ctb0039_layer[, dsi := as.numeric(dsi)]
summary(ctb0039_layer[, dsi])
check_empty_layer(ctb0039_layer, "dsi")

str(ctb0039_layer)

# Merge ############################################################################################
# events and layers
ctb0039 <- merge(ctb0039_event, ctb0039_layer, all = TRUE)
ctb0039[, dataset_id := "ctb0039"]

# Set terrafina = 1000 g/kg for Latossolo and Nitossolo and layer not R
ctb0039[
  grepl("Latossolo|Nitossolo", taxon_sibcs) & !grepl("R$", camada_nome),
  terrafina := 1000
]
summary(ctb0039[, terrafina])
check_empty_layer(ctb0039, "terrafina")

# citation
ctb0039 <- merge(ctb0039, ctb0039_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0039)
# Layers: 37
# Events: 9
# Georeferenced events: 9

# Plot with mapview
if (FALSE) {
  ctb0039_sf <- sf::st_as_sf(
    ctb0039[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0039_sf["argila"])
}

# Write to disk ####################################################################################
ctb0039 <- select_output_columns(ctb0039)
data.table::fwrite(ctb0039, "ctb0039/ctb0039.csv")
data.table::fwrite(ctb0039_event, "ctb0039/ctb0039_event.csv")
data.table::fwrite(ctb0039_layer, "ctb0039/ctb0039_layer.csv")
