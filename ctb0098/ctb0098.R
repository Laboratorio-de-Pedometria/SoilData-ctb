# autor: Felipe Brun Vergani
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
if (!require("mapview")) {
  install.packages("mapview")
  library("mapview")
}
if (!require("parzer")) {
  install.packages("parzer")
  library("parzer")
}


# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0098
# Dados de "Caracterização das classes de solo, aptidão agrícola das terras e análise da vulnerabilidade do
# solo na Microbacia Lajeado Pessegueiro, Guaruja do Sul, SC"
# 
# 
# https://docs.google.com/spreadsheets/d/11a5FkTcoqCX4pMMRtt5Ou55rnb7mqKa881fw47MHx1Y/edit?usp=sharing


ctb0098_ids <- soildata_catalog("ctb0098")

# validation #####################################################################################

ctb0098_validation <- google_sheet(ctb0098_ids$gs_id, ctb0098_ids$gid_validation)
str(ctb0098_validation)

# Check for negative validation results
sum(ctb0098_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0098_citation <- google_sheet(ctb0098_ids$gs_id, ctb0098_ids$gid_citation)
str(ctb0098_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0098_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0098_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0098_citation <- data.table::data.table(
  dataset_id = "ctb0098",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0098_citation)

# event #####################################################################################
ctb0098_event <- google_sheet(ctb0098_ids$gs_id, ctb0098_ids$gid_event)
str(ctb0098_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0098_event, old = "ID do evento", new = "observacao_id")
ctb0098_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0098_event[, observacao_id]) > 1)

# data_ano
# Ano [coleta] -> data_coleta_ano
data.table::setnames(ctb0098_event, old = "Ano [coleta]", new = "data_ano")
ctb0098_event[, data_ano := as.integer(data_ano)]
ctb0098_event[, .N, by = data_ano]

# ano_fonte
ctb0098_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0098_event[, .N, by = ano_fonte]

#There is no latitude and longitude data in this work.

# Longitude -> coord_x
data.table::setnames(ctb0098_event, old = "Longitude", new = "coord_x")
ctb0098_event[, coord_x := as.character(coord_x)]
summary(ctb0098_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0098_event, old = "Latitude", new = "coord_y")
ctb0098_event[, coord_y := as.character(coord_y)]
summary(ctb0098_event[, coord_y])


#There is no Datum/Precision/Fonte data in this work.
# Datum (coord) -> coord_datum
# DATUM == WGS84 already.
data.table::setnames(ctb0098_event, old = "Datum [coord]", new = "coord_datum")
ctb0098_event[, coord_datum := NA_real_]


# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0098_event, old = "Precisão [coord]", new = "coord_precisao")
ctb0098_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0098_event, old = "Fonte [coord]", new = "coord_fonte")
ctb0098_event[, coord_fonte := NA_real_]
summary(ctb0098_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0098_event, old = "País", new = "pais_id")
ctb0098_event[, pais_id := "BR"]


# Estado [UF] -> estado_id
data.table::setnames(ctb0098_event, old = "Estado [UF]", new = "estado_id")
ctb0098_event[, estado_id := as.character(estado_id)]
ctb0098_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0098_event, old = "Município", new = "municipio_id")
ctb0098_event[, municipio_id := as.character(municipio_id)]
ctb0098_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0098_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0098_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0098_event[, amostra_area])

# SiBCS (2013) -> taxon_sibcs
data.table::setnames(ctb0098_event, old = "SiBCS (2013)", new = "taxon_sibcs")
ctb0098_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0098_event[, .N, by = taxon_sibcs]

# taxon_st 
# missing edition of this soil taxonomy
ctb0098_event[, taxon_st := NA_character_]


# Pedregosidade (superficie) 
data.table::setnames(ctb0098_event, old = "Pedregosidade", new = "pedregosidade")
ctb0098_event[, pedregosidade := as.character(pedregosidade)]
ctb0098_event[, .N, by = pedregosidade]

# Rochosidade (superficie)
#no rockiness data in this work

ctb0098_event[, rochosidade := NA_character_]



str(ctb0098_event)

# layers ###########################################################################################
ctb0098_layer <- google_sheet(ctb0098_ids$gs_id, ctb0098_ids$gid_layer)
str(ctb0098_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0098_layer, old = "ID do evento", new = "observacao_id")
ctb0098_layer[, observacao_id := as.character(observacao_id)]
ctb0098_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0098_layer, old = "ID da camada", new = "camada_nome")
ctb0098_layer[, camada_nome := as.character(camada_nome)]
ctb0098_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
data.table::setnames(ctb0098_layer, old = "ID da amostra", new = "amostra_id")
ctb0098_layer[, amostra_id := as.character(amostra_id)]
ctb0098_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0098_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0098_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0098_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0098_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0098_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0098_layer[, profund_inf])

#areia
# old: Areia [g/kg]
# new: areia
data.table::setnames(ctb0098_layer, old = "Areia [g/kg]", new = "areia")
ctb0098_layer[, areia := as.numeric(areia)]
summary(ctb0098_layer[, areia])

#silte
# old: Silte [g/kg]
# new: silte
data.table::setnames(ctb0098_layer, old = "Silte [g/kg]", new = "silte")
ctb0098_layer[, silte := as.numeric(silte)]
summary(ctb0098_layer[, silte])

#argila
# old: Argila [g/kg]
# new: argila
data.table::setnames(ctb0098_layer, old = "Argila [g/kg]", new = "argila")
ctb0098_layer[, argila := as.numeric(argila)]
summary(ctb0098_layer[, argila])


#terrafina
# is missing in this document.
ctb0098_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0098_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0098_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0098_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: CO [g/kg]
# new: carbono
data.table::setnames(ctb0098_layer, old = "CO [g/kg]", new = "carbono")
ctb0098_layer[, carbono := as.numeric(carbono)/1.724]
ctb0098_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0098_layer[, carbono])

# ctc
# old: CTC pH7,0 [cmol/kg]
# new: ctc
data.table::setnames(ctb0098_layer, old = "CTC pH7,0 [cmol/kg]", new = "ctc")
ctb0098_layer[, ctc := as.numeric(ctc)]
summary(ctb0098_layer[, ctc])
check_empty_layer(ctb0098_layer, "ctc")

# ph
# old: pH H₂O
# new: ph
data.table::setnames(ctb0098_layer, old = "pH H₂O", new = "ph")
ctb0098_layer[, ph := as.numeric(ph)]
summary(ctb0098_layer[, ph])
check_empty_layer(ctb0098_layer, "ph")

# dsi
# dsi is missing in this document.
ctb0098_layer[, dsi := NA_real_]


str(ctb0098_layer)

# Merge ############################################################################################
# events and layers
ctb0098 <- merge(ctb0098_event, ctb0098_layer, all = TRUE)
ctb0098[, dataset_id := "ctb0098"]
# citation
ctb0098 <- merge(ctb0098, ctb0098_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0098)


#Layers: 122
#Events: 29
#Georeferenced events: 0


# Plot using mapview
if (FALSE) {
  ctb0098_sf <- sf::st_as_sf(
    ctb0098[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0098_sf["argila"])
}

# Write to disk ####################################################################################
ctb0098 <- select_output_columns(ctb0098)
data.table::fwrite(ctb0098, "ctb0098/ctb0098.csv")
data.table::fwrite(ctb0098_event, "ctb0098/ctb0098_event.csv")
data.table::fwrite(ctb0098_layer, "ctb0098/ctb0098_layer.csv")
