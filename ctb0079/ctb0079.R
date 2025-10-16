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
# ctb0079
# Dados de "Estoques de carbono e nitrogênio e condutividade
# hidráulica de solos afetados pela silvicultura nos campos do Pampa"
# 
# 
# https://docs.google.com/spreadsheets/d/1VXZ1xjDmkryPEonS4kj1m1imxuE8iv6vkZ7-62SmRgE/edit?usp=sharing


ctb0079_ids <- soildata_catalog("ctb0079")

# validation #####################################################################################

ctb0079_validation <- google_sheet(ctb0079_ids$gs_id, ctb0079_ids$gid_validation)
str(ctb0079_validation)

# Check for negative validation results
sum(ctb0079_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0079_citation <- google_sheet(ctb0079_ids$gs_id, ctb0079_ids$gid_citation)
str(ctb0079_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0079_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0079_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0079_citation <- data.table::data.table(
  dataset_id = "ctb0079",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0079_citation)

# event #####################################################################################
ctb0079_event <- google_sheet(ctb0079_ids$gs_id, ctb0079_ids$gid_event)
str(ctb0079_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0079_event, old = "ID do evento", new = "observacao_id")
ctb0079_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0079_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_ano
# 2018 to 2019 so we set 2018
data.table::setnames(ctb0079_event, old = "Ano (coleta)", new = "data_ano")
ctb0079_event[, data_ano := 2018]
ctb0079_event[, .N, by = data_ano]

# ano_fonte
ctb0079_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0079_event[, .N, by = ano_fonte]

#####################################################################
#Longitude, Latitude, Datum (coord), Precisão (coord), Fonte (coord)
#is missing from the official document;
#####################################################################

# coord_x
# Longitude -> coord_x
ctb0079_event[, coord_x := NA_real_]


# coord_y
# Latitude-> coord_y
ctb0079_event[, coord_y := NA_real_]


# Datum (coord) -> coord_datum
ctb0079_event[, coord_datum := NA_character_]


# Precisão (coord) -> coord_precisao
# We set it to NA_real_
ctb0079_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
ctb0079_event[, coord_fonte := NA_character_]


# País -> pais_id
data.table::setnames(ctb0079_event, old = "País", new = "pais_id")
ctb0079_event[, pais_id := "BR"]


# Estado (UF) -> estado_id
data.table::setnames(ctb0079_event, old = "Estado (UF)", new = "estado_id")
ctb0079_event[, estado_id := as.character(estado_id)]
ctb0079_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0079_event, old = "Município", new = "municipio_id")
ctb0079_event[, municipio_id := as.character(municipio_id)]
ctb0079_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0079_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0079_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0079_event[, amostra_area])

# Classificação -> taxon_sibcs
data.table::setnames(ctb0079_event, old = "Classificação", new = "taxon_sibcs")
ctb0079_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0079_event[, .N, by = taxon_sibcs]

# taxon_st_1999

ctb0079_event[, taxon_st := NA_character_]

# Pedregosidade (superficie) 
# pedregosidade is missing in this document

ctb0079_event[, pedregosidade := NA_character_]


# Rochosidade (superficie)
# rochosidade is missing in this document

ctb0079_event[, rochosidade := NA_character_]


str(ctb0079_event)


# layers ###########################################################################################
ctb0079_layer <- google_sheet(ctb0079_ids$gs_id, ctb0079_ids$gid_layer)
str(ctb0079_layer)

# Process fields

# Identificação do evento -> observacao_id
data.table::setnames(ctb0079_layer, old = "ID do evento", new = "observacao_id")
ctb0079_layer[, observacao_id := as.character(observacao_id)]
ctb0079_layer[, .N, by = observacao_id]

# Identificação da camada -> camada_nome
data.table::setnames(ctb0079_layer, old = "ID da camada", new = "camada_nome")
ctb0079_layer[, camada_nome := as.character(camada_nome)]
ctb0079_layer[, .N, by = camada_nome]

# Identificação da amostra -> amostra_id
data.table::setnames(ctb0079_layer, old = "ID da amostra", new = "amostra_id")
ctb0079_layer[, amostra_id := as.character(amostra_id)]
ctb0079_layer[, .N, by = amostra_id]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0079_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0079_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0079_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0079_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0079_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0079_layer[, profund_inf])


# areia
# old: Areia [%]
# new: areia
data.table::setnames(ctb0079_layer, old = "Areia [%]", new = "areia")
ctb0079_layer[, areia := as.numeric(areia)*10]
ctb0079_layer[is.na(areia), .(observacao_id, camada_nome, profund_sup, profund_inf, areia)]


# silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0079_layer, old = "Silte [%]", new = "silte")
ctb0079_layer[, silte := as.numeric(silte)*10]
ctb0079_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0079_layer, old = "Argila [%]", new = "argila")
ctb0079_layer[, argila := as.numeric(argila)*10]
ctb0079_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# Não existe fração fina na tabela apenas fração grossa portanto esta N/A
ctb0079_layer[, terrafina := NA_character_]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0079_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0079_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0079_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C (%)
# new: carbono
data.table::setnames(ctb0079_layer, old = "C (%)", new = "carbono")
ctb0079_layer[, carbono := as.numeric(carbono)*10]
summary(ctb0079_layer[, carbono])
check_empty_layer(ctb0079_layer, "carbono")

# ctc
# is missing in this document so, we set N/A.
ctb0079_layer[, ctc := NA_real_]

# ph
# is missing in this document so, we set N/A.
ctb0079_layer[, ph := NA_real_]

# dsi
# old: Densidade [g/cm3]
# new: dsi
data.table::setnames(ctb0079_layer, old = "Densidade [g/cm3]", new = "dsi")
ctb0079_layer[, dsi := as.numeric(dsi)]
str(ctb0079_layer)

# Merge ############################################################################################
# events and layers
ctb0079 <- merge(ctb0079_event, ctb0079_layer, all = TRUE)
ctb0079[, dataset_id := "ctb0079"]
# citation
ctb0079 <- merge(ctb0079, ctb0079_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0079)

# Layers: 168
# Events: 24
# Georeferenced events: 0


# Plot using mapview
if (FALSE) {
  ctb0079_sf <- sf::st_as_sf(
    ctb0079[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0079_sf["argila"])
}

# Write to disk ####################################################################################
ctb0079 <- select_output_columns(ctb0079)
data.table::fwrite(ctb0079, "ctb0079/ctb0079.csv")
data.table::fwrite(ctb0079_event, "ctb0079/ctb0079_event.csv")
data.table::fwrite(ctb0079_layer, "ctb0079/ctb0079_layer.csv")
