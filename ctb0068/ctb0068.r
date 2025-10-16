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
# ctb0068
# Dados de "Fatores formadores da paisagem litoranea: A Bacia do Guaratuba, São Paulo - Brasil"
# https://docs.google.com/spreadsheets/d/1PGiqQ7bztXGQWJC74bTINXfa9SZj5Efw2eNsriTmFag/edit?usp=sharing


ctb0068_ids <- soildata_catalog("ctb0068")

# validation #####################################################################################

ctb0068_validation <- google_sheet(ctb0068_ids$gs_id, ctb0068_ids$gid_validation)
str(ctb0068_validation)

# Check for negative validation results
sum(ctb0068_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0068_citation <- google_sheet(ctb0068_ids$gs_id, ctb0068_ids$gid_citation)
str(ctb0068_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0068_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0068_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0068_citation <- data.table::data.table(
  dataset_id = "ctb0068",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0068_citation)

# event #####################################################################################
ctb0068_event <- google_sheet(ctb0068_ids$gs_id, ctb0068_ids$gid_event)
str(ctb0068_event)

#PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0068_event, old = "ID do evento", new = "observacao_id")
ctb0068_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0068_event[, observacao_id]) > 1)


# data_ano
# The defense date of this work was 1999, so we estimate that the collection was a year earlier.
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0068_event, old = "Ano (coleta)", new = "data_ano")
ctb0068_event[, data_ano := as.integer(data_ano)]
ctb0068_event[, data_ano := 1998]

# ano_fonte
ctb0068_event[, ano_fonte := "estimativa"]
ctb0068_event[, .N, by = data_ano]


# Missing Longitude / Latitude
# do not have location, accuracy and coord_datum data
# Longitude -> coord_x
data.table::setnames(ctb0068_event, old = "Longitude", new = "coord_x")
ctb0068_event[, coord_x := NA_real_]


# Latitude -> coord_y
data.table::setnames(ctb0068_event, old = "Latitude", new = "coord_y")
ctb0068_event[, coord_y := NA_real_]

# old:  Datum (coord)
# new:  coord_datum
data.table::setnames(ctb0068_event, old = "Datum (coord)", new = "coord_datum")
ctb0066_event[, coord_datum := NA_real_]

# Precisão (coord) [m] -> coord_precisao
# Coordinates were attributed with little knowledge of the precision. We set it to NA_real_
data.table::setnames(ctb0068_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0068_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0068_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0068_event[, coord_fonte := NA_real_]

# País -> pais_id
data.table::setnames(ctb0068_event, old = "País", new = "pais_id")
ctb0068_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0068_event, old = "Estado (UF)", new = "estado_id")
ctb0068_event[, estado_id := "SP"]

# Município -> municipio_id
# Município is missing so, we set N/A.
data.table::setnames(ctb0068_event, old = "Município", new = "municipio_id")
ctb0068_event[, municipio_id := NA_real_]


# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0068_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0068_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0068_event[, amostra_area])


# Classificação (CNPS/EMBRAPA) <- taxon_sibcs
data.table::setnames(ctb0068_event, old = "Classificação (CNPS/EMBRAPA)", new = "taxon_sibcs")
ctb0068_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0068_event[, .N, by = taxon_sibcs]

# taxon_st_1999

ctb0068_event[, taxon_st := NA_character_]

# Pedregosidade (superficie)
# Stoneness is missing in this document...

ctb0068_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)
# Roughness is missing in this document, however, based on the estimated analysis, 
# there is no roughness in this work.

ctb0068_event[, rochosidade := NA_character_]


str(ctb0068_event)

# layers ###########################################################################################
ctb0068_layer <- google_sheet(ctb0068_ids$gs_id, ctb0068_ids$gid_layer)
str(ctb0068_layer)

# PROCESS FIELDS

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0068_layer, old = "ID do evento", new = "observacao_id")
ctb0068_layer[, observacao_id := as.character(observacao_id)]
ctb0068_layer[, .N, by = observacao_id]

# camada_nome
# old: ID da camada
# new: camada_nome
data.table::setnames(ctb0068_layer, old = "ID da camada", new = "camada_nome")
ctb0068_layer[, camada_nome := as.character(camada_nome)]
ctb0068_layer[, .N, by = camada_nome]

# amostra_id
# old: ID da amostra
# new: amostra_id
data.table::setnames(ctb0068_layer, old = "ID da amostra", new = "amostra_id")
ctb0068_layer[, amostra_id := NA_real_]

# perfil_id
# Don't have perfil informations on this document. So, we set N/A.
ctb0068_layer[, perfil_id := NA_character_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0068_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0068_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0068_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0068_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0068_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0068_layer[, profund_inf])

# camada_id
ctb0068_layer <- ctb0068_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0068_layer[, camada_id := 1:.N, by = observacao_id]
ctb0068_layer[, .N, by = camada_id]
summary(ctb0068_layer[, camada_id])

# terrafina

#terrafina is missing on main document.
ctb0068_layer[, terrafina := NA_real_]

# areia_grossa
# old: Areia grossa [%]
# new: areia_grossa
data.table::setnames(ctb0068_layer, old = "Areia grossa [%]", new = "areia_grossa")
ctb0068_layer[, areia_grossa := as.numeric(areia_grossa) * 10]
ctb0068_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: Areia fina [%]
# new: areia_fina

data.table::setnames(ctb0068_layer, old = "Areia fina [%]", new = "areia_fina")
ctb0068_layer[, areia_fina := as.numeric(areia_fina) * 10]
ctb0068_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

# areia
# old: Areia [%]
data.table::setnames(ctb0068_layer, old = "Areia [%]", new = "areia")
ctb0068_layer[, areia := as.numeric(areia) * 10]
ctb0068_layer[is.na(areia), areia:= (areia_grossa + areia_fina)]

# silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0068_layer, old = "Silte [%]", new = "silte")
ctb0068_layer[, silte := as.numeric(silte) * 10]
ctb0068_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0068_layer, old = "Argila [%]", new = "argila")
ctb0068_layer[, argila := as.numeric(argila) * 10]
ctb0068_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0068_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0068_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0068_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C [%]
# new: carbono
data.table::setnames(ctb0068_layer, old = "C [%]", new = "carbono")
ctb0068_layer[, carbono := as.numeric(carbono) * 10]
summary(ctb0068_layer[, carbono])
check_empty_layer(ctb0068_layer, "carbono")

# ctc
# old: T [cmol_c/kg]
# new: ctc
data.table::setnames(ctb0068_layer, old = "T [cmol_c/kg]", new = "ctc")
ctb0068_layer[, ctc := as.numeric(ctc)]
summary(ctb0068_layer[, ctc])
check_empty_layer(ctb0068_layer, "ctc")

# ph
# old: pHH2O
# new: ph
data.table::setnames(ctb0068_layer, old = "pHH2O", new = "ph")
ctb0068_layer[, ph := as.numeric(ph)]
summary(ctb0068_layer[, ph])
check_empty_layer(ctb0068_layer, "ph")

# dsi
# dsi is missing in this document. So, we set N/A.
ctb0068_layer[, dsi := NA_real_]


str(ctb0068_layer)

# Merge ############################################################################################
# events and layers
ctb0068 <- merge(ctb0068_event, ctb0068_layer, all = TRUE)
ctb0068[, dataset_id := "ctb0068"]
# citation
ctb0068 <- merge(ctb0068, ctb0068_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0068)



# Plot using mapview
if (FALSE) {
  ctb0068_sf <- sf::st_as_sf(
    ctb0068[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0068_sf["argila"])
}

# Write to disk ####################################################################################
ctb0068 <- select_output_columns(ctb0068)
data.table::fwrite(ctb0068, "ctb0068/ctb0068.csv")
data.table::fwrite(ctb0068_event, "ctb0068/ctb0068_event.csv")
data.table::fwrite(ctb0068_layer, "ctb0068/ctb0068_layer.csv")