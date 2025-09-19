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
  install.packages("mapview", dependencies = TRUE)
  library("mapview")
}
if (!require("parzer")) {
  install.packages("parzer")
  library("parzer")
}

# Source helper functions
source("./helper.R")



# Google Sheet #####################################################################################
# ctb0067
# Dados de "Caracterização e gênese de Espodossolos da planície costeira do Estado de São Paulo"
# 
# https://docs.google.com/spreadsheets/d/1Nbl5kY9qX-VYINdK04bTjmcdyvc_ME64vKbBSz2oLR4/edit?usp=sharing


ctb0067_ids <- soildata_catalog("ctb0067")

# validation #####################################################################################

ctb0067_validation <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_validation)
str(ctb0067_validation)

# Check for negative validation results
sum(ctb0067_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0067_citation <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_citation)
str(ctb0067_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0067_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0067_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0067_citation <- data.table::data.table(
  dataset_id = "ctb0067",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0067_citation)

# event #####################################################################################
ctb0067_event <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_event)
str(ctb0067_event)

#PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0067_event, old = "ID do evento", new = "observacao_id")
ctb0067_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0067_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0067_event, old = "Ano (coleta)", new = "data_ano")
ctb0067_event[, data_ano := as.integer(data_ano)]
ctb0067_event[, .N, by = data_ano]

# ano_fonte
ctb0067_event[!is.na(data_ano), ano_fonte := "original"]
ctb0067_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0067_event, old = "Longitude (m)", new = "coord_x")
ctb0067_event[, coord_x := as.numeric(coord_x)]
summary(ctb0067_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0067_event, old = "Latitude (m)", new = "coord_y")
ctb0067_event[, coord_y := as.numeric(coord_y)]
summary(ctb0067_event[, coord_y])


# Datum (coord) -> coord_datum
# SIRGAS 2000 / UTM zone 23S
data.table::setnames(ctb0067_event, old = "Datum (coord)", new = "coord_datum")
ctb0067_event[coord_datum == "UTM zona 23S", coord_datum := 32723]
ctb0067_event[, coord_datum := as.integer(coord_datum)]
ctb0067_event[, .N, by = coord_datum]

# Transform coordinates to WGS84
ctb0067_event_sf <- sf::st_as_sf(
  ctb0067_event[coord_datum == 32723],
  coords = c("coord_x", "coord_y"), crs = 32723
)
ctb0067_event_sf <- sf::st_transform(ctb0067_event_sf, 4326)
ctb0067_event_sf <- sf::st_coordinates(ctb0067_event_sf)
ctb0067_event[coord_datum == 32723, coord_x := ctb0067_event_sf[, 1]]
ctb0067_event[coord_datum == 32723, coord_y := ctb0067_event_sf[, 2]]
ctb0067_event[coord_datum == 32723, coord_datum := 4326]
rm(ctb0067_event_sf)
summary(ctb0067_event[, .(coord_datum, coord_x, coord_y)])

# Check for duplicated coordinates
ctb0067_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Precisão (coord) [m] -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0067_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0067_event[, coord_precisao := NA_real_]
summary(ctb0067_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0067_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0067_event[, coord_fonte := NA_real_]
summary(ctb0067_event[, coord_fonte])


# País -> pais_id
data.table::setnames(ctb0067_event, old = "País", new = "pais_id")
ctb0067_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0067_event, old = "Estado (UF)", new = "estado_id")
ctb0067_event[, estado_id := "SP"]

# Município -> municipio_id
data.table::setnames(ctb0067_event, old = "Município", new = "municipio_id")
ctb0067_event[, municipio_id := as.character(municipio_id)]
ctb0067_event[, .N, by = municipio_id]

# Área amostrada [m^2] -> amostra_area
data.table::setnames(ctb0067_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0067_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0067_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0067_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0067_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0067_event[, .N, by = taxon_sibcs]

# taxon_st_1999

ctb0067_event[, taxon_st := NA_character_]

# Pedregosidade (superficie) 
data.table::setnames(ctb0067_event, old = "Pedregosidade", new = "pedregosidade")
ctb0067_event[, pedregosidade := as.character(pedregosidade)]
ctb0067_event[, .N, by = pedregosidade]

# Rochosidade (superficie)

data.table::setnames(ctb0067_event, old = "Rochosidade", new = "rochosidade")
ctb0067_event[, rochosidade := as.character(rochosidade)]
ctb0067_event[, .N, by = rochosidade]



str(ctb0067_event)

# layers ###########################################################################################
ctb0067_layer <- google_sheet(ctb0067_ids$gs_id, ctb0067_ids$gid_layer)
str(ctb0067_layer)

# PROCESS FIELDS

# observacao_id
# old: ID do evento
# new: observacao_id
data.table::setnames(ctb0067_layer, old = "ID do evento", new = "observacao_id")
ctb0067_layer[, observacao_id := as.character(observacao_id)]
ctb0067_layer[, .N, by = observacao_id]

# camada_nome
# old: ID da camada
# new: camada_nome
data.table::setnames(ctb0067_layer, old = "ID da camada", new = "camada_nome")
ctb0067_layer[, camada_nome := as.character(camada_nome)]
ctb0067_layer[, .N, by = camada_nome]

# amostra_id
# old: ID da amostra
# new: amostra_id
data.table::setnames(ctb0067_layer, old = "ID da amostra", new = "amostra_id")
ctb0067_layer[, amostra_id := NA_real_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0067_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0067_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0067_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0067_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0067_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0067_layer[, profund_inf])


# argila
# old: Argila (<0,002mm) [g kg⁻1]
# new: argila
# argila is missing for some layers...
data.table::setnames(ctb0067_layer, old = "Argila (<0,002mm) [g kg⁻1]", new = "argila")
ctb0067_layer[, argila := as.numeric(argila)]
ctb0067_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# silte
# old: Silte [g kg⁻1]
# new: silte
# silte is missing for some layers...
data.table::setnames(ctb0067_layer, old = "Silte [g kg⁻1]", new = "silte")
ctb0067_layer[, silte := as.numeric(silte)]
ctb0067_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# areia_grossa
# old: Areia grossa (0,25-2mm) [g kg⁻1]
# new: areia_grossa
# areia_grossa is missing for some layers...
data.table::setnames(ctb0067_layer, old = "Areia grossa (0,25-2mm) [g kg⁻1]", new = "areia_grossa")
ctb0067_layer[, areia_grossa := as.numeric(areia_grossa)]
ctb0067_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: Areia fina (0,05-0,25 mm) [g kg⁻1]
# new: areia_fina
# areia_fina is missing for some layers...
data.table::setnames(ctb0067_layer, old = "Areia fina (0,05-0,25 mm) [g kg⁻1]", new = "areia_fina")
ctb0067_layer[, areia_fina := as.numeric(areia_fina)]
ctb0067_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

# areia
# criação da coluna areia 
ctb0067_layer[, areia:= areia_grossa+areia_fina]

# terrafina
# is missing on main document. Pelas caracteristicas do solo, é estimado que seja 100% de terrafina
# (nunca somar silte, areia, argila para tererafina)
ctb0067_layer[, terrafina := (1000)] 

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0067_layer[, psd := round(argila + silte + areia_fina + areia_grossa)]
psd_lims <- 900:1100
# Check the limits
ctb0067_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0067_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# O trabalho possui Carbono Total para algumas amostras e para maioria existe o dado do Carbono g/kg
# Foi utilizado o Carbono g/kg
# old: C [g/kg]
# new: carbono
data.table::setnames(ctb0067_layer, old = "C [g/kg]", new = "carbono")
ctb0067_layer[, carbono := as.numeric(carbono)]
summary(ctb0067_layer[, carbono])
check_empty_layer(ctb0067_layer, "carbono")

# ctc
# old: T [cmolc kg⁻1]
# new: ctc
data.table::setnames(ctb0067_layer, old = "T [cmolc kg⁻1]", new = "ctc")
ctb0067_layer[, ctc := as.numeric(ctc)]
summary(ctb0067_layer[, ctc])
check_empty_layer(ctb0067_layer, "ctc")

# ph
# old: pH H2O [-]
# new: ph
data.table::setnames(ctb0067_layer, old = "pH H2O [-]", new = "ph")
ctb0067_layer[, ph := as.numeric(ph)]
summary(ctb0067_layer[, ph])
check_empty_layer(ctb0067_layer, "ph")

# dsi
# dsi is missing in this document (need review)
ctb0067_layer[, dsi := NA_real_]

str(ctb0067_layer)

# Merge ############################################################################################
# events and layers
ctb0067 <- merge(ctb0067_event, ctb0067_layer, all = TRUE)
ctb0067[, dataset_id := "ctb0067"]
# citation
ctb0067 <- merge(ctb0067, ctb0067_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0067)

#Layers: 274
#Events: 31
#Georeferenced events: 31

# Plot using mapview
if (FALSE) {
  ctb0067_sf <- sf::st_as_sf(
    ctb0067[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0067_sf["argila"])
}

# Write to disk ####################################################################################
ctb0067 <- select_output_columns(ctb0067)
data.table::fwrite(ctb0067, "ctb0067/ctb0067.csv")
data.table::fwrite(ctb0067_event, "ctb0067/ctb0067_event.csv")
data.table::fwrite(ctb0067_layer, "ctb0067/ctb0067_layer.csv")




