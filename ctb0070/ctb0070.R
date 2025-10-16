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
# ctb0070
# Dados de "Distribuição, classificação, características e limitações de solos de vinhedos
# experimentais de Bento Gonçalves, Pinheiro Machado e Sant'Ana do Livramento, RS, Brasil"
# 
# https://docs.google.com/spreadsheets/d/17zWiT9Fi-7hU6m_jfjwyNbBrCT-yi9dPmh7Vc_Fzg4U/edit?usp=sharing


ctb0070_ids <- soildata_catalog("ctb0070")

# validation #####################################################################################

ctb0070_validation <- google_sheet(ctb0070_ids$gs_id, ctb0070_ids$gid_validation)
str(ctb0070_validation)

# Check for negative validation results
sum(ctb0070_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0070_citation <- google_sheet(ctb0070_ids$gs_id, ctb0070_ids$gid_citation)
str(ctb0070_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0070_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0070_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0070_citation <- data.table::data.table(
  dataset_id = "ctb0070",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0070_citation)

# event #####################################################################################
ctb0070_event <- google_sheet(ctb0070_ids$gs_id, ctb0070_ids$gid_event)
str(ctb0070_event)

#PROCESS FIELDS


# observacao_id
# ID-do-evento -> observacao_id
data.table::setnames(ctb0070_event, old = "ID-do-evento", new = "observacao_id")
ctb0070_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0070_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0070_event, old = "Ano (coleta)", new = "data_ano")
ctb0070_event[, data_ano := as.integer(data_ano)]
ctb0070_event[, .N, by = data_ano]

# ano_fonte
ctb0070_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0070_event[, .N, by = ano_fonte]


#prepare the data to be merged into just one variable
data.table::setnames(ctb0070_event, old = "Latitude graus", new = "lat_graus")
data.table::setnames(ctb0070_event, old = "Latitude minutos", new = "lat_minutos")
data.table::setnames(ctb0070_event, old = "Latitude segundos", new = "lat_segundos")
data.table::setnames(ctb0070_event, old = "Longitude graus", new = "lon_graus")
data.table::setnames(ctb0070_event, old = "Longitude minutos", new = "lon_minutos")
data.table::setnames(ctb0070_event, old = "Longitude segundos", new = "lon_segundos")

ctb0070_event[, lat_graus := gsub(",", ".", lat_graus)]
ctb0070_event[, lat_minutos := gsub(",", ".", lat_minutos)]
ctb0070_event[, lat_segundos := gsub(",", ".", lat_segundos)]
ctb0070_event[, lon_graus := gsub(",", ".", lon_graus)]
ctb0070_event[, lon_minutos := gsub(",", ".", lon_minutos)]
ctb0070_event[, lon_segundos:= gsub(",", ".", lon_segundos)]

# coord_x
ctb0070_event[, coord_x := paste(lon_graus, lon_minutos, lon_segundos, "W")]
ctb0070_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0070_event[, coord_x])

# coord_y
ctb0070_event[, coord_y := paste(lat_graus, lat_minutos, lat_segundos, "S")]
ctb0070_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0070_event[, coord_y])

ctb0070_event <- ctb0070_event[!is.na(coord_x) & !is.na(coord_y)]

# Datum (coord) -> coord_datum
# already in WGS 84
data.table::setnames(ctb0070_event, old = "Datum (coord)", new = "coord_datum")
ctb0070_event[, coord_datum := NULL]
ctb0070_event[, coord_datum := 4326]

summary(ctb0070_event[, .(coord_datum, coord_x, coord_y)])


#coord_precisao
# We set it to NA_real_
ctb0070_event[, coord_precisao := NA_real_]
summary(ctb0070_event[, coord_precisao])

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0070_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0070_event[, coord_fonte := as.character(coord_fonte)]
summary(ctb0070_event[, coord_fonte])

# País -> pais_id
data.table::setnames(ctb0070_event, old = "País", new = "pais_id")
ctb0070_event[, pais_id := "BR"]

# Estado (UF) -> estado_id
data.table::setnames(ctb0070_event, old = "Estado (UF)", new = "estado_id")
ctb0070_event[, estado_id := as.character(estado_id)]
ctb0070_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0070_event, old = "Município", new = "municipio_id")
ctb0070_event[, municipio_id := as.character(municipio_id)]
ctb0070_event[, .N, by = municipio_id]

#amostra_area
ctb0070_event[, amostra_area := NA_real_]
summary(ctb0070_event[, amostra_area])

# Classificação do solo segundo o SiBCS -> taxon_sibcs
data.table::setnames(ctb0070_event, old = "Classificação do solo segundo o SiBCS", new = "taxon_sibcs")
ctb0070_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0070_event[, .N, by = taxon_sibcs]

# taxon_st <- Classificação do solo segundo o Soil Taxonomy
data.table::setnames(ctb0070_event, old = "Classificação do solo segundo o Soil Taxonomy", new = "taxon_st")
ctb0070_event[, taxon_st := as.character(taxon_st)]
ctb0070_event[, .N, by = taxon_st]

#there is no information about rockiness and stoniness in the spreadsheet of this document#########

# Pedregosidade (superficie) 

ctb0070_event[, pedregosidade := NA_character_]

# Rochosidade (superficie)


ctb0070_event[, rochosidade := NA_character_]

################################################################################################


str(ctb0070_event)



# layers ###########################################################################################
ctb0070_layer <- google_sheet(ctb0070_ids$gs_id, ctb0070_ids$gid_layer)
str(ctb0070_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0070_layer, old = "ID do evento", new = "observacao_id")
ctb0070_layer[, observacao_id := as.character(observacao_id)]
ctb0070_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0070_layer, old = "ID da camada", new = "camada_nome")
ctb0070_layer[, camada_nome := as.character(camada_nome)]
ctb0070_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0070_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0070_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0070_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0070_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0070_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0070_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0070_layer[, profund_inf])

# areia_grossa
# old: "Areia grossa [%]"
# new: areia_grossa
data.table::setnames(ctb0070_layer, old = "Areia grossa [%]", new = "areia_grossa")
ctb0070_layer[, areia_grossa := as.numeric(areia_grossa)*10]
ctb0070_layer[is.na(areia_grossa), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_grossa)]

# areia_fina
# old: "Areia fina [%]"
# new: areia_fina
# areia_fina is missing for some layers...
data.table::setnames(ctb0070_layer, old = "Areia fina [%]", new = "areia_fina")
ctb0070_layer[, areia_fina := as.numeric(areia_fina)*10]
ctb0070_layer[is.na(areia_fina), .(observacao_id, camada_nome, profund_sup, profund_inf, areia_fina)]

# areia
# criação da coluna areia 
ctb0070_layer[, areia:= areia_grossa+areia_fina]

# silte
# old: Silte [%]
# new: silte
# silte is missing for some layers...
data.table::setnames(ctb0070_layer, old = "Silte [%]", new = "silte")
ctb0070_layer[, silte := as.numeric(silte)*10]
ctb0070_layer[is.na(silte), .(observacao_id, camada_nome, profund_sup, profund_inf, silte)]

# argila
# old: Argila [%]
# new: argila
# argila is missing for some layers...
data.table::setnames(ctb0070_layer, old = "Argila [%]", new = "argila")
ctb0070_layer[, argila := as.numeric(argila)*10]
ctb0070_layer[is.na(argila), .(observacao_id, camada_nome, profund_sup, profund_inf, argila)]

# terrafina
# new: terrafina
ctb0070_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0070_layer[, psd := round(argila + silte + areia_fina + areia_grossa)]
psd_lims <- 900:1100
# Check the limits
ctb0070_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0070_layer[!psd %in% psd_lims & !is.na(psd), ..cols]

# carbono
# old: C orgânico [%]
# new: carbono
data.table::setnames(ctb0070_layer, old = "C orgânico [%]", new = "carbono")
ctb0070_layer[, carbono := as.numeric(carbono)*10]
summary(ctb0070_layer[, carbono])
check_empty_layer(ctb0070_layer, "carbono")

# ctc
# old: Valor T [me/100g]
# new: ctc
data.table::setnames(ctb0070_layer, old = "Valor T [me/100g]", new = "ctc")
ctb0070_layer[, ctc := as.numeric(ctc)]
summary(ctb0070_layer[, ctc])
check_empty_layer(ctb0070_layer, "ctc")

# ph
# old: pH em H_2O
# new: ph
data.table::setnames(ctb0070_layer, old = "pH em H_2O", new = "ph")
ctb0070_layer[, ph := as.numeric(ph)]
summary(ctb0070_layer[, ph])
check_empty_layer(ctb0070_layer, "ph")

# dsi
# old: Densidade aparente [g/cm^3]
# new: dsi
data.table::setnames(ctb0070_layer, old = "Densidade aparente [g/cm^3]", new = "dsi")
ctb0070_layer[, dsi := as.numeric(dsi)]
summary(ctb0070_layer[, dsi])

str(ctb0070_layer)

# Merge ############################################################################################
# events and layers
ctb0070 <- merge(ctb0070_event, ctb0070_layer, all = TRUE)
ctb0070[, dataset_id := "ctb0070"]
# citation
ctb0070 <- merge(ctb0070, ctb0070_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0070)

#Layers: 34
#Events: 7
#Georeferenced events: 1

# Plot using mapview
if (FALSE) {
  ctb0070_sf <- sf::st_as_sf(
    ctb0070[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0070_sf["argila"])
}

# Write to disk ####################################################################################
ctb0070 <- select_output_columns(ctb0070)
data.table::fwrite(ctb0070, "ctb0070/ctb0070.csv")
data.table::fwrite(ctb0070_event, "ctb0070/ctb0070_event.csv")
data.table::fwrite(ctb0070_layer, "ctb0070/ctb0070_layer.csv")
