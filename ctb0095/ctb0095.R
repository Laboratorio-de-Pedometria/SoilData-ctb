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
# ctb0095
# Dados de "Caracterização e gênese de solos em ambientes de Cordilheira e
# Campo de inundação periódica da sub-região do Pantanal de Poconé, Mato Grosso"
# 
# 
# https://docs.google.com/spreadsheets/d/1hLM_xrDFtf9fjpIvBqQZsNqaXg8fdQmgs_MQxPClKZA/edit?usp=sharing


ctb0095_ids <- soildata_catalog("ctb0095")

# validation #####################################################################################

ctb0095_validation <- google_sheet(ctb0095_ids$gs_id, ctb0095_ids$gid_validation)
str(ctb0095_validation)

# Check for negative validation results
sum(ctb0095_validation == FALSE, na.rm = TRUE)

# citation #####################################################################################
ctb0095_citation <- google_sheet(ctb0095_ids$gs_id, ctb0095_ids$gid_citation)
str(ctb0095_citation)


# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0095_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0095_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0095_citation <- data.table::data.table(
  dataset_id = "ctb0095",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0095_citation)

# event #####################################################################################
ctb0095_event <- google_sheet(ctb0095_ids$gs_id, ctb0095_ids$gid_event)
str(ctb0095_event)

#PROCESS FIELDS


# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0095_event, old = "ID do evento", new = "observacao_id")
ctb0095_event[, observacao_id := as.character(observacao_id)]
any(table(ctb0095_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0095_event, old = "Ano (coleta)", new = "data_ano")
ctb0095_event[, data_ano := as.integer(data_ano)]
ctb0095_event[, .N, by = data_ano]

# ano_fonte
ctb0095_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0095_event[, .N, by = ano_fonte]


# Longitude -> coord_x
data.table::setnames(ctb0095_event, old = "Longitude", new = "coord_x")
ctb0095_event[, coord_x := parzer::parse_lon(coord_x)]
summary(ctb0095_event[, coord_x])

# Latitude -> coord_y
data.table::setnames(ctb0095_event, old = "Latitude", new = "coord_y")
ctb0095_event[, coord_y := parzer::parse_lat(coord_y)]
summary(ctb0095_event[, coord_y])

# Check for duplicate coordinates
ctb0095_event[, .N, by = .(coord_x, coord_y)][N > 1]

# Datum (coord) -> coord_datum
# Como o parzer converte para o padrão de graus decimais, o datum é WGS84.
data.table::setnames(ctb0095_event, old = "Datum (coord)", new = "coord_datum")
ctb0095_event[, coord_datum := NULL]
ctb0095_event[, coord_datum := 4326]

# Precisão (coord) -> coord_precisao
# We set it to NA_real_
data.table::setnames(ctb0095_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0095_event[, coord_precisao := NA_real_]

# Fonte (coord) -> coord_fonte
#We set it to NA_real_
data.table::setnames(ctb0095_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0095_event[, coord_fonte := NA_real_]


# País -> pais_id
data.table::setnames(ctb0095_event, old = "País", new = "pais_id")
ctb0095_event[, pais_id := "BR"]

# #Mapeamento dos estados para sigla se necessário utilizar a função 'recode'
 mapa_siglas <- c(
   "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
   "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
   "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
   "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
   "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
   "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
   "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
   "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
   "Tocantins" = "TO"
  )


# Estado (UF) -> estado_id
data.table::setnames(ctb0095_event, old = "Estado (UF)", new = "estado_id")
ctb0095_event[, estado_id := recode(estado_id, !!!mapa_siglas)]
ctb0095_event[, .N, by = estado_id]


# Município -> municipio_id
data.table::setnames(ctb0095_event, old = "Município", new = "municipio_id")
ctb0095_event[, municipio_id := as.character(municipio_id)]
ctb0095_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
#
data.table::setnames(ctb0095_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0095_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0095_event[, amostra_area])

# SiBCS (1999) -> taxon_sibcs
data.table::setnames(ctb0095_event, old = "SiBCS (1999)", new = "taxon_sibcs")
ctb0095_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0095_event[, .N, by = taxon_sibcs]

# taxon_st 
# missing this soil taxonomy on document
ctb0095_event[, taxon_st := NA_character_]
ctb0095_event[, .N, by = taxon_st]

# Pedregosidade (superficie) 
data.table::setnames(ctb0095_event, old = "Pedregosidade", new = "pedregosidade")
ctb0095_event[, pedregosidade := as.character(pedregosidade)]
ctb0095_event[, .N, by = pedregosidade]

# Rochosidade (superficie)

data.table::setnames(ctb0095_event, old = "Rochosidade", new = "rochosidade")
ctb0095_event[, rochosidade := as.character(rochosidade)]
ctb0095_event[, .N, by = rochosidade]


str(ctb0095_event)

# layers ###########################################################################################
ctb0095_layer <- google_sheet(ctb0095_ids$gs_id, ctb0095_ids$gid_layer)
str(ctb0095_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0095_layer, old = "ID do evento", new = "observacao_id")
ctb0095_layer[, observacao_id := as.character(observacao_id)]
ctb0095_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0095_layer, old = "ID da camada", new = "camada_nome")
ctb0095_layer[, camada_nome := as.character(camada_nome)]
ctb0095_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing in this document.
ctb0095_layer[, amostra_id := NA_real_]


# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0095_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0095_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0095_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0095_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0095_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0095_layer[, profund_inf])

#areia grossa
# old: Areia grossa [%]
# new: areia_grossa
data.table::setnames(ctb0095_layer, old = "Areia grossa [%]", new = "areia_grossa")
ctb0095_layer[, areia_grossa := as.numeric((areia_grossa)/100)*1000]
summary(ctb0095_layer[, areia_grossa])

#areia fina
# old: Areia fina [%]
# new: areia_grossa
data.table::setnames(ctb0095_layer, old = "Areia fina [%]", new = "areia_fina")
ctb0095_layer[, areia_fina := as.numeric((areia_fina)/100)*1000]
summary(ctb0095_layer[, areia_fina])

#areia 
ctb0095_layer[, areia := areia_grossa + areia_fina]
summary(ctb0095_layer[, areia])


#silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0095_layer, old = "Silte [%]", new = "silte")
ctb0095_layer[, silte := as.numeric((silte)/100)*1000]
summary(ctb0095_layer[, silte])


#argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0095_layer, old = "Argila [%]", new = "argila")
ctb0095_layer[, argila := as.numeric((argila)/100)*1000]
summary(ctb0095_layer[, argila])


#terrafina
# is missing in this document.
ctb0095_layer[, terrafina := NA_real_]


# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0095_layer[, psd := round(rowSums(.SD, na.rm = TRUE)), .SDcols = c("argila", "silte", "areia")]
psd_lims <- 900:1100
# Check the limits
ctb0095_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0095_layer[!psd %in% psd_lims & !is.na(psd), ..cols]



# carbono
# old: C [dag/kg]
# new: carbono
# multiply by 10 to convert to g/kg
data.table::setnames(ctb0095_layer, old = "C [dag/kg]", new = "carbono")
ctb0095_layer[, carbono := as.numeric(carbono)*10]
ctb0095_layer[is.na(carbono), .(observacao_id, camada_nome, profund_sup, profund_inf, carbono)]
summary(ctb0095_layer[, carbono])

# ctc
# old: T [cmolc/dm^3]
# new: ctc
data.table::setnames(ctb0095_layer, old = "T [cmolc/dm^3]", new = "ctc")
ctb0095_layer[, ctc := as.numeric(ctc)]
summary(ctb0095_layer[, ctc])
check_empty_layer(ctb0095_layer, "ctc")

# ph
# old: pH em H_2O
# new: ph
data.table::setnames(ctb0095_layer, old = "pH em H_2O", new = "ph")
ctb0095_layer[, ph := as.numeric(ph)]
summary(ctb0095_layer[, ph])
check_empty_layer(ctb0095_layer, "ph")

# dsi 
# old: Densidade do solo [g/cm^3]
# new: dsi
data.table::setnames(ctb0095_layer, old = "Densidade do solo [g/cm^3]", new = "dsi")
ctb0095_layer[, dsi := as.numeric(dsi)]
summary(ctb0095_layer[, dsi])



str(ctb0095_layer)

# Merge ############################################################################################
# events and layers
ctb0095 <- merge(ctb0095_event, ctb0095_layer, all = TRUE)
ctb0095[, dataset_id := "ctb0095"]
# citation
ctb0095 <- merge(ctb0095, ctb0095_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0095)

#Layers: 11
#Events: 2
#Georeferenced events: 2


# Plot using mapview
if (FALSE) {
  ctb0095_sf <- sf::st_as_sf(
    ctb0095[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0095_sf["argila"])
}

# Write to disk ####################################################################################
ctb0095 <- select_output_columns(ctb0095)
data.table::fwrite(ctb0095, "ctb0095/ctb0095.csv")
data.table::fwrite(ctb0095_event, "ctb0095/ctb0095_event.csv")
data.table::fwrite(ctb0095_layer, "ctb0095/ctb0095_layer.csv")
