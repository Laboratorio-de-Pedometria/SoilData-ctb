# autor: Felipe Brun Vergani
# data: 2026

# Source helper functions and packages
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0071
# Dados de "Relações pedologia, geomorfologia e sedimentologia no Pantanal Norte"
#
# Google Drive: https://drive.google.com/drive/u/1/folders/1-ZuzR5Ma_kdWIpSnOGg_MlYRvhE7BRfe
ctb0071_ids <- soildata_catalog("ctb0071")

# validation #####################################################################################
ctb0071_validation <- google_sheet(ctb0071_ids$gs_id, ctb0071_ids$gid_validation)
check_sheet_validation(ctb0071_validation)

# citation #####################################################################################
ctb0071_citation <- google_sheet(ctb0071_ids$gs_id, ctb0071_ids$gid_citation)
str(ctb0071_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0071_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0071_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0071_citation <- data.table::data.table(
  dataset_id = "ctb0071",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0071_citation)

# event #####################################################################################
ctb0071_event <- google_sheet(ctb0071_ids$gs_id, ctb0071_ids$gid_event)
str(ctb0071_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0071_event, old = "ID do evento", new = "observacao_id")
ctb0071_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
any(table(ctb0071_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0071_event, old = "Ano (coleta)", new = "data_ano")
ctb0071_event[, data_ano := NA_character_]
ctb0071_event[, .N, by = data_ano]

# ano_fonte
ctb0071_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0071_event[is.na(data_ano), ano_fonte := "Estimativa"]
ctb0071_event[, .N, by = ano_fonte]

ctb0071_event[is.na(data_ano), data_ano := ctb0071_event[!is.na(data_ano), unique(data_ano)][1]]
ctb0071_event[, .N, by = data_ano]

# coord_x
# Longitude -> coord_x
data.table::setnames(ctb0071_event, old = "Longitude", new = "coord_x")
ctb0071_event[, coord_x := NA_character_]

# coord_y
# Latitude -> coord_y
data.table::setnames(ctb0071_event, old = "Latitude", new = "coord_y")
ctb0071_event[, coord_y := NA_character_]


# Datum (coord) -> coord_datum
data.table::setnames(ctb0071_event, old = "Datum (coord)", new = "coord_datum")
ctb0071_event[, coord_datum := NA_character_]
ctb0071_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
data.table::setnames(ctb0071_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0071_event[, coord_fonte := NA_character_]
ctb0071_event[, .N, by = coord_fonte]

# Precisão (coord) [m] -> coord_precisao
data.table::setnames(ctb0071_event, old = "Precisão (coord) [m]", new = "coord_precisao")
ctb0071_event[, coord_precisao := NA_character_]
summary(ctb0071_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0071_event, old = "País", new = "pais_id")
ctb0071_event[, pais_id := as.character(pais_id)]
ctb0071_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0071_event, old = "Estado (UF)", new = "estado_id")
ctb0071_event[, estado_id := as.character(estado_id)]
ctb0071_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0071_event, old = "Município", new = "municipio_id")
ctb0071_event[, municipio_id := as.character(municipio_id)]
ctb0071_event[, .N, by = municipio_id]

# Área amostrada[m^2] -> amostra_area
# The area of the sampling points is not informed in this dataset. However, we know that there are
# soil profiles and auger drillings. Therefore, we can infer the size of the sampling points.
data.table::setnames(ctb0071_event, old = "Área amostrada [m^2]", new = "amostra_area")
ctb0071_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0071_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0071_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0071_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0071_event[, .N, by = taxon_sibcs]

# taxon_st
# The soil classification according to Soil Taxonomy is not informed in this document.
ctb0071_event[, taxon_st := NA_character_]

# Pedregosidade -> pedregosidade
data.table::setnames(ctb0071_event, old = "Pedregosidade", new = "pedregosidade")
ctb0071_event[, pedregosidade := as.character(pedregosidade)]
ctb0071_event[, .N, by = pedregosidade]

# Rochosidade -> rochosidade
data.table::setnames(ctb0071_event, old = "Rochosidade", new = "rochosidade")
ctb0071_event[, rochosidade := as.character(rochosidade)]
ctb0071_event[, .N, by = rochosidade]

# cobertura
# Concatenates one or more source columns (e.g. situacao, uso_atual, cobertura) into a single
# field. Adjust the vector below with the names of the already-renamed source columns.
data.table::setnames(ctb0071_event, old = "Situação, declive e cobertura vegetal", new="situacao")
data.table::setnames(ctb0071_event, old = "Uso e cobertura da terra", new="uso_atual")
cobertura_cols <- c("situacao", "uso_atual")
concat_columns(ctb0071_event, target = "cobertura", sources = cobertura_cols)

#vegetacao
data.table::setnames(ctb0071_event, old="Vegetação primária", new= "vegetacao")
ctb0071_event[, vegetacao := as.character(vegetacao)]
ctb0071_event[, .N, by = vegetacao]

# erosao
data.table::setnames(ctb0071_event, old = "Erosão", new="erosao")
erosao_cols <- c("erosao")
concat_columns(ctb0071_event, target = "erosao", sources = erosao_cols)
ctb0071_event[, .N, by = erosao]



str(ctb0071_event)

# layers ###########################################################################################
ctb0071_layer <- google_sheet(ctb0071_ids$gs_id, ctb0071_ids$gid_layer)
str(ctb0071_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0071_layer, old = "ID do evento", new = "observacao_id")
ctb0071_layer[, observacao_id := as.character(observacao_id)]
ctb0071_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0071_layer, old = "ID da camada", new = "camada_nome")
ctb0071_layer[, camada_nome := as.character(camada_nome)]
ctb0071_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0071_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0071_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0071_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0071_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0071_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0071_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0071_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0071_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0071_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0071_layer[, profund_inf])

# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile.
ctb0071_layer <- ctb0071_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0071_layer[, camada_id := 1:.N, by = observacao_id]
ctb0071_layer[, .N, by = camada_id]

# Check for duplicated layers
check_repeated_layer(ctb0071_layer)

# Check for missing layers
check_missing_layer(ctb0071_layer)

# Fração fina (<2mm) -> terrafina
# The documentation does not provide explicit analytical values for material coarser than 2 mm
ctb0071_layer[, terrafina := NA_character_]

# Compute mid depth
ctb0071_layer[, mid_depth := (profund_sup + profund_inf) / 2]

# areia
# old: "Areia [%]"
# new: areia
# areia is missing for some layers...
data.table::setnames(ctb0071_layer, old = "Areia [%]", new = "areia")
ctb0071_layer[, areia := (as.numeric(areia)*100)]
summary(ctb0071_layer[, areia])

check_empty_layer(ctb0071_layer, "areia")
# Fill missing areia using spline interpolation by observacao_id
ctb0071_layer[,
  areia := fill_empty_layer(areia, mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]
summary(ctb0071_layer[, areia])

# silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0071_layer, old = "Silte [%]", new = "silte")
ctb0071_layer[, silte := (as.numeric(silte)*100)]
summary(ctb0071_layer[, silte])
check_empty_layer(ctb0071_layer, "silte")
# Fill missing silte using spline interpolation by observacao_id
ctb0071_layer[,
  silte := round(fill_empty_layer(silte, mid_depth, ylim = c(0, 1000))),
  by = observacao_id
]

# argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0071_layer, old = "Argila [%]", new = "argila")
ctb0071_layer[, argila := as.numeric(argila)]
summary(ctb0071_layer[, argila])
check_empty_layer(ctb0071_layer, "argila")
# Fill missing argila using spline interpolation by observacao_id
ctb0071_layer[,
  argila := round(fill_empty_layer(argila, mid_depth, ylim = c(0, 1000))),
  by = observacao_id
]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0071_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0071_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0071_layer[!psd %in% psd_lims & !is.na(psd), ..cols]
# No layers with psd != 1000.
ctb0071_layer[, psd := NULL]

# carbono
# old: C [g/kg]
# new: carbono
data.table::setnames(ctb0071_layer, old = "C [g/kg]", new = "carbono")
ctb0071_layer[, carbono := as.numeric(carbono)]
summary(ctb0071_layer[, carbono])
check_empty_layer(ctb0071_layer, "carbono")
# Fill missing carbono using spline interpolation by observacao_id
ctb0071_layer[,
  carbono := fill_empty_layer(carbono, mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]

# ctc
# old: CTC [mmolc/kg]
# new: ctc
#converting mmolc/kg to cmolc/kg
data.table::setnames(ctb0071_layer, old = "CTC [mmolc/kg]", new = "ctc")
ctb0071_layer[, ctc := (as.numeric(ctc)/10)]
summary(ctb0071_layer[, ctc])
check_empty_layer(ctb0071_layer, "ctc")
# Fill missing ctc using spline interpolation by observacao_id
ctb0071_layer[,
  ctc := fill_empty_layer(ctc, mid_depth),
  by = observacao_id
]

# ph
# old: pH 
# new: ph
data.table::setnames(ctb0071_layer, old = "pH", new = "ph")
ctb0071_layer[, ph := as.numeric(ph)]
summary(ctb0071_layer[, ph])
check_empty_layer(ctb0071_layer, "ph")
# Fill missing ph using spline interpolation by observacao_id
ctb0071_layer[,
  ph := fill_empty_layer(ph, mid_depth),
  by = observacao_id
]

#Densidade  (g cm-3) -> dsi
#converting g/cm-3 to kg/dm-3
data.table::setnames(ctb0071_layer, old = "Densidade  (g cm-3)", new = "dsi")
ctb0071_layer[, dsi := (as.numeric(dsi)*1000)]
summary(ctb0071_layer[, dsi])
check_empty_layer(ctb0071_layer, "dsi")

str(ctb0071_layer)

# Merge ############################################################################################
# events and layers
ctb0071 <- merge(ctb0071_event, ctb0071_layer, all = TRUE)
ctb0071[, dataset_id := "ctb0071"]

# citation
ctb0071 <- merge(ctb0071, ctb0071_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0071)
# Layers: 79
# Events: 11
# Georeferenced events: 0

# Plot using mapview
if (FALSE) {
  ctb0071_sf <- sf::st_as_sf(
    ctb0071[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0071_sf["argila"])
}

# Write to disk ####################################################################################
ctb0071 <- select_output_columns(ctb0071)
data.table::fwrite(ctb0071, "ctb0071/ctb0071.csv")
