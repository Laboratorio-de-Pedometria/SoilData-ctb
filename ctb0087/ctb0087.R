# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0087
# Dados de "Levantamento de reconhecimento de solos e aptidão agrícola em áreas abrangidas pelo PDRI, 
# AM - Município de Parintins: relatório técnico final"
#
# Google Drive: https://drive.google.com/drive/folders/1uljy8IC0Gx9_MI1FI1-IqARyN4HV9jc0?usp=drive_link
ctb0087_ids <- soildata_catalog("ctb0087")

# validation #####################################################################################
ctb0087_validation <- google_sheet(ctb0087_ids$gs_id, ctb0087_ids$gid_validation)
#check_sheet_validation(ctb0087_validation)

# citation #####################################################################################
ctb0087_citation <- google_sheet(ctb0087_ids$gs_id, ctb0087_ids$gid_citation)
str(ctb0087_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0087_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0087_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0087_citation <- data.table::data.table(
  dataset_id = "ctb0087",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0087_citation)

# event #####################################################################################
ctb0087_event <- google_sheet(ctb0087_ids$gs_id, ctb0087_ids$gid_event)
str(ctb0087_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0087_event, old = "ID do evento", new = "observacao_id")
ctb0087_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
any(table(ctb0087_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0087_event, old = "Ano (coleta)", new = "data_ano")
ctb0087_event[, data_ano := as.integer(data_ano)]
# The sampling date is missing for 21 events. These are 19 mechanized drillings (AT1 through AT19)
# and two additional profiles (AP13 and AP15). We suppose that these were sampled in the same years
# as the other soil profiles. We assign the year from the existing years in the dataset.
ctb0087_event[, .N, by = data_ano]

# ano_fonte
ctb0087_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0087_event[is.na(data_ano), ano_fonte := "Estimativa"]
ctb0087_event[, .N, by = ano_fonte]

# Fill missing data_ano with the first non-missing year found
ctb0087_event[is.na(data_ano), data_ano := ctb0087_event[!is.na(data_ano), unique(data_ano)][1]]
ctb0087_event[, .N, by = data_ano]

# coord_x
# Longitude -> coord_x
data.table::setnames(ctb0087_event, old = "Longitude", new = "longitude")
ctb0087_event[, coord_x := as.numeric(longitude)]
summary(ctb0087_event[, coord_x])

# coord_y
# Latitude -> coord_y
data.table::setnames(ctb0087_event, old = "Latitude", new = "latitude")
ctb0087_event[, coord_y := as.numeric(latitude)]
summary(ctb0087_event[, coord_y])

# Check for duplicate coordinates
check_equal_coordinates(ctb0087_event)

# Datum (coord) -> coord_datum
#data.table::setnames(ctb0087_event, old = "Datum (coord)", new = "coord_datum")
#ctb0087_event[, coord_datum := as.character(coord_datum)]
#ctb0087_event[, coord_datum := gsub("WGS-84", 4326, coord_datum)]
#ctb0087_event[, coord_datum := gsub("WGS84", 4326, coord_datum)]
#ctb0087_event[, coord_datum := as.integer(coord_datum)]
#ctb0087_event[is.na(coord_datum) & !is.na(coord_x) & !is.na(coord_y), coord_datum := 4326L]
#ctb0087_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
# The sources strongly indicate that the author and their research team used a GPS device in the
# field to register the locations of sampling points.
#data.table::setnames(ctb0087_event, old = "Fonte (coord)", new = "coord_fonte")
#ctb0087_event[, coord_fonte := as.character(coord_fonte)]
#ctb0087_event[is.na(coord_fonte) & !(is.na(coord_x) & is.na(coord_y)), coord_fonte := "GPS"]
#ctb0087_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is not informed in this dataset. However, the coordinates were
# likelly collected using a GPS device. Therefore, we will assume a precision of 10 meters.
#data.table::setnames(ctb0087_event, old = "Precisão (coord)", new = "coord_precisao")
#ctb0087_event[, coord_precisao := as.numeric(coord_precisao)]
#ctb0087_event[is.na(coord_precisao) & !(is.na(coord_x) & is.na(coord_y)), coord_precisao := 10]
#summary(ctb0087_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0087_event, old = "País", new = "pais_id")
ctb0087_event[, pais_id := as.character(pais_id)]
ctb0087_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0087_event, old = "Estado (UF)", new = "estado_id")
ctb0087_event[, estado_id := as.character(estado_id)]
ctb0087_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0087_event, old = "Município", new = "municipio_id")
ctb0087_event[, municipio_id := as.character(municipio_id)]
ctb0087_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
data.table::setnames(ctb0087_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0087_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0087_event[, amostra_area])

# SiBCS (1983) -> taxon_sibcs
data.table::setnames(ctb0087_event, old = "SiBCS (1983)", new = "taxon_sibcs")
ctb0087_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0087_event[, .N, by = taxon_sibcs]

# taxon_st
# The soil classification according to Soil Taxonomy is not informed in this document.
ctb0087_event[, taxon_st := NA_character_]

# Pedregosidade -> pedregosidade
#data.table::setnames(ctb0087_event, old = "Pedregosidade", new = "pedregosidade")
ctb0087_event[, pedregosidade := NA_character_]
ctb0087_event[, .N, by = pedregosidade]

# Rochosidade -> rochosidade
#data.table::setnames(ctb0087_event, old = "Rochosidade", new = "rochosidade")
ctb0087_event[, rochosidade := NA_character_]
ctb0087_event[, .N, by = rochosidade]

str(ctb0087_event)

# layers ###########################################################################################
ctb0087_layer <- google_sheet(ctb0087_ids$gs_id, ctb0087_ids$gid_layer)
str(ctb0087_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0087_layer, old = "ID do evento", new = "observacao_id")
ctb0087_layer[, observacao_id := as.character(observacao_id)]
ctb0087_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0087_layer, old = "ID da camada", new = "camada_nome")
ctb0087_layer[, camada_nome := as.character(camada_nome)]
ctb0087_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0087_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0087_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0087_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0087_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0087_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0087_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0087_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0087_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0087_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0087_layer[, profund_inf])

# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile.
ctb0087_layer <- ctb0087_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0087_layer[, camada_id := 1:.N, by = observacao_id]
ctb0087_layer[, .N, by = camada_id]

# Check for duplicated layers
check_repeated_layer(ctb0087_layer)

# Check for missing layers
check_missing_layer(ctb0087_layer)

# areia_grossa
# old: Areia grossa [%]
# new: areia_grossa
data.table::setnames(ctb0087_layer,
  old = "Areia grossa [%]", new = "areia_grossa"
)
ctb0087_layer[, areia_grossa := as.numeric(areia_grossa)*100]
summary(ctb0087_layer[, areia_grossa])
check_empty_layer(ctb0087_layer, "areia_grossa")

# Compute mid depth
ctb0087_layer[, mid_depth := (profund_sup + profund_inf) / 2]

# Fill missing areia_grossa using spline interpolation by observacao_id
ctb0087_layer[,
              areia_grossa := fill_empty_layer(areia_grossa, mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]


# areia_fina
# old: "Areia fina [%]"
# new: areia_fina
# areia_fina is missing for some layers...
data.table::setnames(ctb0087_layer, old = "Areia fina [%]", new = "areia_fina")
ctb0087_layer[, areia_fina := as.numeric(areia_fina)*100]
summary(ctb0087_layer[, areia_fina])

check_empty_layer(ctb0087_layer, "areia_fina")
# Fill missing areia_fina using spline interpolation by observacao_id
ctb0087_layer[,
  areia_fina := fill_empty_layer(areia_fina, mid_depth, ylim = c(0, 1000)),
  by = observacao_id
]

# areia
# Combine all sand fractions into a single areia column
ctb0087_layer[
  ,
  areia := areia_grossa + areia_fina
]
ctb0087_layer[, areia := round(areia)]
summary(ctb0087_layer[, areia])

check_empty_layer(ctb0087_layer, "areia")

# silte
# old: Silte [%]
# new: silte
data.table::setnames(ctb0087_layer, old = "Silte [%]", new = "silte")
ctb0087_layer[, silte := as.numeric(silte)]
summary(ctb0087_layer[, silte])

check_empty_layer(ctb0087_layer, "silte")
# Fill missing silte using spline interpolation by observacao_id
ctb0087_layer[,
  silte := round(fill_empty_layer(silte, mid_depth, ylim = c(0, 1000))),
  by = observacao_id
]

# argila
# old: Argila [%]
# new: argila
data.table::setnames(ctb0087_layer, old = "Argila [%]", new = "argila")
ctb0087_layer[, argila := as.numeric(argila)]
summary(ctb0087_layer[, argila])

check_empty_layer(ctb0087_layer, "argila")

ctb0087_layer[,
  argila := round(fill_empty_layer(argila, mid_depth, ylim = c(0, 1000))),
  by = observacao_id
]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0087_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0087_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0087_layer[!psd %in% psd_lims & !is.na(psd), ..cols]
# No layers with psd != 1000.
ctb0087_layer[, psd := NULL]

# carbono
ctb0087_layer[, carbono := NA_real_]
summary(ctb0087_layer[, carbono])


# ctc
# old: T [mE/100g]
# new: ctc
data.table::setnames(ctb0087_layer, old = "T [mE/100g]", new = "ctc")
ctb0087_layer[, ctc := as.numeric(ctc)]
summary(ctb0087_layer[, ctc])

check_empty_layer(ctb0087_layer, "ctc")

ctb0087_layer[,
  ctc := fill_empty_layer(ctc, mid_depth),
  by = observacao_id
]

# ph
# old: pH H2O
# new: ph
data.table::setnames(ctb0087_layer, old = "pH H2O", new = "ph")
ctb0087_layer[, ph := as.numeric(ph)]
summary(ctb0087_layer[, ph])

check_empty_layer(ctb0087_layer, "ph")

ctb0087_layer[,
  ph := fill_empty_layer(ph, mid_depth),
  by = observacao_id
]

# Densidade aparente [g/cm^3] -> dsi
data.table::setnames(ctb0087_layer, old = "Densidade aparente [g/cm^3]", new = "dsi")
ctb0087_layer[, dsi := as.numeric(dsi)]
summary(ctb0087_layer[, dsi])

check_empty_layer(ctb0087_layer, "dsi")

str(ctb0087_layer)

# Merge ############################################################################################
# events and layers
ctb0087 <- merge(ctb0087_event, ctb0087_layer, all = TRUE)
ctb0087[, dataset_id := "ctb0087"]

# citation
ctb0087 <- merge(ctb0087, ctb0087_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0087)
# Layers: 
# Events: 
# Georeferenced events: 

# Plot using mapview
if (FALSE) {
  ctb0087_sf <- sf::st_as_sf(
    ctb0087[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0087_sf["argila"])
}

# Write to disk ####################################################################################
ctb0087 <- select_output_columns(ctb0087)
data.table::fwrite(ctb0087, "ctb0087/ctb0087.csv")
