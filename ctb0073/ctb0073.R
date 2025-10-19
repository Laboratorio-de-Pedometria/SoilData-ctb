# autor: Felipe Brun Vergani and Alessandro Samuel-Rosa
# data: 2025

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}
if (!requireNamespace("parzer")) {
  install.packages("parzer")
}

# Source helper functions
source("./helper.R")

# Google Sheet #####################################################################################
# ctb0073
# Dados de "Relações pedologia, geomorfologia e sedimentologia no Pantanal Norte"
#
# Google Drive: https://drive.google.com/drive/u/1/folders/1-ZuzR5Ma_kdWIpSnOGg_MlYRvhE7BRfe
ctb0073_ids <- soildata_catalog("ctb0073")

# validation #####################################################################################
ctb0073_validation <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_validation)
check_sheet_validation(ctb0073_validation)

# citation #####################################################################################
ctb0073_citation <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_citation)
str(ctb0073_citation)

# dataset_titulo
# Check for the string "Título" in column "campo". Then get the corresponding row value from column
# "valor".
dataset_titulo <- ctb0073_citation[campo == "Título", valor]

# dataset_licenca
# Check for the string "Termos de uso" in column "campo". Then get the corresponding row value from
# column "valor".
dataset_licenca <- ctb0073_citation[campo == "Termos de uso", valor]

# Refactor data.table
ctb0073_citation <- data.table::data.table(
  dataset_id = "ctb0073",
  dataset_titulo = dataset_titulo,
  dataset_licenca = dataset_licenca
)
print(ctb0073_citation)

# event #####################################################################################
ctb0073_event <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_event)
str(ctb0073_event)

# PROCESS FIELDS

# observacao_id
# ID do evento -> observacao_id
data.table::setnames(ctb0073_event, old = "ID do evento", new = "observacao_id")
ctb0073_event[, observacao_id := as.character(observacao_id)]
# check for duplicated observacao_id
any(table(ctb0073_event[, observacao_id]) > 1)

# data_ano
# Ano (coleta) -> data_coleta_ano
data.table::setnames(ctb0073_event, old = "Ano (coleta)", new = "data_ano")
ctb0073_event[, data_ano := as.integer(data_ano)]
# The sampling date is missing for 21 events. These are 19 mechanized drillings (AT1 through AT19)
# and two additional profiles (AP13 and AP15). We suppose that these were sampled in the same years
# as the other soil profiles. We assign the year from the existing years in the dataset.
ctb0073_event[, .N, by = data_ano]

# ano_fonte
ctb0073_event[!is.na(data_ano), ano_fonte := "Original"]
ctb0073_event[is.na(data_ano), ano_fonte := "Estimativa"]
ctb0073_event[, .N, by = ano_fonte]

# Fill missing data_ano with the first non-missing year found
ctb0073_event[is.na(data_ano), data_ano := ctb0073_event[!is.na(data_ano), unique(data_ano)][1]]
ctb0073_event[, .N, by = data_ano]

# coord_x
# Longitude -> coord_x
# coord_x = (Longitude grau + Longitude minuto / 60 + longitude segundo / 3600) * -1
data.table::setnames(ctb0073_event, old = "Longitude grau", new = "longitude_grau")
ctb0073_event[, longitude_grau := as.numeric(longitude_grau)]
data.table::setnames(ctb0073_event, old = "Longitude minuto", new = "longitude_minuto")
ctb0073_event[, longitude_minuto := as.numeric(longitude_minuto)]
data.table::setnames(ctb0073_event, old = "Longitude segundo", new = "longitude_segundo")
ctb0073_event[, longitude_segundo := as.numeric(longitude_segundo)]
ctb0073_event[, coord_x := -(longitude_grau + longitude_minuto / 60 + longitude_segundo / 3600)]
summary(ctb0073_event[, coord_x])
# There are 21 events with missing longitude. These are the same events with missing data_ano.
# However, the authors provided a map (Figure 6) with the locations of all sampling points. This 
# means that the authors have the coordinates of these points. We leave them as NA for now.

# coord_y
# Latitude -> coord_y
# coord_y = (Latitude grau + Latitude minuto / 60 + latitude segundo / 3600) * -1
data.table::setnames(ctb0073_event, old = "Latitude grau", new = "latitude_grau")
ctb0073_event[, latitude_grau := as.numeric(latitude_grau)]
data.table::setnames(ctb0073_event, old = "Latitude minuto", new = "latitude_minuto")
ctb0073_event[, latitude_minuto := as.numeric(latitude_minuto)]
data.table::setnames(ctb0073_event, old = "Latitude segundo", new = "latitude_segundo")
ctb0073_event[, latitude_segundo := as.numeric(latitude_segundo)]
ctb0073_event[, coord_y := -(latitude_grau + latitude_minuto / 60 + latitude_segundo / 3600)]
summary(ctb0073_event[, coord_y])
# There are 21 events with missing latitude. These are the same events with missing longitude.

# Check for duplicate coordinates
check_equal_coordinates(ctb0073_event)

# Datum (coord) -> coord_datum
# The methodology section, which describes the use of remote sensing tools and the establishment of
# a spatial database (SIG), states that all images were stored in a spatial database (SIG) and
# registered in the same datum WGS-84
data.table::setnames(ctb0073_event, old = "Datum (coord)", new = "coord_datum")
ctb0073_event[, coord_datum := as.character(coord_datum)]
ctb0073_event[, coord_datum := gsub("WGS-84", 4326, coord_datum)]
ctb0073_event[, coord_datum := gsub("WGS84", 4326, coord_datum)]
ctb0073_event[, coord_datum := as.integer(coord_datum)]
ctb0073_event[is.na(coord_datum) & !is.na(coord_x) & !is.na(coord_y), coord_datum := 4326L]
ctb0073_event[, .N, by = coord_datum]

# Fonte (coord) -> coord_fonte
# The sources strongly indicate that the author and their research team used a GPS device in the
# field to register the locations of sampling points.
data.table::setnames(ctb0073_event, old = "Fonte (coord)", new = "coord_fonte")
ctb0073_event[, coord_fonte := as.character(coord_fonte)]
ctb0073_event[is.na(coord_fonte) & !(is.na(coord_x) & is.na(coord_y)), coord_fonte := "GPS"]
ctb0073_event[, .N, by = coord_fonte]

# Precisão (coord) -> coord_precisao
# The precision of the coordinates is not informed in this dataset. However, the coordinates were
# likelly collected using a GPS device. Therefore, we will assume a precision of 10 meters.
data.table::setnames(ctb0073_event, old = "Precisão (coord)", new = "coord_precisao")
ctb0073_event[, coord_precisao := as.numeric(coord_precisao)]
ctb0073_event[is.na(coord_precisao) & !(is.na(coord_x) & is.na(coord_y)), coord_precisao := 10]
summary(ctb0073_event[, coord_precisao])

# País -> pais_id
data.table::setnames(ctb0073_event, old = "País", new = "pais_id")
ctb0073_event[, pais_id := as.character(pais_id)]
ctb0073_event[, .N, by = pais_id]

# Estado (UF) -> estado_id
data.table::setnames(ctb0073_event, old = "Estado (UF)", new = "estado_id")
ctb0073_event[, estado_id := as.character(estado_id)]
ctb0073_event[, .N, by = estado_id]

# Município -> municipio_id
data.table::setnames(ctb0073_event, old = "Município", new = "municipio_id")
ctb0073_event[, municipio_id := as.character(municipio_id)]
ctb0073_event[, .N, by = municipio_id]

# Área do evento [m^2] -> amostra_area
# The area of the sampling points is not informed in this dataset. However, we know that there are
# soil profiles and auger drillings. Therefore, we can infer the size of the sampling points.
data.table::setnames(ctb0073_event, old = "Área do evento [m^2]", new = "amostra_area")
ctb0073_event[, amostra_area := as.numeric(amostra_area)]
summary(ctb0073_event[, amostra_area])

# SiBCS (2006) -> taxon_sibcs
data.table::setnames(ctb0073_event, old = "SiBCS (2006)", new = "taxon_sibcs")
ctb0073_event[, taxon_sibcs := as.character(taxon_sibcs)]
ctb0073_event[, .N, by = taxon_sibcs]

# taxon_st
# The soil classification according to Soil Taxonomy is not informed in this document.
ctb0073_event[, taxon_st := NA_character_]

# Pedregosidade -> pedregosidade
data.table::setnames(ctb0073_event, old = "Pedregosidade", new = "pedregosidade")
ctb0073_event[, pedregosidade := as.character(pedregosidade)]
ctb0073_event[, .N, by = pedregosidade]

# Rochosidade -> rochosidade
data.table::setnames(ctb0073_event, old = "Rochosidade", new = "rochosidade")
ctb0073_event[, rochosidade := as.character(rochosidade)]
ctb0073_event[, .N, by = rochosidade]

str(ctb0073_event)

# layers ###########################################################################################
ctb0073_layer <- google_sheet(ctb0073_ids$gs_id, ctb0073_ids$gid_layer)
str(ctb0073_layer)

# Process fields

# ID do evento -> observacao_id
data.table::setnames(ctb0073_layer, old = "ID do evento", new = "observacao_id")
ctb0073_layer[, observacao_id := as.character(observacao_id)]
ctb0073_layer[, .N, by = observacao_id]

# ID da camada -> camada_nome
data.table::setnames(ctb0073_layer, old = "ID da camada", new = "camada_nome")
ctb0073_layer[, camada_nome := as.character(camada_nome)]
ctb0073_layer[, .N, by = camada_nome]

# ID da amostra -> amostra_id
# amostra_id is missing. We assume it is NA
ctb0073_layer[, amostra_id := NA_character_]

# profund_sup
# old: Profundidade inicial [cm]
# new: profund_sup
data.table::setnames(ctb0073_layer, old = "Profundidade inicial [cm]", new = "profund_sup")
ctb0073_layer[, profund_sup := depth_slash(profund_sup), by = .I]
ctb0073_layer[, profund_sup := as.numeric(profund_sup)]
summary(ctb0073_layer[, profund_sup])

# profund_inf
# old: Profundidade final [cm]
# new: profund_inf
data.table::setnames(ctb0073_layer, old = "Profundidade final [cm]", new = "profund_inf")
ctb0073_layer[, profund_inf := depth_slash(profund_inf), by = .I]
ctb0073_layer[, profund_inf := depth_plus(profund_inf), by = .I]
ctb0073_layer[, profund_inf := as.numeric(profund_inf)]
summary(ctb0073_layer[, profund_inf])

# camada_id
# We will create a unique identifier for each layer indicating the order of the layers in each soil
# profile.
ctb0073_layer <- ctb0073_layer[order(observacao_id, profund_sup, profund_inf)]
ctb0073_layer[, camada_id := 1:.N, by = observacao_id]
ctb0073_layer[, .N, by = camada_id]

# Check for duplicated layers
check_repeated_layer(ctb0073_layer)

# Check for missing layers
check_missing_layer(ctb0073_layer)

# Fração fina (<2mm) -> terrafina
# The documentation does not provide explicit analytical values for material coarser than 2 mm
# (e.g., gravel, pebbles, or the total weight of coarse fragments) that was excluded by the 2 mm
# sieve, though large features like nodules and concretions are noted qualitatively and
# morphologically. We will keep it as NA for now.
data.table::setnames(ctb0073_layer, old = "Fração fina (<2mm)", new = "terrafina")
ctb0073_layer[, terrafina := as.numeric(terrafina)]

# Sand in this document is separated into
# Very coarse sand, Coarse sand, Medium sand, Fine sand, and Very fine sand.

# areia_muito_grossa
# old: Areia muito grossa (2-1 mm) [g/Kg]
# new: areia_muito_grossa
data.table::setnames(ctb0073_layer,
  old = "Areia muito grossa (2-1 mm) [g/Kg]", new = "areia_muito_grossa"
)
ctb0073_layer[, areia_muito_grossa := as.numeric(areia_muito_grossa)]
summary(ctb0073_layer[, areia_muito_grossa])
# Very coarse sand is missing for 17 layers, 15 of the from AP13 and AP15. These were added as
# "auxiliary profiles," appearing in the Annex A tables alongside the mechanized drillings
# (tradagens - AT). The other two layers are from AP4 (3Cg1) and AT11 (575-615). For both of
# these layers, there is no data for any soil property, so maybe the samples were lost.
check_empty_layer(ctb0073_layer, "areia_muito_grossa")

# Compute mid depth
ctb0073_layer[, mid_depth := (profund_sup + profund_inf) / 2]

# Fill missing areia_muito_grossa using spline interpolation by observacao_id
ctb0073_layer[,
  areia_muito_grossa := fill_empty_layer(areia_muito_grossa, mid_depth),
  by = observacao_id
]

# areia_grossa
# old: "Areia grossa (1-0,5 mm) [g/Kg]"
# new: areia_grossa
# areia_grossa is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia grossa (1-0,5 mm) [g/Kg]", new = "areia_grossa")
ctb0073_layer[, areia_grossa := as.numeric(areia_grossa)]
summary(ctb0073_layer[, areia_grossa])
# Coarse sand is missing for 17 layers, the same layers with missing very coarse sand.
check_empty_layer(ctb0073_layer, "areia_grossa")
# Fill missing areia_grossa using spline interpolation by observacao_id
ctb0073_layer[,
  areia_grossa := fill_empty_layer(areia_grossa, mid_depth),
  by = observacao_id
]

# areia_media
# old: "Areia média (0,5-0,25 mm) [g/Kg]"
# new: areia_media
# areia_media is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia média (0,5-0,25 mm) [g/Kg]", new = "areia_media")
ctb0073_layer[, areia_media := as.numeric(areia_media)]
summary(ctb0073_layer[, areia_media])
# Medium sand is missing for 17 layers, the same layers with missing very coarse and coarse sand.
check_empty_layer(ctb0073_layer, "areia_media")
# Fill missing areia_media using spline interpolation by observacao_id
ctb0073_layer[,
  areia_media := fill_empty_layer(areia_media, mid_depth),
  by = observacao_id
]

# areia_fina
# old: "Areia fina (0,25-0,1 mm) [g/Kg]"
# new: areia_fina
# areia_fina is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia fina (0,25-0,1 mm) [g/Kg]", new = "areia_fina")
ctb0073_layer[, areia_fina := as.numeric(areia_fina)]
summary(ctb0073_layer[, areia_fina])
# Fine sand is missing for 17 layers, the same layers with missing very coarse, coarse, and medium sand.
check_empty_layer(ctb0073_layer, "areia_fina")
# Fill missing areia_fina using spline interpolation by observacao_id
ctb0073_layer[,
  areia_fina := fill_empty_layer(areia_fina, mid_depth),
  by = observacao_id
]

# areia_muito_fina
# old: "Areia muito fina (0,1-0,05 mm) [g/Kg]"
# new: areia_muito_fina
# areia_muito_fina is missing for some layers...
data.table::setnames(ctb0073_layer, old = "Areia muito fina (0,1-0,05 mm) [g/Kg]", new = "areia_muito_fina")
ctb0073_layer[, areia_muito_fina := as.numeric(areia_muito_fina)]
summary(ctb0073_layer[, areia_muito_fina])
# Very fine sand is missing for 17 layers, the same layers with missing coarse, medium, fine, and very coarse sand.
check_empty_layer(ctb0073_layer, "areia_muito_fina")
# Fill missing areia_muito_fina using spline interpolation by observacao_id
ctb0073_layer[,
  areia_muito_fina := fill_empty_layer(areia_muito_fina, mid_depth),
  by = observacao_id
]

# areia
# Combine all sand fractions into a single areia column
ctb0073_layer[
  ,
  areia := areia_muito_grossa + areia_grossa + areia_media + areia_fina + areia_muito_fina
]
ctb0073_layer[, areia := round(areia)]
summary(ctb0073_layer[, areia])
# There are 15 layers with missing sand content. These are the 15 layers from AP13 and AP15.
check_empty_layer(ctb0073_layer, "areia")

# silte
# old: Silte (0,05-0,002 mm) [g/Kg]
# new: silte
data.table::setnames(ctb0073_layer, old = "Silte (0,05-0,002 mm) [g/Kg]", new = "silte")
ctb0073_layer[, silte := as.numeric(silte)]
summary(ctb0073_layer[, silte])
# Silt is missing for 17 layers, the same 15 layers from AP13 and AP15, plus two additional layers
# from AP4 (3Cg1) and AT11 (575-615).
check_empty_layer(ctb0073_layer, "silte")
# Fill missing silte using spline interpolation by observacao_id
ctb0073_layer[,
  silte := round(fill_empty_layer(silte, mid_depth)),
  by = observacao_id
]

# argila
# old: Argila > 0,002 mm (g/kg)
# new: argila
data.table::setnames(ctb0073_layer, old = "Argila (<0,002) [g/Kg]", new = "argila")
ctb0073_layer[, argila := as.numeric(argila)]
summary(ctb0073_layer[, argila])
# Clay is missing for 17 layers, the same 15 layers from AP13 and AP15, plus two additional layers
# from AP4 (3Cg1) and AT11 (575-615).
check_empty_layer(ctb0073_layer, "argila")
# Fill missing argila using spline interpolation by observacao_id
ctb0073_layer[,
  argila := round(fill_empty_layer(argila, mid_depth)),
  by = observacao_id
]

# Check the particle size distribution
# The sum of argila, silte and areia should be 1000 g/kg
ctb0073_layer[, psd := round(argila + silte + areia)]
psd_lims <- 900:1100
# Check the limits
ctb0073_layer[!psd %in% psd_lims & !is.na(psd), .N]
# 0 layers have a sum of the particle size distribution outside the limits.
# Print the rows with psd != 1000
cols <- c("observacao_id", "camada_nome", "profund_sup", "profund_inf", "psd")
ctb0073_layer[!psd %in% psd_lims & !is.na(psd), ..cols]
# No layers with psd != 1000.
ctb0073_layer[, psd := NULL]

# carbono
# old: C [g/Kg]
# new: carbono
data.table::setnames(ctb0073_layer, old = "C [g/Kg]", new = "carbono")
ctb0073_layer[, carbono := as.numeric(carbono)]
summary(ctb0073_layer[, carbono])
# There are 3 layers with missing carbon content: AP4 (3Cg1), AT12 (590-615), and AT18 (480-515).
# AT12 (590-615) actually is in the document, but the data is recorded under AT11. We will fix this
# in the spreadsheet. For AP4 (3Cg1) and AT18 (480-515), we will use spline interpolation to fill
# the missing values.
check_empty_layer(ctb0073_layer, "carbono")
# Fill missing carbono using spline interpolation by observacao_id
ctb0073_layer[,
  carbono := fill_empty_layer(carbono, mid_depth),
  by = observacao_id
]

# ctc
# old: T [cmolc/Kg]
# new: ctc
data.table::setnames(ctb0073_layer, old = "T [cmolc/Kg]", new = "ctc")
ctb0073_layer[, ctc := as.numeric(ctc)]
summary(ctb0073_layer[, ctc])
# There are 3 layers with missing CTC: AP4 (3Cg1), AT12 (590-615), and AT18 (480-515).
check_empty_layer(ctb0073_layer, "ctc")
# Fill missing ctc using spline interpolation by observacao_id
ctb0073_layer[,
  ctc := fill_empty_layer(ctc, mid_depth),
  by = observacao_id
]

# ph
# old: pH 1:2,5 em H_2O
# new: ph
data.table::setnames(ctb0073_layer, old = "pH 1:2,5 em H_2O", new = "ph")
ctb0073_layer[, ph := as.numeric(ph)]
summary(ctb0073_layer[, ph])
# There are 3 layers with missing pH: AP4 (3Cg1), AT12 (590-615), and AT18 (480-515).
check_empty_layer(ctb0073_layer, "ph")
# Fill missing ph using spline interpolation by observacao_id
ctb0073_layer[,
  ph := fill_empty_layer(ph, mid_depth),
  by = observacao_id
]

# Densidade do solo [Kg/dm^-3] -> dsi
data.table::setnames(ctb0073_layer, old = "Densidade do solo [Kg/dm^-3]", new = "dsi")
ctb0073_layer[, dsi := as.numeric(dsi)]
summary(ctb0073_layer[, dsi])
# There are 276 layers with missing soil density. The total number of unique layers presented
# across the AP1-AP12 profiles is 74. Out of these, 17 layers have reported soil density values.
# We better use other methods to estimate soil density rather than interpolation.
check_empty_layer(ctb0073_layer, "dsi")

str(ctb0073_layer)

# Merge ############################################################################################
# events and layers
ctb0073 <- merge(ctb0073_event, ctb0073_layer, all = TRUE)
ctb0073[, dataset_id := "ctb0073"]

# citation
ctb0073 <- merge(ctb0073, ctb0073_citation, by = "dataset_id", all.x = TRUE)
summary_soildata(ctb0073)
# Layers: 322
# Events: 33
# Georeferenced events: 12

# Plot using mapview
if (FALSE) {
  ctb0073_sf <- sf::st_as_sf(
    ctb0073[coord_datum == 4326],
    coords = c("coord_x", "coord_y"), crs = 4326
  )
  mapview::mapview(ctb0073_sf["argila"])
}

# Write to disk ####################################################################################
ctb0073 <- select_output_columns(ctb0073)
data.table::fwrite(ctb0073, "ctb0073/ctb0073.csv")
