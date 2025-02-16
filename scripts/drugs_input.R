library(data.table)
library(here)

i_am("scripts/drugs_input.R")

# Source: PubChem ID exchange based on CMap drug identifiers.
drugs_cmap_pubchem <- fread(here("scripts/input/drugs_cmap_pubchem.tsv"))
drugs_cmap_pubchem <- na.omit(drugs_cmap_pubchem)

# Source: UniChem ID mapping
drugs_chembl_pubchem <- fread(here("scripts/input/drugs_chembl_pubchem.tsv"))

# Source: ChEMBL SQLite database
# SELECT DISTINCT
#   chembl_id,
#   synonyms AS name,
#   mesh_heading AS indication,
#   mechanism_of_action
# FROM molecule_dictionary
#   LEFT JOIN drug_indication
#     ON molecule_dictionary.molregno = drug_indication.molregno
#   LEFT JOIN drug_mechanism
#     ON molecule_dictionary.molregno = drug_mechanism.molregno
#   LEFT JOIN (
#       SELECT molregno, synonyms FROM molecule_synonyms WHERE syn_type == 'INN'
#     ) AS molecule_synonyms
#     ON molecule_dictionary.molregno = molecule_synonyms.molregno
#   WHERE name IS NOT NULL
#     OR indication IS NOT NULL
#     OR mechanism_of_action IS NOT NULL;
drugs_chembl <- fread(here("scripts/input/drugs_chembl.csv"))

# Source: PubChem ID list upload based on identifiers converted from CMap
# drug names using the PubChem ID exchange.
drugs_pubchem <- fread(here("scripts/input/drugs_pubchem.csv"))

drugs_pubchem <- drugs_pubchem[, .(cid, cmpdname, annotation)]
drugs_pubchem <- unique(drugs_pubchem, by = "cid")
drugs_pubchem <- drugs_pubchem[,
  .(
    cmpdname,
    annotation = strsplit(annotation, "|", fixed = TRUE) |> unlist()
  ),
  by = cid
]

# Filter for WHO ATC annotations
drugs_pubchem <- drugs_pubchem[stringr::str_detect(annotation, "^[A-Z] - ")]

# Extract ATC levels

drugs_pubchem[, atc_1 := stringr::str_match(
  annotation,
  "^[A-Z] - ([^>]*)"
)[, 2] |> stringr::str_trim()]

drugs_pubchem[, atc_2 := stringr::str_match(
  annotation,
  "> [A-Z][0-9][0-9] - ([^>]*)"
)[, 2] |> stringr::str_trim()]

drugs_pubchem[, atc_3 := stringr::str_match(
  annotation,
  "> [A-Z][0-9][0-9][A-Z] - ([^>]*)"
)[, 2] |> stringr::str_trim()]

drugs_pubchem <- drugs_pubchem[, .(cid, cmpdname, atc_1, atc_2, atc_3)]
setnames(drugs_pubchem, c("cid", "cmpdname"), c("pubchem_cid", "pubchem_name"))

drugs <- merge(
  drugs_cmap_pubchem,
  drugs_chembl_pubchem,
  by = "pubchem_cid",
  all.x = TRUE
)

drugs <- merge(
  drugs,
  drugs_chembl,
  by = "chembl_id",
  all.x = TRUE
)

drugs <- merge(
  drugs,
  drugs_pubchem,
  by = "pubchem_cid",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Prefer INN name, then PubChem, then CMap:
drugs[name == "", name := NA]
drugs[is.na(name), name := pubchem_name]
drugs[name == "", name := NA]
drugs[is.na(name), name := stringr::str_to_sentence(drug)]
drugs[, pubchem_name := NULL]

# Clean up empty values:
drugs[indication == "", indication := NA]
drugs[mechanism_of_action == "", mechanism_of_action := NA]

fwrite(drugs, file = here("scripts/output/drugs.csv"))
