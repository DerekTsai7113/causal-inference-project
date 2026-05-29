# =========================================================
# download_data.R
# Download 2018 MEPS data
# Private insurance vs uninsured analysis
# =========================================================

library(haven)
library(tidyverse)

set.seed(2026)


dir.create(
  "data",
  showWarnings = FALSE
)

dir.create(
  "data/raw",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

# Download 2018 MEPS Full-Year Consolidated File


url = "https://meps.ahrq.gov/mepsweb/data_files/pufs/h209/h209dta.zip"

temp_zip = tempfile(fileext = ".zip")

download.file(
  url,
  destfile = temp_zip,
  mode = "wb"
)

# Unzip


unzipped_files = unzip(
  temp_zip,
  exdir = tempdir()
)


print(unzipped_files)

# Find .dta file

dta_file = unzipped_files[
  grepl("\\.dta$", unzipped_files)
][1]


print(dta_file)

fyc = read_dta(dta_file)

saveRDS(
  fyc,
  "data/raw/fyc2018_raw.rds"
)


print(dim(fyc))

print(names(fyc)[1:30])



# Insurance coding reminder
print("INSCOV18 labels")

print(attr(fyc$INSCOV18, "labels"))


coding_table = data.frame(
  Code = c(1, 2, 3),
  Meaning = c(
    "Any private insurance",
    "Public insurance only",
    "Uninsured"
  )
)

print(coding_table)