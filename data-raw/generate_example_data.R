# This script can be run by package developers to refresh external example files.
# It is intentionally lightweight and does not depend on internet access.

library(phytogrowR)
library(readr)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

write_csv(growth_wide_example, "inst/extdata/growth_wide_example.csv")
write_csv(growth_long_example, "inst/extdata/growth_long_example.csv")
write_csv(harvest_example, "inst/extdata/harvest_example.csv")
