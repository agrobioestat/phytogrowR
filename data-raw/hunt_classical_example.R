# data-raw/hunt_classical_example.R
# Regenerates `hunt_classical_example` from in-package simulation code.

# Run from package root:
# usethis::use_data(hunt_classical_example, overwrite = TRUE)

library(phytogrowR)

# Inspect current dataset used in package examples/tests.
print(dplyr::glimpse(hunt_classical_example))
