library("readr")
library("sf")


wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",") |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

# Task 1 ----

# Task 2 ----

# Task 3 ----

# Task 4 ----

# Task 5 ----