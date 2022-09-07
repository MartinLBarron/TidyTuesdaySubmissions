library(tidyverse)
library(here)

inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

saveRDS(inventories,  here("2022", "Week36", "data", "inventories.rds"))
saveRDS(inventory_sets,  here("2022", "Week36", "data", "inventory_sets.rds"))
saveRDS(sets, here("2022", "Week36", "data", "sets.rds"))
