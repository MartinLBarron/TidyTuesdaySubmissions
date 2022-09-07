library(tidyverse)

# Example code
all_df <- left_join(inventories, inventory_sets, by = "set_num") |>
  left_join(sets, by = "set_num") 

ex_plot <- all_df |> 
  ggplot(aes(x = num_parts)) +
  geom_density() +
  scale_x_log10()

ggsave(here("2022", "Week36", "output","ex_plot.png"), ex_plot, dpi = "retina", height = 4, width = 6)
