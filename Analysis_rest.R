
library(tidyverse)
detach(package:plyr)

colors_a = c('#B0D0D3', '#C08497','#F7AF9D')

groups = read_csv('groups.csv', col_names = TRUE) %>% dplyr::select(-1,-2)
groups_long = groups %>%
  dplyr::select(id, first, second, third) %>%
  pivot_longer(2:4, names_to = 'block', values_to = 'type')


# Affective state --------------------------
dat_affect <- read_csv('dat_affect.csv')



# attributes/affiliation -----------------------
dat_affil <- read_csv('dat_affil.csv')



# connection/closeness --------------------------------
dat_close <- read_csv('dat_closeness.csv')


#   Maze Game --------------------------------
dat_maze <- read_csv('dat_maze.csv')


# IDiff ------------------------------
dat_idiff <- read_csv('dat_IDiff.csv')

# Sias ------

# IRI --------------


