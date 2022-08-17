
library(tidyverse)
library(rstatix)
detach(package:plyr)

colors_a = c('#B0D0D3', '#C08497','#F7AF9D')

groups = read_csv('groups.csv', col_names = TRUE) %>% dplyr::select(-1,-2)
groups_long = groups %>%
  dplyr::select(id, first, second, third) %>%
  pivot_longer(2:4, names_to = 'block', values_to = 'type')


# Affective state --------------------------
dat_affect <- read_csv('dat_affect.csv') %>%
  dplyr::select(Participant_Public_ID, Task_Name, response) %>%
  separate(Task_Name, into = c('task', 'block'), -1) %>%
  mutate(block = ifelse(block == 1, 'first', 
                        ifelse(block==2, 'second', 'third'))) %>%
  dplyr::select(-task) %>%
  rename('id' = 'Participant_Public_ID') %>%
  distinct()

dat_affect_soc = dat_affect %>%
  inner_join(groups_long)

ggplot(dat_affect_soc, aes(x = type, y = response)) +
  geom_boxplot()
ggplot(dat_affect_soc, aes(x = block, y = response)) +
  geom_boxplot()
ggplot(dat_affect_soc, aes(x = block, y = response, fill = type)) +
  geom_boxplot()

  
res.aov <- anova_test(data = dat_affect_soc, dv = response, wid = id, within = type)
get_anova_table(res.aov)
plot(mod_aff)

pwc <- dat_affect_soc %>%
  pairwise_t_test(
    response ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc




# attributes/affiliation -----------------------
dat_affil <- read_csv('dat_affil.csv') 
  dplyr::select(Participant_Public_ID, Task_Name, response) %>%
  separate(Task_Name, into = c('task', 'block'), -1) %>%
  mutate(block = ifelse(block == 1, 'first', 
                        ifelse(block==2, 'second', 'third'))) %>%
  dplyr::select(-task) %>%
  rename('id' = 'Participant_Public_ID') %>%
  distinct()

dat_affect_soc = dat_affect %>%
  inner_join(groups_long)

ggplot(dat_affect_soc, aes(x = type, y = response)) +
  geom_boxplot()
ggplot(dat_affect_soc, aes(x = block, y = response)) +
  geom_boxplot()
ggplot(dat_affect_soc, aes(x = block, y = response, fill = type)) +
  geom_boxplot()



# connection/closeness --------------------------------
dat_close <- read_csv('dat_closeness.csv')


#   Maze Game --------------------------------
dat_maze <- read_csv('dat_maze.csv')


# IDiff ------------------------------
dat_idiff <- read_csv('dat_IDiff.csv')

# Sias ------

# IRI --------------


