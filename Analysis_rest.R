
library(tidyverse)
library(rstatix)
detach(package:plyr)

`%!in%` = Negate(`%in%`)

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

pwc <- dat_affect_soc %>%
  pairwise_t_test(
    response ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc




# attributes/affiliation -----------------------
dat_affil <- read_csv('dat_affil.csv') %>%
  dplyr::select(Participant_Public_ID, response,Question_Key) %>%
  separate(Question_Key, into = c('task', 'block'), -1) %>%
  mutate(block = ifelse(block == 1, 'first', 
                        ifelse(block==2, 'second', 'third'))) %>%
  rename('id' = 'Participant_Public_ID') %>%
  distinct()

# Get one repsonse per participant per block
# reverse code some items 
rev_sc = c('Distrust', 'Hostility', 'Avoidance')
r_sc <- paste(rev_sc, collapse="|")

afil_recoded <- dat_affil %>%
  mutate(Resp = ifelse(str_detect(task, r_sc),100-response, response))

# add confederate type by block and get average per block
dat_afil_soc = afil_recoded %>%
  inner_join(groups_long) %>%
  select(id, type, Resp, type, block) %>%
  distinct() %>%
  group_by(id, type, block) %>%
  summarise(Resp = mean(Resp)) %>%
  ungroup()
  

ggplot(dat_afil_soc, aes(x = type, y = Resp)) +
  geom_boxplot()
ggplot(dat_afil_soc, aes(x = block, y = Resp)) +
  geom_boxplot()
ggplot(dat_afil_soc, aes(x = block, y = Resp, fill = type)) +
  geom_boxplot()

res.aov2 <- anova_test(data = dat_afil_soc, dv = Resp, wid = id, within = type)
get_anova_table(res.aov2)

pwc2 <- dat_afil_soc %>%
  pairwise_t_test(
    Resp ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


# connection/closeness --------------------------------
dat_close <- read_csv('dat_closeness.csv') %>%
  dplyr::select(Participant_Public_ID, block, response) %>%
  rename('id' = 'Participant_Public_ID') %>%
  mutate(block = ifelse(block == 1, 'first', 
                        ifelse(block==2, 'second', 'third'))) %>%
  right_join(groups_long) %>%
  distinct()

dat_close %>%
  ggplot(aes(x = type, y = response)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

res.aov3 <- anova_test(data = dat_close, dv = response, wid = id, within = type)
get_anova_table(res.aov3)

pwc3 <- dat_close %>%
  pairwise_t_test(
    response ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc3


#   Maze Game --------------------------------

groups_maze = groups %>%
  select(id, contains('conf')) 
colnames(groups_maze) = c('id','first','second','third')

groups_maze2 = groups_maze %>%
  pivot_longer(2:4, names_to = 'block', values_to = 'conf') %>%
  full_join(groups_long) %>%
  select(id, conf, type)

dat_maze <- read_csv('dat_maze.csv') %>%
  filter(Screen_Name %in% c('ChooseDoor', 'Help')) %>%
  pivot_wider(names_from = Screen_Name, values_from = Response) %>%
  filter(Participant_Public_ID != 'ucjf5it7') 

dat_maze_conf = dat_maze %>%
  filter(Zone_Name %in% c('AnnaButton', 'BethButton')) %>%
  group_by(Participant_Public_ID, Help) %>%
  count() %>%
  mutate(perc_chosen = n/12*100) %>%
  mutate(conf = ifelse(Help == 'Anna', 'M',
                       ifelse(Help == 'Claire', 'A',
                              ifelse(Help == 'Beth', 'L',0)))) %>%
  mutate(id = Participant_Public_ID) %>%
  full_join(groups_maze2) %>%
  na.omit() 

ggplot(dat_maze_conf, aes(x = type, y = perc_chosen)) +
  geom_boxplot()

t_dat_maze = dat_maze_conf %>%
  filter(perc_chosen != 100)

t.test(data = t_dat_maze, perc_chosen ~ type, paired = TRUE)
  
summary_perc = dat_maze_conf %>%
  group_by(type) %>%
  summarise(perc = mean(perc_chosen))


dat_maze_resp = dat_maze %>%
  filter(Zone_Name %!in% c('AnnaButton','BethButton')) %>%
  mutate(hint = ifelse(str_detect(Hint, 'left'), 'Left',
                       ifelse(str_detect(Hint, 'right'), 'Right', 0))) %>%
  mutate(chose_hint = ifelse(hint == ChooseDoor,1 ,0)) %>%
  group_by(Participant_Public_ID) %>%
  mutate(perc_followed = sum(chose_hint)/12*100)
  
  
  mutate(confed = ifelse(str_detect(Task_Name, 'Amie'), 'Claire',
                         ifelse(str_detect(Task_Name, 'Lucile'), 'Beth',
                                ifelse(str_detect(Task_Name, 'Anna'), 'Marie', 0))),
         hint = ifelse(str_detect(Hint, 'left'), 'Left',
                       ifelse(str_detect(Hint, 'right'), 'Right', 0))) %>%
  mutate(chose_hint = ifelse(hint == Response,1 ,0)) %>%
  group_by(Participant_Public_ID) %>%
  mutate(perc_followed = sum(chose_hint)/12*100)

mutate(followed_hint )





# IDiff ------------------------------
dat_idiff <- read_csv('dat_IDiff.csv') %>%
  filter(Screen_name == )
  mutate(followed_hint )

# Sias ------

# IRI --------------


