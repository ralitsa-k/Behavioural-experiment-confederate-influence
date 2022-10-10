
library(tidyverse)
library(rstatix)
detach(package:plyr)
library(lme4)

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

# first get the groups ready (aka which confederate was what type of mimicking)
groups_maze = groups %>%
  select(id, contains('conf')) 
colnames(groups_maze) = c('id','first','second','third')

groups_maze2 = groups_maze %>%
  pivot_longer(2:4, names_to = 'block', values_to = 'conf') %>%
  full_join(groups_long) %>%
  select(id, conf, type)

# read in the maze data (produced by load_data.R)
dat_maze1 <- read_csv('dat_maze.csv') %>%
  filter(Screen_Name %in% c('ChooseDoor', 'Help')) %>%
  pivot_wider(names_from = Screen_Name, values_from = Response) 
colnames(dat_maze1) = c('X1',colnames(dat_maze1)[2:7])



# Fix the one participant with 2 results 
dat_maze_idd = dat_maze1 %>%
  filter(Participant_Public_ID == 'ucjf5it7') %>%
filter(X1 < 264)

dat_maze = dat_maze1 %>%
  filter(Participant_Public_ID != 'ucjf5it7') %>%
  bind_rows(dat_maze_idd)

# Check if each participant has the same number of rows
ddc <- dat_maze %>% count(Participant_Public_ID)

## Confederate choices -----------------------
# Get the maze data for Which confederate was chosen to play with 
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

dat_maze_conf %>%
  ggplot(aes(x = type, y = perc_chosen)) +
  geom_violin() +
  scale_y_continuous(breaks = c(seq(from=0, to =100, 10)))

dat_maze_conf_glm = dat_maze_conf %>%
  mutate(perc_chosen2 = 100-perc_chosen) %>%
  filter(type == 'choice') %>%
  mutate(choice_over_motor = perc_chosen - perc_chosen2)

t.test(dat_maze_conf_glm$choice_over_motor, mu = 0)


# Add a 0 if choice was 100 
dat_maze_conf2 <- dat_maze_conf %>%
  filter(perc_chosen == 100) %>%
  mutate(perc_chosen = 0) %>%
  mutate(type = ifelse(type == 'choice', 'motor','choice')) 

dat_maze_conf = dat_maze_conf %>%
  full_join(dat_maze_conf2)

# Plot maze - probability to choose hint for - choice and motor 
ggplot(dat_maze_conf, aes(x = type, y = perc_chosen)) +
  geom_boxplot()

# Do the t-test (paired samples t-test)
t_dat_maze = dat_maze_conf

t.test(data = t_dat_maze, perc_chosen ~ type, paired = TRUE)
  
dat_maze_conf %>%
  group_by(type) %>%
  summarise(perc = mean(perc_chosen))

## Follow hint -------------------------
# We can keep the full data (as 0's and 1's and do a logistic regression)
# Predicting if mimicking choice vs mimicking motor makes it more likley to follow the hint or not
dat_maze_conf3 = dat_maze %>%
  mutate(conf = ifelse(Help == 'Anna', 'M',
                       ifelse(Help == 'Claire', 'A',
                              ifelse(Help == 'Beth', 'L',0)))) %>%
  select(-ChooseDoor) %>%
  na.omit() %>%
  rename('id' = 'Participant_Public_ID') %>%
  full_join(groups_maze2) %>%
  na.omit() %>% 
  select(-Zone_Name) %>%
  arrange(id) %>%
  mutate(id_n = as.numeric(as.factor(id)),
         qid = rep(1:12, times = length(unique(groups$id)))) %>%
  select(-X1)

ids = unique(dat_maze_conf3$id)

# Only 45 participants found in maze game choice - should 46, add additional 
# Was confederate followed or not? 
dat_maze_resp = dat_maze %>%
  rename('id' = 'Participant_Public_ID') %>%
  filter(id %in% ids) %>%
  filter(Zone_Name %!in% c('AnnaButton','BethButton')) %>%
  mutate(hint = ifelse(str_detect(Hint, 'left'), 'Left',
                       ifelse(str_detect(Hint, 'right'), 'Right', 0))) %>%
  arrange(id) %>%
  mutate(id_n = as.numeric(as.factor(id)),
         qid = rep(1:12, times = length(unique(groups$id)))) %>%

  select(-Help, - X1) %>%
  mutate(chose_hint = ifelse(hint == ChooseDoor,1 ,0)) %>%
  full_join(dat_maze_conf3)

mod_maze = glm(data = dat_maze_resp, chose_hint ~ type, family = 'binomial')
summary(mod_maze)


hint_plot = dat_maze_resp %>%
  group_by(type,id) %>%
  summarise(perc_followed_hint = mean(chose_hint)*100)

ggplot(hint_plot, aes(x = type, y = perc_followed_hint))+
  geom_boxplot() + 
  geom_jitter(alpha = 0.3)

dat_maze_resp %>%
  group_by(type) %>%
  summarise(perc = mean(chose_hint)*100)

# for choice mimicker they followed  85.7% (out of all choice mimicking trials) 
# for motor mimicker they followed 78.1% (of all trials with motor mimicking) 



# IDiff ------------------------------
dat_idiff <- read_csv('dat_IDiff.csv') %>%
  filter(Task_Name == "self construal-dependence/independence") %>%
  filter(str_detect(Question_Key,'quantised')) %>%
  mutate(followed_hint)

iri_scoring <- read_csv('iri_scoring.csv', col_names = TRUE)
sias_scoring <- read_csv('SIAS_scoring.csv',col_names = TRUE)
# Sias ------
dat_idiff_sias <-read_csv('dat_IDiff.csv') %>%
  filter(grepl('SIAS', Question_Key)) %>%
  filter(!grepl('quantised', Question_Key)) %>%
  mutate(SIAS_id = Question_Key) %>%
  full_join(sias_scoring) %>%
  mutate(final_score = ifelse(score_order == 0, Response, 4 - Response))

# IRI --------------


