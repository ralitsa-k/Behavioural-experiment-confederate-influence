
library(tidyverse)
library(rstatix)
library(lme4)
library(ggsignif)


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
  distinct() %>%
  rename('score' = 'response')

dat_affect_soc = dat_affect %>%
  mutate(response = block) %>%
  full_join(groups_long) %>%
  mutate(score = ifelse(score == 0, 31, score)) %>%
  na.omit()
  

ggplot(dat_affect_soc, aes(x = type, y = score)) +
  geom_boxplot()
ggplot(dat_affect_soc, aes(x = block, y = score)) +
  geom_boxplot()
ggplot(dat_affect_soc, aes(x = block, y = score, fill = type)) +
  geom_boxplot()

dat_model = dat_affect_soc %>%
  dplyr::select( -block) %>%
  ungroup() %>%
  unique()
  
dat_affect_soc %>%
  ggplot(aes(x= type, y = score))+ geom_boxplot()

m_aff <- aov(data = dat_model, score ~ type + Error(id/type))
summary(m_aff)

res.aov <- anova_test(data = dat_model, dv = score, wid = id, within = type)
get_anova_table(res.aov)

pwc <- dat_model %>%
  distinct() %>%
  pairwise_t_test(
    score ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

pwc

dat_model %>%
  group_by(type) %>% summarise(mean(score), sd(score))


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
  dplyr::select(-response) %>%
  inner_join(groups_long) %>%
  dplyr::select(id, type, Resp, type, block) %>%
  distinct()  %>%
  group_by(id, type, block) %>%
  summarise(Resp = mean(Resp)) %>%
  ungroup()
  
write.csv(dat_afil_soc, 'afil_recoded_social_added.csv')


ggplot(dat_afil_soc, aes(x = type, y = Resp)) +
  geom_boxplot()
ggplot(dat_afil_soc, aes(x = block, y = Resp)) +
  geom_boxplot()
ggplot(dat_afil_soc, aes(x = block, y = Resp, fill = type)) +
  geom_boxplot()


res.aov2 <- anova_test(data = dat_afil_soc, dv = Resp, within = type, wid = id)
get_anova_table(res.aov2)



pwc2 <- dat_afil_soc %>%
  pairwise_t_test(
    Resp ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

dat_afil_soc %>%
  group_by(type) %>%
  summarise(mean(Resp), sd(Resp))


# Affiliation separate quesitons ----------------------
afil_questions = groups_long %>%
 # rename('block' = 'response') %>%
  full_join(afil_recoded) %>%
  mutate(question = as.numeric(as.factor(task)))

tasks = unique(afil_questions$task)

for (i in 1:7){
afil_questions2 = groups_long %>%
  #rename('block' = 'response') %>%
  full_join(afil_recoded) %>%
  mutate(question = as.numeric(as.factor(task))) %>%
  filter(task == tasks[i]) %>%
  filter(type != 'control')

#m3 = aov(data = afil_questions2, Resp ~ type + Error(id))
#summary(m3)
res.aov <- anova_test(data = afil_questions2, dv = Resp, wid = id, within = type)
get_anova_table(res.aov)

print(paste0('Test of response depending on mimicking type within task ', tasks[i],' p = ',round(get_anova_table(res.aov)[[5]],4)))
}

ggplot(afil_questions,aes(x = task, y = Resp, fill = type))+
  geom_boxplot()
ggplot(afil_questions,aes(x = task, y = response, fill = type))+
  geom_boxplot() 


# connection/closeness --------------------------------
dat_close <- read_csv('dat_closeness.csv') %>%
  dplyr::select(Participant_Public_ID, block, response) %>%
  rename('id' = 'Participant_Public_ID') %>%
  mutate(block = ifelse(block == 1, 'first', 
                        ifelse(block==2, 'second', 'third'))) %>%

  right_join(groups_long) %>%
  distinct() %>%
  mutate(response = (response/7)*100)
  

dat_close %>%
  ggplot(aes(x = type, y = response)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

res.aov <- anova_test(data = dat_close, dv = response, within = c(type), wid = id)
get_anova_table(res.aov)

dat_close %>%
  group_by(type) %>%
  summarise(mean(response), sd(response))

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
  mutate(chose_hint = ifelse(hint == ChooseDoor, 1 ,0)) %>%
  full_join(dat_maze_conf3) %>%
  mutate(contr = ifelse(type=='motor', 1, 0))

mod_maze = glm(data = dat_maze_resp, chose_hint ~ contr, family =  binomial(link = "logit"))
summary(mod_maze)

exp(as.numeric(mod_maze$coefficients[2]))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(as.numeric(mod_maze$coefficients[2]))

chisq <- chisq.test(dat_maze_resp$type,dat_maze_resp$chose_hint)
chisq
dat_maze_resp %>%
  group_by(type) %>%
  count(chose_hint) %>%
  pivot_wider(names_from = type, values_from = n)

# Effect size ------------
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE, col= colorRampPalette(c("white","pink", "red"))(100) )


# Out of the trials they chose to play with this person, how many times they followed their hint? 
#

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
  filter(str_detect(Question_Key,'quantised'))

iri_scoring <- read_csv('iri_scoring.csv', col_names = TRUE)
sias_scoring <- read_csv('SIAS_scoring.csv',col_names = TRUE)

# Sias ------
dat_idiff_sias <-read_csv('dat_IDiff.csv') %>%
  filter(grepl('SIAS', Question_Key)) %>%
  filter(!grepl('quantised', Question_Key)) %>%
  mutate(SIAS_id = Question_Key) %>%
  full_join(sias_scoring) %>%
  mutate(Response = as.numeric(Response)) %>%
  mutate(final_score = ifelse(score_order == 0, Response, 4 - Response))

dat_sias = dat_idiff_sias %>%
  group_by(Participant_Public_ID) %>%
  summarise(mean_score = mean(final_score)) %>%
  na.omit() %>%
  mutate(id = Participant_Public_ID)

sias_bi_liking = bi_dat_by_group %>%
  full_join(dat_sias)

sias_bi_liking %>%
  na.omit() %>% 
ggplot(aes(x = factor(type), y = mean_score, fill = Type_of_question)) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.3)

log_reg <- glmer(factor(type) ~ Type_of_question * mean_score + (1|id), data = sias_bi_liking, family = 'binomial')
summary(log_reg)

## SIAS for motor condition only ------------------
sias_motor <- sias_bi_liking %>%
  filter(type == 'motor')
log_reg <- lm(mean_score ~ Type_of_question, data = sias_motor)
summary(log_reg)

sias_motor <- sias_bi_liking %>%
  filter(type == 'control')
log_reg <- lm(mean_score ~ Type_of_question, data = sias_motor)
summary(log_reg)



# IRI --------------
dat_idiff_iri <-read_csv('dat_IDiff.csv') %>%
  filter(grepl('IRI', Question_Key)) %>%
  filter(!grepl('quantised', Question_Key)) %>%
  mutate(IRI_id = Question_Key) %>%
  full_join(iri_scoring) %>%
  mutate(Response = as.numeric(Response)) %>%
  mutate(final_score = ifelse(score_order == 0, Response, 4 - Response))

dat_iri = dat_idiff_iri %>%
  group_by(Participant_Public_ID) %>%
  summarise(mean_score = mean(final_score)) %>%
  na.omit() %>%
  mutate(id = Participant_Public_ID)

iri_bi_liking = bi_dat_by_group %>%
  full_join(dat_iri)

log_reg <- glmer(factor(type) ~ Type_of_question * mean_score + (1|id), data = iri_bi_liking, family = 'binomial')
summary(log_reg)

iri_bi_liking %>%
  na.omit() %>% 
  ggplot(aes(x = factor(type), y = mean_score, fill = Type_of_question)) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.3)


# SC scores ---------------
sc_scoring <- read_csv('sc_scoring.csv')

dat_sc <-read_csv('dat_SC.csv') %>%
  full_join(sc_scoring) %>%
  mutate(id = Participant_Public_ID) %>%
  group_by(id, Scale) %>%
  summarise(score_sc = sum(Response)) %>%
  pivot_wider(names_from = Scale, values_from = score_sc) %>%
  mutate(mean_score = ind - intr)

dat_sc_liking = bi_dat_by_group %>%
  full_join(dat_sc) %>%
  filter(type!='control') %>%
  filter(id != 'ucjf5it7')

log_reg <- glmer(factor(type) ~ Type_of_question * mean_score + (1|id), data = dat_sc_liking, family = 'binomial')
summary(log_reg)

counts_d <- dat_sc_liking %>%
  group_by(id) %>%
  filter(id != 'ucjf5it7') %>%
  mutate(n = n()) %>%
  group_by(id, type) %>%
  mutate(count_type = n())%>%
  dplyr::select(id, type, count_type, n, mean_score) %>%
  distinct() %>%
  mutate(perc_chosen = (count_type/n)*100)

ggplot(counts_d, aes(x = mean_score, y = perc_chosen, color = type)) +
  geom_point() +
  geom_smooth(method = 'lm')



# higher interdependence 

counts_d2 <- dat_sc_liking %>%
  group_by(id) %>%
  filter(id != 'ucjf5it7') %>%
  mutate(n = n()) %>%
  group_by(id, type) %>%
  mutate(count_type = n())%>%
  dplyr::select(id, type, count_type, n, mean_score) %>%
  distinct() %>%
  mutate(perc_chosen = (count_type/n)*100) %>%
  dplyr::select(-n, -count_type) %>%
  pivot_wider(names_from = type, values_from = perc_chosen) %>%
  mutate(choice = ifelse(is.na(choice), 0, choice),
         motor = ifelse(is.na(motor), 0, motor)) %>%
  mutate(choice_more_than_motor = choice- motor)

dat_sc2 <-read_csv('dat_SC.csv') %>%
  full_join(sc_scoring) %>%
  mutate(id = Participant_Public_ID) %>%
  group_by(id, Scale) %>%
  summarise(score_sc = sum(Response)) %>%
  pivot_wider(names_from = Scale, values_from = score_sc) 

intr_choice <- counts_d2 %>%
  inner_join(dat_sc2)

ggplot(intr_choice, aes(intr, choice_more_than_motor)) +
  geom_point() +
  geom_smooth(method = 'lm')
  
# Interdependence does not predict choosing choice mimicker more often than
#    motor mimicker 


# Art interest indiff -----------------------------

dat_art_indiff <- read_csv('dat_art.csv') %>%
  filter(Response < 10) %>%
  mutate(id = Participant_Public_ID) 

art_interest_full <- dat_art_indiff %>%
  group_by(id) %>%
  summarise(full_score = mean(Response))

# Art and triple questions
liking_triple <- read_csv('liking_triple_questions.csv')

dat_art_indiff_soc <- dat_art_indiff %>%
  full_join(liking_triple, by = 'id') %>%
  na.omit() %>%
  distinct() %>%
  group_by(id) %>%
  mutate(full_score = mean(Response))


ggplot(dat_art_indiff, aes(x = Question_Key, y = Response)) +
  geom_boxplot() +
  geom_jitter()


ggplot(dat_art_indiff_soc, aes(x = Question_Key, y = full_score, color = type)) +
  geom_boxplot() +
  geom_jitter()

liking_art <- dat_art_indiff %>%
  mutate(id = Participant_Public_ID) %>%
  group_by(id) %>%
  summarise(mean_art= mean(Response)) %>%
  full_join(liking_triple, by = 'id') %>%
  group_by(id, mean_art, type) %>%
  summarise(mean_liking = mean(perc)) %>%
  ungroup() %>%
  na.omit()

# Tripple questions PLOT
liking_art %>%
  na.omit() %>%
ggplot(aes(x = mean_art, mean_liking)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(~type)

## Liking influenced by art interest (tripple questions) -----------------------

art.aov <- anova_test(data = liking_art, dv = mean_liking, wid = id, within = type, between = mean_art)
get_anova_table(art.aov)



## Art interest ------------

# Art separate 3 questions
liking_art <- dat_art_indiff %>%
  mutate(id = Participant_Public_ID) %>%
  filter(Question_Key == 'art_interest') %>%
  group_by(id) %>%
  summarise(mean_art= mean(Response)) %>%
  full_join(liking_triple, by = 'id') %>%
  group_by(id, mean_art, type) %>%
  summarise(mean_liking = mean(perc)) %>%
  ungroup() %>%
  na.omit()

# Tripple questions PLOT
liking_art %>%
  na.omit() %>%
  ggplot(aes(x = mean_art, mean_liking)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(~type)

art.aov <- anova_test(data = liking_art, dv = mean_liking, wid = id, within = type, between = mean_art)
get_anova_table(art.aov)

# Art interest no interaction 

## Art knowledge  -----------------------

liking_art <- dat_art_indiff %>%
  mutate(id = Participant_Public_ID) %>%
  filter(Question_Key == 'art_knowledge') %>%
  group_by(id) %>%
  summarise(mean_art= mean(Response)) %>%
  full_join(liking_triple, by = 'id') %>%
  group_by(id, mean_art, type) %>%
  summarise(mean_liking = mean(perc)) %>%
  ungroup() %>%
  na.omit()

# Tripple questions PLOT
liking_art %>%
  na.omit() %>%
  ggplot(aes(x = mean_art, mean_liking)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(~type)

art.aov <- anova_test(data = liking_art, dv = mean_liking, wid = id, within = type, between = mean_art)
get_anova_table(art.aov)


## Art visit -----------------------

liking_art <- dat_art_indiff %>%
  mutate(id = Participant_Public_ID) %>%
  filter(Question_Key == 'art_visits') %>%
  group_by(id) %>%
  summarise(mean_art= mean(Response)) %>%
  full_join(liking_triple, by = 'id') %>%
  group_by(id, mean_art, type) %>%
  summarise(mean_liking = mean(perc)) %>%
  ungroup() %>%
  na.omit()

# Tripple questions PLOT
liking_art %>%
  na.omit() %>%
  ggplot(aes(x = mean_art, mean_liking)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(~type)

art.aov <- anova_test(data = liking_art, dv = mean_liking, wid = id, within = type, between = mean_art)
get_anova_table(art.aov)





## ART on Rating questions -----------------------

rating_questions_scores <-  read_csv('rating_questions_scores.csv') %>%
  group_by(id,type) %>%
  summarise(mean_rating = mean(mean_r)) %>%
  full_join(art_interest_full)



# Does art interest (individual difference = full_score) influence rating of each type (mimick)
summary(lm(data = rating_questions_scores, mean_rating ~ full_score*type))
# Not significant, no difference on rating scored depedning on art interest 

rating_questions_scores %>%
  na.omit() %>%
  ggplot(aes(x = full_score, y = mean_rating)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(~type) +
  labs(x = 'Art score', y = 'Rating score')

# Dividing ratings by warmth and competence does not result in significant results 
rating_questions_scores <-  read_csv('rating_questions_scores.csv') %>%
  group_by(id,type, Type_of_question) %>%
  summarise(mean_rating = mean(mean_r)) %>%
  full_join(art_interest_full)

summary(lm(data = rating_questions_scores, mean_rating ~ full_score*type*Type_of_question))




library(tidyverse)
library(lme4)

colors_a = c('#B0D0D3', '#C08497','#F7AF9D')

groups = read_csv('groups.csv', col_names = TRUE) %>%dplyr::select(-1,-2)
groups_long = groups %>%
  dplyr::select(id, first, second, third) %>%
  pivot_longer(2:4, names_to = 'block', values_to = 'type')


# Load rapport data  ----------------
dat_rapport = read_csv('rap_data.csv')

dat_rapport2 = dat_rapport %>%
  dplyr::select('Participant_Public_ID','Question_Key','Response')
colnames(dat_rapport2) <- c('id', 'question', 'response')

dat_rapport3 <- dat_rapport2 %>%
  mutate(response = as.numeric(response)) %>%
  filter(response < 101) %>%
  separate(question, into = c('question', 'block'), -1) %>%
  mutate(block = ifelse(block == 1, 'first',
                        ifelse(block == 2, 'second', 'third'))) %>%
  mutate(Type_of_question = ifelse(str_detect(question, 'understanding'), 'understanding',
                                   ifelse(str_detect(question, 'connection'), 'connection',
                                          ifelse(str_detect(question, 'established'), 'established', 0)))) %>%
  full_join(groups_long) %>%
  na.omit() %>%
  dplyr::rename('Mimicker' = 'type') %>%
  distinct() %>%
  filter(response > 3)

write.csv(dat_rapport3, 'tidy_rapport.csv')

## Plot ------------------------
dat_rapport3 %>% 
  ggplot(aes(x = Mimicker, y = response, fill = Type_of_question)) +
  geom_boxplot() +
  scale_fill_manual(values=colors_a) +
  labs(y = 'Response', title = 'Means for rapport for each mimick type') +
  theme_minimal() 

dat_rapport3 %>% 
  ggplot(aes(x = Mimicker, y = response)) +
  geom_boxplot() +
  scale_fill_manual(values=colors_a) +
  labs(y = 'Response', title = 'Means for rapport for each mimick type') +
  theme_minimal()

mod_rapport_3 <- dat_rapport3 %>%
  group_by(id, Mimicker) %>%
  summarise(mean_resp = mean(response)) %>%
  ungroup() %>%
  filter(id != 'p1el4un1')

## analysis with type of question -----------------------
res.aov <- aov(data = mod_rapport_3, mean_resp ~ Mimicker + Error(id))
summary(res.aov)

res.aov <- anova_test(data = mod_rapport_3, dv = mean_resp, within = c(Mimicker), wid = id)
get_anova_table(res.aov)

pwc <- mod_rapport_3 %>%
  pairwise_t_test(
    mean_resp ~ Mimicker, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
# diagnostics are fine for this model check with:  plot(mod1)

mod_rapport_3 %>%
  group_by(Mimicker) %>%
  summarise(mean(mean_resp), sd(mean_resp))


# analysis without type of question
mod2 <- lm(data = dat_rapport3, response ~ Mimicker)
summary(mod2)


## blocks by order, rapport ?  ----------------------------


dat_rapport3 %>% 
  ggplot(aes(x = block, y = response, fill = Type_of_question)) +
  geom_boxplot() +
  scale_fill_manual(values=colors_a) +
  labs(y = 'Response', title = 'Means for rapport for each mimick type') +
  theme_minimal()

mod3 <- lm(data = dat_rapport3, response ~ block * Type_of_question )
summary(mod3)
# Third block is significantly different from the first one, higher rapport in third block 

dat_rapport3 %>%
  filter(block != 'third') %>%
  ggplot(aes(x = block, y = response, fill = Mimicker)) +
  geom_boxplot() +
  scale_fill_manual(values=colors_a) +
  labs(y = 'Response', title = 'Means for rapport for each mimick type') +
  theme_minimal()

dat_rapport_blocks = dat_rapport3 %>%
  filter(block != 'third')

mod3 <- lm(data = dat_rapport_blocks, response ~ block * Mimicker )
summary(mod3)





# Affiliation, Rapport, Closeness, Rating questions -----------------------
# cronbachs 


afil_recoded_social <- read_csv('afil_recoded_social_added.csv') %>%
  mutate(afil_resp = Resp)

rapport_tidy <- read_csv('tidy_rapport.csv') %>%
  mutate(type = Mimicker) %>%
  group_by(id, type) %>%
  summarise(mean_rapport = mean(response)) 
  

dat_close <- read_csv('dat_closeness.csv') %>%
  mutate(close_resp = response) %>%
  dplyr::select(Participant_Public_ID, block, close_resp) %>%
  rename('id' = 'Participant_Public_ID') %>%
  mutate(block = ifelse(block == 1, 'first', 
                        ifelse(block==2, 'second', 'third'))) %>%
  mutate(response = block) %>%
  right_join(groups_long, by = c('id')) %>%
  distinct() %>%
  dplyr::select(id, type, close_resp) 


rating_questions_scores <-  read_csv('rating_questions_scores.csv') %>%
  group_by(id,type) %>%
  summarise(mean_rating = mean(mean_r)) 


general_liking <- afil_recoded_social %>%
  full_join(dat_close) %>%
  full_join(rating_questions_scores) %>%
  full_join(rapport_tidy) %>%
  dplyr::select(5:9) %>%
  na.omit()

cronbach.alpha(general_liking)



