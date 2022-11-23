
library(tidyverse)
library(psy)
library(lme4)
colors_a = c('#B0D0D3', '#C08497','#F7AF9D')

groups = read_csv('groups.csv', col_names = TRUE) %>%dplyr::select(-1,-2)
groups_long = groups %>%
  dplyr::select(id, first, second, third) %>%
  pivot_longer(2:4, names_to = 'response', values_to = 'type')

groups_order = groups %>%
  dplyr::select(id, conf1, conf2, conf3) %>%
  pivot_longer(2:4, names_to = 'block', values_to = 'confed_name') %>%
  mutate(block = ifelse(block == 'conf1', 'first', 
                        ifelse(block == 'conf2', 'second',
                               ifelse(block == 'conf3', 'third', 0))))

# ADD art interest
dat_art_indiff <- read_csv('dat_art.csv') %>%
  filter(Response < 10) %>%
  mutate(id = Participant_Public_ID) 

art_interest_full <- dat_art_indiff %>%
  group_by(id) %>%
  summarise(full_score = mean(Response))




groups_mim = groups_long %>%
  rename('block' = 'response') %>%
  full_join(groups_order)



# Rating data ------------------------------
dat_rating2 <- read_csv('ratings_data.csv', col_names = TRUE) %>%
  dplyr::select(Participant_Public_ID, Question_Key, Response) 

colnames(dat_rating2) <- c('id','question','response')

dat_rating3 <- dat_rating2 %>%
  filter(response < 10) %>%
  filter(str_detect(question, "quantised")) 

# group the data by type of question 
warmth_k = c('friendly', 'similarity', 'attractiveness')
comp_k = c('competent', 'art_knowledge', 'rational')
w_k <- paste(warmth_k, collapse="|") # "ll|lo|hl"
ratings_data <- dat_rating3 %>%
  mutate(rating = response) %>%
  mutate(Type_of_question = ifelse(str_detect(question, w_k),'warmth','competence'),
         response = ifelse(str_detect(question, '1'), 'first',
                           ifelse(str_detect(question, '2'), 'second',
                                  ifelse(str_detect(question, '3'),'third', 'baseline'))))

ratings_by_group = ratings_data %>%
  full_join(groups_long) %>%
  mutate(type = ifelse(response == 'baseline', 'baseline', type)) %>%
  na.omit() %>%
  mutate(confed_name = ifelse(str_detect(question, 'Lucile'), 'L', 
                              ifelse(str_detect(question, 'Maria'), 'M',
                                     ifelse(str_detect(question, 'Amie'), 'A', 99)))) %>%
  rename('block' = 'response')



# Ratings across mimicking for each question --------------------

vars2 = c('friendly', 'competent', 'attractiveness', 
         'art_knowledge', 'similarity', 'rational')

for (i in 1:6) {
ratings_by_group_fr <- ratings_by_group %>%
  filter(grepl(vars2[i], question)) %>%
  filter(block != 'baseline') %>%
  distinct()

mod_Fr <- aov(data = ratings_by_group_fr, rating~type)
summary(mod_Fr)

TukeyHSD(mod_Fr)

gg = ggplot(ratings_by_group_fr, aes(type, rating))+
  geom_violin() +
  geom_jitter() +
  ggtitle(paste0(vars2[i], ' has a p value of ',
                 round(summary(mod_Fr)[[1]][['Pr(>F)']][1],4)))   
  print(paste0(vars2[i], ' has a p value of ',
               round(summary(mod_Fr)[[1]][['Pr(>F)']][1],4)))
print(gg)
}

# Ratings questions chronbach alpha -------------------
chr_d <- ratings_by_group %>%
  dplyr::select(id, question, rating) %>%
  na.omit() %>%
  mutate(confed_name = ifelse(str_detect(question, 'Lucile'), 'L', 
                              ifelse(str_detect(question, 'Maria'), 'M',
                                     ifelse(str_detect(question, 'Amie'), 'A', 99)))) %>%
  filter(confed_name == 99) %>%
  distinct() %>%
  pivot_wider(names_from = question, values_from = rating) %>%
  dplyr::select(-1,-2) %>%
  dplyr::select(contains(c('friendly', 'similar', 'attractiveness')))
(0.8 + 0.744 + 0.81)/3
# Average of cronbachs alphas 0.79

cronbach(chr_d)


# warmth chronbach
chr_d <- ratings_by_group %>%
  dplyr::select(id, question, rating) %>% na.omit() %>%
  mutate(confed_name = ifelse(str_detect(question, 'Lucile'), 'L', 
                              ifelse(str_detect(question, 'Maria'), 'M',
                                     ifelse(str_detect(question, 'Amie'), 'A', 99)))) %>%
  filter(confed_name == 99) %>% distinct() %>%
  pivot_wider(names_from = question, values_from = rating) %>%
  dplyr::select(-1,-2) %>%
  dplyr::select(contains(c('friendly', 'similar', 'attractiveness')))
cronbach(chr_d)

# competence chronbach
chr_d <- ratings_by_group %>%
  dplyr::select(id, question, rating) %>% na.omit() %>%
  mutate(confed_name = ifelse(str_detect(question, 'Lucile'), 'L', 
                              ifelse(str_detect(question, 'Maria'), 'M',
                                     ifelse(str_detect(question, 'Amie'), 'A', 99)))) %>%
  filter(confed_name == 99) %>% distinct() %>%
  pivot_wider(names_from = question, values_from = rating) %>%
  dplyr::select(-1,-2) %>%
  dplyr::select(contains(c('art', 'compet', 'rational')))
cronbach(chr_d)


# Liking tripple questions and rating scale ------------------------
rating_questions_scores <-  read_csv('rating_questions_scores.csv')%>%
  group_by(id, type) %>%
  summarise(mean_rating = mean(mean_r)) 

liking_triple <- read_csv('liking_triple_questions.csv') 

composite_rating_questions_scores <- rating_questions_scores %>%
  group_by(id, type) %>%
  summarise(mean_rating = mean(mean_rating))

liking_tripple_and_rating <- liking_triple %>%
  ungroup() %>%
  group_by(id, type) %>%
  summarise(mean_liking_tripple = mean(perc)) %>%
  full_join(composite_rating_questions_scores, by = c("id", 'type')) %>%
  na.omit()



# Rating and liking tripple cronbachs alpha ---------------------

liking_tripple_and_rating <- liking_triple %>%
  group_by(id, type) %>%
  summarise(mean_liking_tripple = mean(perc)) %>%
  full_join(composite_rating_questions_scores, by = c("id", 'type')) %>%
  mutate(mean_liking_tripple = ifelse(is.na(mean_liking_tripple) & !is.na(mean_rating), 1, mean_liking_tripple)) %>%
  na.omit() %>%
  mutate(mean_liking_tripple = (mean_liking_tripple/100)*5)

cronbachs_liking = liking_tripple_and_rating %>%
  pivot_longer(3:4) %>%
  mutate(liking_by_mimicking = paste0(type, name)) %>%
  na.omit() %>%
  dplyr::select(id, liking_by_mimicking, value) %>%
  pivot_wider(names_from = liking_by_mimicking, values_from = value) %>%
  ungroup() %>%
  dplyr::select(-id)

cronbach(cronbachs_liking)

cronbachs_liking %>%
  ggplot(aes(choicemean_liking_tripple, y = choicemean_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') 

cronbachs_liking %>%
  ggplot(aes(controlmean_liking_tripple, y = controlmean_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') 

cronbachs_liking %>%
  ggplot(aes(motormean_liking_tripple, y = motormean_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') 



# Add baseline --------------------------

ratings_baseline = ratings_data %>%
  full_join(groups_long) %>%
  mutate(type = ifelse(response == 'baseline', 'baseline', type)) %>%
  na.omit() %>%
  mutate(confed_name = ifelse(str_detect(question, 'Lucile'), 'L', 
                              ifelse(str_detect(question, 'Maria'), 'M',
                                     ifelse(str_detect(question, 'Amie'), 'A', 99)))) %>%
  filter(confed_name != '99') %>%
  full_join(groups_order) %>%
  na.omit() %>%
  dplyr::select(-response) 


ratings_all = full_join(ratings_baseline, ratings_by_group) %>% filter(block != 'baseline')

ratings_avg = ratings_all %>%
  group_by(id, block, confed_name, Type_of_question) %>%
  summarise(mean_r = mean(rating))
  

baseline_mean = ratings_avg %>%
  filter(confed_name != 99) %>%
  rename('mean_r_base' = 'mean_r')


groups_mim = groups_long %>%
  rename('block' = 'response') %>%
  full_join(groups_order) %>%
  dplyr::select(id, confed_name, type)


# Ratings controlled for baseline (by subtracting the baseline) -------------------
exp_means = ratings_avg %>% filter(confed_name == 99) %>%  ungroup() %>%
  dplyr::select(-confed_name) %>%
  full_join(baseline_mean) %>%
  mutate(change_in_rating = mean_r - mean_r_base) %>%
  full_join(groups_mim)

ggplot(exp_means, aes(x = type, y = change_in_rating, fill = Type_of_question))+
  geom_boxplot() +
  scale_fill_manual(values = colors_a[1:2]) +
  labs(y = 'Mean response', title = 'Means for competence or warmth for each mimick type') +
  theme_minimal()

ggplot(exp_means, aes(fill = factor(Type_of_question, levels = c('warmth', 'competence')), x = factor(type, levels = c('choice', 'control', 'motor')), y = change_in_rating)) +
  geom_boxplot() +
  scale_fill_manual(values = c(colors_a[1], colors_a[2])) +
  labs(y = 'Mean response', title = 'Means for competence or warmth for each mimick type', fill = 'Type_of_q') +
  theme_minimal() 
  

exp_means <- exp_means %>%
  mutate(type = as.factor(type))

exp_means <- within(exp_means, type <- relevel(type, ref = 'motor'))

mod2 <- lm(data = exp_means, change_in_rating ~ type * Type_of_question)
summary(mod2)

write.csv(exp_means, 'rating_questions_scores.csv')
# diagnostics are fine --- plot(mod2)
# change from baseline differs between control and choice 
# (higher score in choice - ratings increased from baseline in choice more than in control)


mod2 <- aov(data = exp_means, change_in_rating ~ type * Type_of_question)
summary(mod2)

TukeyHSD(mod2)


# Pointplot of means only
ratings2 = ratings_by_group %>%
  group_by(id,Type_of_question, type) %>% 
  mutate(mean_r = mean(rating)) 

ggplot(ratings2, aes(x = type, y = mean_r, color = Type_of_question)) +
    geom_boxplot() +
      geom_jitter() + 
    geom_point(aes(x = type,y = mean_r)) +
    labs(title = 'Means for competence or warmth for each mimick type',y = 'Mean response') +
    theme_minimal()

# Boxplot
data_by_id = ratings_by_group %>%
  group_by(id, Type_of_question, type) %>%
  mutate(mean_r = mean(rating)) %>%
ggplot(aes(x = type, y = mean(rating), fill = Type_of_question))+
  geom_boxplot() +
  scale_fill_manual(values = colors_a[1:2]) +
  labs(y = 'Mean response', title = 'Means for competence or warmth for each mimick type') +
  theme_minimal()

# Analysis ratings --------------------------------

avg_ratings = ratings_by_group %>%
  group_by(id, Type_of_question, type) %>% 
  summarise(mean_r = mean(rating))

r_model <- lm(mean_r ~ Type_of_question * type, data = avg_ratings)
summary(r_model)


# Only significant difference between warmth and competence ratings in the motor mimickry 
# Higher ratings for competence than warmth (in the case of motor mimicry)
avg_ratings_motor <- avg_ratings %>%
  filter(type == 'motor')

r_model2 <- lm(mean_r ~ Type_of_question, data = avg_ratings_motor)
summary(r_model2)


# Histograms to check motor 
avg_ratings_motor2 = avg_ratings_motor %>% filter(type == 'motor') %>% filter(Type_of_question == 'warmth')


