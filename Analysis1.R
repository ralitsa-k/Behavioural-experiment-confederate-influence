library(tidyverse)
library(lme4)
require(nnet)
library("gplots")
library(ltm)
require(ltm)

rm(list= ls())

colors_a = c('#B0D0D3', '#C08497','#F7AF9D')

# First run load_data to get csv files for task and questionnaire data 

# Read in the full data sets --------------------------
task_data = read_csv('task_data.csv')
quest_data = read_csv('questionnaire_data.csv')

groups = read_csv('groups.csv', col_names = TRUE) %>%dplyr::select(-1,-2)
# Binary questions ---------------------------------------
  
bi_dat <- quest_data %>%
  dplyr::select(Participant_Public_ID, Question_Key, Response) 

# Change the column names so we can work with them
colnames(bi_dat) <- c('id', 'question','response') 
# create some new columns with categories of type of question
bi_dat2 <- bi_dat %>%
  na.omit() %>%
  filter(grepl('liking', question)) %>%
  filter(grepl('quantised', question)) %>%
  mutate(response = as.numeric(response)) %>%
  filter(response < 5) %>%
  mutate(Type_of_question = case_when(question == "liking_art_exhibition-quantised" | question == "liking_family_issue-quantised" | question == "liking_pub-quantised" | question == "liking_walk-quantised" ~ 'warmth', 
                                      question == "liking_driving_car-quantised" | question == "liking_job_preparation-quantised" | question == "liking_fixing_computer-quantised" | question == "liking_essay-quantised" ~ 'competence'))

bi_dat_sum <- bi_dat2 %>%
  group_by(Type_of_question) %>%
  count(response)

bi_dat3 <- bi_dat2 %>%
         mutate(response = case_when(response == '1' ~ 'first',
                            response == '2' ~ 'second',
                            response == '3' ~ 'third')) 

groups_long = groups %>%
  dplyr::select(id,first, second, third) %>%
  pivot_longer(2:4, names_to = 'response', values_to = 'type')


recoding_tripple <- bi_dat_by_group %>%
  filter(id == 'u4ckj0xb') %>%
  pivot_wider(names_from=question,values_from= type)

bi_dat_by_group = bi_dat3 %>%
  full_join(groups_long) %>%
  filter(!is.na(type)) %>%
  filter(!is.na(question))

sum_by_id = bi_dat_by_group %>% group_by(id,Type_of_question) %>% count(type) %>%
  mutate(sum_n = sum(n), perc = 100*(n/sum_n)) 

bi_dat_by_group %>% group_by(Type_of_question) %>% count(type) %>%
  mutate(sum_n = sum(n), perc = 100*(n/sum_n)) %>%
  
  ggplot(aes(x = Type_of_question, y = perc, fill = type)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors_a) +
  labs(x = 'Question category', y = 'Percent chosen') +
  ggtitle('Bin=ary questions. Percentage of times each mimicker was chosen') +
  theme_minimal()

write.csv(bi_dat_by_group, 'bi_dat_by_group.csv')
write.csv(sum_by_id, 'liking_triple_questions.csv')

# Binary questions no car -----------------
no_car <- bi_dat_by_group %>% 
  filter(!grepl('car', question)) 

questions_bi = no_car %>% 
  select(question)  %>%
  distinct() 
  
  
#model3 <- glm(data = no_car, type ~ Type_of_question, family = 'binomial')
#summary(model3)

# Chi squared -----------------------------------
chisq <- chisq.test(bi_dat_by_group$Type_of_question,bi_dat_by_group$type)
chisq
bi_dat_by_group %>%
  group_by(Type_of_question) %>%
  count(type) %>%
  pivot_wider(names_from = type, values_from = n)

# Effect size ------------
phi = sqrt(15.382 / 28)
phi


chisq <- chisq.test(bi_dat_by_group$Type_of_question,bi_dat_by_group$type)



# Percentages warmth - competence 
bi_dat_by_group %>%
  group_by(Type_of_question) %>%
  count(type) %>%
  group_by(Type_of_question) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prob = (n/sum)*100)

library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

# Multinomial Logistic Regression -----------------------------
mult_model <- multinom(factor(type) ~ Type_of_question, data = bi_dat_by_group)
summary(mult_model)

pred_vals <- data.frame(pr = predict(mult_model, bi_dat_by_group))
unique(pred_vals$pr)

bi_dat_by_group$type <- relevel(factor(bi_dat_by_group$type), ref = "motor")
mult_model2 <- multinom(factor(type) ~ Type_of_question, data = bi_dat_by_group)
summary(mult_model2)

pValue_extract <- function(x){
  z <- summary(x)$coefficients/summary(x)$standard.errors
  # 2-tailed Wald z tests to test significance of coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  p }
# choice-motor p = 0.0002, control-motor  p = 0.003
pValue_extract(mult_model2)

log_reg <- glmer(factor(type) ~ Type_of_question + (1|id), data = bi_dat_by_group, family = 'binomial')
summary(log_reg)



# Chronbach's alpha All together --------------------
chr <- bi_dat_by_group %>%
  select(id, question, type) %>%
  mutate(type = as.numeric(as.factor(type))) %>%
  distinct() %>%
  spread(question,type) %>%
  select(-id) 

cronbach.alpha(chr)

# Chronbach's alpha warmth --------------------
chr <- bi_dat_by_group %>%
  filter(Type_of_question == 'warmth') %>%
  select(id, question, type) %>%
  mutate(type = as.numeric(as.factor(type))) %>%
  distinct() %>%
  spread(question,type) %>%
  select(-id) 

cronbach.alpha(chr)

# Chronbach's alpha competence --------------------

chr <- bi_dat_by_group %>%
  filter(Type_of_question == 'competence') %>%
  select(id, question, type) %>%
  mutate(type = as.numeric(as.factor(type))) %>%
  distinct() %>%
  spread(question,type) %>%
  select(-id) %>%
  select(-contains('car'))

cronbach.alpha(chr)


mean_ratio_alpha = cronbach.alpha(chr, standardized = FALSE)






# 8 Binary by question ------------------------

# Percentages warmth - competence 8 questions 
bi_dat_8 <- bi_dat_by_group %>%
  separate(question, -10, into = c('question', 'del')) %>%
  group_by(question, Type_of_question) %>%
  count(type) %>%
  mutate(sum = sum(n),
         perc = (n/sum)*100) %>%
  group_by(question) %>%
  mutate(label_y = cumsum(perc) - 0.5 * perc)

ggplot(bi_dat_8, aes(x = question, y = perc, fill = type)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors_a) +
  theme(axis.text.x = element_text(angle = 10)) +
  facet_grid(rows = vars(Type_of_question), scales = "free_y", switch = "y", space = "free_y")  +
  geom_text(aes(y = 100-label_y, label = round(perc)), vjust = 1.5, colour = "white") +
  coord_flip()


# Check order ----------------
bi_dat_by_group %>% group_by(Type_of_question) %>% count(response) %>%
  mutate(sum_n = sum(n), perc = 100*(n/sum_n)) %>%
  ggplot(aes(x = Type_of_question, y = perc, fill = response)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors_a) +
  labs(x = 'Question category', y = 'Percent chosen') +
  ggtitle('Binary questions. Percentage of times each mimicker was chosen') +
  theme_minimal()

chisq2 <- chisq.test(bi_dat_by_group$Type_of_question,bi_dat_by_group$response)
chisq2


# Check confederate --------------------

conf_order <- groups %>%
  dplyr::select(group, first, second, third) %>%
  distinct() %>%
  pivot_longer(2:4, names_to = 'order', values_to = 'type_mimick') 

conf_type <- groups %>%
  dplyr::select(group,conf1, conf2, conf3) %>%
  distinct() %>%
  pivot_longer(2:4, names_to = 'conf', values_to = 'conf_name') %>%
  dplyr::select(-group)

sj_info = read_csv('groups_of_participant.csv') %>%
  dplyr::select(1:2) %>% na.omit() %>%
  rename('group' = 'Group', 'id' = `Gorilla ID`)

conf_info = bind_cols(conf_order, conf_type)

dat_id_conf <- bi_dat_by_group %>% 
  full_join(sj_info) %>%
  full_join(conf_info) %>%
  mutate(conf = ifelse(conf == 'conf1', 'first',
                       ifelse(conf == 'conf2', 'second',
                              ifelse(conf == 'conf3', 'third',0)))) %>%
  mutate(keep = ifelse(response == conf, 1, 0)) %>%
  filter(keep == 1) %>%
  distinct()

conf_tab <- dat_id_conf %>%
  group_by(type_mimick) %>%
  count(conf_name)

chisq2 <- chisq.test(dat_id_conf$type_mimick,dat_id_conf$response)
chisq2

# Number of Participants per group
dat_id_conf %>% select(id, group) %>% distinct %>% count(group)

# number of times 
check_conf_times =dat_id_conf %>%
  group_by(type_mimick, conf_name) %>%
  count()
# number of times of order
check_conf_times = dat_id_conf %>%
  group_by(order, conf_name) %>%
  count(conf_name)


# Check if confederate type (name) 

