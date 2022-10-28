
library(tidyverse)
library(lme4)
dat_liking <- read_csv('bi_dat_by_group.csv')
dat_questions <- read_csv('dat_IDiff.csv')

# Idiff data ----

# Select only needed data
dat_idiff <- dat_questions %>%
  filter(grepl('IRI', Question_Key)) %>%
  filter(!grepl('quantised', Question_Key))

# Check NAs 
dat_idiff %>%
  filter(!is.na(Response))
# na.omit() ? remove everything 

# Histogram of raw scores
hist(dat_idiff$Response)

# Get mean per participant 
sum_dat_idiff <- dat_idiff %>%
  rename('id' = 'Participant_Public_ID') %>%
  group_by(id) %>%
  summarise(mean_R = mean(Response, na.omit = FALSE))

hist(sum_dat_idiff$mean_R)


# Liking data ----

# count number of responses per participant? 
# have a look at hist of the raw data
# NAs ?
# Do we need characters to be numeric? (Types of the data)

dat_liking_sorted <- dat_liking %>%
  select(-1) %>%
  distinct() %>%
  mutate(id_n = as.numeric(as.factor(id))) %>%
  filter(!type == 'control')

# Count your data
cou <- dat_liking2 %>%
  count(id) 

# Get percentage of times motor and choice
dat_sum <- dat_liking_sorted %>%
  group_by(id,type) %>%
  count() %>%
  group_by(id) %>%
  mutate(c_all = sum(n),
         perc = (n/c_all)*100) 
# What percentage of all responses are motor? 
# x of 8 = motor?
# x% * 8 = motor?
# x/100 * 8 = count_motor
# x/100 =  count_motor / 8
# x = (count_motor/8)*100

ggplot(dat_sum, aes(x = type, y = perc)) +
  geom_boxplot() 

ggplot(dat_sum, aes(x = perc, fill = type)) +
  geom_density(alpha = 0.3)

# Diagnostics for t-test
m <- as.vector(subset(dat_sum, type == 'motor', select = perc))
shapiro.test(m$perc)

ch <- as.vector(subset(dat_sum, type == 'choice', select = perc))
shapiro.test(ch$perc)

t.test(data = dat_sum, perc ~ type)

# diagnostics for lm 
mod3 <- lm(data = dat_sum, perc ~ type)
plot(mod3)



# Hypothesis ---------------------
# Is there an influence of Idiff on type of choice (choice/motor)

idiff_and_choices <- full_join(dat_sum, sum_dat_idiff) %>%
  mutate(mean_idiff = mean_R) %>%
  mutate(id_n = as.numeric(as.factor(id)))

summary(lm(data = idiff_and_choices, perc ~ type * mean_idiff + (1|id_n)))

ggplot(idiff_and_choices, aes(x = mean_idiff, y = perc, color = type)) +
  geom_point() +
  geom_smooth(method = 'lm')




# Notes -----------------------------

# How to re-type ID and join with another data frame 
dd <- tibble(id_n = 1:2)

# Separate separator
d <- tibble(id = c('sub_1', 'sub_3', 'sub_3')) %>%
  separate(id, c('del', 'id'), '_')
full_join(d, dd)

# Separate by position
d <- tibble(id = c('sub1', 'sub3', 'sub3', 'sub14')) %>%
  separate(id, c('del', 'id'), 3)
full_join(d, dd)

# when a variable has string, cannot turn to numeric automatically 
# first turn to a factor 
d<- d %>%
  mutate(id_n = as.numeric(as.factor(id))) %<%
  mutate(id = id_n)
full_join(d, dd)



