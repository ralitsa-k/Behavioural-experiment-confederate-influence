
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

# Plot ------------------------
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

# analysis with type of question -----------------------
mod1 <- aov(data = dat_rapport3, response ~  Mimicker)
summary(mod1)
TukeyHSD(mod1)
# diagnostics are fine for this model check with:  plot(mod1)

# analysis without type of question
mod2 <- lm(data = dat_rapport3, response ~ Mimicker)
summary(mod2)


# blocks by order ?  ----------------------------


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








