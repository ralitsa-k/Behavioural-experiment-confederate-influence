
library(tidyverse)
library(plyr)


read_data <- function(pattern) {
  temp <- list.files(path_data, pattern=c('.csv'))
  pattern <- c(pattern)
  pattern1 <- paste(pattern, collapse="|") # "ll|lo|hl"
  match = grepl(temp, pattern=pattern1) # returns TRUE
  this_data = ldply(paste0(path_data,'/',temp[match]), read_csv, col_names = TRUE)
  names(this_data) <-str_replace_all(names(this_data), c(" " = "_" , "," = "" )) 
  print(names(this_data))
  return(this_data)
  }

`%!in%` <- Negate(`%in%`)
exclude = read_csv('exclude.csv', col_names = 'id')
path_data = 'C:/Users/ralitsaa/OneDrive - University of Glasgow/Work/Paula/Exp2Data'


# Binary (3) ----------------------------
pattern=c('qjpk','*.csv')
dat_quest11 <- read_data(pattern)
dat_quest <- dat_quest11 %>%
  filter(Event_Index != 'END OF FILE') %>%
  mutate(id = Participant_Public_ID) %>%
  filter(id %!in% exclude$id)

dat_quest1 <- dat_quest %>%
  dplyr::select(Participant_Public_ID, Participant_Private_ID) %>%
  distinct()

unique(dat_quest$Participant_Public_ID)
unique(dat_quest$Participant_Private_ID)

# Check if ids match between questionnaire and task data 
ids = data.frame(ids = unique(dat_quest$Participant_Private_ID)) %>%
  full_join(ids1)


write.csv(dat_quest, 'questionnaire_data.csv')
write.csv(dat_quest1, 'ids.csv')

# Fix groups ------------------------

groups_info = read_csv('groups_info.csv') 
groups_by_id = read_csv('groups_of_participant.csv', col_names = c('id', 'group')) %>% 
  dplyr::select(1,2) %>% na.omit() %>%
  mutate(group = as.numeric(group))

groups = groups_info %>%
  full_join(groups_by_id) %>%
  na.omit()

write.csv(groups, 'groups.csv')


# Load ratings -------------------------------
pattern <- c('wvjl','zmp1', 'vf6w', '9i2z','e3lq','omxu')
dat_rating2 <- read_data(pattern)

dat_rating <- dat_rating2 %>%
  filter(Event_Index != 'END OF FILE') %>%
  mutate(id = Participant_Public_ID)

write.csv(dat_rating, 'ratings_data.csv')



path_exp = 'C:/Users/ralitsaa/OneDrive - University of Glasgow/Work/Paula/Exp2'


# Load rapport ----------------------
pattern <- c('sacs','i9pw', '5q1d')
dat_rapport <- read_data(pattern)
write.csv(dat_rapport, paste0(path_exp, '/rap_data.csv'))


# Affective state --------------------------
pattern <- c('7nt5','fggy', 'bmaj')
dat_affect <- read_data(pattern)

dat_affect2 <- dat_affect %>%
  dplyr::select(Participant_Public_ID, Task_Name, Response) %>%
  mutate(response = as.numeric(Response)) %>%
  filter(response < 200)
write.csv(dat_affect2, 'dat_affect.csv')


# attributes/affiliation -----------------------
# questions: is there a difference in social perception/specific attributes between blocks?
# # Affiliation questions 1 - qxva, Affiliation questions 2 - 8s8b, Affiliation questions 3 - ftmy


pattern <- c('qxva','8s8b', 'ftmy')
dat_affil <- read_data(pattern)

dat_affil2 <- dat_affil %>%
  dplyr::select(Participant_Public_ID, Task_Name, Response) %>%
  mutate(response = as.numeric(Response)) %>%
  filter(response < 200)
write.csv(dat_affil2, 'dat_affil.csv')


# 
# connection/closeness --------------------------------
# closeness1 - task-fj2g, closeness2 - task-4n1r, closeness3 - task-ow85
# Is there a difference between closeness levels between blocks?

pattern <- c('fj2g','4n1r', 'ow85')
dat_close <- read_data(pattern)

dat_close2 <- dat_close %>%
  dplyr::select(Participant_Public_ID, Task_Name, Response) %>%
  separate(Response, into = c('response', 'del'), 1) %>%
  na.omit() %>%
  mutate(response = as.numeric(response)) %>%
  separate(Task_Name, into= c('question','block'), -1)

write.csv(dat_close2, 'dat_closeness.csv')


#   
#   Maze Game --------------------------------
# Maze game_Amie&Maria (group 1 and 4)  -  task-v1ub,
# Maze game_Lucile&Amie (group 2 and 5) - task-zvls, 
# Maze game_Maria&Lucile (group 3 and 6) - task-2pt3 

pattern <- c('v1ub','zvls', '2pt3')
dat_maze <- read_data(pattern)

dat_maze2 <- dat_maze %>%
  dplyr::select(Participant_Public_ID, Task_Name, Screen_Name, Zone_Name ,Response, Hint) %>%
  na.omit(Response)

write.csv(dat_maze2, 'dat_maze.csv')


# IDiff ------------------------------
# 5. Individual differences - SIAS questionnaire (social anxiety) - aqvy, 
# IRI (autistic traits) - tgdr, self construal-dependence/independence - e44t 
# Art interests - questionnaire-542i


pattern <- c('aqvy','tgdr', 'e44t','542i')
dat_IDiff <- read_data(pattern)

dat_IDiff2 <- dat_IDiff %>%
  dplyr::select(Participant_Public_ID, Task_Name, Question_Key,Response) %>%
  na.omit(Response) %>%
  mutate(response = as.numeric(Response)) %>%
  filter(response < 1000)

write.csv(dat_IDiff2, 'dat_IDiff.csv')





