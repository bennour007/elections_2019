library(tidyverse)
library(stringdist)
library(stringr)
library(ggpol)


source('code/FUNCTIONS.R')

files <- list.files("data",full.names = T)

data_raw <- map(files, read_csv)

names(data_raw) <- tools::file_path_sans_ext(files) %>% str_remove("data/")

n <- names(data_raw)

data_raw <- map2(data_raw,n, function(x,y){
  mutate(x, state = y)
}
)

# preliminary needed vectors and data 
# seats per state
seats_state <- n %>% 
  as_tibble_col(column_name = "state") %>% 
  mutate(state_seats = c(8,6,10,9,7,7,8,9,9,5,6,8,7,9,9,7,6,7,9,8,6,10,4,4,9,8,5))

# clean names of the possible winning parties without a threshhold
# vector of clean party names to match with dirty ones in the comigng sections

my_parties_names <- c('افاق تونس','القائمة المستقلة الإقلاع','حركة النهضة','حركة نداء تونس','الجبهة الشعبية',
                      'تيار المحبة','الجبهة الوطنية للإنقاذ','الحزب الجمهوري','حركة الشعب',
                      'حزب الاتحاد الوطني الحر', 'حزب التحالف' , 'حزب التيار الديمقراطي',
                      'المؤتمر من أجل الجمهورية', 'حزب المبادرة', 'حزب صوت الفلاحين',  'لمجد الجريد' )

# Unite all data in one huge ass table

data_unite <- data_raw %>%
  map(~ pivot_longer(., cols = !c(list,state), names_to = "deleg", values_to = "votes")) %>%
  map(~ rename(., party = "list")) %>%
  map(~ group_by(., party, state) %>% summarise(votes = sum(votes))) %>%
  map(~ ungroup(.)) %>%
  #map(~ nest_by(., state)) %>% 
  bind_rows() %>%
  left_join(seats_state, by = "state")  %>% 
  #mutate(data = list(data)) %>%
  group_by(state) %>%
  mutate(total_votes = sum(votes),
         hare_quota = total_votes/state_seats) %>% 
  ungroup() %>% # this ungroup can be the the issue 
  mutate(percent = votes/total_votes) %>% 
  # adding percent step (3ataba)
  #filter(percent >= 0.05) %>% 
  mutate(party_quota = votes/hare_quota,
         quota_seats = as.integer(party_quota),
         remains = party_quota - quota_seats, 
         remains_seats = 0) %>%
  group_by(state) %>%
  mutate(remaining_seat = state_seats - sum(quota_seats)) %>%
  #adding a 3ataba 5%
  #filter(percent >= 0.05) %>% 
  arrange(desc(party_quota), .by_group = T) %>%
  ungroup() %>% 
  mutate(party = str_replace_all(party, pattern = '\r', replacement = ' ')) %>% 
  matchy(., c = my_parties_names) %>% 
  mutate(party = new_names)

# create a vector with each state and its respective remainng seats

slice_factor <- data_unite %>% 
  group_by(state, state_seats, remaining_seat) %>% 
  select(state, remaining_seat) %>% 
  slice_head(., n = 1) %>%
  ungroup() %>% 
  select(remaining_seat) %>% 
  as.matrix()

# create a table where the remaining sequence is ready and all is needed i just to add +1

remaining_list <- data_unite %>% 
  select(state, party, remaining_seat) %>% 
  nest_by(state, remaining_seat) %>% 
  mutate(data2 = map2(data, remaining_seat, function(x,y) rep(x,3)[1:y])) %>% 
  unnest(data2) %>% 
  add_column(remains_seats = 0) %>% 
  select(-data) %>% 
  nest() 


good_data_remains <- data_unite %>% 
  group_by(state, state_seats) %>%
  nest() %>% 
  # slice when the percent requirement is not 0%
  mutate(data = map(data, slicer)) %>% 
  left_join(remaining_list, by = 'state') %>% 
  unnest(data.y) %>% 
  mutate(remains_seats = remains_seats+1) %>% 
  group_by(data2) %>% 
  summarise(r_seats = sum(remains_seats)) 

  #mutate(data2 = str_replace_all(data2, pattern = '\r', replacement = ' ')) %>% 
  


good_data_quota <- data_unite %>% 
  group_by(party) %>%
  summarise(q_seats = sum(quota_seats))
  #mutate(party = str_replace_all(party, pattern = '\r', replacement = ' '))


#
# Clean each good data on it own 
#


# To visualize this data properly, we will need to match the names of each party with a list of coherent names
# I've done this before I just nned to rememeber how I ddid it? I think in the education project
# found this one in the map making code, I need to find a way to make this one  work properly with my data : 

# cleaning the \r characters that durties our data and prohibit matching, we will still have to match the output though with clean names

# issue is here


good_data_1 <- right_join(good_data_quota, 
                          good_data_remains,
                          by = c('party' = 'data2')) %>% 
  mutate(total_seats = q_seats + r_seats) 




#Viz


good_data_1 %>% 
  mutate(party = fct_reorder(party, total_seats)) %>% 
  ggplot() +
  geom_col(aes(x = total_seats,
               y = party))

geom_parliament(aes(seats = total_seats, fill = party)) +
  scale_fill_brewer(labels = good_data_1$party, palette = 'Set1')






