# Importing data

data <- readxl::read_xlsx('data/general_results_2019.xlsx')
seats <- readxl::read_xlsx('data/seats.xlsx')

source('code/FUNCTIONS.R')
source('code/Infrastructure.R')
# preparing data 

data_clean <- data %>% 
  clean_names() %>% 
  mutate_all(.funs = zirou)

seats <-seats %>% 
  rename('state' = 'Circonscription',
         'n_of_seats' = Sièges)

# keep in mind that the average number of candidate lists in each state is about 50+ 
# the data only captures the major candidates and aggregates the others in an other candidate
# if not payed attention to they can sweep in the election, we cant allow that lol 

# Tidy up the data and join it with the seats data

data_ready <- data_clean %>% 
  pivot_longer(cols = afek_tounes:autres,
               values_to = 'votes',
               names_to = 'party') %>% 
  rename('state'='name_fr') %>% 
  left_join(seats, by = 'state')
# should I save this table for suture use?

# Calculations

data_numbers <- data_ready %>% 
  group_by(state) %>%
  mutate(total_votes = sum(votes),
         percent = votes/total_votes) %>% 
  #filter(percent >= 0.05) %>% 
  filter(party != 'autres') %>% 
  mutate(hare_quota = total_votes/n_of_seats,
         party_quota = votes/hare_quota,
         quota_seats = as.integer(party_quota),
         remains = party_quota - quota_seats, 
         remains_seats = 0) %>% 
  mutate(remaining_seat = n_of_seats - sum(quota_seats)) %>% 
  arrange(desc(remains), .by_group = T) %>% 
  ungroup()
  
remaining_list <- data_numbers %>% 
  select(state, party, remaining_seat) %>% 
  nest_by(state, remaining_seat) %>% 
  mutate(data = map2(data, remaining_seat, function(x,y) rep(x,3)[1:y])) %>% 
  unnest(data) %>% 
  add_column(remains_seats = 0) %>% 
  mutate(remains_seats = remains_seats+1)

good_data_remains <- remaining_list %>% 
  group_by(data) %>% # cna group by state fro mapping later for an anlysis for a state by state 
  summarize(r_seats = sum(remains_seats)) %>% 
  rename('party' = 'data')


good_data_quota <- data_numbers %>% 
  group_by(party) %>%
  summarise(q_seats = sum(quota_seats))


good_data_all <- good_data_quota %>% 
  left_join(good_data_remains, by = 'party') %>% 
  mutate(r_seats = zirou(r_seats),
         total_seats = q_seats + r_seats) %>% 
  filter(total_seats != 0)


good_data_all %>% 
  mutate(party = fct_reorder(party, total_seats)) %>% 
  ggplot() +
  geom_col(aes(x = total_seats,
               y = party, fill = party))+
  scale_fill_manual(labels = good_data_all$total_seats)

good_data_all %>% 
  mutate(party = fct_reorder(party, total_seats)) %>% 
  ggplot() +
  geom_parliament(aes(seats = total_seats, fill = party)) +
  scale_fill_discrete(labels = good_data_all$party)


