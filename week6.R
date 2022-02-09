library(tidytuesdayR) 
library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(datasets) #import us regions dataset
library(treemapify) #to plot treemap

tidytuesday_url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data'
tidytuesday_directory = 'set_your_folder'

us_states = tibble(state.abb,state.region) %>% clean_names() %>% set_names(c('state','region')) 
us_states[] = lapply(us_states[], function(x) as.character(as.factor(x))) 
us_states[,2] = lapply(us_states[,2], function(x) as.character(gsub("North Central", "Midwest", x))) #rename region. This is an old dataset

airmen.R = read_csv(paste0(tidytuesday_url,'/2022/2022-02-08/airmen.csv')) %>% tibble() %>% clean_names()
airmen = airmen.R %>% subset(select = c(graduation_date,pilot_type,state)) %>% drop_na() #select desired data

unique(airmen$pilot_type) #check if data is ok
unique(airmen$state)

airmen = airmen %>% mutate(graduation_date = ymd(graduation_date))
airmen[,2] = lapply(airmen[,2], function(x) as.character(gsub("Liason pilot", "Liaison pilot", x))) #correct typos
airmen[,3] = lapply(airmen[,3], function(x) as.character(gsub("HT", "Other", x))) 
airmen[,3] = lapply(airmen[,3], function(x) as.character(gsub("Haiti", "Other", x)))
airmen[,3] = lapply(airmen[,3], function(x) as.character(gsub("Unk", "Other", x)))
airmen[,3] = lapply(airmen[,3], function(x) as.character(gsub("In", "IN", x)))
airmen = airmen %>% subset(select = c(state)) 
airmen = airmen %>% group_by(state) %>% summarise(n = n()) ##count pilots by state
airmen = right_join(airmen,us_states, by = 'state') %>% drop_na() #drop missing data
nr = dim(airmen)[1]
for (i in c(1:nr)) {
  if(airmen[i,2] < 10){airmen[i,1] = 'Other'}
} #create a "state" states with few pilots
airmen = airmen %>% group_by(state,region) %>% summarise(n = sum(n)) #count by state and region
airmen %>% filter(state == 'Other') #check duplicates
airmen = unique(airmen) #delete duplicates

airmen_chart = 
  ggplot(airmen, aes(area = n, label = state,
                     subgroup = region, fill = region)) + #basic treemap
  geom_treemap(size = 5, color = '#ffffff') + #white thick border
  geom_treemap_subgroup_border(size = 15, color = '#ffffff') + #thin black border
  scale_fill_manual(values = c('Northeast' = '#004e89', 'Midwest' = '#f29479', 'South' = '#d62839', 'West' = '#9dcee2')) + #set colors
  geom_treemap_text(colour = NA) +
  theme(legend.position = 'none') #drop legend
ggsave(filename = 'airmen_chart.png',plot = airmen_chart, width = 15, height = 15, device = 'png', 
       path = tidytuesday_directory) #save plot

#From here onwards, everything was done with Canva
