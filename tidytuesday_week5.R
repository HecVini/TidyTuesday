# TidyTuesday Week 5
library(tidyverse)
library(janitor)
library(zoo)
library(tidylog)
library(tidytuesdayR) 
library(ggh4x) #for intermediate ticks

week5_url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01'

#raw datasets (I had some trouble using the tidytuesdayR package. "Error in parse_url(url) : length(url) == 1 is not TRUE")
breed_traits.R = read_csv(paste0(week5_url,'/breed_traits.csv')) %>% tibble() %>% clean_names()
traits_descriptions.R = read_csv(paste0(week5_url,'/trait_description.csv')) %>% tibble() %>% clean_names()
traits_ranking.R = read_csv(paste0(week5_url,'/breed_rank.csv')) %>% tibble() %>% clean_names()

#traits categories to create the average score
agreeability = c('affectionate_with_family','good_with_young_children','good_with_other_dogs',
                 'openness_to_strangers', 'playfulness_level','watchdog_protective_nature','energy_level')
easiness = c('shedding_level','coat_grooming_frequency','drooling_level','adaptability_level','trainability_level',
             'barking_level','mental_stimulation_needs')

coats_lengths = breed_traits.R %>% subset(select = c(breed,coat_length)) 

breed_traits = breed_traits.R %>% subset(select = -c(coat_type,coat_length)) #subset non-numeric traits
breed_traits = breed_traits %>% gather(trait,score,c(2:15)) #wide to long format
nr = dim(breed_traits)[1]
for (i in c(1:nr)) {
  if(breed_traits[i,2] %in% agreeability){breed_traits[i,2] = 'agreeability'}
  if(breed_traits[i,2] %in% easiness){breed_traits[i,2] = 'easiness'}
  } #rename traits according to category
breed_traits = breed_traits %>% group_by(breed,trait) %>% summarise(average_score = mean(score)) %>% ungroup() #create the average score
breed_traits = right_join(breed_traits,coats_lengths, by = 'breed') #joint coat lenght
breed_traits = breed_traits %>% spread(trait,average_score) #long to wide format (with agreeability and easiness)
breed_traits = breed_traits %>% filter(easiness > 0, agreeability > 0) #delete non-positive scores

chart_dogs_score = 
  ggplot(breed_traits, aes(x = easiness, y = agreeability)) +
  geom_point(aes(color = coat_length), size = 32) + #scatter plot
  scale_color_manual(values = c('Short' = '#aed6dc', 'Long' = '#4a536b', 'Medium' = '#ff9a8d')) + #mannualy set points colors
  geom_hline(yintercept = 1.5, size = 6, color = '#000000') + #x-axis base line
  geom_vline(xintercept = 1.5, size = 6, color = '#000000') + #y-axis base line
  theme(plot.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour = '#333132', size = 54, margin = margin(1,0,0,0,"cm"), angle = 0),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, colour = '#333132', size = 54, margin = margin(0,1,0,0,"cm"), angle = 0),
        axis.title = element_blank(),
        axis.ticks.length.x = unit(1.5,'cm'),
        axis.ticks.length.y = unit(1.5,'cm'), 
        axis.ticks.x = element_line(color = '#000000', size = 1.5),
        axis.ticks.y = element_line(color = '#000000', size = 1.5),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none',
        ggh4x.axis.ticks.length.minor = rel(.5)) + #set the minor breaks ticks length
  scale_y_continuous(limits = c(1.5,5), breaks = seq(2,5,1), minor_breaks = seq(2,5,.5), guide = "axis_minor") +
  scale_x_continuous(limits = c(1.5,5), breaks = seq(2,5,1), minor_breaks = seq(2,5,.5), guide = "axis_minor")

#Then, other changes were made with Canvas. 

ggsave(filename = 'chart_dogs_score.png',plot = chart_dogs_score, width = 29, height = 29, device = 'png', 
       path = '/Users/hec_vini/Library/CloudStorage/OneDrive-Personal/r_basics/TidyTuesday/week5') 

