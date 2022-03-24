# TidyTuesday Week12 
project_directory = '/Users/hec_vini/Library/CloudStorage/OneDrive-Personal/r_basics/TidyTuesday/week12'

# Load Packages 
library(tidyverse)
library(lubridate)
library(zoo)
library(janitor)
library(data.table)
library(stringi)
library(rvest)
library(splitstackshape)

#Get Biblical Names
biblical_names_URLs = c('https://www.behindthename.com/namesakes/list/biblical/alpha',
                        'https://www.behindthename.com/namesakes/list/biblical/alpha/2',
                        'https://www.behindthename.com/namesakes/list/biblical/alpha/3',
                        'https://www.behindthename.com/namesakes/list/biblical/alpha/4') #urls with bilblical names to extract

biblical_names_list = tibble() #empty list to appened names
#for i in c(1:4) = do the same thing for all 4 urls above
for (i in c(1:4)) {
  biblical_names = biblical_names_URLs[i] %>% read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T) #download data
  biblical_names = biblical_names[[4]] #select desired table
  biblical_names = biblical_names[-1,c(1,4)] #remove undesired rows and cols
  biblical_names[,2] = lapply(biblical_names[,2], function(x) as.character(gsub("[()]", "", x))) #remove parenthesis
  biblical_names = biblical_names %>% mutate_all(na_if,"") #substitute null by NA (to easily remove it later)
  biblical_names = biblical_names %>% cSplit("X4", ",") #split cols with multiple names
  biblical_names = biblical_names %>% tibble() 
  ncol = dim(biblical_names)[2]
  colnames(biblical_names) = rep('name',times = ncol) #renames cols equally (to rbind it later)
  biblical_names2 = vector() #empty vector to append names
  for (j in c(1:ncol)) {
    biblical_names2 = rbind(biblical_names2,biblical_names[,j]) #rbind side-to-side cols, to easily work with all names
    biblical_names2 = biblical_names2 %>% drop_na() #drop empty
  }
  biblical_names2 = biblical_names2 %>% arrange(name) #arrange alphabetically
  biblical_names_list = rbind(biblical_names_list,biblical_names2) #store URL names with the others
}
biblical_names_list = biblical_names_list %>% unique() #remove duplicates
biblical_names_list = biblical_names_list$name #make a df column into list

# Clean and Organize Data
babynames.R = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')
babynames = babynames.R %>% subset(select = c(year,name,sex,n,prop)) #select desired coluns
babynames = babynames %>% mutate(biblical = case_when(name %in% biblical_names_list ~ 'Y', TRUE ~ 'N')) #new column with biblical categorization

babynames_biblical = babynames %>% subset(select = -sex)%>% group_by(year,biblical) %>%
  summarise(n = sum(n)) %>% ungroup() #sum by bilbical dummy
babynames_biblical = babynames_biblical %>% group_by(year) %>%
  mutate(pct = (n/sum(n))*100) %>% ungroup() #get bilbical percentage
babynames_biblical = babynames_biblical %>% filter(biblical == 'Y') #filter only biblical names

babynames_ranking = babynames %>% group_by(year) %>% mutate(pct = (n/sum(n))*100) %>% ungroup()
babynames_ranking1950 = babynames_ranking %>% filter(year == 1950) %>% arrange(desc(prop))
babynames_ranking2015 = babynames_ranking %>% filter(year == 2015) %>% arrange(desc(prop))

babynames_chart = 
  ggplot(babynames_biblical,aes(x = year,y = pct)) +
  geom_area(fill = '#6f46a6',color = '#02010a', size = 1, alpha = 1) + 
  geom_hline(yintercept = 0, size = 2, color = '#000000') + #thick y-axis zero line
  theme(plot.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5, colour = '#333132', size = 22, margin = unit(c(-1,0,.5,0),'cm'),angle = 0),
        axis.text.y = element_text(hjust = 1, colour = '#333132', size = 27, margin = unit(c(0,.5,0,0),'cm'),angle = 0), #edit axis layout
        axis.title = element_blank(), 
        axis.ticks.length.x = unit(.5,'cm'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = '#acb0bd'), 
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') + #remove legend
  scale_x_continuous(breaks = seq(1880,2015,15)) + #set years breaks 
  scale_y_continuous(breaks = seq(0,40,10), limits = c(0,40), labels = c('',seq(10,40,10))) #set pct breaks
ggsave(filename = 'babynames_chart.png',width = 14, height = 7,
       plot = babynames_chart, device = 'png', path = project_directory) #save file




