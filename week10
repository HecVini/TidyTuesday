library(tidyverse)
library(tidytuesdayR)
library(geosphere)
library(janitor)
library(zoo)
library(ggalluvial)
library(countrycode)

project_directory = '/Users/hec_vini/Library/CloudStorage/OneDrive-Personal/r_basics/TidyTuesday/week10'

europe_regions = codelist %>% filter(continent == 'Europe') #match World Bank country regions
europe_regions = europe_regions %>% subset(select = c(iso3c,region23))
colnames(europe_regions) = c('sending_iso3c','region23')

erasmus.R = tt_load(2022, week = 10)
erasmus = erasmus.R$erasmus %>% clean_names() %>% tibble()
erasmus = erasmus %>% filter(academic_year == '2019-2020')
erasmus = erasmus %>% subset(select = c(sending_country_code,receiving_country_code,participants))
erasmus = erasmus %>% 
  mutate(sending_iso3c = countrycode(erasmus$sending_country_code,origin = 'iso2c',destination = 'iso3c',
                                     custom_match = c('EL' = 'GRC','UK' = 'GBR','XK' = 'KSV'))) #special cases
erasmus = erasmus %>% 
  mutate(receiving_iso3c = countrycode(erasmus$receiving_country_code,origin = 'iso2c',destination = 'iso3c',
                                     custom_match = c('EL' = 'GRC','UK' = 'GBR','XK' = 'KSV')))
erasmus = erasmus %>% subset(select = c(sending_iso3c,receiving_iso3c,participants)) #match iso2c to iso3c
erasmus = erasmus %>% group_by(sending_iso3c,receiving_iso3c) %>% 
  summarise(participants = sum(participants)) %>% ungroup() #summarise by origin and destinations
erasmus = erasmus %>% arrange(desc(participants))
erasmus = erasmus %>% mutate(cross = 1)
nr = dim(erasmus)[1]
for(i in c(1:nr)){
  if(erasmus[i,1] == erasmus[i,2]){erasmus[i,4] = 0} #exclude domestic countries
}
erasmus = erasmus %>% filter(cross == 1)
erasmus = right_join(erasmus,europe_regions, by = 'sending_iso3c')
erasmus = erasmus %>% drop_na(participants)

sending_amount = erasmus %>% group_by(sending_iso3c) %>% summarise(participants = sum(participants))
sending_amount = sending_amount %>% arrange(desc(participants))
sending_amount = sending_amount %>% top_n(7) #top sending countries

receving_amount = erasmus %>% group_by(receiving_iso3c) %>% summarise(participants = sum(participants))
receving_amount = receving_amount %>% arrange(desc(participants))
receving_amount = receving_amount %>% top_n(7)
receving_amount = receving_amount$receiving_iso3c #top recieving

erasmus = erasmus %>% filter(sending_iso3c %in% c(sending_amount$sending_iso3c))

nr = dim(erasmus)[1]
for(i in c(1:nr)){
  if(erasmus[i,2] %!in% receving_amount){erasmus[i,2] = 'OTH'}
} #if not on top receivers, call it "OTH" as in "other"
 
top_senders = erasmus %>% group_by(sending_iso3c) %>% 
  summarise(participants = sum(participants)) %>% arrange(desc(participants))
top_receivers = erasmus %>% group_by(receiving_iso3c) %>% 
  summarise(participants = sum(participants)) %>% arrange(desc(participants))
erasmus$sending_iso3c = factor(erasmus$sending_iso3c, levels = top_senders$sending_iso3c)
top_receivers = top_receivers$receiving_iso3c 

nr = length(top_receivers)
for(i in c(1:nr-1)){
  top_receivers[i] = top_receivers[i+1]
}
top_receivers[nr] = 'OTH'
erasmus$receiving_iso3c = factor(erasmus$receiving_iso3c, levels = c("FRA","BEL","ESP","DEU","ITA","AUT","LTU","OTH"))
erasmus = erasmus %>% arrange(factor(sending_iso3c, levels = top_senders$sending_iso3c),
                              factor(receiving_iso3c, levels = top_receivers))
erasmus = erasmus %>% group_by(sending_iso3c,receiving_iso3c,region23) %>%
  summarise(participants = sum(participants)) %>% ungroup() #arrange display data


chart_erasmus =
  ggplot(data = erasmus,aes(axis1 = sending_iso3c, 
                            axis2 = receiving_iso3c, 
                            y = participants)) +
  geom_alluvium(aes(fill = region23), alpha = 0.7,width = 0,knot.pos = .3) +
  geom_stratum(color = '#ffffff', fill = NA, size = 5,width = 0.1) +
  scale_fill_manual(values = c('Northern Europe' = '#023047',
                               'Southern Europe' = '#fb8500',
                               'Eastern Europe' = '#ffb703',
                               'Western Europe' = '#8ecae6')) +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
ggsave(filename = 'chart_erasmus.png',plot = chart_erasmus, device = 'png', 
       path = project_directory)



