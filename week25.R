# TidyTuesday - Week 25 (Juneteenth)

# 1. Load Packages ####
project_directory = '/Users/hec_vini/Library/CloudStorage/OneDrive-Personal/r_basics/TidyTuesday/week25/'

library(tidyverse)
library(lubridate) #To easily work with dates
library(tidylog) #To see what's going on with the code
library(janitor) #clean_names() package
library(writexl) #Create .xlsx files
library(readxl) #Read .xlsx files
library(circlize) #Make the chart


# 2. Load Data ####
slave_voyages.R = read.csv(paste0(project_directory,'SlaveVoyages_V2019.csv')) %>% tibble() %>% clean_names()

# 3. Clean Data ####
slave_voyages = slave_voyages.R %>% subset(select = c(majbyimp,majbyimp1,mjselimp,mjselimp1,slamimp)) 
slave_voyages = slave_voyages %>% set_names(c('purchase_region','purchase_continent','disembark_region',
                                              'disembark_continent','total_slaves')) 
slave_voyages = slave_voyages %>% group_by(purchase_region,purchase_continent,disembark_region,disembark_continent) %>%
  summarise(total_slaves = sum(total_slaves)) %>% ungroup() 
slave_voyages[] = lapply(slave_voyages[], function(x) as.numeric(as.integer(x)))

slave_voyages_codelist = slave_voyages %>% subset(select = c(1:4))
slave_voyages_codelist = unlist(slave_voyages_codelist) %>% tibble()
slave_voyages_codelist = slave_voyages_codelist %>% setNames(c('location'))
slave_voyages_codelist = unique(slave_voyages_codelist)
slave_voyages_codelist = slave_voyages_codelist %>% arrange(location)
slave_voyages_codelist = slave_voyages_codelist %>% mutate(location_name = 'x')

#write_xlsx(slave_voyages_codelist,paste0(project_directory,'slave_voyages_codelist_raw.xlsx'))
slave_voyages_codelist = read_xlsx(paste0(project_directory,'slave_voyages_codelist.xlsx')) %>% tibble()
slave_voyages = slave_voyages %>% set_names(c('purchase_region_code','purchase_continent_code',
                                              'disembark_region_code','disembark_continent_code','total_slaves'))
slave_voyages = left_join(slave_voyages,slave_voyages_codelist, by = c('purchase_region_code' = 'location'))
colnames(slave_voyages)[6] = 'purchase_region'
slave_voyages = left_join(slave_voyages,slave_voyages_codelist, by = c('purchase_continent_code' = 'location'))
colnames(slave_voyages)[7] = 'purchase_continent'
slave_voyages = left_join(slave_voyages,slave_voyages_codelist, by = c('disembark_region_code' = 'location'))
colnames(slave_voyages)[8] = 'disembark_region'
slave_voyages = left_join(slave_voyages,slave_voyages_codelist, by = c('disembark_continent_code' = 'location'))
colnames(slave_voyages)[9] = 'disembark_continent'
slave_voyages = slave_voyages %>% subset(select = c(6:9,5))
slave_voyages = slave_voyages %>% drop_na(total_slaves)

top_origins = slave_voyages %>% group_by(purchase_region) %>% summarise(total_slaves = sum(total_slaves)) %>% arrange(desc(total_slaves)) 
top_destinations = slave_voyages %>% group_by(disembark_region) %>% summarise(total_slaves = sum(total_slaves)) %>% arrange(desc(total_slaves)) 
other_origins = top_origins %>% tail(6) %>% as.list()
other_origins = other_origins$purchase_region
other_destinantions = top_destinations %>% tail(65) %>% as.list()
other_destinantions = other_destinantions$disembark_region

slave_voyages = slave_voyages %>% 
  mutate(purchase_region = case_when(purchase_region %in% other_origins ~ "Other", TRUE ~ as.character(purchase_region)),
         disembark_region = case_when(disembark_region %in% other_destinantions ~ "Other", TRUE ~ as.character(disembark_region)))
slave_voyages = slave_voyages %>% 
  mutate(purchase_region = case_when(purchase_region == 'Other' ~ paste(purchase_region,purchase_continent), TRUE ~ as.character(purchase_region)),
         disembark_region = case_when(disembark_region == 'Other' ~ paste(disembark_region,disembark_continent), TRUE ~ as.character(disembark_region)))
slave_voyages = slave_voyages %>% group_by(purchase_region,disembark_region) %>%
  summarise(total_slaves = sum(total_slaves)) %>% ungroup()

slave_voyages = slave_voyages %>% filter(purchase_region %!in% c('Other NA','Other Other'))
slave_voyages = slave_voyages %>% filter(disembark_region %!in% c('Other Other'))
slave_voyages = slave_voyages %>% arrange(total_slaves)

slave_voyages_matrix = slave_voyages %>% 
  pivot_wider(names_from = disembark_region, values_from = total_slaves)

slave_voyages_matrix = data.matrix(slave_voyages_matrix,rownames.force = NULL)
rownames(slave_voyages_matrix) = c('Benin','Gold Coast','Biafra','Angola','Other Africa')
slave_voyages_matrix = slave_voyages_matrix[,-1]

# 4. Set Circle Parameters ####
slave_voyages_order = c('Angola',
                        'Benin',
                        'Biafra',
                        'Gold Coast',
                        'Other Africa',
                        'Other North America',
                        'Other Europe',
                        'Dutch Guianas',
                        'Other Spanish Mainland Americas',
                        'Bahia',
                        'Pernambuco',
                        'Southeast Brazil',
                        'Other Brazil',
                        'Jamaica',
                        'Cuba',
                        'Saint-Domingue',
                        'Barbados',
                        'Other Caribbean')
circos.clear() 
circos.par(gap.after = c('Angola' = 1,
                         'Benin' = 1,
                         'Biafra' = 1,
                         'Gold Coast' = 1,
                         'Other Africa' = 8,
                         'Other North America' = 1,
                         'Other Europe' = 8,
                         'Dutch Guianas' = 1,
                         'Other Spanish Mainland Americas' = 8,
                         'Bahia' = 1,
                         'Pernambuco' = 1,
                         'Southeast Brazil' = 1,
                         'Other Brazil' = 8,
                         'Jamaica' = 1,
                         'Cuba' = 1,
                         'Saint-Domingue' = 1,
                         'Barbados' = 1,
                         'Other Caribbean' = 8))

edge_colors = c('Angola' = '#d90429',
                'Benin' = '#d90429',
                'Biafra' = '#d90429',
                'Gold Coast' = '#d90429',
                'Other Africa' = '#d90429',
                'Other North America' = '#343a40',
                'Other Europe' = '#343a40',
                'Dutch Guianas' = '#fb8500',
                'Other Spanish Mainland Americas' = '#fb8500',
                'Bahia' = '#006400',
                'Pernambuco' = '#006400',
                'Southeast Brazil' = '#006400',
                'Other Brazil' = '#006400',
                'Jamaica' = '#0077b6',
                'Cuba' = '#0077b6',
                'Saint-Domingue' = '#0077b6',
                'Barbados' = '#0077b6',
                'Other Caribbean' = '#0077b6')

colors_matrix = slave_voyages_matrix
colors_matrix = t(colors_matrix)
colors_matrix[c(2),] = '#d90429'
colors_matrix[c(1,6),] = '#343a40'
colors_matrix[c(5,9),] = '#fb8500'
colors_matrix[c(3,4,7,10),] = '#006400'
colors_matrix[c(8,11,12,13,14),] = '#0077b6'
colors_matrix = as.vector(colors_matrix) 

link_borders_colors = slave_voyages_matrix
link_borders_colors = t(link_borders_colors)
link_borders_colors[is.na(link_borders_colors)] = 0
link_borders_colors[] = NA
link_borders_colors[7,1] = '#000000'
link_borders_colors[8,5] = '#000000'
link_borders_colors[14,5] = '#000000'
link_borders_colors[7,4] = '#000000'

transparency_matrix = slave_voyages_matrix
transparency_matrix = t(transparency_matrix)
transparency_matrix[is.na(transparency_matrix)] = 0
transparency_matrix[] = .8
transparency_matrix[7,1] = .3
transparency_matrix[8,5] = .3
transparency_matrix[14,5] = .3
transparency_matrix[7,4] = .3

#4. Make the Chart ####
chart_slave_voyages =
  chordDiagram(t(slave_voyages_matrix),
               order = slave_voyages_order,
               grid.col = edge_colors,
               col = colors_matrix,
               transparency = transparency_matrix,link.largest.ontop = FALSE,
               link.sort = FALSE,link.decreasing = TRUE,
               scale = FALSE,annotationTrack = c('name','grid'),annotationTrackHeight = mm_h(c(2,5)))
circos.clear()









