library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(lmPerm)
library(broom)
library(ggforce)
library(scales)


theme_set(theme_bw())

#PATH_ANALYSIS = "/Volumes/Nexus/Commits/Vibration_artifacts/xspectrogram"
PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/xspectrogram"
PATH_DB = paste0(PATH_ANALYSIS,"/data")
PATH_FIG = paste0(PATH_ANALYSIS,"/fig")

setwd(PATH_ANALYSIS)
xspectrogram_db <- read_tsv('results/xspectrogram.tsv') %>%
  mutate(electrode_type = tolower(str_extract(electrode, "^[a-zA-Z0-9]+"))) %>%
  left_join(select(read_tsv('results/electrodes_renamed.tsv'),electrode,electrode2)) %>%
  mutate(electrode=electrode2) %>%
  filter(!is.na(electrode)) %>%
  pivot_longer(cols=ends_with('xcorr'),names_to='type',values_to='xcorr') %>%
  mutate(type=str_remove(type,fixed('_xcorr')))


xspectrogram_db %>%
  mutate(electrode_type2 = ifelse(electrode_type=='ecog',str_sub(electrode,1,6),electrode_type)) %>%
  filter(electrode_type2!='ecog_3') %>%
  mutate(electrode_type2 = factor(electrode_type2,levels=c('ecog_2','ecog_1','dbs','macro','micro','v0'))) %>%
  filter(!is.na(electrode_type2)) %>%
  mutate(subject2 = str_sub(subject,6,7)) %>%
  filter(type=='global')%>%
  filter(str_sub(subject,4,4)=='3') %>%
  filter(!(subject=='DBS3006' & electrode_type2=='dbs' & session_id<4)) %>%
  filter(!(subject=='DBS3014' & electrode_type2=='dbs' & session_id<4)) %>%
  filter(!(electrode_type=='dbs' & str_sub(electrode,5,5)=='R')) %>%
  group_by(subject,electrode_type2) %>%
  mutate(electrode2=as.numeric(factor(electrode))) %>%
  ungroup() %>%
  filter((electrode_type!='v0') | (electrode %in% c('V0_con4a','V0_con8a'))) %>%
  filter(!as.numeric(subject2)%in%c(26,29)) %>%
  ggplot() +
  aes(x=factor(session_id),y=factor(electrode2),fill=xcorr)+
  geom_tile()+
  scale_fill_gradient(low='white',high=muted('red'),
                      limits=c(0,1),oob=scales::squish,na.value = "gray90",
                      breaks=c(0,1),labels=c(0,1),name='') +
  facet_grid(electrode_type2~subject2,scales='free',space='free_y') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
ggsave('fig/A11_xspectrogram_raster_3000_series.pdf',width=5.6*1.7,height=4.4*1.7)

# 
# 
# xspectrogram_db %>%
#   mutate(electrode_type2 = ifelse(electrode_type=='ecog',str_sub(electrode,1,6),electrode_type)) %>%
#   filter(electrode_type2!='ecog_3') %>%
#   mutate(electrode_type2 = factor(electrode_type2,levels=c('ecog_2','ecog_1','dbs','macro','micro','v0'))) %>%
#   mutate(subject2 = str_sub(subject,6,7)) %>%
#   filter(type=='global')%>%
#   filter(str_sub(subject,4,4)=='4') %>%
#   group_by(subject,electrode_type2) %>%
#   mutate(electrode2=as.numeric(factor(electrode))) %>%
#   ungroup() %>%
#   filter(!as.numeric(str_sub(subject,6,7))%in%c(61,68,79,85,88)) %>%
#   #filter((electrode_type!='dbs') | (electrode %in% c('dbs_L1','dbs_L2A','dbs_L2B','dbs_L2C','dbs_L3A','dbs_L3B','dbs_L3C','dbs_L4'))) %>%
#   filter((electrode_type!='v0') | (electrode %in% c('V0_con4a','V0_con8a'))) %>%
#   ggplot() +
#   aes(x=factor(session_id),y=factor(electrode2),fill=xcorr)+
#   geom_tile()+
#   scale_fill_gradient(low='white',high=muted('red'),
#                       limits=c(0,1),oob=scales::squish,na.value = "gray90",
#                       breaks=c(0,1),labels=c(0,1),name='') +
#   facet_grid(electrode_type2~subject2,scales='free',space='free') +
#   theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
#         axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
# ggsave('fig/A11_xspectrogram_raster_4000_series.pdf',width=5.6*1.7,height=4.4*1.7)


