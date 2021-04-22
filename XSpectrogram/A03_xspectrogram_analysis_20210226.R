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
PATH_DB = paste0(PATH_ANALYSIS,"/results")
PATH_FIG = paste0(PATH_ANALYSIS,"/fig")


setwd(PATH_ANALYSIS)
xspec_db <- read_tsv('results/xspectrogram.tsv') %>%
  mutate(electrode_type = tolower(str_extract(electrode, "^[a-zA-Z0-9]+"))) %>%
  left_join(select(read_tsv('results/electrodes_renamed.tsv'),electrode,electrode2)) %>%
  mutate(electrode=electrode2) %>%
  filter(!is.na(electrode)) %>%
  pivot_longer(cols=ends_with('xcorr'),names_to='type',values_to='xcorr') %>%
  mutate(type=str_remove(type,fixed('_xcorr')))


xspec_db %>%
  ggplot() +
  aes(x=xcorr,y=..density..) + 
  geom_histogram(binwidth = 0.01) + 
  #geom_vline(xintercept =,color='red') +
  facet_grid(electrode_type~type)+
  xlim(c(0,1))
ggsave('fig/A03_01_xspec_histogram_by_type.png',width=5,height=5)

xspec_db %>%
  filter(type=='prod')%>%
  filter(str_sub(subject,4,4)=='3') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=xcorr)+
  geom_tile()+
  #scale_fill_gradient(limits=c(3,10),oob=squish)+
  scale_fill_gradient(limits=c(0,1),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/A03_02_prod_xcorr_raster_3000_series.png',width=30,height=18)


xspec_db %>%
  filter(type=='global')%>%
  filter(str_sub(subject,4,4)=='3') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=xcorr)+
  geom_tile()+
  #scale_fill_gradient(limits=c(3,10),oob=squish)+
  scale_fill_gradient(limits=c(0,1),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/A03_02b_global_xcorr_raster_3000_series.png',width=30,height=18)




xspec_db %>%
  filter(as.numeric(str_sub(subject,6,7))<=14) %>%
  filter(electrode_type %in% c('ecog')) %>%  
  filter(str_sub(electrode,6,6)=='1') %>%
  filter(type=='prod')%>%
  filter(str_sub(subject,4,4)=='3') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=xcorr)+
  geom_tile()+
  scale_fill_gradient(limits=c(0,1),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/A03_03_xcorr_raster_upto3014_ecog.png',width=12,height=10)

# 
# coherence_agg %>%
#   filter(type=='prod')%>%
#   filter(str_sub(subject,4,4)=='4') %>%
#   ggplot() +
#   aes(x=session_id,y=electrode,fill=rNC)+
#   geom_tile()+
#   #scale_fill_gradient(limits=c(3,10),oob=squish)+
#   scale_fill_gradient(limits=c(3,15),oob=squish,low='white',high=muted('red'))+
#   facet_grid(electrode_type~subject,scales='free',space='free')
# ggsave('fig/05_norm_coherence_raster_4000_series.png',width=20,height=18)

xspec_db %>%
  filter(type=='prod') %>%
  write_tsv("results/xspectrogram_by_electrode.tsv")


xspec_db %>%
  write_tsv("results/xspectrogram_by_electrode_all_types.tsv")


# coherence_agg3 <- coherence_agg2 %>%
#   group_by(subject,session_id) %>%
#   summarise(median_rNC = median(rNC,na.rm=TRUE))
# write_tsv(coherence_agg3,"data/coherence_audio_p_by_session.tsv")


