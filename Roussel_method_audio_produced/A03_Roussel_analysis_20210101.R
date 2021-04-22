library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(lmPerm)
library(broom)
library(ggforce)
library(scales)
library(stringr)



theme_set(theme_bw())

#PATH_ANALYSIS = "/Volumes/Nexus/Commits/Vibration_artifacts/roussel_analysis"
PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/roussel_analysis"
PATH_DB = paste0(PATH_ANALYSIS,"/data")
PATH_FIG = paste0(PATH_ANALYSIS,"/figures")


setwd(PATH_ANALYSIS)
roussel_db <- read_csv('data/Roussel_method.csv') %>%
  mutate(electrode = electrode_group) %>%
  mutate(electrode_type = str_extract(electrode, "[^_]+")) %>%
  mutate(criterion_value_fdr = p.adjust(criterion_value,method = 'fdr')) %>%
  mutate(pp_criterion_value = -log10(criterion_value)) %>%
  mutate(pp_criterion_value_fdr = -log10(criterion_value_fdr)) 
roussel_db$pp_criterion_value[roussel_db$pp_criterion_value>10] <- 10
roussel_db$pp_criterion_value_fdr[roussel_db$pp_criterion_value_fdr>10] <- 10
roussel_db$criterion_value_fdr[is.nan(roussel_db$dataset_measure)] <- NA
roussel_db$pp_criterion_value[is.nan(roussel_db$dataset_measure)] <- NA
roussel_db$pp_criterion_value_fdr[is.nan(roussel_db$dataset_measure)] <- NA

roussel_db %>%
  ggplot() +
  aes(y=criterion_value_fdr,x=criterion_value) +
  geom_point(size=0.1,alpha=0.2)

roussel_db %>%
  ggplot() +
  aes(y=log10(criterion_value_fdr),x=log10(criterion_value)) +
  geom_point(size=0.1,alpha=0.2)

roussel_db %>%
  ggplot() +
  aes(x=log10(criterion_value_fdr),fill=factor((criterion_value<1e-2) + (criterion_value_fdr<1e-2))) +
  geom_histogram(binwidth=0.01)

roussel_db %>%
  ggplot() +
  aes(x=pp_criterion_value,fill=factor((criterion_value<1e-3) + (criterion_value_fdr<1e-3))) +
  geom_histogram(binwidth=0.1)

roussel_db %>%
  #filter(!is.na(criterion_value))%>%
  filter(str_sub(subject,4,4)=='3') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=pp_criterion_value)+
  geom_tile()+
  scale_fill_gradient(limits=c(1.3,4),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/03_roussel_DBS3000_pp.png',width=12,height=9)

roussel_db %>%
  #filter(!is.na(criterion_value))%>%
  filter(str_sub(subject,4,4)=='3') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=pp_criterion_value_fdr)+
  geom_tile()+
  scale_fill_gradient(limits=c(1.3,4),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/03_roussel_DBS3000_pp_fdr.png',width=12,height=9)

roussel_db %>%
  #filter(!is.na(criterion_value))%>%
  filter(str_sub(subject,4,4)=='4') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=pp_criterion_value_fdr)+
  geom_tile()+
  scale_fill_gradient(limits=c(1.3,4),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/03_roussel_DBS4000_pp_fdr.png',width=12,height=9)




roussel_db %>%
  ggplot() +
  aes(x=dataset_measure) +
  geom_histogram(binwidth=0.01)

roussel_db %>%
  #filter(!is.na(criterion_value))%>%
  filter(str_sub(subject,4,4)=='3') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=dataset_measure)+
  geom_tile()+
  scale_fill_gradient(limits=c(0,0.2),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/03_roussel_DBS3000_dataset_measure.png',width=12,height=9)

roussel_db %>%
  #filter(!is.na(criterion_value))%>%
  filter(str_sub(subject,4,4)=='4') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=dataset_measure)+
  geom_tile()+
  scale_fill_gradient(limits=c(0,0.2),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')
ggsave('fig/03_roussel_DBS4000_dataset_measure.png',width=12,height=9)

