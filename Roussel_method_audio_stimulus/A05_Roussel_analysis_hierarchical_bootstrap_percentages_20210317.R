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

#PATH_ANALYSIS = "/Volumes/Nexus/Commits/Vibration_artifacts/roussel_analysis_syllable_triplet_task"
PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/roussel_analysis_syllable_triplet_task_audio_s"
PATH_DB = paste0(PATH_ANALYSIS,"/data")
PATH_FIG = paste0(PATH_ANALYSIS,"/figures")

rename_db <- read_tsv("data/electrodes_renamed.tsv") %>%
  select(electrode,electrode2)

setwd(PATH_ANALYSIS)
roussel_db <- read_csv('data/Roussel_method_audio_s.csv') %>%
  mutate(electrode = electrode_group) %>%
  left_join(rename_db) %>%
  mutate(electrode = electrode2) %>%
  mutate(electrode_type = str_extract(electrode, "[^_]+")) %>%
  mutate(criterion_value_fdr = p.adjust(criterion_value,method = 'fdr')) %>%
  mutate(pp_criterion_value = -log10(criterion_value)) %>%
  mutate(pp_criterion_value_fdr = -log10(criterion_value_fdr)) 
roussel_db$pp_criterion_value[roussel_db$pp_criterion_value>10] <- 10
roussel_db$pp_criterion_value_fdr[roussel_db$pp_criterion_value_fdr>10] <- 10
roussel_db$criterion_value_fdr[is.nan(roussel_db$dataset_measure)] <- NA
roussel_db$pp_criterion_value[is.nan(roussel_db$dataset_measure)] <- NA
roussel_db$pp_criterion_value_fdr[is.nan(roussel_db$dataset_measure)] <- NA

roussel_db_f <- roussel_db %>%
  filter(str_sub(subject,4,4)=='3') %>%
  filter(!subject%in%c('DBS3001','DBS3002')) %>%
  filter(!is.na(dataset_measure)) %>%
  filter(!(subject=='DBS3006' & electrode_type=='dbs' & session_id<4)) %>%
  filter(!(subject=='DBS3014' & electrode_type=='dbs' & session_id<4))  %>%
  filter(!(electrode_type=='dbs' & str_sub(electrode,5,5)=='R')) 

roussel_db_f %>%
  #filter(!is.na(criterion_value))%>%
  filter(str_sub(subject,4,4)=='3') %>%
  ggplot() +
  aes(x=session_id,y=electrode,fill=pp_criterion_value)+
  geom_tile()+
  scale_fill_gradient(limits=c(1.3,4),oob=squish,low='white',high=muted('red'))+
  facet_grid(electrode_type~subject,scales='free',space='free')


#### Calculating percent of significant channels per type and epoch

get_percent_signif<-function(db){
  db %>%
    group_by(electrode_type) %>%
    summarise(percent_significant = 100 * sum(criterion_value_fdr<0.05,na.rm=TRUE) / n()) %>%
    arrange(electrode_type) 
}


get_hb_samp <- function(db){
  us<-unique(db$subject) 
  sample(us,length(us),replace=TRUE) %>%
    map_dfr(function(s){
      db %>% filter(subject==s) %>% sample_frac(replace=TRUE)  
    })
}



percent_signif_hboot <-
  1:1000 %>%
  map_dfr(function(x){
    roussel_db_f %>%
      get_hb_samp() %>%
      get_percent_signif()
  })

percent_signif_hboot %>%
  ggplot() +
  aes(x=percent_significant) +
  geom_histogram(binwidth=1) +
  facet_wrap(~electrode_type,scale='free')


percent_signif_hboot_ci <- percent_signif_hboot %>%
  group_by(electrode_type) %>%
  summarise(percent_mean = median(percent_significant),
            percent_ci95=quantile(percent_significant,0.95),
            percent_ci05=quantile(percent_significant,0.05),
            se=sd(percent_significant))

percent_signif <- 
  roussel_db_f %>%
  get_percent_signif() %>%
  left_join(percent_signif_hboot_ci) 

percent_signif %>%
  write_tsv("data/A5_percent_signif_audio_s.tsv")

#doing hierarchical bootstrapping 





