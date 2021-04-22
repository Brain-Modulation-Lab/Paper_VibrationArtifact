library(tidyverse)
library(glue)
library(readr)
library(magrittr)

#PATH_ANALYSIS = "/Volumes/Nexus/Commits/Vibration_artifacts/roussel_analysis"
PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/roussel_analysis_syllable_triplet_task_audio_s"
PATH_DB = paste0(PATH_ANALYSIS,"/data")

setwd(PATH_ANALYSIS)

# Loading fooof results
subjects<-
  tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>% 
  mutate(path_xcorr_audio=glue(paste0(PATH_DB,'/{subject}_Roussel_method_audio_s.csv'))) %>%
  mutate(exists_db=file.exists(path_xcorr_audio)) %>%
  filter(exists_db)

xcorr_files <- subjects$path_xcorr_audio
names(xcorr_files) <- subjects$subject
xcorr_db <- xcorr_files %>% 
  map_df(read_csv, .id = "subject")

#saving table
write_csv(xcorr_db,paste0('data/Roussel_method_audio_s.csv'))
s