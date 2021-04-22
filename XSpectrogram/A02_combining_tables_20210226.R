library(tidyverse)
library(glue)
library(readr)
library(magrittr)

#PATH_ANALYSIS = "/Volumes/Nexus/Users/busha/Analysis/2020-08-05-audio_p-coherence"
PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/xspectrogram"
PATH_DB = paste0(PATH_ANALYSIS,"/results")

setwd(PATH_ANALYSIS)

# Loading fooof results
subjects<-
  tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>% 
  mutate(path_xcorr_audio=glue(paste0(PATH_DB,'/{subject}_xspectrogram.csv'))) %>%
  mutate(exists_db=file.exists(path_xcorr_audio)) %>%
  filter(exists_db)

xcorr_files <- subjects$path_xcorr_audio
names(xcorr_files) <- subjects$subject
xcorr_db <- xcorr_files %>% 
  map_df(read_tsv, .id = "subject")

#removing unnecessary variables
xcorr_db <- xcorr_db %>%
  select(-X7) %>%
  rename(session_id=session, electrode=n_ch)

#saving table
write_tsv(xcorr_db,paste0('results/xspectrogram.tsv'))
