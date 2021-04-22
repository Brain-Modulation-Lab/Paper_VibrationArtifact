library(tidyverse)
library(glue)
library(readr)
library(magrittr)

#PATH_ANALYSIS = "/Volumes/Nexus/Users/busha/Analysis/2020-08-05-audio_p-coherence"
PATH_ANALYSIS = "Z:\\Users\\busha\\Analysis\\2020-10-21-audio_s_coherence"
PATH_DB = paste0(PATH_ANALYSIS,"/data")

setwd(PATH_ANALYSIS)

# Loading fooof results
subjects<-
  tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>% 
  mutate(path_xcorr_audio=glue(paste0(PATH_DB,'/{subject}_coherence_audio_s.txt'))) %>%
  mutate(exists_db=file.exists(path_xcorr_audio)) %>%
  filter(exists_db)

xcorr_files <- subjects$path_xcorr_audio
names(xcorr_files) <- subjects$subject
xcorr_db <- xcorr_files %>% 
  map_df(read_tsv, .id = "subject")

#removing unnecessary variables
xcorr_db <- select(xcorr_db, -id, -ends, -duration_C, -duration_V, -epoch_id, -channel2_epoch_orig, -channel2_right)

#saving table
write_tsv(xcorr_db,paste0('data/coherence_audio_s.txt'))
# 




