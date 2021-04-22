library(tidyverse)
library(glue)
library(readr)
library(magrittr)

#PATH_ANALYSIS = "/Volumes/Nexus/Commits/Vibration_artifacts/roussel_analysis"
PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/roussel_analysis"
PATH_DB = paste0(PATH_ANALYSIS,"/data")

setwd(PATH_ANALYSIS)

# Loading fooof results
subjects<-
  tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>% 
  mutate(path_xcorr_audio=glue(paste0(PATH_DB,'/{subject}_Roussel_method.csv'))) %>%
  mutate(exists_db=file.exists(path_xcorr_audio)) %>%
  filter(exists_db)

xcorr_files <- subjects$path_xcorr_audio
names(xcorr_files) <- subjects$subject
xcorr_db <- xcorr_files %>% 
  map_df(read_csv, .id = "subject")

#saving table
write_csv(xcorr_db,paste0('data/Roussel_method.csv'))
# 
# # Loading preprocessing annotation tables
# subjects<-
#   tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>%
#   mutate(path_annot=glue("Z:\\DBS\\{subject}\\Preprocessed Data\\Sync\\annot")) %>%
#   mutate(path_electrode=glue("{path_annot}\\{subject}_electrode.txt")) %>%
#   mutate(path_session=glue("{path_annot}\\{subject}_session.txt")) %>%
#   mutate(exists_electrode=file.exists(path_electrode)) %>%
#   filter(exists_electrode)
# 
# electrode_files <- subjects$path_electrode
# names(electrode_files) <- subjects$subject
# electrode <- electrode_files %>% 
#   map_df(read_tsv, .id = "subject") %>%
#   mutate(subject_electrode = paste0(subject,'_',electrode)) %>%
#   mutate(id=seq_len(n()))
# 
# write_tsv(electrode,'data/electrode.txt')
# 
# 
# session_files <- subjects$path_session
# names(session_files) <- subjects$subject
# session <- session_files %>% 
#   map_df(read_tsv, .id = "subject",col_types=cols_only(
#                                                   id = col_double(),
#                                                   starts = col_double(),
#                                                   ends = col_double(),
#                                                   duration = col_double(),
#                                                   session_id = col_double()
#                                                 )) %>%
#   mutate(id=seq_len(n()))
# write_tsv(session,'data/session.txt')
# 
# 
# subject_electrode <- electrode %>%
#   select(subject,electrode) %>%
#   group_by(subject,electrode) %>%
#   summarise()
# 
# session_electrode <- inner_join(session,subject_electrode,by='subject') %>%
#   mutate(subject_electrode = paste0(subject,'_',electrode)) %>%
#   mutate(id=seq_len(n()))
# write_tsv(session_electrode,'data/session_electrode_tmp.txt')
# 
# 
# 
# 



