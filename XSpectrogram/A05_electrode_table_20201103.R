library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(formattable)

PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/xspectrogram"
setwd(PATH_ANALYSIS)

theme_set(theme_bw())

subjects<-
  tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>%
  mutate(path_annot=glue("Z:\\DBS\\{subject}\\Preprocessed Data\\Sync\\annot")) %>%
  mutate(path_electrode=glue("{path_annot}\\{subject}_electrode.txt")) %>%
  mutate(path_session=glue("{path_annot}/{subject}_session.txt")) %>%
  mutate(exists_electrode=file.exists(path_electrode)) %>%
  filter(exists_electrode)

electrode_files <- subjects$path_electrode
names(electrode_files) <- subjects$subject
electrode <- electrode_files %>% 
  map_df(read_tsv, .id = "subject") %>%
  mutate(subject_electrode = paste0(subject,'_',electrode)) %>%
  mutate(id=row_number())
write_tsv(electrode,'results/electrode.txt')


session_files <- subjects$path_session
names(session_files) <- subjects$subject
session <- session_files %>% 
  map_df(read_tsv, .id = "subject",col_types=cols_only(
    id = col_double(),
    starts = col_double(),
    ends = col_double(),
    duration = col_double(),
    session_id = col_double()
  )) %>%
  mutate(id=seq_len(n()))
write_tsv(session,'results/session.txt')


subject_electrode <- electrode %>%
  select(subject,electrode) %>%
  group_by(subject,electrode) %>%
  summarise()

session_electrode <- inner_join(session,subject_electrode,by='subject') %>%
  mutate(subject_electrode = paste0(subject,'_',electrode)) %>%
  mutate(id=seq_len(n()))
write_tsv(session_electrode,'results/session_electrode_tmp.txt')









