library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(formattable)

setwd("Z:\\Commits\\Vibration_artifacts\\2020-08-05-audio_p-coherence")

theme_set(theme_bw())

electrode_session_db <- read_tsv('data/electrode_session.txt') %>%
  left_join(select(read_tsv('data/electrodes_renamed.tsv'),electrode,electrode2)) %>%
  mutate(electrode=electrode2)
coherence_db <- read_tsv('data/coherence_audio_p_by_electrode.tsv')

electrode_session_coherence_db <- electrode_session_db %>%
  left_join(coherence_db, by=c('subject','session_id','electrode')) %>%
  filter(!is.na(rNC))

electrode_session_coherence_db %>%
  write_tsv('data/electrode_session_coherence.tsv')




