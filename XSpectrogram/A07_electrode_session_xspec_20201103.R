library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(formattable)

setwd("Z:/Commits/Vibration_artifacts/xspectrogram")

theme_set(theme_bw())

electrode_session_db <- read_tsv('results/electrode_session.txt') %>%
  left_join(select(read_tsv('results/electrodes_renamed.tsv'),electrode,electrode2)) %>%
  mutate(electrode=electrode2)
xspec_db <- read_tsv('results/xspectrogram_by_electrode.tsv')

electrode_session_xspec_db <- electrode_session_db %>%
  left_join(xspec_db, by=c('subject','session_id','electrode')) 

electrode_session_xspec_db %>%
  write_tsv('results/electrode_session_xspec.tsv')




