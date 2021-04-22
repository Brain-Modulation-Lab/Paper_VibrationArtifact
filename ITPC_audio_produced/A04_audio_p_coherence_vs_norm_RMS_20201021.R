library(tidyverse)
library(glue)
library(readr)
library(magrittr)

setwd("Z:\\Users\\busha\\Analysis\\2020-08-05-audio_p-coherence")


# Produced phoneme
subjects<-
  tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>%
  mutate(path_annot=glue("Z:\\DBS\\{subject}\\Preprocessed Data\\Sync\\annot")) %>%
  mutate(path_phoneme=glue("{path_annot}\\{subject}_produced_phoneme.txt")) %>%
  mutate(exists_phoneme=file.exists(path_phoneme)) %>%
  filter(exists_phoneme)

phoneme_files <- subjects$path_phoneme
names(phoneme_files) <- subjects$subject
phoneme <- phoneme_files %>% 
  map_df(read_tsv, .id = "subject")

vowel <- phoneme %>%
  filter(type=='vowel')

vowel_agg <- vowel %>%
  group_by(subject,session_id) %>%
  summarise(median_rms_audio_p = median(rms_audio_p,na.rm=TRUE))


# Inter Trial Interval
subjects<-
  tibble(subject=paste0('DBS',c(seq(3001,3032),seq(4057,4088)))) %>%
  mutate(path_annot=glue("Z:\\DBS\\{subject}\\Preprocessed Data\\Sync\\annot")) %>%
  mutate(path_ITI=glue("{path_annot}\\{subject}_inter_trial_interval.txt")) %>% 
  mutate(exists_ITI=file.exists(path_ITI)) %>%
  filter(exists_ITI)

ITI_files <- subjects$path_ITI
names(ITI_files) <- subjects$subject
ITI <- ITI_files %>% 
  map_df(read_tsv, .id = "subject")

ITI_agg <- ITI %>%
  group_by(subject,session_id) %>%
  summarise(ITI_median_rms_audio_p = median(rms_audio_p,na.rm=TRUE))

vowel_agg <- vowel_agg %>%
  left_join(ITI_agg)

vowel_agg <- vowel_agg %>%
  mutate(SNR_audio_p = 20 * log10(median_rms_audio_p/ITI_median_rms_audio_p))

coherence_agg2 <- read_tsv("data/coherence_audio_p_by_electrode.tsv")
coherence_agg3 <- read_tsv("data/coherence_audio_p_by_session.tsv")
coherence_agg4 <- write_tsv("data/coherence_audio_p_by_session_stim_volume.tsv")


coherence_agg3 <- coherence_agg3 %>%
  left_join(vowel_agg)


coherence_agg3 %>%
  ggplot() +
  aes(x=SNR_audio_p, y=log10(median_rNC), color=subject) +
  geom_point() +
  geom_smooth(aes(group=1),method='lm')+
  theme(legend.position = 'none')
ggsave('fig/07_log10_median_rNC_vs_SNR_audio_p.png',width=5,height=5)

coherence_agg3 %>%
  ggplot() +
  aes(x=SNR_audio_p, y=median_rNC, color=subject) +
  geom_point() +
  geom_smooth(aes(group=1),method='lm')+
  theme(legend.position = 'none')
ggsave('fig/07_median_rNC_vs_SNR_audio_p.png',width=5,height=5)

vowel_agg2 <- vowel %>%
  group_by(subject,session_id,stim_volume) %>%
  summarise(median_rms_audio_p = median(rms_audio_p,na.rm=TRUE)) %>%
  left_join(ITI_agg) %>%
  mutate(SNR_audio_p = 20 * log10(median_rms_audio_p/ITI_median_rms_audio_p))


coherence_agg4 <- coherence_agg4 %>%
  left_join(vowel_agg2)


coherence_agg4 %>%
  ggplot() +
  aes(x=SNR_audio_p, y=log10(median_rNC), color=subject, shape=factor(stim_volume)) +
  geom_point() +
  geom_smooth(aes(group=1),method='lm')+
  theme(legend.position = 'none')
ggsave('fig/08_log10_median_rNC_vs_SNR_audio_p_by_stim_volume.png',width=5,height=5)


coherence_agg4 %>%
  ggplot() +
  aes(x=SNR_audio_p, y=log10(median_rNC), shape=factor(stim_volume)) +
  geom_point() +
  geom_smooth(aes(group=1),method='lm')+
  facet_wrap(~subject)+
  theme(legend.position = 'none')
ggsave('fig/09_log10_median_rNC_vs_SNR_audio_p_by_stim_volume.png',width=15,height=15)







