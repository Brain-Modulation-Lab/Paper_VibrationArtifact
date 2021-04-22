library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(lmPerm)
library(broom)
library(ggforce)

theme_set(theme_bw())

#PATH_ANALYSIS = "/Volumes/Nexus/Users/busha/Analysis/2020-08-05-audio_p-coherence/Permutation-control"
PATH_ANALYSIS = "Z:/Users/busha/Analysis/2020-08-05-audio_p-coherence/Permutation-control"
PATH_DB = paste0(PATH_ANALYSIS,"/data")
PATH_FIG = paste0(PATH_ANALYSIS,"/fig")


setwd(PATH_ANALYSIS)
coherence_db <- read_tsv('data/coherence_audio_p.txt',
                         col_types=cols(
                           subject = col_character(),
                           starts = col_double(),
                           duration = col_double(),
                           session_id = col_double(),
                           trial_id = col_double(),
                           type = col_character(),
                           syl_id = col_double(),
                           stim = col_character(),
                           stim_volume = col_double(),
                           rms_audio_p = col_double(),
                           channel1 = col_character(),
                           coherence = col_character(),
                           norm1 = col_double(),
                           norm2 = col_double()
                         )) %>%
  rename(electrode = channel1) %>%
  mutate(electrode_type = tolower(str_extract(electrode, "^[a-zA-Z0-9]+"))) %>%
  mutate(coherence = str_replace(coherence,fixed('+-'),'-')) %>%
  mutate(coherence = as.complex(coherence)) %>%
  filter(!is.na(norm1)) %>%
  filter(!is.na(coherence)) %>%
  left_join(select(read_tsv('data/electrodes_renamed.tsv'),electrode,electrode2)) %>%
  mutate(electrode=electrode2)

coherence_db_norm_db <- coherence_db %>%
  mutate(norm_coherence = coherence / (norm1 * norm2))

# 
# agg_xcorr_db <- xcorr_norm_db %>%
#   group_by(subject,session_id,type,electrode_type,electrode) %>%
#   summarise(median_lag = median(lag),
#             wmean_lag = sum(lag*norm_maxcorr)/sum(norm_maxcorr),
#             sd_lag = sd(lag),
#             median_revlag = median(revlag),
#             wmean_revlag = sum(revlag*norm_maxrevcorr)/sum(norm_maxrevcorr),
#             sd_revlag = sd(revlag),
#             norm_maxcorr_q90 = quantile(norm_maxcorr,0.9,na.rm=TRUE),
#             norm_maxrevcorr_q90 = quantile(norm_maxrevcorr,0.9,na.rm=TRUE),
#             norm_maxcorr_median = median(norm_maxcorr,na.rm=TRUE),
#             norm_maxrevcorr_median = median(norm_maxrevcorr,na.rm=TRUE)
#             )
# 


# === Fig 01c corr vs lag for each channel, epoch, session, subject ========
se <- function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))

plot_coherence <- function(data){
  mdata <- data %>%
    group_by(type,electrode) %>%
    summarise(mRNC = mean(Re(norm_coherence),na.rm=TRUE), 
              mINC = mean(Im(norm_coherence),na.rm=TRUE),
              seRNC = se(Re(norm_coherence)), 
              seINC = se(Im(norm_coherence)))
  
  p<-ggplot(data) +
    aes(x=Re(norm_coherence),y=Im(norm_coherence),color=type,shape=type)+
    geom_point(size=0.2,alpha=0.5) +
    geom_vline(xintercept=0,color='grey',size=0.2) +
    geom_hline(yintercept=0,color='grey',size=0.2) +
    geom_point(aes(x=mRNC*10,y=mINC*10,shape=type),data=mdata,size=3,color='white') +   
    geom_point(aes(x=mRNC*10,y=mINC*10,color=type,shape=type),data=mdata,size=1.5) +
    geom_errorbar(aes(x=mRNC*10,y=NULL,color=type,ymax=(mINC+seINC)*10,ymin=(mINC-seINC)*10),data=mdata) +
    geom_errorbarh(aes(x=NULL,y=mINC*10,color=type,xmax=(mRNC+seRNC)*10,xmin=(mRNC-seRNC)*10),data=mdata) +
    facet_wrap(~electrode) +
    coord_fixed(ratio = 1, xlim=c(-0.3,0.3), ylim=c(-0.3,0.3))
  
  return(p)
}


coherence_db_norm_db %>%
  #filter(subject=='DBS3004') %>%
  filter(electrode_type %in% c("ecog","dbs")) %>%
  group_by(subject, session_id) %>%
  group_walk(~ggsave(paste0(PATH_FIG,'/01c_coherence_',.y[[1]],'_S',.y[[2]],'.png'),
                     plot=plot_coherence(.),
                     width=12,height=12,units='in')
  )
# ========================================================================
# 
# coherence_db_norm_db %>%
#   filter(subject=='DBS3004' & session_id==1) %>%
#   filter(electrode_type %in% c("ecog","dbs")) %>%
#   plot_coherence()
# 



# === Fig 01d corr vs lag for each channel, epoch, session, subject ========
se <- function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))

plot_coherence2 <- function(data){
  mdata <- data %>%
    group_by(type,electrode) %>%
    summarise(mRNC = mean(Re(norm_coherence),na.rm=TRUE), 
              mINC = mean(Im(norm_coherence),na.rm=TRUE),
              seRNC = se(Re(norm_coherence)), 
              seINC = se(Im(norm_coherence)),
              seNC = (seRNC + seINC)/2)
  
  p<-ggplot(mdata) +
    aes(x=mRNC/seNC,y=mINC/seNC,color=type,shape=type) +
    geom_point(size=1.5) +
    geom_errorbar(aes(x=mRNC/seNC,y=NULL,color=type,ymax=(mINC+seINC)/seNC,ymin=(mINC-seINC)/seNC)) +
    geom_errorbarh(aes(x=NULL,y=mINC/seNC,color=type,xmax=(mRNC+seRNC)/seNC,xmin=(mRNC-seRNC)/seNC)) +
    geom_point(size=0.2,alpha=0.5) +
    geom_vline(xintercept=0,color='grey',size=0.2) +
    geom_hline(yintercept=0,color='grey',size=0.2) +
    geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=1),color='grey',size=0.4)+
    geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=3),color='grey',size=0.4)+
    facet_wrap(~electrode) +
    coord_fixed(ratio = 1)
  
  return(p)
}


coherence_db_norm_db %>%
  #filter(subject=='DBS3004') %>%
  filter(electrode_type %in% c("ecog","dbs")) %>%
  group_by(subject, session_id) %>%
  group_walk(~ggsave(paste0(PATH_FIG,'/01d_coherence_',.y[[1]],'_S',.y[[2]],'.png'),
                     plot=plot_coherence2(.),
                     width=12,height=12,units='in')
  )
# ========================================================================

coherence_db_norm_db %>%
  filter(subject=='DBS3004' & session_id==1) %>%
  filter(electrode_type %in% c("ecog","dbs")) %>%
  plot_coherence2()











coherence_db_norm_db %>%
  filter(subject=='DBS3014' & session_id==3 & electrode=='ecog_128') %>%
  filter(electrode_type %in% c("ecog","dbs")) %>%
  group_by(subject, session_id) %>%
  ggplot() +
   aes(x=Re(norm_coherence),y=Im(norm_coherence),color=trial_id)+
   geom_point(size=0.5,alpha=1) +
   geom_vline(xintercept=0,color='grey',size=0.2) +
   geom_hline(yintercept=0,color='grey',size=0.2) +
   facet_wrap(type~electrode)




# === Fig 02 lag histogram for channel with clear vibration artifact
xcorr_norm_db %>% 
  filter(subject=='DBS3010' & session_id==2 & electrode == 'ecog_156') %>%
  filter(electrode_type == "ecog" & type=="prod") %>%
  ggplot() +
  aes(x=lag)+
  geom_histogram(binwidth=1) +
  facet_wrap(~electrode) +
  scale_x_continuous(breaks=-80:80,limits=c(-5,45)) 
ggsave(paste0(PATH_FIG,'/02_lag-histogram_DBS3010_S2_ecog_156.png'))
# ========================================================================

#from foof analysis
#subject	session_id	electrode	epoch_type	gaus_id	gaus_freq	gaus_amp	gaus_sigma
#DBS3010	1	audio_p	prod	6	127.795632045264	1.83827211407161	16.2842251128281

# === Fig 03 norm max corr histogram =====================================
xcorr_norm_db %>% 
  filter(electrode_type %in% c('dbs','ecog','audio')) %>%
  ggplot() +
  aes(x=norm_maxcorr)+
  geom_histogram(binwidth=0.01) +
  facet_grid(type~electrode_type)
ggsave(paste0(PATH_FIG,'/03_norm_maxcorr_historgam.png'))
# ========================================================================



# === Fig 04 lag histogram for channel with clear vibration artifact
xcorr_norm_db %>% 
  filter(subject=='DBS3006' & session_id==1 & electrode == 'ecog_110') %>%
  filter(electrode_type == "ecog" & type=="stim") %>%
  ggplot() +
  aes(x=lag)+
  geom_histogram(binwidth=1) +
  facet_wrap(~electrode) +
  scale_x_continuous(breaks=seq(-80,80,5))
ggsave(paste0(PATH_FIG,'/04_lag-histogram_DBS3006_S1_ecog_110_stim.png'))
# ========================================================================






# Experimental stuff from here on




agg_xcorr_db %>%
  filter(electrode_type %in% c('dbs','ecog','audio')) %>%
  ggplot() +
  aes(x=log10(norm_maxcorr_q90))+
  geom_histogram(binwidth=0.01) +
  facet_grid(type~electrode_type) +
  xlim(c(-1,0.1))


agg_xcorr_db %>%
  filter(electrode_type %in% c('dbs','ecog','audio')) %>%
  ggplot() +
  aes(x=log10(norm_maxcorr_median))+
  geom_histogram(binwidth=0.01) +
  facet_grid(type~electrode_type) +
  xlim(c(-1,0.1))


agg_xcorr_db %>%
  filter(electrode_type == 'ecog' & type == 'prod' & norm_maxcorr_q90 > 0.25) %>%
  select(c(1:5,12,14)) %>%
  View()



agg_xcorr_db %>%
  filter(electrode == 'ecog_138' & subject=='DBS4084') %>%
  View()

agg_xcorr_db %>%
  filter( subject=='DBS4084') %>%
  filter(electrode %in% c('audio_p')) %>%
  ggplot() +
  aes(x=log10(norm_maxcorr_q90))+
  geom_histogram(binwidth=0.01) +
  facet_grid(type~electrode_type) +
  xlim(c(-2,0.1))



agg_xcorr_db %>%
  filter( subject=='DBS4084') %>%
  filter(electrode_type %in% c('dbs','ecog','audio')) %>%
  ggplot() +
  aes(x=log10(norm_maxcorr_q90))+
  geom_histogram(binwidth=0.01) +
  facet_grid(type~electrode_type) +
  xlim(c(-2,1.5))


agg_xcorr_db %>%
  filter( subject=='DBS4084') %>%
  filter(electrode_type %in% c('dbs','ecog','audio')) %>%
  ggplot() +
  aes(x=sd_lag)+
  geom_histogram() +
  facet_grid(type~electrode_type)


agg_xcorr_db %>%
  filter(subject=='DBS4084') %>%
  filter(sd_lag < 35) %>%
  View()


agg_xcorr_db %>%
  filter(subject=='DBS4084') %>%
  filter(electrode_type == 'ecog' & type == 'prod' & norm_maxcorr_q90 > 0.1) %>%
  View()


xcorr_norm_db %>%
  filter(electrode_type %in% c('dbs','ecog','audio')) %>%
  ggplot() +
  aes(x=maxcorr, y=maxrevcorr)+
  scale_x_log10()+
  scale_y_log10()+
  geom_point() +
  geom_abline(slope=1) + 
  facet_grid(electrode_type ~ type)
  





# age <- read_tsv('data/subjects_3000_4000_age.txt') %>%
#   mutate(subject = dbspatientid) %>%
#   select(subject, dbs_age)
# 
# dx <-  read_tsv('data/subjects_3000_4000_dx.txt') %>%
#   select(subject, dx)
# 
# updrs <- read_tsv('data/subjects_3000_4000_preop_updrs.txt') %>%
#   select(subject, on_score, off_score)


xcorr_db %>%
  filter(electrode_type %in% c('dbs','ecog','audio')) %>%
  ggplot() +
  aes(x=log10(norm1)) +
  geom_histogram() +
  facet_wrap(~electrode_type,scale='free')

xcorr_db %>%
  filter(electrode %in% c('audio_p')) %>%
  filter(log10(norm1) > 0) %>%
  ggplot() +
  aes(x=log10(norm1),fill=type) +
  geom_histogram() +
  facet_grid(type~electrode_type,scale='free')

xcorr_db %>%
  filter(electrode_type %in% c('ecog')) %>%
  filter(log10(norm1) > 0) %>%
  ggplot() +
  aes(x=log10(norm1),fill=type) +
  geom_histogram() +
  facet_grid(type~electrode_type,scale='free')






#electrode_session <- read_tsv('data/electrode_session.txt') 

xcorr_db %>%
  filter(electrode_type == 'audio') %>%
  filter(norm_maxcorr > 0.25) %>%
  View()

xcorr_db %>%
  filter(electrode_type == 'ecog') %>%
  ggplot() +
  aes(x = maxcorr) +
  geom_histogram(binwidth = 1e5) +
  xlim(c(0,0.2e8))
  


xcorr_norm_db %>%
  #filter(abs(lag) < 5) %>%
  ggplot() +
  aes(x = lag, y = norm_maxcorr) + 
  geom_jitter(size=0.1)

xcorr_db %>%
  filter(log10(abs(norm_maxcorr)) > -10) %>%
  filter(electrode_type %in% c("ecog","audio","dbs")) %>%
  ggplot() +
  aes(x=log10(abs(norm_maxcorr)),fill=electrode_type) +
  geom_histogram(binwidth=0.01) +
  facet_wrap(~lag)
ggsave('fig/01_maxcorr_vs_lag_histogram.png',width=15,height=15,units='in')




xcorr_db %>%
  filter(electrode_type %in% c("ecog","audio","dbs")) %>%
  ggplot() +
  aes(x=log10(abs(norm_maxcorr)),fill=electrode_type) +
  geom_histogram(binwidth=0.01) +
  facet_wrap(~lag)
ggsave('fig/02_maxcorr_vs_lag_histogram.png',width=15,height=15,units='in')





xcorr_db %>%
  filter(lag==0) %>%
  filter(log10(abs(norm_maxcorr)) > 0) %>%
  View()


xcorr_db %>%
  filter(abs(norm_maxcorr) < 2) %>%
  ggplot() +
  aes(x = norm_maxcorr) + 
  geom_histogram(binwidth=0.01)+
  xlim(c(0,1))

+
  facet_wrap(~lag)+
  xlim(c(-0.1,0.5))

xcorr_db %>%
  filter(maxcorr > 0.5) %>%
  filter(electrode != 'audio_p') %>%
  View()




agg_xcorr_db <- xcorr_db %>%
  group_by(subject,session_id,type,electrode,electrode_type) %>%
  summarise(maxcorr = quantile(maxcorr,0.9,na.rm=TRUE))


agg_xcorr_db %>%
  ggplot() +
  aes(x=maxcorr) +
  geom_histogram(binwidth = 1e5) +
  xlim(c(0,0.2e8))

agg_xcorr_db %>%
  filter(electrode_type == 'ecog') %>%
  filter(maxcorr > 1e6) %>%
  View()


xcorr_norm_db %>% 
  filter(electrode_type == "ecog" & type=="prod") %>%
  ggplot() +
  aes(x=lag, y=norm_maxcorr)+
  geom_jitter(size=0.1,alpha=0.1)





xcorr_norm_db %>% 
  filter(subject=='DBS3010' & session_id==2) %>%
  filter(electrode_type == "ecog" & type=="prod") %>%
  ggplot() +
  aes(x=lag)+
  geom_histogram(binwidth=1) +
  facet_wrap(~electrode)


xcorr_norm_db %>% 
  filter(subject=='DBS3010' & session_id==2 & electrode == 'ecog_156') %>%
  filter(electrode_type == "ecog" & type=="prod") %>%
  ggplot() +
  aes(x=lag)+
  geom_histogram(binwidth=1) +
  facet_wrap(~electrode) +
  scale_x_continuous(breaks=-80:80,limits=c(-5,45)) 




xcorr_norm_db %>% 
  filter(electrode_type == "ecog" & type=="prod" & session_id == 1) %>%
  ggplot() +
  aes(x=lag)+
  geom_histogram(binwidth=1) +
  facet_wrap(~trial_id)

xcorr_norm_db %>% 
  filter(electrode_type == "ecog" & type=="prod") %>%
  ggplot() +
  aes(x=trial_id,y=electrode,fill=norm_maxcorr)+
  geom_tile() + 
  facet_grid(subject~session_id)


xcorr_norm_db %>% 
  filter(electrode_type == "ecog" & type=="prod") %>%
  filter(subject %in% c("DBS3010")) %>%
  ggplot() +
  aes(x=trial_id,y=electrode,fill=norm_maxcorr)+
  geom_tile() + 
  facet_grid(subject~session_id)



agg_xcorr_syl_db <- xcorr_norm_db %>%
  group_by(subject,session_id,trial_id,type,electrode_type,electrode) %>%
  summarise(median_norm_maxcorr = median(norm_maxcorr,na.rm=TRUE),
            median_norm_maxrevcorr = median(norm_maxrevcorr,na.rm=TRUE))

agg_xcorr_syl_db %>% 
  filter(electrode_type == "ecog" & type=="prod") %>%
  filter(subject %in% c("DBS3010")) %>%
  ggplot() +
  aes(x=trial_id,y=electrode,fill=median_norm_maxcorr)+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
  geom_tile() + 
  facet_grid(subject~session_id)


















#######################################################################################

# Filtering out bad fits
aper_all %>%
  ggplot() + 
  aes(x=r_squared) +
  geom_histogram(binwidth=0.001) +
  geom_vline(xintercept=0.98,color='red')
ggsave('figures_aper/00a_aper_r_squared_hist.pdf',width=4,height=4)

aper_all %>%
  ggplot() + 
  aes(x=r_squared) +
  geom_histogram(binwidth=0.001) +
  xlim(c(0.9,1.))
ggsave('figures_aper/00a2_aper_r_squared_hist.pdf',width=3,height=3)


aper_all %>%
  with(median(r_squared,na.rm=TRUE))


aper_all %>%
  ggplot() + 
  aes(x=error) +
  geom_histogram(binwidth=0.001)+
  geom_vline(xintercept=0.14,color='red')
ggsave('figures_aper/00b_aper_error_hist.pdf',width=4,height=4)

aper_all %>%
  filter(!is.nan(log10_tau)) %>%
  filter(log10_tau > -3.5) %>%
  filter(log10_tau < -0.5 ) %>%
  ggplot() + 
  aes(x=log10_tau) +
  geom_histogram(binwidth=0.05) +
  scale_x_continuous(breaks=c(-3,-2.5,-2,-1.5,-1,-0.5),labels = c(1,3.16,10,31.6,100,316))+
  xlab('Tau (ms)')
ggsave('figures_aper/00c_aper_log10_tau_hist.pdf',width=4,height=4)

aper_all %>%
  filter(!is.nan(log10_tau)) %>%
  filter(log10_tau > -3.5) %>%
  filter(log10_tau < -0.5 ) %>%
  ggplot() + 
  aes(x=log10_tau, y=..density.., fill=electrode_type) +
  geom_histogram(binwidth=0.05) +
  facet_wrap(~electrode_type) +
  scale_x_continuous(breaks=c(-3,-2.5,-2,-1.5,-1,-0.5),labels = c(1,3.16,10,31.6,100,316))+
  xlab('Tau (ms)')
ggsave('figures_aper/00d_aper_log10_tau_hist_by_electrode_type.pdf',width=4,height=4)





aper_all %>% tally()

aper_all %>%
  filter(!is.nan(log10_tau)) %>%
  filter(log10_tau > -3.5) %>%
  #filter(log10_tau < -0.5 ) %>%
  tally()

#checking if nans in log10_tau are randomly distributed or not
aper_all %>%
  filter(electrode_type %in% c('ecog','dbs','macro','micro')) %>%
  group_by(subject, electrode_type) %>%
  summarise(n_nan = sum(is.nan(log10_tau)), n=n()) %>%
  mutate(frac_nan = n_nan/n, se_frac_nan = frac_nan*(1-frac_nan)/n) %>%
  ggplot() +
  aes(x=electrode_type, color=electrode_type, y=frac_nan, ymax = frac_nan + se_frac_nan, ymin = frac_nan - se_frac_nan) +
  geom_jitter() 
ggsave('figures_aper/00d_frac_non_knee_per_electrode_type.pdf',width=6,height=4)


aper <- aper_all %>%
  filter(r_squared > 0.98) %>%
  filter(error < 0.14) %>%
  filter(!subject %in% c('DBS4080','DBS4086')) %>% #strange PSD plots
  filter(!subject %in% c('DBS4088')) #epilepsy patient

aper %>%
  filter(electrode_type %in% c('macro','micro','dbs','ecog','v0')) %>%
  ggplot() +
  aes(y=aper_offset,x=subject,color=epoch_type)+
  scale_y_log10()+
  geom_boxplot()+
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/01_aper_offset.pdf',width=12,height=8)

aper %>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  ggplot() +
  aes(y=aper_offset,x=subject,color=epoch_type)+
  scale_y_log10()+
  geom_boxplot()+
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/01b_aper_offset.pdf',width=12,height=4)

aper %>%
  filter(electrode_type %in% c('macro','micro','dbs','ecog','v0')) %>%
  ggplot() +
  aes(y=aper_knee,x=subject,color=epoch_type)+
  scale_y_log10()+
  geom_boxplot()+
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/02_aper_knee.pdf',width=12,height=8)

aper %>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  ggplot() +
  aes(y=aper_knee,x=subject,color=epoch_type)+
  scale_y_log10()+
  geom_boxplot()+
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/02b_aper_knee.pdf',width=12,height=4)


aper %>%
  filter(electrode_type %in% c('macro','micro','dbs','ecog','v0')) %>%
  ggplot() +
  aes(y=log10_tau,x=subject,color=epoch_type)+
  geom_boxplot()+
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/02c_aper_log10_tau.pdf',width=12,height=8)

aper %>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  ggplot() +
  aes(y=log10_tau,x=subject,color=epoch_type)+
  geom_boxplot()+
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/02d_log10_tau.pdf',width=12,height=4)


aper %>%
  filter(electrode_type %in% c('macro','micro','dbs','ecog','v0')) %>%
  ggplot() +
  aes(y=aper_exp,x=subject,color=epoch_type)+
  geom_boxplot()+  
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/03_aper_exp.pdf',width=12,height=8)

aper %>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  ggplot() +
  aes(y=aper_exp,x=subject,color=epoch_type)+
  geom_boxplot()+  
  facet_wrap(~electrode_type,ncol=1)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
ggsave('figures_aper/03b_aper_exp.pdf',width=12,height=4)

#===== Doing correlation of fooof params to demographic data

agg_aper <- aper %>%
  filter(electrode_type == 'ecog') %>%
  filter(epoch_type == 'ITI') %>%
  group_by(subject) %>%
  summarise(med_exp = median(exp,na.rm=TRUE),
            med_offset = median(offset,na.rm=TRUE),
            med_knee = median(knee,na.rm=TRUE),
            med_log10_tau = median(log10_tau,na.rm=TRUE)) %>%
  left_join(age) %>%
  left_join(dx) %>%
  left_join(updrs)

#==== exponent

ggplot(agg_aper) +
  aes(x=dbs_age, y=med_exp) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/04_aper_exp_vs_age.pdf',width=4,height=4)

cor.test(x=agg_aper$dbs_age, y=agg_aper$med_exp, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$dbs_age and agg_aper$med_exp
# S = 7498.8, p-value = 0.1552
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# -0.253142 

agg_aper %>%
  filter(dx %in% c('PD','ET')) %>%
  ggplot() +
  aes(x=dx, y=med_exp) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')
ggsave('figures_aper/05_aper_exp_vs_dx.pdf',width=4,height=4)

summary(lmp(med_exp ~ dx, data =  filter(agg_aper, dx %in% c('PD','ET'))))
# Coefficients:
#   Estimate Iter Pr(Prob)
# dx1 -0.08276   63    0.619
# 
# Residual standard error: 0.5292 on 31 degrees of freedom
# Multiple R-Squared: 0.02353,	Adjusted R-squared: -0.007968 
# F-statistic: 0.747 on 1 and 31 DF,  p-value: 0.3941 

kruskal.test(dx ~ med_exp, data = filter(agg_aper, dx %in% c('PD','ET')) )
# Kruskal-Wallis rank sum test
# 
# data:  dx by med_exp
# Kruskal-Wallis chi-squared = 25, df = 25, p-value = 0.4624
# 

agg_aper %>%
  filter(dx %in% c('PD','ET')) %>%
  with(ks.test(med_exp[dx=='PD'],med_exp[dx=='ET']))


ks.test(dx ~ med_exp, data = filter(agg_aper, dx %in% c('PD','ET')) )


ggplot(agg_aper) +
  aes(x=on_score, y=med_exp) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/06_aper_exp_vs_preop_updrs_on.pdf',width=4,height=4)

cor.test(x=agg_aper$on_score, y=agg_aper$med_exp, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$on_score and agg_aper$med_exp
# S = 307.23, p-value = 0.001786
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.682938 

ggplot(agg_aper) +
  aes(x=off_score, y=med_exp) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/07_aper_exp_vs_preop_updrs_off.pdf',width=4,height=4)

cor.test(x=agg_aper$off_score, y=agg_aper$med_exp, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$off_score and agg_aper$med_exp
# S = 989.36, p-value = 0.2757
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.256121 

#==== offset

ggplot(agg_aper) +
  aes(x=dbs_age, y=log10(med_offset)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/08_aper_offset_vs_age.pdf',width=4,height=4)

cor.test(x=agg_aper$dbs_age, y=log10(agg_aper$med_offset), method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$dbs_age and log10(agg_aper$med_offset)
# S = 7676.2, p-value = 0.1108
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.2827953 


agg_aper %>%
  filter(dx %in% c('PD','ET')) %>%
  ggplot() +
  aes(x=dx, y=log10(med_offset)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')
ggsave('figures_aper/09_aper_offset_vs_dx.pdf',width=4,height=4)

summary(lmp(log10(med_offset) ~ dx, data =  filter(agg_aper, dx %in% c('PD','ET'))))
# Coefficients:
#   Estimate Iter Pr(Prob)
# dx1 -0.009007   51    0.784
# 
# Residual standard error: 0.07924 on 29 degrees of freedom
# Multiple R-Squared: 0.01193,	Adjusted R-squared: -0.02214 
# F-statistic: 0.3501 on 1 and 29 DF,  p-value: 0.5586 



kruskal.test(dx ~ log10(med_offset),data = filter(agg_aper, dx %in% c('PD','ET')) )
# Kruskal-Wallis rank sum test
# 
# data:  dx by log10(med_offset)
# Kruskal-Wallis chi-squared = 32, df = 32, p-value = 0.4667

ggplot(agg_aper) +
  aes(x=on_score, y=med_offset) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/10_aper_offset_vs_preop_updrs_on.pdf',width=4,height=4)

cor.test(x=agg_aper$on_score, y=agg_aper$med_offset, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$on_score and agg_aper$med_offset
# S = 290.14, p-value = 0.001203
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.7005822 

ggplot(agg_aper) +
  aes(x=off_score, y=med_offset) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/11_aper_offset_vs_preop_updrs_off.pdf',width=4,height=4)

cor.test(x=agg_aper$off_score, y=agg_aper$med_offset, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$off_score and agg_aper$med_offset
# S = 1024.4, p-value = 0.3298
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2297556 


#==== knee

agg_aper %>%
  ggplot() +
  aes(x=dbs_age, y=log10(med_knee)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/12_aper_knee_vs_age.pdf',width=4,height=4)

cor.test(x=agg_aper$dbs_age, y=agg_aper$med_knee, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$dbs_age and agg_aper$med_knee
# S = 7160.7, p-value = 0.08168
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.3124438 


agg_aper %>%
  filter(dx %in% c('PD','ET')) %>%
  ggplot() +
  aes(x=dx, y=log10(med_knee))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')
ggsave('figures_aper/13_aper_knee_vs_dx.pdf',width=4,height=4)


ggplot(agg_aper) +
  aes(x=on_score, y=log10(med_knee)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/14_aper_knee_vs_preop_updrs_on.pdf',width=4,height=4)

cor.test(x=agg_aper$on_score, y=agg_aper$med_knee, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$on_score and agg_aper$med_aper_knee
# S = 417.86, p-value = 0.01377
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.568769 

agg_aper %>%
  #filter(subject != 'DBS3021') %>%
  ggplot() +
  aes(x=off_score, y=log10(med_knee)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/15_aper_knee_vs_preop_updrs_off.pdf',width=4,height=4)

agg_aper %>%
  #filter(subject != 'DBS3021') %>%
  with(cor.test(x=off_score, y=log10(med_knee), method= "spearman"))

# Spearman's rank correlation rho
# 
# data:  agg_aper$off_score and agg_aper$med_aper_knee
# S = 1188.7, p-value = 0.6558
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1062149 




#==== tau

ggplot(agg_aper) +
  aes(x=dbs_age, y=med_log10_tau)+
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/16_aper_log10_tau_vs_age.pdf',width=4,height=4)

agg_aper %>%
  with(cor.test(x=dbs_age, y=med_log10_tau, method= "spearman"))
# Spearman's rank correlation rho
# 
# data:  agg_aper$dbs_age and agg_aper$med_log10_tau
# S = 3311.1, p-value = 0.02602
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3931278 

agg_aper %>%
  filter(dx %in% c('PD','ET')) %>%
  ggplot() +
  aes(x=dx, y=med_log10_tau)+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')
ggsave('figures_aper/17_aper_log10_tau_vs_dx.pdf',width=4,height=4)


ggplot(agg_aper) +
  aes(x=on_score, y=med_log10_tau) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/18_aper_log10_tau_vs_preop_updrs_on.pdf',width=4,height=4)

cor.test(x=agg_aper$on_score, y=agg_aper$med_log10_tau, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper$on_score and agg_aper$med_log10_tau
# S = 1320, p-value = 0.1396
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# -0.362227 

ggplot(agg_aper) +
  aes(x=off_score, y=med_log10_tau) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/19_aper_log10_tau_vs_preop_updrs_off.pdf',width=4,height=4)

cor.test(x=agg_aper$off_score, y=agg_aper$med_log10_tau, method= "spearman")

# Spearman's rank correlation rho
# 
# data:  agg_aper$off_score and agg_aper$med_log10_tau
# S = 1133.6, p-value = 0.5345
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1476462 


###################### DBS channels ##############

agg_aper_dbs <- aper %>%
  filter(electrode_type == 'dbs') %>%
  filter(epoch_type == 'ITI') %>%
  group_by(subject) %>%
  summarise(med_exp = median(exp,na.rm=TRUE),
            med_offset = median(offset,na.rm=TRUE),
            med_knee = median(knee,na.rm=TRUE),
            med_log10_tau = median(log10_tau,na.rm=TRUE),
            ) %>%
  left_join(age) %>%
  left_join(dx) %>%
  left_join(updrs) 
#==== exponent

ggplot(agg_aper_dbs) +
  aes(x=dbs_age, y=med_exp) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/24_aper_dbs_exp_vs_age.pdf',width=4,height=4)

cor.test(x=agg_aper_dbs$dbs_age, y=agg_aper_dbs$med_exp, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper_dbs$dbs_age and agg_aper_dbs$med_exp
# S = 2893.1, p-value = 0.5916
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1127203 

agg_aper_dbs %>%
  filter(dx %in% c('PD','ET')) %>%
  ggplot() +
  aes(x=dx, y=med_exp) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')
ggsave('figures_aper/25_aper_dbs_exp_vs_dx.pdf',width=4,height=4)


summary(lmp(med_exp ~ dx, data =  filter(agg_aper_dbs, dx %in% c('PD','ET'))))
# Coefficients:
#   Estimate Iter Pr(Prob)
# dx1  0.01805   51    0.902
# 
# Residual standard error: 0.3606 on 20 degrees of freedom
# Multiple R-Squared: 0.00266,	Adjusted R-squared: -0.04721 
# F-statistic: 0.05333 on 1 and 20 DF,  p-value: 0.8197 

kruskal.test(dx ~ med_exp, data = filter(agg_aper_dbs, dx %in% c('PD','ET')) )
# Kruskal-Wallis rank sum test
# 
# data:  dx by med_exp
# Kruskal-Wallis chi-squared = 23, df = 23, p-value = 0.4608


ggplot(agg_aper_dbs) +
  aes(x=on_score, y=med_exp) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/26_aper_dbs_exp_vs_preop_updrs_on.pdf',width=4,height=4)

cor.test(x=agg_aper_dbs$on_score, y=agg_aper_dbs$med_exp, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper_dbs$on_score and agg_aper_dbs$med_exp
# S = 177.81, p-value = 0.5721
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1917828 

ggplot(agg_aper_dbs) +
  aes(x=off_score, y=med_exp) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/27_aper_exp_vs_preop_updrs_off.pdf',width=4,height=4)

cor.test(x=agg_aper_dbs$off_score, y=agg_aper_dbs$med_exp, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper_dbs$off_score and agg_aper_dbs$med_exp
# S = 408.12, p-value = 0.6932
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1212126 


#==== offset

ggplot(agg_aper_dbs) +
  aes(x=dbs_age, y=log10(med_offset)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/28_aper_dbs_offset_vs_age.pdf',width=4,height=4)

cor.test(x=agg_aper_dbs$dbs_age, y=log10(agg_aper_dbs$med_offset), method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper_dbs$dbs_age and log10(agg_aper_dbs$med_offset)
# S = 2510.8, p-value = 0.269
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.2405174 

agg_aper_dbs %>%
  filter(dx %in% c('PD','ET')) %>%
  ggplot() +
  aes(x=dx, y=log10(med_offset)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')
ggsave('figures_aper/29_aper_dbs_offset_vs_dx.pdf',width=4,height=4)

summary(lmp(log10(med_offset) ~ dx, data =  filter(agg_aper_dbs, dx %in% c('PD','ET'))))

# Coefficients:
#   Estimate Iter Pr(Prob)
# dx1 -0.007212   51     0.98
# 
# Residual standard error: 0.1367 on 20 degrees of freedom
# Multiple R-Squared: 0.002953,	Adjusted R-squared: -0.0469 
# F-statistic: 0.05923 on 1 and 20 DF,  p-value: 0.8102 

kruskal.test(dx ~ log10(med_offset),data = filter(agg_aper_dbs, dx %in% c('PD','ET')) )
# Kruskal-Wallis rank sum test
# 
# data:  dx by log10(med_offset)
# Kruskal-Wallis chi-squared = 23, df = 23, p-value = 0.4608


ggplot(agg_aper_dbs) +
  aes(x=on_score, y=med_offset) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/30_aper_dbs_offset_vs_preop_updrs_on.pdf',width=4,height=4)

cor.test(x=agg_aper_dbs$on_score, y=agg_aper_dbs$med_offset, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper_dbs$on_score and agg_aper_dbs$med_offset
# S = 234.06, p-value = 0.8519
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#         rho 
# -0.06392761 

ggplot(agg_aper_dbs) +
  aes(x=off_score, y=med_offset) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/31_aper_dbs_offset_vs_preop_updrs_off.pdf',width=4,height=4)

cor.test(x=agg_aper_dbs$off_score, y=agg_aper_dbs$med_offset, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper_dbs$off_score and agg_aper_dbs$med_offset
# S = 505.39, p-value = 0.1896
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.3884312 


#==== knee

ggplot(agg_aper_dbs) +
  aes(x=dbs_age, y=log10(med_knee)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/32_aper_dbs_knee_vs_age.pdf',width=4,height=4)

cor.test(x=agg_aper_dbs$dbs_age, y=agg_aper_dbs$med_offset, method= "spearman")
# Spearman's rank correlation rho
# 
# data:  agg_aper_dbs$dbs_age and agg_aper_dbs$med_offset
# S = 2893.1, p-value = 0.5916
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1127203 

agg_aper_dbs %>%
  filter(dx %in% c('PD','ET')) %>%
  ggplot() +
  aes(x=dx, y=log10(med_aper_knee))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')
ggsave('figures_aper/33_aper_dbs_knee_vs_dx.pdf',width=4,height=4)


ggplot(agg_aper_dbs) +
  aes(x=on_score, y=log10(med_aper_knee)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/34_aper_dbs_knee_vs_preop_updrs_on.pdf',width=4,height=4)


ggplot(agg_aper_dbs) +
  aes(x=off_score, y=log10(med_aper_knee)) +
  geom_smooth(method='lm') +
  geom_point()
ggsave('figures_aper/35_aper_dbs_knee_vs_preop_updrs_off.pdf',width=4,height=4)


###################################
#Looking at ECoG strips by cortical location

# merging localization info of ecog electrodes
aper_ecog <- aper %>%
  filter(electrode_type == 'ecog') %>%
  left_join(electrode_session %>% select(subject,session_id,electrode,HCPMMP1_label_1,target))


aper_ecog %>%
  filter(epoch_type=='ITI') %>%
  ggplot()+
  aes(y=aper_exp,x=HCPMMP1_label_1,color=epoch_type)+
  geom_boxplot()+
  facet_wrap(~subject) +
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))

#selecting cortical locations with most coverage for this cohort
coverage <- aper_ecog %>%
  group_by(subject, HCPMMP1_label_1) %>%
  tally(name='n_e') %>%
  group_by(HCPMMP1_label_1) %>%
  summarise(n_e=sum(n_e),n_s=n()) %>%
  arrange(n_s)

coverage %>%
  write_tsv('data/ecog_coverage_by_HCPMMP1_parcel.txt')
  
roi <- coverage %>%
  filter(n_s >= 7)

aper_ecog_roi <- aper_ecog %>%
  filter(HCPMMP1_label_1 %in% roi$HCPMMP1_label_1)

aper_ecog_roi %>%
  filter(epoch_type=='ITI') %>%
  ggplot()+
  aes(y=aper_exp,x=HCPMMP1_label_1,color=epoch_type)+
  geom_boxplot()+
  facet_wrap(~subject) +
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))

aper_ecog_roi_agg <- aper_ecog_roi %>%
  group_by(subject,epoch_type,HCPMMP1_label_1) %>%
  summarise(exp = median(exp), knee = median(knee), offset = median(offset), log10_tau = median(log10_tau)) %>%
  group_by(epoch_type) %>%
  mutate(exp_c = exp - median(exp), knee_c = knee - median(knee), offset_c = offset - median(offset), log10_tau_c = log10_tau - median(log10_tau)) %>%
  group_by(subject,epoch_type) %>% 
  mutate(exp_cs = exp - median(exp), knee_cs = knee - median(knee), offset_cs = offset - median(offset), log10_tau_cs = log10_tau - median(log10_tau)) %>%
  ungroup() %>%
  mutate(HCPMMP1_label_1 = paste0('L',HCPMMP1_label_1))


aper_ecog_roi_agg_lm_exp <- aper_ecog_roi_agg %>%
  group_by(epoch_type) %>%
  group_modify(~tidy(lm(exp ~ HCPMMP1_label_1 + subject, data=.x))) 

aper_ecog_roi_agg_lm_exp %>%
  filter(grepl('HCPMMP1_label_1',term)) %>%
  mutate(cortical_region = str_remove(term,'HCPMMP1_label_1L')) %>%
  ggplot() +
  aes(x=cortical_region,y=estimate,size=-log10(p.value),alpha=-log10(p.value),shape=p.value<0.05,color=epoch_type) +
  geom_point() +
  scale_alpha(range=c(0.4,1))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave('figures_aper/40_aper_ecog_exp_vs_cortical_region_lm_effect.pdf',width=7,height=4)


aper_ecog_roi_agg %>%
  ggplot() + 
  aes(x=HCPMMP1_label_1,y=exp,color=subject,group=subject)+
  geom_point() + 
  geom_line() +
  facet_wrap(~epoch_type)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave('figures_aper/41_aper_ecog_exp_vs_cortical_region.pdf',width=8,height=5)

aper_ecog_roi_agg %>%
  ggplot() + 
  aes(x=HCPMMP1_label_1,y=exp_cs)+
  geom_boxplot() + 
  facet_wrap(~epoch_type)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave('figures_aper/41b_aper_ecog_exp_vs_cortical_region.pdf',width=8,height=5)


aper_ecog_roi_agg_lm_tau <- aper_ecog_roi_agg %>%
  filter(!is.nan(log10_tau)) %>%
  filter(log10_tau > -4) %>%
  filter(log10_tau < 0 ) %>%
  group_by(epoch_type) %>%
  group_modify(~tidy(lm(log10_tau ~ HCPMMP1_label_1 + subject, data=.x))) 

aper_ecog_roi_agg_lm_tau %>%
  filter(grepl('HCPMMP1_label_1',term)) %>%
  mutate(cortical_region = str_remove(term,'HCPMMP1_label_1L')) %>%
  ggplot() +
  aes(x=cortical_region,y=estimate,size=-log10(p.value),alpha=-log10(p.value),shape=p.value<0.05,color=epoch_type) +
  geom_point() +
  scale_alpha(range=c(0.4,1))+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave('figures_aper/42_aper_ecog_log10_tau_vs_cortical_region_lm_effect.pdf',width=7,height=4)

aper_ecog_roi_agg %>%
  ggplot() + 
  aes(x=HCPMMP1_label_1,y=log10_tau,color=subject,group=subject)+
  geom_point() + 
  geom_line() +
  facet_wrap(~epoch_type)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave('figures_aper/43_aper_ecog_log10_tau_vs_cortical_region.pdf',width=8,height=5)

aper_ecog_roi_agg %>%
  ggplot() + 
  aes(x=HCPMMP1_label_1,y=log10_tau_cs)+
  geom_boxplot() + 
  facet_wrap(~epoch_type)+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave('figures_aper/43b_aper_ecog_log10_tau_cs_vs_cortical_region.pdf',width=8,height=5)


aper %>%
  filter(!is.nan(log10_tau)) %>%
  filter(log10_tau > -3.5) %>%
  filter(log10_tau < -0.5 ) %>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  ggplot() + 
  aes(x=log10_tau, y=..density.., fill=electrode_type, group=electrode_type) +
  geom_histogram(binwidth=0.05, position=position_identity(), alpha=0.5) +
  scale_x_continuous(breaks=c(-3,-2.5,-2,-1.5,-1,-0.5),labels = c(1,3.16,10,31.6,100,316))+
  xlab('Tau (ms)')
ggsave('figures_aper/50_aper_log10_tau_hist_by_electrode_type_dbs_ecog.pdf',width=6,height=4)


aper %>%
  replace_na(list(log10_tau=0)) %>%
  filter(!is.nan(log10_tau)) %>%
  filter(log10_tau > -3.5) %>%
  filter(log10_tau < 0.1 ) %>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  ggplot() + 
  aes(x=log10_tau, y=..density.., fill=electrode_type, group=electrode_type) +
  geom_histogram(binwidth=0.05, position=position_identity(), alpha=0.5) +
  scale_x_continuous(breaks=c(-3,-2.5,-2,-1.5,-1,-0.5,0),labels = c(1,3.16,10,31.6,100,316,1000))+
  xlab('Tau (ms)')
ggsave('figures_aper/51_aper_log10_tau_hist_by_electrode_type_dbs_ecog_with_nans.pdf',width=6,height=4)



aper %>%
  left_join(electrode_session %>% select(subject,session_id,electrode,HCPMMP1_label_1,target)) %>%
  mutate(target=tolower(target)) %>%
  filter(target %in% c('broca','pfc','sma','smc','stn','vim')) %>%
  mutate(anat = fct_collapse(target, Cortex=c('broca','pfc','sma','smc'), Thalamus='vim', STN='stn'))%>%
  filter(!is.nan(log10_tau)) %>%
  filter(log10_tau > -3.5) %>%
  filter(log10_tau < -0.5 ) %>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  ggplot() + 
  aes(x=log10_tau, fill=anat, group=anat) +
  geom_histogram(binwidth=0.05, position=position_identity(), alpha=0.5) +
  scale_x_continuous(breaks=c(-3,-2.5,-2,-1.5,-1,-0.5),labels = c(1,3.16,10,31.6,100,316))+
  xlab('Tau (ms)') +
  facet_wrap(~anat,ncol=1,scale='free_y')
ggsave('figures_aper/53_aper_log10_tau_hist_by_anatomical_target.pdf',width=6,height=6)



aper %>%
  left_join(electrode_session %>% select(subject,session_id,electrode,HCPMMP1_label_1,target)) %>%
  mutate(target=tolower(target)) %>%
  filter(target %in% c('broca','pfc','sma','smc','stn','vim')) %>%
  mutate(anat = fct_collapse(target, Cortex=c('broca','pfc','sma','smc'), Thalamus='vim', STN='stn'))%>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  filter(knee<300) %>%
  ggplot() + 
  #aes(x=log10(abs(knee)), fill=anat, group=anat) +
  aes(x=knee, fill=anat, group=anat) +
  geom_histogram(binwidth=2,position=position_identity(), alpha=0.5) +
  facet_wrap(~anat,ncol=1,scale='free_y')
ggsave('figures_aper/54_aper_knee_hist_by_anatomical_target.pdf',width=6,height=6)




aper %>%
  left_join(electrode_session %>% select(subject,session_id,electrode,HCPMMP1_label_1,target)) %>%
  mutate(target=tolower(target)) %>%
  filter(target %in% c('broca','pfc','sma','smc','stn','vim')) %>%
  mutate(anat = fct_collapse(target, Cortex=c('broca','pfc','sma','smc'), Thalamus='vim', STN='stn'))%>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  with(ks.test(knee[anat=='STN'],knee[anat=='Thalamus']))




aper %>%
  left_join(electrode_session %>% select(subject,session_id,electrode,HCPMMP1_label_1,target)) %>%
  mutate(target=tolower(target)) %>%
  filter(target %in% c('broca','pfc','sma','smc','stn','vim')) %>%
  mutate(anat = fct_collapse(target, Cortex=c('broca','pfc','sma','smc'), Thalamus='vim', STN='stn'))%>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  filter(knee<300) %>%
  ggplot() + 
  aes(x=exp, fill=anat, group=anat) +
  geom_histogram(binwidth=0.05,position=position_identity(), alpha=0.5) +
  facet_wrap(~anat,ncol=1,scale='free_y')
ggsave('figures_aper/54_aper_exp_hist_by_anatomical_target.pdf',width=6,height=6)



aper %>%
  left_join(electrode_session %>% select(subject,session_id,electrode,HCPMMP1_label_1,target)) %>%
  mutate(target=tolower(target)) %>%
  filter(target %in% c('broca','pfc','sma','smc','stn','vim')) %>%
  mutate(anat = fct_collapse(target, Cortex=c('broca','pfc','sma','smc'), Thalamus='vim', STN='stn'))%>%
  filter(electrode_type %in% c('dbs','ecog')) %>%
  filter(knee<300) %>%
  ggplot() + 
  aes(x=offset, fill=anat, group=anat) +
  geom_histogram(binwidth=0.05,position=position_identity(), alpha=0.5) +
  facet_wrap(~anat,ncol=1,scale='free_y')
ggsave('figures_aper/54_aper_offset_hist_by_anatomical_target.pdf',width=6,height=6)







#===== Doing correlation of fooof params to demographic data

agg_aper_parcel <- aper %>%
  filter(electrode_type == 'ecog') %>%
  filter(epoch_type == 'ITI') %>%
  left_join(select(electrode_session,subject,session_id,electrode,HCPMMP1_label_1)) %>%
  group_by(subject,HCPMMP1_label_1) %>%
  summarise(med_exp = median(exp,na.rm=TRUE),
            med_offset = median(offset,na.rm=TRUE),
            med_knee = median(knee,na.rm=TRUE),
            med_log10_tau = median(log10_tau,na.rm=TRUE)) %>%
  ungroup() %>%
  left_join(age) %>%
  left_join(dx) %>%
  left_join(updrs)

sel_parcels <- coverage %>%
  filter(n_s >= 8) %>%
  filter(HCPMMP1_label_1 != 'NA') %>%
  filter(!is.na(HCPMMP1_label_1)) %>%
  with(unique(HCPMMP1_label_1))

agg_aper_parcel %>%
  filter(HCPMMP1_label_1 %in%sel_parcels) %>%
  ggplot() +
  aes(x=dbs_age, y=med_log10_tau)+
  geom_smooth(method='lm') +
  geom_point() +
  facet_wrap(~HCPMMP1_label_1)
ggsave('figures_aper/55_aper_log10_tau_vs_age_by_parcel.pdf',width=10,height=8)



agg_aper_parcel %>%
  filter(HCPMMP1_label_1 %in%sel_parcels) %>%
  group_by(HCPMMP1_label_1) %>%
  nest() %>% 
  mutate(
    test = map(data, ~ cor.test(.x$dbs_age, .x$med_log10_tau)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied) %>%
  select(-data) %>%
  ungroup() %>%
  mutate(adjust_p_value=p.adjust(p.value,'bonferroni')) %>%
  select(HCPMMP1_label_1,estimate,p.value,adjust_p_value) %>%
  mutate(signif = adjust_p_value < 0.05) %>%
  View()
  
  
agg_aper_parcel %>%
  filter(HCPMMP1_label_1 %in%sel_parcels) %>%
  ggplot() +
  aes(x=on_score, y=med_log10_tau)+
  geom_smooth(method='lm') +
  geom_point() +
  facet_wrap(~HCPMMP1_label_1)

agg_aper_parcel %>%
  filter(HCPMMP1_label_1 %in%sel_parcels) %>%
  ggplot() +
  aes(x=on_score, y=med_exp)+
  geom_smooth(method='lm') +
  geom_point() +
  facet_wrap(~HCPMMP1_label_1)

agg_aper_parcel %>%
  filter(HCPMMP1_label_1 %in%sel_parcels) %>%
  group_by(HCPMMP1_label_1) %>%
  nest() %>% 
  mutate(
    test = map(data, ~ cor.test(.x$on_score, .x$med_exp)), # S3 list-col
    tidied = map(test, tidy)
  ) %>% 
  unnest(tidied) %>%
  select(-data) %>%
  ungroup() %>%
  mutate(adjust_p_value=p.adjust(p.value,'bonferroni')) %>%
  select(HCPMMP1_label_1,estimate,p.value,adjust_p_value) %>%
  mutate(signif = adjust_p_value < 0.05) %>%
  View()

ggsave('figures_aper/55_aper_log10_tau_vs_age_by_parcel.pdf',width=10,height=8)



cor.test(x=agg_aper_dbs$on_score, y=agg_aper_dbs$med_offset, method= "spearman")



