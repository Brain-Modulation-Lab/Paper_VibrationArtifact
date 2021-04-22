library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(lmPerm)
library(broom)
library(ggforce)
library(scales)


theme_set(theme_bw())

#PATH_ANALYSIS = "/Volumes/Nexus/Commits/Vibration_artifacts/audio_p-coherence_syllable_triplet"
PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/audio_p-coherence_syllable_triplet"
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
  left_join(select(read_tsv('data/electrode_session.txt'),subject,session_id,electrode,connector,HCPMMP1_label_1)) %>%
  left_join(select(read_tsv('data/electrodes_renamed.tsv'),electrode,electrode2)) %>%
  mutate(electrode=electrode2)

coherence_db_norm_db <- coherence_db %>%
  mutate(norm_coherence = coherence / (norm1 * norm2))

se <- function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))

coherence_agg <- coherence_db_norm_db %>%
  filter(electrode_type %in% c('ecog','dbs','macro','micro','v0')) %>%
  group_by(subject,session_id,type,electrode_type,electrode,connector,HCPMMP1_label_1) %>%
  summarise(mNC = mean(norm_coherence,na.rm=TRUE), 
            mRNC = mean(Re(norm_coherence),na.rm=TRUE), 
            mINC = mean(Im(norm_coherence),na.rm=TRUE),
            seRNC = se(Re(norm_coherence)), 
            seINC = se(Im(norm_coherence)),
            seNC = sqrt(seRNC^2 + seINC^2)) %>%
  mutate(rNC = sqrt(mRNC^2 + mINC^2)/seNC)

#### Calculating percent of significant channels per type and epoch

get_percent_signif<-function(db){
  db %>%
  group_by(type,electrode_type) %>%
  summarise(percent_coherent = 100 * sum(rNC>3.08,na.rm=TRUE) / n()) %>%
  arrange(type,electrode_type) 
}


get_hb_samp <- function(db){
  us<-unique(db$subject) 
  sample(us,length(us),replace=TRUE) %>%
    map_dfr(function(s){
      db %>% filter(subject==s) %>% sample_frac(replace=TRUE)  
    })
}


coherence_agg_f <- coherence_agg %>%
  filter(str_sub(subject,4,4)=='3') %>%
  filter(!subject%in%c('DBS3001','DBS3002')) %>%
  filter(!is.na(rNC)) %>%
  filter(!(subject=='DBS3006' & electrode_type=='dbs' & session_id<4)) %>%
  filter(!(subject=='DBS3014' & electrode_type=='dbs' & session_id<4))  %>%
  filter(!(electrode_type=='dbs' & str_sub(electrode,5,5)=='R')) 

percent_signif_hboot <-
1:1000 %>%
  map_dfr(function(x){
    coherence_agg_f %>%
    get_hb_samp() %>%
    get_percent_signif()
  })


percent_signif_hboot %>%
  ggplot() +
  aes(x=percent_coherent) +
  geom_histogram(binwidth=1) +
  facet_grid(electrode_type~type,scale='free')


percent_signif_hboot_ci <- percent_signif_hboot %>%
  group_by(electrode_type,type) %>%
  summarise(percent_mean = median(percent_coherent),
            percent_ci95=quantile(percent_coherent,0.95),
            percent_ci05=quantile(percent_coherent,0.05),
            se=sd(percent_coherent))

percent_signif <- 
  coherence_agg_f %>%
  get_percent_signif() %>%
  left_join(percent_signif_hboot_ci) 

percent_signif %>%
  write_tsv("data/A11_percent_signif.tsv")

#doing hierarchical bootstrapping 


#### plotting


coherence_agg %>% filter(subject=="DBS3014" & session_id==4 & electrode=="ecog_109" & type=="prod")

coherence_agg %>% filter(subject=="DBS3006" & session_id==4 & type=="prod" & electrode_type=="dbs") %>% View()

# plotting complex values dispersion

coherence_db_norm_db %>%
  filter(subject=="DBS3011" & session_id==4 & electrode=="ecog_109") %>%
  ggplot() +
  aes(x=Re(norm_coherence),y=Im(norm_coherence),color=type,shape=type)+
  geom_point(size=.4,alpha=0.8) +
  geom_vline(xintercept=0,color='grey',size=0.2) +
  geom_hline(yintercept=0,color='grey',size=0.2) +
  geom_segment(aes(xend=mRNC,yend=mINC),x=0,y=0,size=0.8,arrow=arrow(length = unit(0.08, "inches")),
               data=coherence_agg %>% filter(subject=="DBS3011" & session_id==4 & electrode=="ecog_109")) +
  scale_color_manual(values=c('#00BA38','#F8766D','#619CFF')) +
  scale_x_continuous(breaks=c(-0.5,0,0.5))+
  scale_y_continuous(breaks=c(-0.5,0,0.5))+
  coord_fixed(ratio = 1, xlim=c(-0.5,0.5), ylim=c(-0.5,0.5))+
  theme(panel.grid=element_blank())+
  labs(x='Real',y='Imag')
ggsave('fig/A11_coherence_complex_DBS3011S4_ecog_109.pdf',width=3,height=2)


coherence_db_norm_db %>%
  filter(subject=="DBS3011" & session_id==4 & electrode=="ecog_109") %>%
  filter(Re(norm_coherence) < -0.3 & Im(norm_coherence) < -0.3) %>%
  View()

#subject=='DBS3011',session_id==4,trial_id==68,type==prod,syl_id==1


coherence_agg %>%
  mutate(electrode_type2 = ifelse(electrode_type=='ecog',str_sub(electrode,1,6),electrode_type)) %>%
  filter(electrode_type2!='ecog_3') %>%
  mutate(electrode_type2 = factor(electrode_type2,levels=c('ecog_2','ecog_1','dbs','macro','micro','v0'))) %>%
  mutate(subject2 = str_sub(subject,6,7)) %>%
  filter(type=='prod')%>%
  filter(str_sub(subject,4,4)=='3') %>%
  filter(!((subject=='DBS3018') & (session_id==2))) %>% #10ms sync mismatch ruins ITPC
  filter(!(electrode_type=='dbs' & str_sub(electrode,5,5)=='R')) %>%
  group_by(subject,electrode_type2) %>%
  mutate(electrode2=as.numeric(factor(electrode))) %>%
  ungroup() %>%
  #filter((electrode_type!='dbs') | (electrode %in% c('dbs_L1','dbs_L2A','dbs_L2B','dbs_L2C','dbs_L3A','dbs_L3B','dbs_L3C','dbs_L4'))) %>%
  filter((electrode_type!='v0') | (electrode %in% c('V0_con4a','V0_con8a'))) %>%
  filter(!as.numeric(str_sub(subject,6,7))%in%c(26,29)) %>%
  ggplot() +
  aes(x=factor(session_id),y=factor(electrode2),fill=log10(rNC))+
  geom_tile()+
  scale_fill_gradient(low='white',high=muted('red'),
                      limits=c(log10(3),2),oob=scales::squish,na.value = "gray90",
                      breaks=c(0.5,1,1.5,2),labels=c(3,10,30,100),name='') +
  facet_grid(electrode_type2~subject2,scales='free',space='free_y') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
ggsave('fig/A11_coherence_raster_3000_series.pdf',width=5.6*1.7,height=4.4*1.7)



iepc_db <- coherence_agg %>%
  mutate(electrode_type2 = ifelse(electrode_type=='ecog',str_sub(electrode,1,6),electrode_type)) %>%
  filter(electrode_type2%in%c('ecog_1','ecog_2')) %>%
  mutate(subject2 = str_sub(subject,6,7)) %>%
  filter(type=='prod') %>%
  filter(str_sub(subject,4,4)=='3') %>%
  group_by(subject,electrode_type2) %>%
  mutate(electrode2=as.numeric(factor(electrode))) %>%
  ungroup() %>%
  filter(!as.numeric(str_sub(subject,6,7))%in%c(26,29)) 

iepc_db %>%
  ggplot() +
  aes(xend=Re(mNC)/abs(mNC),yend=Im(mNC)/abs(mNC))+
  geom_segment(x=0,y=0,arrow = arrow(length = unit(0, "cm")),alpha=0.1) +
  geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,xend=NULL,yend=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=1),color='grey',size=0.4)+
  facet_grid(electrode_type2~subject2, space='free') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank()) +
  coord_equal()
ggsave('fig/A11_coherence_raster_3000_series_phase cluster_accros_ecog1&2_contacts_w2.pdf',width=9,height=2)


iepc_db %>%
  filter(rNC > 3.08) %>%
  ggplot() +
  aes(xend=Re(mNC)/abs(mNC),yend=Im(mNC)/abs(mNC))+
  geom_segment(x=0,y=0,arrow = arrow(length = unit(0, "cm")),alpha=0.1) +
  geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,xend=NULL,yend=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=1),color='grey',size=0.4)+
  facet_grid(electrode_type2~subject2, space='free') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank()) +
  coord_equal()
ggsave('fig/A11_coherence_raster_3000_series_phase cluster_accros_ecog1&2_contacts_w2_only-signif.pdf',width=9,height=2)



iepc_db %>%
  filter(rNC > 3.08) %>%
  ggplot() +
  aes(xend=Re(mNC)/abs(mNC),yend=Im(mNC)/abs(mNC))+
  geom_segment(x=0,y=0,arrow = arrow(length = unit(0, "cm")),alpha=0.1) +
  geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,xend=NULL,yend=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=1),color='grey',size=0.4)+
  facet_grid(electrode_type2+connector~subject2, space='free') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank()) +
  coord_equal()
ggsave('fig/A11_coherence_raster_3000_series_phase cluster_accros_ecog_connector_contacts_w2_only-signif.pdf',width=9,height=4)


iepc_db %>%
  filter(rNC > 3.08) %>%
  ggplot() +
  aes(xend=Re(mNC)/abs(mNC),yend=Im(mNC)/abs(mNC))+
  geom_segment(x=0,y=0,arrow = arrow(length = unit(0, "cm")),alpha=0.1) +
  geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,xend=NULL,yend=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=1),color='grey',size=0.4)+
  facet_grid(electrode_type2+session_id~subject2, space='free') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank()) +
  coord_equal()
ggsave('fig/A11_coherence_raster_3000_series_phase cluster_accros_ecog_session-id_contacts_w2_only-signif.pdf',width=9,height=4)


# doing stats for clustering across electrodes
iepc_db %>%
  filter(rNC > 3.08) %>%
  ggplot() +
  aes(xend=Re(mNC)/abs(mNC),yend=Im(mNC)/abs(mNC))+
  geom_segment(x=0,y=0,arrow = arrow(length = unit(0, "cm")),alpha=0.1) +
  geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,xend=NULL,yend=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=1),color='grey',size=0.4)+
  facet_grid(electrode_type2~subject2, space='free') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank()) +
  coord_equal()

#want to make metric based on number of electrodes within 90 degrees of mean across electrodes

frac_aligned <- function(phi, angle = pi/4){
  #Calculate fraction of complex values aligned with the mean of the set
  phi <- phi[!is.na(phi)]
  m <- mean(phi/abs(phi))
  cosangle <- Re(phi * Conj(m)) / (abs(phi) * abs(m))
  return(sum(cosangle > cos(angle))/length(phi))
}

frac_aligned_pval <- function(phi, angle = pi/4, n_rand=1e4){
  #Calcualte pvalue of fraction aligned
  phi <- phi[!is.na(phi)]
  n <- length(phi)
  h0 <- replicate(n_rand,frac_aligned(phi=exp(1i * runif(n, min = 0, max = 2*pi)),angle=angle))
  return(sum(h0 >= frac_aligned(phi,angle=angle))/n_rand)
}




iepc_agg <- iepc_db %>%
  filter(rNC > 3.08) %>%
  group_by(subject2,electrode_type2,session_id) %>%
  summarise(pval = frac_aligned_pval(mNC),f = frac_aligned(mNC))

iepc_agg_signif_homo <- iepc_agg %>%
  ungroup() %>%
  mutate(pval_fdr = p.adjust(pval,'fdr')) %>%
  group_by(subject2,electrode_type2) %>%
  summarise(all_signif = all(pval_fdr<0.05),all_homo = all(f > 0.9)) %>%
  mutate(homo_label = ifelse(all_signif & all_homo,'H',''))


iepc_agg2 <- iepc_db %>%
  filter(rNC > 3.08) %>%
  group_by(subject2,electrode_type2) %>%
  summarise(pval = frac_aligned_pval(mNC),f = frac_aligned(mNC)) %>%
  ungroup() %>%
  mutate(pval_fdr = p.adjust(pval,'fdr')) %>%
  mutate(homo_label = ifelse(pval_fdr<0.05 & f > 0.9,'*',''))


    
iepc_db %>%
  filter(rNC > 3.08) %>%
  ggplot() +
  aes(xend=Re(mNC)/abs(mNC),yend=Im(mNC)/abs(mNC))+
  geom_segment(x=0,y=0,arrow = arrow(length = unit(0, "cm")),alpha=0.1) +
  geom_circle(aes(x0=x0,y0=y0,r=r,x=NULL,y=NULL,xend=NULL,yend=NULL,color=NULL,shape=NULL),data=tibble(x0=0,y0=0,r=1),color='grey',size=0.4)+
  facet_grid(electrode_type2~subject2, space='free') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank()) +
  coord_equal() +
  geom_text(aes(xend=NULL,yend=NULL,label=homo_label),vjust=1,hjust=0,x=-1,y=1,
            data=iepc_agg2,size=3)+
  facet_grid(electrode_type2~subject2, space='free') 
ggsave('fig/A11_coherence_raster_3000_series_phase_cluster_accros_ecog_connector_contacts_w2_only-signif_iepc_homogenous.pdf',width=9,height=4)








iepc_agg%>%
  ggplot()+
  aes(x=Re(mean_mNC),y=Im(mean_mNC)) +
  

coherence_agg %>%
  mutate(electrode_type2 = ifelse(electrode_type=='ecog',str_sub(electrode,1,6),electrode_type)) %>%
  filter(electrode_type2!='ecog_3') %>%
  mutate(electrode_type2 = factor(electrode_type2,levels=c('ecog_2','ecog_1','dbs','macro','micro','v0'))) %>%
  mutate(subject2 = str_sub(subject,6,7)) %>%
  filter(type=='prod')%>%
  filter(str_sub(subject,4,4)=='4') %>%
  group_by(subject,electrode_type2) %>%
  mutate(electrode2=as.numeric(factor(electrode))) %>%
  ungroup() %>%
  filter(!as.numeric(str_sub(subject,6,7))%in%c(61,68,79,85,88)) %>%
  #filter((electrode_type!='dbs') | (electrode %in% c('dbs_L1','dbs_L2A','dbs_L2B','dbs_L2C','dbs_L3A','dbs_L3B','dbs_L3C','dbs_L4'))) %>%
  filter((electrode_type!='v0') | (electrode %in% c('V0_con4a','V0_con8a'))) %>%
  ggplot() +
  aes(x=mRNC/seNC,y=mINC/seNC)+
  geom_segment()+
  scale_fill_gradient(low='white',high=muted('red'),
                      limits=c(log10(3),2),oob=scales::squish,na.value = "gray90",
                      breaks=c(0.5,1,1.5,2),labels=c(3,10,30,100),name='') +
  facet_grid(electrode_type2~subject2,scales='free',space='free') +
  theme(panel.grid=element_blank(),panel.background=element_rect(fill='gray90'),panel.spacing = unit(0, "lines"),
        axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
ggsave('fig/A11_coherence_raster_4000_series.pdf',width=5.6*1.7,height=4.4*1.7)


