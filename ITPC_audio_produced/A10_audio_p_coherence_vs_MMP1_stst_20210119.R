library(tidyverse)
library(glue)
library(readr)
library(magrittr)
library(lmPerm)
library(broom)
library(ggforce)
library(scales)
library(lme4)
library(lmerTest)

theme_set(theme_bw())

PATH_ANALYSIS = "/Volumes/Nexus/Commits/Vibration_artifacts/audio_p-coherence_syllable_triplet"
#PATH_ANALYSIS = "Z:/Commits/Vibration_artifacts/2020-08-05-audio_p-coherence"
PATH_DB = paste0(PATH_ANALYSIS,"/data")
PATH_FIG = paste0(PATH_ANALYSIS,"/fig")

setwd(PATH_ANALYSIS)

electrode <- read_tsv('data/electrode_session.txt') %>%
  filter(type=='ecog') %>%
  select(subject,session_id,electrode,HCPMMP1_label_1,connector,mni_nonlinear_x,mni_nonlinear_y,mni_nonlinear_z) %>%
  arrange(subject,session_id,electrode) %>%
  mutate(electrode_idx=as.numeric(str_sub(electrode,7,8))) %>%
  mutate(electrode_y = ((electrode_idx-1) %% 21)+1, electrode_x = ((electrode_idx-1) %/% 21)+1) %>%
  group_by(subject, session_id, connector) %>%
  mutate(headstage_pin = 1:n()) 

coherence_db <- read_tsv('data/coherence_audio_p_by_electrode.tsv') %>%
  filter(electrode_type=='ecog') %>%
  left_join(electrode) %>%
  rename(MMP1=HCPMMP1_label_1) %>%
  mutate(log_rNC = log10(rNC)) 

#grouping over sessions
coherence_agg <- coherence_db %>%
  group_by(subject,electrode,MMP1,electrode_idx,electrode_x,electrode_y,headstage_pin) %>%
  summarise(mean_rNC = mean(rNC,na.rm=TRUE),sd_rNC=sd(rNC,na.rm=TRUE),
            mean_log_rNC = mean(log_rNC,na.rm=TRUE),sd_log_rNC=sd(log_rNC,na.rm=TRUE),
            mni_nonlinear_x=mean(mni_nonlinear_x,na.rm=TRUE),
            mni_nonlinear_y=mean(mni_nonlinear_y,na.rm=TRUE),
            mni_nonlinear_z=mean(mni_nonlinear_z,na.rm=TRUE))

write_tsv(coherence_agg,'data/coherence_audio_p_by_electrode_mean.tsv') 


coherence_agg %>%
  ggplot()+
  aes(x=mean_log_rNC)+
  geom_histogram() +
  facet_wrap(~subject)
ggsave('fig/A10_01_mean-log-rNC_vs_subject.png',width=14, height=10)

coherence_agg %>%
  ggplot()+
  aes(x=mean_rNC)+
  geom_histogram() +
  facet_wrap(~subject)
ggsave('fig/A10_02_mean-rNC_vs_subject.png',width=14, height=10)

coherence_agg %>%
  ggplot()+
  aes(x=electrode_y,y=electrode_x,color=mean_log_rNC)+
  geom_jitter(size=1,position=position_jitter(width=0,height=0.2)) +
  scale_color_gradient2(low=muted('blue'),mid='white',high=muted('red'),midpoint=0)+
  facet_wrap(~subject)
ggsave('fig/A10_03_ecog_strip_layout.png',width=14, height=6)


#Residuals after accounting for subject and area
coherence_res <- coherence_agg %>%
  group_by(subject,MMP1) %>%
  mutate(res = mean_log_rNC - mean(mean_log_rNC), n=n()) %>%
  ungroup() %>%
  filter(n>1) %>%
  filter(~is.na(res))

coherence_res %>%
  ggplot()+
  aes(x=res)+
  geom_histogram(aes(y=..density..),binwidth=0.02)+
  stat_function(fun = dnorm, args = list(mean = mean(coherence_res$res,na.rm=TRUE), sd = sd(coherence_res$res,na.rm=TRUE)),color='red')
ggsave('fig/A10_04_residuals-mixed-effect-model.png',width=4, height=4)

coherence_agg %>%
  group_by(subject,MMP1) %>%
  mutate(res = mean_log_rNC - mean(mean_log_rNC)) %>%
  ungroup() %>%
  ggplot()+
  aes(sample=res)+
  geom_qq()
ggsave('fig/A10_05_qq-mixed-effect-model.png',width=4, height=4)


lmer1_db <- coherence_agg %>%
  group_by(subject,MMP1) %>%
  mutate(n_subarea=n()) %>%
  filter(n_subarea>3) %>%
  group_by(MMP1) %>%
  filter(length(unique(subject))>=10)

lmer1 <- lmer(mean_log_rNC ~ 1 + (1|subject) + MMP1, lmer1_db)
summary(lmer1)
coherence_agg

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: mean_log_rNC ~ 1 + (1 | subject) + MMP1
#    Data: lmer1_db
# 
# REML criterion at convergence: 1374.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -5.5331 -0.5488  0.0844  0.6460  3.6626 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept) 0.05139  0.2267  
#  Residual             0.07563  0.2750  
# Number of obs: 4279, groups:  subject, 51
# 
# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  4.387e-01  3.362e-02  6.012e+01  13.049  < 2e-16 ***
# MMP143      -3.303e-03  2.881e-02  4.227e+03  -0.115  0.90873    
# MMP144      -2.844e-02  2.185e-02  4.234e+03  -1.302  0.19313    
# MMP155b     -6.817e-02  2.076e-02  4.223e+03  -3.283  0.00104 ** 
# MMP16r      -2.204e-02  3.061e-02  4.225e+03  -0.720  0.47149    
# MMP16v      -3.914e-02  1.582e-02  4.230e+03  -2.474  0.01340 *  
# MMP18Av     -9.660e-02  2.113e-02  4.230e+03  -4.572 4.98e-06 ***
# MMP18C      -9.633e-02  2.341e-02  4.232e+03  -4.115 3.94e-05 ***
# MMP1A4       1.010e-02  1.798e-02  4.238e+03   0.562  0.57429    
# MMP1A5      -3.107e-02  2.696e-02  4.234e+03  -1.153  0.24917    
# MMP1OP4      2.850e-02  2.137e-02  4.237e+03   1.334  0.18232    
# MMP1p9-46v  -8.065e-02  2.986e-02  4.229e+03  -2.701  0.00694 ** 
# MMP1PF       4.830e-02  2.103e-02  4.241e+03   2.297  0.02166 *  
# MMP1PFop     1.035e-02  1.887e-02  4.226e+03   0.549  0.58325    
# MMP1PSL     -9.227e-02  3.319e-02  4.230e+03  -2.780  0.00545 ** 
# MMP1STV     -7.000e-02  3.001e-02  4.235e+03  -2.332  0.01972 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lmer2_db <- coherence_agg %>%
  group_by(subject,MMP1) %>%
  mutate(n_subarea=n()) %>%
  filter(n_subarea>3) %>%
  summarise(mean_log_rNC=mean(mean_log_rNC,na.rm=TRUE)) %>%
  group_by(MMP1) %>%
  filter(length(unique(subject))>=10) %>%
  ungroup() 

lmer2 <- lmer(mean_log_rNC ~ 1 + (1|subject) + MMP1, lmer2_db)
summary(lmer2)




lmer3_db <- coherence_agg %>%
  mutate(electrode_idx = factor(electrode_idx))

lmer3 <- lmer(mean_log_rNC ~ 1 + (1|subject) + electrode_idx, lmer3_db)
summary(lmer3)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: mean_log_rNC ~ 1 + (1 | subject) + electrode_idx
#    Data: lmer3_db
# 
# REML criterion at convergence: 2018.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -5.5470 -0.5525  0.0804  0.6342  3.7395 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept) 0.05555  0.2357  
#  Residual             0.07714  0.2777  
# Number of obs: 5560, groups:  subject, 53
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      4.781e-01  4.253e-02  1.483e+02  11.243  < 2e-16 ***
# electrode_idx2  -2.420e-03  3.972e-02  5.445e+03  -0.061  0.95144    
# electrode_idx3  -7.495e-02  3.951e-02  5.445e+03  -1.897  0.05786 .  
# electrode_idx4  -5.168e-02  3.940e-02  5.445e+03  -1.312  0.18973    
# electrode_idx5  -2.131e-02  3.963e-02  5.445e+03  -0.538  0.59077    
# electrode_idx6  -7.380e-03  3.891e-02  5.445e+03  -0.190  0.84958    
# electrode_idx7   1.429e-02  3.996e-02  5.445e+03   0.358  0.72060    
# electrode_idx8  -1.262e-02  3.931e-02  5.445e+03  -0.321  0.74812    
# electrode_idx9  -3.455e-04  3.973e-02  5.445e+03  -0.009  0.99306    
# electrode_idx10 -6.963e-02  4.007e-02  5.445e+03  -1.738  0.08230 .  
# electrode_idx11 -6.931e-02  3.973e-02  5.445e+03  -1.744  0.08118 .  
# electrode_idx12 -9.243e-02  3.985e-02  5.445e+03  -2.320  0.02040 *  
# electrode_idx13 -5.599e-02  4.020e-02  5.445e+03  -1.393  0.16374    
# electrode_idx14 -6.287e-02  4.069e-02  5.445e+03  -1.545  0.12242    
# electrode_idx15 -2.444e-03  3.996e-02  5.445e+03  -0.061  0.95124    
# electrode_idx16 -5.640e-02  4.044e-02  5.445e+03  -1.395  0.16317    
# electrode_idx17 -4.658e-02  4.044e-02  5.445e+03  -1.152  0.24938    
# electrode_idx18 -7.524e-02  4.032e-02  5.445e+03  -1.866  0.06211 .  
# electrode_idx19 -1.038e-01  3.974e-02  5.445e+03  -2.612  0.00903 ** 
# electrode_idx20 -1.063e-01  4.045e-02  5.445e+03  -2.627  0.00864 ** 
# electrode_idx21 -5.032e-02  4.070e-02  5.445e+03  -1.236  0.21638    
# electrode_idx22 -5.410e-02  3.963e-02  5.445e+03  -1.365  0.17230    
# electrode_idx23 -9.654e-02  3.996e-02  5.445e+03  -2.416  0.01573 *  
# electrode_idx24 -5.981e-02  4.008e-02  5.445e+03  -1.492  0.13564    
# electrode_idx25 -7.939e-02  3.974e-02  5.445e+03  -1.998  0.04578 *  
# electrode_idx26 -2.982e-02  4.033e-02  5.445e+03  -0.739  0.45966    
# electrode_idx27  2.353e-02  4.009e-02  5.445e+03   0.587  0.55731    
# electrode_idx28  1.457e-02  3.963e-02  5.445e+03   0.368  0.71319    
# electrode_idx29  5.705e-03  3.963e-02  5.445e+03   0.144  0.88554    
# electrode_idx30 -1.046e-02  3.996e-02  5.445e+03  -0.262  0.79360    
# electrode_idx31 -1.020e-02  4.020e-02  5.445e+03  -0.254  0.79974    
# electrode_idx32 -4.567e-03  4.044e-02  5.445e+03  -0.113  0.91008    
# electrode_idx33 -3.086e-02  4.096e-02  5.445e+03  -0.753  0.45132    
# electrode_idx34 -5.581e-02  4.139e-02  5.445e+03  -1.348  0.17758    
# electrode_idx35 -9.120e-02  4.084e-02  5.445e+03  -2.233  0.02559 *  
# electrode_idx36 -1.068e-01  4.058e-02  5.445e+03  -2.631  0.00854 ** 
# electrode_idx38 -1.015e-01  4.057e-02  5.445e+03  -2.501  0.01240 *  
# electrode_idx39 -1.309e-01  4.137e-02  5.445e+03  -3.165  0.00156 ** 
# electrode_idx40 -9.402e-02  4.020e-02  5.445e+03  -2.339  0.01937 *  
# electrode_idx41 -8.072e-02  4.070e-02  5.445e+03  -1.984  0.04736 *  
# electrode_idx42 -7.669e-02  4.084e-02  5.445e+03  -1.878  0.06048 .  
# electrode_idx43 -8.110e-02  4.056e-02  5.445e+03  -2.000  0.04560 *  
# electrode_idx44 -4.417e-02  4.021e-02  5.445e+03  -1.099  0.27200    
# electrode_idx45 -7.687e-02  4.057e-02  5.445e+03  -1.895  0.05816 .  
# electrode_idx46 -2.775e-02  4.046e-02  5.445e+03  -0.686  0.49272    
# electrode_idx47 -6.728e-02  3.985e-02  5.445e+03  -1.688  0.09146 .  
# electrode_idx48 -2.903e-02  4.069e-02  5.445e+03  -0.713  0.47564    
# electrode_idx49 -1.910e-02  3.998e-02  5.445e+03  -0.478  0.63288    
# electrode_idx50  2.710e-02  4.060e-02  5.445e+03   0.668  0.50439    
# electrode_idx51 -1.833e-02  4.070e-02  5.445e+03  -0.450  0.65245    
# electrode_idx52 -7.312e-03  4.112e-02  5.445e+03  -0.178  0.85888    
# electrode_idx37 -7.039e-02  4.021e-02  5.445e+03  -1.751  0.08009 .  
# electrode_idx53 -2.093e-02  4.124e-02  5.445e+03  -0.507  0.61183    
# electrode_idx54 -4.104e-02  3.998e-02  5.445e+03  -1.026  0.30472    
# electrode_idx55 -8.243e-02  4.171e-02  5.446e+03  -1.976  0.04816 *  
# electrode_idx56 -3.383e-02  4.171e-02  5.446e+03  -0.811  0.41731    
# electrode_idx57 -9.712e-02  4.186e-02  5.446e+03  -2.320  0.02038 *  
# electrode_idx58 -3.478e-02  4.217e-02  5.446e+03  -0.825  0.40955    
# electrode_idx59 -6.927e-02  4.320e-02  5.446e+03  -1.603  0.10891    
# electrode_idx60 -4.890e-02  4.251e-02  5.446e+03  -1.150  0.24999    
# electrode_idx61 -1.042e-01  4.201e-02  5.446e+03  -2.481  0.01314 *  
# electrode_idx62 -4.085e-02  4.171e-02  5.446e+03  -0.980  0.32734    
# electrode_idx63 -7.097e-02  4.185e-02  5.445e+03  -1.696  0.09001 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lmer4_db <- coherence_agg %>%
  mutate(electrode_x = factor(electrode_x))

lmer4 <- lmer(mean_log_rNC ~ 1 + (1|subject) + electrode_x, lmer4_db)
summary(lmer4)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: mean_log_rNC ~ 1 + (1 | subject) + electrode_x
#    Data: lmer4_db
# 
# REML criterion at convergence: 1816.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -5.4644 -0.5578  0.0771  0.6452  3.6232 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept) 0.05534  0.2352  
#  Residual             0.07772  0.2788  
# Number of obs: 5560, groups:  subject, 53
# 
# Fixed effects:
#                Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   4.335e-01  3.297e-02  5.426e+01  13.148   <2e-16 ***
# electrode_x2 -8.790e-03  9.045e-03  5.506e+03  -0.972    0.331    
# electrode_x3 -1.556e-03  9.273e-03  5.509e+03  -0.168    0.867    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) elct_2
# electrod_x2 -0.134       
# electrod_x3 -0.131  0.481


lmer5_db <- coherence_agg %>%
  mutate(electrode_y = factor(electrode_y))

lmer5 <- lmer(mean_log_rNC ~ 1 + (1|subject) + electrode_y, lmer5_db)
summary(lmer5)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: mean_log_rNC ~ 1 + (1 | subject) + electrode_y
#    Data: lmer5_db
# 
# REML criterion at convergence: 1866.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -5.6350 -0.5504  0.0852  0.6471  3.7224 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept) 0.05547  0.2355  
#  Residual             0.07716  0.2778  
# Number of obs: 5560, groups:  subject, 53
# 
# Fixed effects:
#                 Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)    4.352e-01  3.636e-02  7.979e+01  11.971   <2e-16 ***
# electrode_y2  -4.465e-03  2.349e-02  5.487e+03  -0.190   0.8492    
# electrode_y3  -2.759e-02  2.353e-02  5.487e+03  -1.173   0.2410    
# electrode_y4  -1.052e-02  2.342e-02  5.487e+03  -0.449   0.6535    
# electrode_y5   3.461e-03  2.347e-02  5.487e+03   0.147   0.8828    
# electrode_y6   3.899e-02  2.343e-02  5.487e+03   1.664   0.0961 .  
# electrode_y7   4.632e-02  2.343e-02  5.487e+03   1.977   0.0480 *  
# electrode_y8   4.883e-02  2.341e-02  5.487e+03   2.086   0.0370 *  
# electrode_y9   3.346e-02  2.358e-02  5.487e+03   1.419   0.1559    
# electrode_y10  1.311e-02  2.376e-02  5.487e+03   0.552   0.5813    
# electrode_y11  1.020e-02  2.376e-02  5.487e+03   0.429   0.6677    
# electrode_y12 -1.267e-02  2.365e-02  5.487e+03  -0.536   0.5922    
# electrode_y13 -2.135e-02  2.412e-02  5.487e+03  -0.885   0.3762    
# electrode_y14 -2.040e-02  2.412e-02  5.487e+03  -0.846   0.3976    
# electrode_y15 -2.358e-02  2.394e-02  5.487e+03  -0.985   0.3248    
# electrode_y16 -1.196e-02  2.401e-02  5.487e+03  -0.498   0.6184    
# electrode_y17 -2.963e-02  2.424e-02  5.487e+03  -1.222   0.2217    
# electrode_y18 -4.285e-02  2.428e-02  5.487e+03  -1.765   0.0776 .  
# electrode_y19 -5.762e-02  2.384e-02  5.487e+03  -2.417   0.0157 *  
# electrode_y20 -3.423e-02  2.404e-02  5.487e+03  -1.424   0.1545    
# electrode_y21 -2.286e-02  2.415e-02  5.487e+03  -0.947   0.3438    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lmer6_db <- coherence_agg %>%
  mutate(headstage_pin = factor(headstage_pin))

lmer6 <- lmer(mean_log_rNC ~ 1 + (1|subject) + headstage_pin, lmer6_db)
summary(lmer6)


coherence_agg %>%
  filter(headstage_pin > 16) %>%

