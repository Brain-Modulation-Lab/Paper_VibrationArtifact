function P10b_time_frequency_arC_lock_speech_fft_20210103(SUBJECT)

% function time_freq_then_extract_power_per_freq_per_trial(SUBJECT)
% Protocol ran in batch by batch_time_freq_then_extract_power_per_freq_per_trial
%
% This protocol performs line noise filtering, rereferencing, defines trials and 
% performs time frequency analysis 
% then extracts power per each trial per each freq point for further
% analysis

bml_defaults

%script configuration parameters
CRITERIA = 'C'; %Artifact Rejection criteria
TF_RATE = 20; %Hz Sampling rate of time frequency plot
%TF_FOI = round(10.^(0.30:0.05:2.4),2,'signif');
TF_FOI = 2:2:250;
TF_BASELINE_WIDTH = 0.5;

%defining paths
%SUBJECT='DBS3027'; %used for development/debugging
PATH_DATA = 'Z:\DBS';
%PATH_DATA = 'Z:\Commits\DBS_speech\Dev\[2020.10.12]U01-Vibration-Artifact-Selected-Dataset';
PATH_SUBJECT = [PATH_DATA filesep SUBJECT];
PATH_SYNC = [PATH_SUBJECT filesep 'Preprocessed Data' filesep 'sync'];
cd(PATH_SYNC)

PATH_ANALYSIS = 'Z:\Commits\Vibration_artifacts\fft_spectrograms';
PATH_TF_DATA = [PATH_ANALYSIS '\data'];
PATH_TF_FIG = [PATH_ANALYSIS '\figures'];
TF_SAVE_FULLNAME = [PATH_TF_DATA filesep SUBJECT '_TF_depth_ar' CRITERIA '.mat'];
DATE=datestr(now,'yyyymmdd');

fprintf('Time Frequency Analysis for subject %s \n',SUBJECT)
  
%loading annotation tables
session = bml_annot_read(['annot' filesep SUBJECT '_session.txt']);
electrode = bml_annot_read(['annot' filesep SUBJECT '_electrode.txt']);
artifact = bml_annot_read(['annot' filesep SUBJECT '_artifact_criteria_' CRITERIA '.txt']);
empty_electrode = bml_annot_read(['annot' filesep SUBJECT '_empty_electrode.txt']);
produced_triplet = bml_annot_read(['annot' filesep SUBJECT '_produced_triplet.txt']);

% initializing outcome table
clearvars outcome_table
outcome_table = table();


%% defining epochs and baseline for time frequency analysis
produced_triplet.prod_onset = produced_triplet.starts;
produced_triplet.prod_offset = produced_triplet.ends;
if isfile([SUBJECT '_stimulus_triplet.txt'])
  stimulus_triplet = bml_annot_read([SUBJECT '_stimulus_triplet.txt']);
  stimulus_triplet.stim_onset = stimulus_triplet.starts;
  stimulus_triplet.stim_offset = stimulus_triplet.ends;
  triplet = bml_annot_left_join(produced_triplet,stimulus_triplet);
else
  triplet = produced_triplet;
  triplet.stim_onset(:) = nan;
  triplet.stim_offset(:) = nan;
end

if isfile([SUBJECT '_inter_trial_interval.txt'])
  ITI = bml_annot_read([SUBJECT '_inter_trial_interval.txt']);

  ITI_post = ITI(:,{'session_id'});
  ITI_post.trial_id = ITI.trial_id_pre;
  ITI_post.ITI_post_starts = ITI.starts;
  ITI_post.ITI_post_ends = ITI.ends;
  triplet = bml_annot_left_join(triplet,ITI_post);

  ITI_pre = ITI(:,{'session_id'});
  ITI_pre.trial_id = ITI.trial_id_post;
  ITI_pre.ITI_pre_starts = ITI.starts;
  ITI_pre.ITI_pre_ends = ITI.ends;
  triplet = bml_annot_left_join(triplet,ITI_pre);
else
  triplet.ITI_post_starts(:) = nan;
  triplet.ITI_post_ends(:) = nan;  
	triplet.ITI_pre_starts(:) = nan;
  triplet.ITI_pre_ends(:) = nan; 
end

% time-locking to speech onset
triplet.t0 = triplet.prod_onset;
triplet.prod_onset_t0 = triplet.prod_onset - triplet.t0 ;
triplet.prod_offset_t0 = triplet.prod_offset - triplet.t0 ;
triplet.stim_onset_t0 = triplet.stim_onset - triplet.t0 ;
triplet.stim_offset_t0 = triplet.stim_offset - triplet.t0 ;
triplet.ITI_post_starts_t0 = triplet.ITI_post_starts - triplet.t0 ;
triplet.ITI_post_ends_t0 = triplet.ITI_post_ends - triplet.t0 ;
triplet.ITI_pre_starts_t0 = triplet.ITI_pre_starts - triplet.t0 ;
triplet.ITI_pre_ends_t0 = triplet.ITI_pre_ends - triplet.t0 ;

% calculating median and robust std of events
event_t0 = bml_annot_describe(triplet(:,endsWith(triplet.Properties.VariableNames,'_t0')));
event_t0_median = unstack(event_t0(:,{'variable','median'}),'median','variable');
event_t0_rstd = unstack(event_t0(:,{'variable','rstd'}),'rstd','variable');

% defining epochs and baseline
if ~isnan(event_t0_median.ITI_pre_starts_t0)
  tf_epoch_starts_t0    = event_t0_median.ITI_pre_starts_t0 - 1.5 .* event_t0_rstd.ITI_pre_starts_t0;
  tf_epoch_ends_t0      = event_t0_median.ITI_post_ends_t0 + 1.5 .* event_t0_rstd.ITI_post_ends_t0;
  tf_baseline_midpoint_t0 = ((event_t0_median.prod_offset_t0 + 1 .* event_t0_rstd.prod_offset_t0) + ...
                             (event_t0_median.ITI_post_ends_t0 - 0 .* event_t0_rstd.ITI_post_ends_t0))./2;
  tf_baseline_speech_t0 = [tf_baseline_midpoint_t0 - TF_BASELINE_WIDTH/2,tf_baseline_midpoint_t0 + TF_BASELINE_WIDTH/2];
else
  tf_epoch_starts_t0 = -3;
  tf_epoch_ends_t0  =3;
  tf_baseline_midpoint_t0 = 2.5;
  tf_baseline_speech_t0 = [2.25 2.75];
end

% snapping epochs and baseline to tf_rate
tf_epoch_starts_t0 = round(tf_epoch_starts_t0 .* TF_RATE) ./ TF_RATE;
tf_epoch_ends_t0   = round(tf_epoch_ends_t0 .* TF_RATE) ./ TF_RATE;
tf_baseline_speech_t0  = round(tf_baseline_speech_t0 .* TF_RATE) ./ TF_RATE;

% adding buffer zone for time frequency analysis
tf_edge_buffer = round(3.5/min(TF_FOI) .* TF_RATE) ./ TF_RATE;
     
% epoching to onset of first vowel
epoch = triplet(:,{'id','session_id','trial_id','t0', 'prod_onset_t0', 'prod_onset', 'prod_offset', 'stim_onset', 'stim_offset', 'prod_offset_t0', 'stim_onset_t0', 'stim_offset_t0'}); 
epoch.starts = epoch.t0 + tf_epoch_starts_t0 - tf_edge_buffer;
epoch.ends = epoch.t0 + tf_epoch_ends_t0 + tf_edge_buffer;
epoch = bml_annot_table(epoch); %creating id and duration columns
epoch = epoch(~ismissing(epoch.starts),:); %removing rows with NaNs

% defining event annotation table
col_prod_starts = "#E41A1C";
col_prod_ends = "#377EB8";
col_stim_starts = "#FF7F00";
col_stim_ends = "#984EA3";
col_baseline = "#A65628";
event_annot = bml_annot_table(cell2table([...
  {event_t0_median.ITI_pre_starts_t0,event_t0_median.ITI_pre_ends_t0,'ITI_{pre}',event_t0_rstd.ITI_pre_starts_t0,event_t0_rstd.ITI_pre_ends_t0,'-',col_prod_ends,NaN};...
  {event_t0_median.stim_onset_t0,event_t0_median.stim_offset_t0,'stim',event_t0_rstd.stim_onset_t0,event_t0_rstd.stim_offset_t0,'-',col_stim_starts,col_stim_ends};...
  {event_t0_median.stim_offset_t0,event_t0_median.prod_onset_t0,'RL',event_t0_rstd.stim_offset_t0,event_t0_rstd.prod_onset_t0,'-',NaN,NaN};...
  {event_t0_median.prod_onset_t0,event_t0_median.prod_offset_t0,'prod',event_t0_rstd.prod_onset_t0,event_t0_rstd.prod_offset_t0,'-',col_prod_starts,col_prod_ends};...
  {event_t0_median.ITI_post_starts_t0,event_t0_median.ITI_post_ends_t0,'ITI_{post}',event_t0_rstd.ITI_post_starts_t0,event_t0_rstd.ITI_post_ends_t0,'-',NaN,col_stim_starts};...
  {tf_baseline_speech_t0(1),tf_baseline_speech_t0(2),'baseline',NaN,NaN,':',col_baseline,col_baseline}...
  ],'VariableNames',{'starts','ends','name','starts_rstd','ends_rstd','linestyle','starts_color','ends_color'}));   
event_annot.symbol = event_annot.name;
event_annot.symbol{strcmp(event_annot.name,'baseline')}=NaN;
event_annot.starts_error = sqrt(event_annot.starts_rstd.^2 + 0.01.^2);
event_annot.ends_error = sqrt(event_annot.ends_rstd.^2 + 0.01.^2);

  
%% loading continuous preprocessed data 
load([PATH_SUBJECT filesep 'Preprocessed Data' filesep 'FieldTrip' filesep SUBJECT '_ft_raw_session.mat'],'D');
  
  
%% removing channels with audio envelope
cfg=[];
cfg.channel = {'all' '-envaudio_p' '-envaudio_s'}; 
D0 = ft_preprocessing(cfg, D);

%clearvars D

%% applying notch filtering
cfg=[];
cfg.channel = {'macro_*','micro_*','dbs_*','*audio_*','ecog_*','V0_*'};
D1 = ft_selectdata(cfg, D0);
%clearvars D0

cfg=[];
cfg.remask_nan = true;
cfg.value = 0;
D1 = bml_mask(cfg, D1);

%notch filtering
freqs=[60 120 180 240];
cfg=[];
cfg.hpfilter='yes';
cfg.hpfreq=1;
cfg.hpfilttype='but';
cfg.hpfiltord=5;
cfg.hpfiltdir='twopass';
cfg.bsfilter='yes';
cfg.bsfreq= [freqs-1; freqs+1]';
D_filt = ft_preprocessing(cfg,D1);

% clearing temporary objects
%clearvars D_sel D_sel_filt D_unfilt D1

%% masking artifacts
%combining artifacts with empty_electrode table
cfg=[];
cfg.groupby='label';
artifact_empty = bml_annot_union(cfg, artifact, empty_electrode);

%masking artifacts and empty_electrodes with NaNs
cfg=[];
cfg.annot=artifact_empty;
cfg.label_colname = 'label';
cfg.complete_trial = false; %masks entire trials
cfg.value=NaN;
D_filt_mask = bml_mask(cfg,D_filt);

%clearvars D_filt

%% redefining speech trials
epoch_orig = epoch;
epoch = bml_annot_intersect('x',epoch,bml_raw2annot(D_filt_mask));
epoch = epoch(epoch.duration > 4,:); 
epoch = epoch(epoch.duration > median(epoch.duration)-2,:);

cfg=[];
cfg.epoch = epoch;
cfg.timelock = 't0';
cfg.timesnap = true;
[D_filt_mask_trial, epoch] = bml_redefinetrial(cfg,D_filt_mask);

%clearvars D_filt_mask

%% rereferencing

% % ecog Common Average Reference
% el_ecog = electrode(electrode.type=="ecog",:);
% cfg=[];
% cfg.label = el_ecog.electrode;
% cfg.group = el_ecog.connector;
% cfg.method = 'CTAR'; %using trimmed average referencing
% cfg.percent = 50; %percentage of 'extreme' channels in group to trim 
% D_filt_trial_mask_ref = bml_rereference(cfg,D_filt_trial_mask);

D_trial_ref = D_filt_mask_trial;
%   
%   % macro bipolar reference to central
%   el_macro = electrode(electrode.type=="macro",:);
%   if ~isempty(el_macro)
%     cfg=[];
%     cfg.method = 'bipolar'; 
%     cfg.label = unique(el_macro.electrode);
%     cfg.refchan = {'macro_c'}; 
%     cfg.refkeep = false; 
%     D_trial_ref = bml_rereference(cfg,D_trial_ref);
%   end
%   
%   % dbs lead bipolar reference
%   el_dbs = electrode(electrode.type=="dbs",:);
%   if ~isempty(el_dbs)
%     cfg=[];
%     cfg.method = 'CAR'; 
%     cfg.group = el_dbs.connector;
%     cfg.label = el_dbs.electrode;
%     D_trial_ref = bml_rereference(cfg,D_trial_ref);
%   end

%   % ecog CTAR
%   el_ecog = electrode(electrode.type=="ecog",:);
%   if ~isempty(el_ecog)
%     cfg=[];
%     cfg.method = 'CTAR'; 
%     cfg.percent = 50;
%     cfg.group = el_ecog.connector;
%     cfg.label = el_ecog.electrode;
%     D_trial_ref = bml_rereference(cfg,D_trial_ref);
%   end
%   
%clearvars D_filt_mask_trial

%% remasking NaNs with zeros
cfg=[];
cfg.value=0;
cfg.remask_nan = true;
cfg.complete_trial = true;
D_trial_ref = bml_mask(cfg,D_trial_ref);

%% performing time frequency analysis

cfg=[];
cfg.foi = TF_FOI;
cfg.method = 'mtmconvol';
cfg.output = 'pow';
cfg.keeptapers = 'no';
cfg.pad = 'nextpow2';
cfg.taper = 'hanning';
dt = 1/TF_RATE;
toirange = [tf_epoch_starts_t0, tf_epoch_ends_t0];
cfg.toi = ( (ceil(toirange(1)./dt)):1:(floor(toirange(2)./dt)-1) ) .* dt;
cfg.t_ftimwin = 2 .* dt .* ones(1,length(TF_FOI));
cfg.keeptrials   = 'yes';
TF_speech_fft = ft_freqanalysis(cfg,D_trial_ref);




%% plotting fft
epoch2 = bml_annot_left_join(epoch,triplet(:,{'session_id','trial_id','stim_volume'}));
for s=1:height(session)

  SESSION_ID=session.session_id(s);
  session_s = session(s,:);
  %SESSION_ID=1;

  cfg=[];
  cfg.trials = epoch2.id(epoch2.session_id==SESSION_ID);
  TF_speech_fft_s = ft_selectdata(cfg,TF_speech_fft);

  electrode_s = bml_annot_filter(electrode,session_s);
  if ismember('type',session.Properties.VariableNames)
    switch session.type{session.session_id == SESSION_ID}
      case 'MER' %plotting LFPs for macro contacts
        session_elec_types = {'micro','macro','ecog','V0'};
      case 'dbs'
        session_elec_types = {'dbs','ecog','V0'};
      case 'Lead'
        session_elec_types = {'dbs','ecog','V0'};
      case 'LEAD'
        session_elec_types = {'dbs','ecog','V0'};
      case 'Ecog only'
        session_elec_types = {'ecog','V0'};
      otherwise
        warning('session type not known %s',session.type{session.session_id == SESSION_ID});
        continue
    end
  else
    session_elec_types = {'dbs','ecog','V0'};
  end
  
  electrode_st = electrode_s(ismember(electrode_s.type,session_elec_types),:);  
  elec = intersect([unique(electrode_st.electrode); {'audio_p';'audio_s'}],TF_speech_fft_s.label);

  zlim_depth = [-5, 5];
  zlim_ecog  = [-5, 5];
  zlim_audio = [-10, 40];
  
  for e = 1:length(elec)
    
    fprintf('%s %i %s',SUBJECT,SESSION_ID,elec{e})
    
    %selecting non-empty trials for this electrode and session
    cfg=[];
    cfg.channel = elec(e);
    TF_speech_fft_se = ft_selectdata(cfg,TF_speech_fft_s);
    trial_sel_filt = squeeze(sum(sum(TF_speech_fft_se.powspctrm,4,'omitnan'),3))' > eps;
    n_trials = sum(trial_sel_filt);
   
    if n_trials<10
      continue
    end
    
    cfg=[];
    cfg.channel = elec(e);
    cfg.trials = find(trial_sel_filt);
    TF_speech_fft_se = ft_selectdata(cfg,TF_speech_fft_s);

    cfg = [];
    cfg.baseline     = tf_baseline_speech_t0; 
    cfg.channel      = elec(e);
    cfg.title        = [SUBJECT ' ' elec{e} ' N trials=' num2str(n_trials)];
    cfg.masknans     = 'yes';
      
    cfg.baselinetype   = 'zscore';
    ft_singleplotTFR(cfg,  TF_speech_fft_se);
    export_fig([PATH_TF_FIG filesep SUBJECT '_S' num2str(s) '_' elec{e}  '_TF_depth_ar_' CRITERIA '_fft_zscore.png'],'-m3',gcf);
    close(gcf);

%   	cfg.baselinetype   = 'db';
%     ft_singleplotTFR(cfg,  TF_speech_fft_se);
%     export_fig([PATH_TF_FIG filesep SUBJECT '_S' num2str(s) '_' elec{e}  '_TF_depth_ar_' CRITERIA '_fft_relative.png'],'-m3',gcf);
%     close(gcf);

  end

end


