function doRousselAnalysis(SUBJECT)
%SUBJECT='DBS3032'; %used for development/debugging

addpath(genpath('E:\MATLAB\zenodo_acoustic_eeg_contamination\Toolbox'));

bml_defaults

%script configuration parameters
CRITERIA = 'C'; %Artifact Rejection criteria
PATH_DATA = 'Z:\DBS';
PATH_SUBJECT = [PATH_DATA filesep SUBJECT];
PATH_SYNC = [PATH_SUBJECT filesep 'Preprocessed Data' filesep 'Sync'];
cd(PATH_SYNC)

PATH_ANALYSIS = 'Z:\Commits\Vibration_artifacts\roussel_analysis';
addpath(PATH_ANALYSIS);
PATH_ANDATA = [PATH_ANALYSIS filesep 'data'];
PATH_ANFIG = [PATH_ANALYSIS filesep 'figures'];
DATE=datestr(now,'yyyymmdd');

fprintf('Roussel Analysis for subject %s \n',SUBJECT)
  
%loading annotation tables
sync      = bml_annot_read(['annot/' SUBJECT '_sync.txt']);
session = bml_annot_read(['annot' filesep SUBJECT '_session.txt']);
electrode = bml_annot_read(['annot' filesep SUBJECT '_electrode.txt']);
artifact = bml_annot_read(['annot' filesep SUBJECT '_artifact_criteria_' CRITERIA '.txt']);
empty_electrode = bml_annot_read(['annot' filesep SUBJECT '_empty_electrode.txt']);

%% loading continuous preprocessed data 
load([PATH_SUBJECT filesep 'Preprocessed Data' filesep 'FieldTrip' filesep SUBJECT '_ft_raw_session.mat'],'D')
 
%% applying notch filtering
cfg=[];
cfg.remask_nan = true;
cfg.value = 0;
% ecog = bml_mask(cfg, ecog);
% audio_p = bml_mask(cfg, audio_p);
D = bml_mask(cfg, D);

% notch filtering
cfg=[];
cfg.hpfilter='yes';
cfg.hpfreq=10;
cfg.hpfilttype='but';
cfg.hpfiltord=5;
cfg.hpfiltdir='twopass';
cfg.bsfilter='yes';
freqs=[60 120 180 240];
cfg.bsfreq= [freqs-(1:4); freqs+(1:4)]';
%freqs=[60];
%cfg.bsfreq= [freqs-2; freqs+2]';
cfg.bsfiltord=2;
% ecog_f = ft_preprocessing(cfg,ecog);
% audio_p_f = ft_preprocessing(cfg,audio_p);
Df = ft_preprocessing(cfg,D);

cfg=[];
cfg.channel = 'audio_s';
audio_s = ft_selectdata(cfg,Df);

%% performing analysis on channel subsets
electrode_groups = unique(electrode.electrode(ismember(electrode.type,{'ecog','dbs','macro','micro','V0'})));
output_table = table();

for eg=1:length(electrode_groups)
    electrode_group = electrode_groups(eg);
    
    cfg=[];
    cfg.channel = [electrode_group{1},'*'];
    Dfeg = ft_selectdata(cfg,Df);
    
    if ~isempty(Dfeg.label)
        for i=1:length(Dfeg.trial)
            session_id=session.session_id(i);

            brain_matfile_path=[PATH_ANDATA filesep SUBJECT '_S' num2str(i) '_' electrode_group{1} '.mat'];
            audio_matfile_path=[PATH_ANDATA filesep SUBJECT '_S' num2str(i) '_audio_s.mat'];
            results_path = [PATH_ANFIG];
            analysis_name = [SUBJECT '_' electrode_group{1} '_S' num2str(i)];

            data = Dfeg.trial{i}';
            fs = bml_getFs(Dfeg);
            createRecordingMatfile(brain_matfile_path, data, fs);

            data = audio_s.trial{i}';
            fs = bml_getFs(audio_s);
            createRecordingMatfile(audio_matfile_path, data, fs);

            %% Create and store a ContaminationAnalysis object
            %
            % results_path:
            %   path to save the results
            % brain_matfile_path:
            %   brain data matfile path (should respect defined format)
            % audio_matfile_path:
            %   audio data matfile path (should respect defined format)
            % analysis_name (optional):
            %   name of files and figures related to the present analysis

            obj = ContaminationAnalysis(...
            results_path,...
            brain_matfile_path,...
            audio_matfile_path,...
            analysis_name);


            %% Select time samples that will be considered in the analysis
            %
            % select_periods:
            %   2-column array defining start and end times of the time periods to
            %   select.
            % exclude_periods:
            %   2-column array defining start and end times of the time periods to
            %   exclude.

            select_periods = [];
            exclude_periods = []; % exclude the first 50 seconds

            obj = selectTime(obj,...
                select_periods,...
                exclude_periods);

            %% Detect artifacts occuring on several channels
            %
            % moving_average_span:
            %   Duration (in seconds) of the moving average window that is used to
            %   detrend the data before artifact detection.
            % artifact_threshold_factor:
            %   'artifact_threshold_factor' multiplied by the MAD of a given channel
            %   defines the artifact threshold of this channel.
            % artifact_channel_ratio:
            %   Ratio of channels crossing their threshold for a sample to be
            %   considered as an artifact
            % artifact_safety_period:
            %   Period of time (in seconds) before and after artifact in which samples
            %   are also considered as artifacts

            moving_average_span = 0.5;
            artifact_threshold_factor = 10;
            artifact_channel_ratio = 1/10;
            artifact_safety_period = 0.5;

            obj = detectArtifacts(obj,...
                moving_average_span,...
                artifact_threshold_factor,...
                artifact_channel_ratio,...
                artifact_safety_period);

            %% Display the results of the artifact detection and save the figure
            %
            % display_channel_nb:
            %   Number of channels to show. The first half of the displayed channels
            %   are the channels with the highest numbers of artifact samples and the
            %   second half are the ones with the lowest numbers.
            %
            % Can return figure handle.

            display_channel_nb = 1;

            displayArtifacts(obj, display_channel_nb)
            saveas(gcf,[PATH_ANFIG filesep SUBJECT '_' electrode_groups{eg} '_S' num2str(i) '_displayArtifacts.png'])
            close()

            %% Compute the spectrograms of the audio and brain recordings
            %
            % window_duration:
            %   Duration of the spectrogram window (in seconds).
            % spg_fs:
            %   Desired sampling frequency of the spectrogram.
            % spg_freq_bounds:
            %   2-element vector containing the lowest and the highest frequencies
            %   considered in the spectrogram (if empty, all frequency bins are kept).

            window_duration = 200e-3;
            spg_fs = 50;
            spg_freq_bounds = [0 250];

            obj = computeSpectrograms(obj,...
                window_duration, spg_fs,spg_freq_bounds);

            %% Compute spectrogram correlations between the audio and the brain data

            obj = computeSpectrogramCorrelations(obj);

            %% Display the spectrogram correlations and save the figures
            %
            % disp_freqs_bounds:
            %   2-element vector containing the lowest and the highest frequencies
            %   displayed in the spectrogram (if empty, all frequency bins are kept).
            % display_channels:
            %   'index' or 'id' of the channels to be displayed.
            % colormap_limits:
            %   2-element vector containing the lowest and the limits of the colormap
            %   displaying the z-scored spectrograls.
            %
            % Can return figure handles.

            display_channels = 1;
            disp_freqs_bounds = [];
            colormap_limits = [0 5];

            displayCorrelations(obj, disp_freqs_bounds, display_channels, colormap_limits);

            saveas(gcf,[PATH_ANFIG filesep SUBJECT '_' electrode_groups{eg} '_S' num2str(i) '_corrOverview.png'])
            close()
            saveas(gcf,[PATH_ANFIG filesep SUBJECT '_' electrode_groups{eg} '_S' num2str(i) '_mostCorrChannels.png'])
            close()


            %% Compute spectrogram cross-correlations between the audio and the brain data
            %
            % max_time_lag:
            %   Maximum absolute time lag in seconds considered when applying positive
            %   and negative delays to the audio spectrogram.

            max_time_lag = 0.5;

            obj = computeSpectrogramCrossCorrelations(obj, max_time_lag);

            %% Display cross-correlations
            %
            % crosscorr_min_max_freqs:
            %   2-element vector containing the lowest and the highest
            %   frequencies to be considered (if empty, all frequency bins are kept).
            % top_corr_ratio:
            %   Ratio of the highest cross-correlograms to display. 0.01 means that the
            %   1% of cross-correlograms reaching the highest values will be displayed.

            crosscorr_min_max_freqs = [75 250]; % frequency range considered
            top_corr_ratio = 0.01; % ratio of the highest correlations to display

            displayCrossCorrelations(obj, crosscorr_min_max_freqs, top_corr_ratio)
            saveas(gcf,[PATH_ANFIG filesep SUBJECT '_' electrode_groups{eg} '_S' num2str(i) '_timelag.png'])
            close()


            %% Compute statistical criterion P
            %
            % criterion_min_max_freqs:
            %   2-element vector containing the lowest and the highest
            %   frequencies to be considered (if empty, all frequency bins are kept).

            criterion_min_max_freqs = [75 Inf];

            [obj, criterion_value, dataset_measure] = computeStatisticalCriterion(obj, criterion_min_max_freqs);

            %% Display statistical criterion P

            displayStatisticalCriterion(obj);
            saveas(gcf,[PATH_ANFIG filesep SUBJECT '_' electrode_groups{eg} '_S' num2str(i) '_statistical_criterion.png'])
            close()

            output_table = bml_annot_rowbind(output_table,table(electrode_group,session_id,criterion_value,dataset_measure));

        end
    end
end

writetable(output_table,[PATH_ANDATA filesep SUBJECT '_Roussel_method_audio_s.csv'])

