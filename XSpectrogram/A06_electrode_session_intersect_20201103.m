
%PATH_ANALYSIS = '/Volumes/Nexus/Users/busha/Analysis/2020-08-05-audio_p-coherence';
PATH_ANALYSIS = 'Z:\Commits\Vibration_artifacts\xspectrogram';

cd(PATH_ANALYSIS)

electrode = bml_annot_read('results/electrode.txt');
electrode = bml_annot_table(electrode,'electrode');
session_electrode_tmp = bml_annot_read('results/session_electrode_tmp.txt');
session_electrode_tmp = bml_annot_table(session_electrode_tmp,'tmp');
session_electrode_tmp.subject=[];
session_electrode_tmp.electrode=[];

cfg=[];
cfg.groupby={'subject_electrode'};
electrode_session = bml_annot_intersect(cfg,session_electrode_tmp,electrode);
electrode_session.tmp_id=[];
electrode_session.electrode_id=[];
electrode_session.tmp_duration=[];
electrode_session.electrode_duration=[];
electrode_session.subject_electrode=[];
electrode_session = sortrows(electrode_session,{'subject','electrode','session_id',});
electrode_session.id = (1:height(electrode_session))';

bml_annot_write(electrode_session,'results/electrode_session.txt')







