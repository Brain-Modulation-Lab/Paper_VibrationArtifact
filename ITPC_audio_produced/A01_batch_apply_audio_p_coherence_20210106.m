PROTOCOL_PATH = 'Z:\Commits\Vibration_artifacts\2020-08-05-audio_p-coherence';
%PROTOCOL_PATH = '/Volumes/Nexus/Users/busha/Analysis/2020-08-05-audio_p-coherence';
PROTOCOL_FUNCTION = 'apply_audio_p_coherence';
PROTOCOL_TABLE = 'Subjets.txt';
exe_daytime = datestr(now,'yyyymmdd_HHMM');
diary([PROTOCOL_PATH filesep 'batch_' PROTOCOL_FUNCTION '_' exe_daytime '.log'])
addpath(PROTOCOL_PATH);

cd(PROTOCOL_PATH)

subject_table = readtable(PROTOCOL_TABLE);         
fprintf('=== Running protocol %s ===\n',PROTOCOL_FUNCTION)

for i=1:height(subject_table)
  SUBJECT = subject_table.subject{i};
  fprintf('Running protocol.')
  %running protocol
  try
    proto = str2func(PROTOCOL_FUNCTION);
    proto(SUBJECT);
    fprintf('OK\n')
  catch err
    fprintf('FAILED: %s\n',err.message)
  end
end

diary('off')

