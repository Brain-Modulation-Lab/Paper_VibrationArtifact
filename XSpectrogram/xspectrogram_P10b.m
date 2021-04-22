
PROTOCOL_PATH = 'Z:\Commits\Vibration_artifacts\xspectrogram';
PROTOCOL_TABLE = 'Subjects.txt';
exe_daytime = datestr(now,'yyyymmdd_HHMM');
addpath(PROTOCOL_PATH);

PATH_DATA = 'Z:\DBS';
cd(PROTOCOL_PATH)

subject_table = readtable(PROTOCOL_TABLE);         

for i=1:height(subject_table)
% for i=2

  SUBJECT = subject_table.subject{i};
   
  fprintf('Running protocol.')
  % run protocol no baseline audio - zscore ecog
  P10b_time_frequency_get_spect(SUBJECT);
end


