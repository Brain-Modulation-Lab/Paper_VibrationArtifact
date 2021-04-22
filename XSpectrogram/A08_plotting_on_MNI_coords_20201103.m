%% Loading paths
ft_defaults
bml_defaults
format long
clear

%% Configuration Variables and Paths
PATH_ANALYSIS = 'Z:\Commits\Vibration_artifacts\xspectrogram';
PATH_DATA='Z:\DBS';
PATH_AVERAGE_MNI = [PATH_DATA '\DBS_subject_lists\MNI_ICBM_2009b_NLIN_ASYM\cortex\CortexLowRes_15000V.mat'];
cd(PATH_ANALYSIS)

electrode = readtable('results/electrode_session_xspec.tsv','Delimiter', '\t', 'TreatAsEmpty', 'NA','FileType','text');

%loading cortical reconstructions
average_mni = load(PATH_AVERAGE_MNI);

%% 

palette=...
	[0,      0.4470, 0.7410; 
   0.8500, 0.3250, 0.0980;
   0.9290, 0.6940, 0.1250;
   0.4940, 0.1840, 0.5560;
   0.4660, 0.6740, 0.1880;
   0.3010, 0.7450, 0.9330;
   0.6350, 0.0780, 0.1840];

figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
scatter3(electrode.mni_nonlinear_x-15, electrode.mni_nonlinear_y, electrode.mni_nonlinear_z,'filled',...
  'MarkerFaceAlpha',0.2,'MarkerFaceColor',palette(1,:))
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/fig/A08_01_mni_nonlin_electrodes_all.png'])
close(gcf)

electrode_c3 = electrode(electrode.xcorr > 0.25,:);
figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
scatter3(electrode_c3.mni_nonlinear_x-15, electrode_c3.mni_nonlinear_y, electrode_c3.mni_nonlinear_z,'filled',...
  'MarkerFaceAlpha',0.2,'MarkerFaceColor',palette(2,:))
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/fig/A08_02_mni_nonlin_electrodes_xcorr_gt_025.png'])
close(gcf)

electrode_c5 = electrode(electrode.xcorr > 0.5,:);
figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
scatter3(electrode_c5.mni_nonlinear_x-15, electrode_c5.mni_nonlinear_y, electrode_c5.mni_nonlinear_z,'filled',...
  'MarkerFaceAlpha',0.2,'MarkerFaceColor',palette(3,:))
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/fig/A08_03_mni_nonlin_electrodes_xcorr_gt_05.png'])
close(gcf)

electrode_c10 = electrode(electrode.xcorr > 0.75,:);
figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
scatter3(electrode_c10.mni_nonlinear_x-15, electrode_c10.mni_nonlinear_y, electrode_c10.mni_nonlinear_z,'filled',...
  'MarkerFaceAlpha',0.2,'MarkerFaceColor',palette(3,:))
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/fig/A08_04_mni_nonlin_electrodes_xcorr_gt_075.png'])
close(gcf)

electrode_c15 = electrode(electrode.xcorr > 0.85,:);
figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
scatter3(electrode_c15.mni_nonlinear_x-15, electrode_c15.mni_nonlinear_y, electrode_c15.mni_nonlinear_z,'filled',...
  'MarkerFaceAlpha',0.2,'MarkerFaceColor',palette(3,:))
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/fig/A08_05_mni_nonlin_electrodes_xcorr_gt_085.png'])
close(gcf)


cmap = colormap('hot');

figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
N=10;
for i = 1:N
    electrode_cmap = electrode((electrode.xcorr > (i-1)/N) & (electrode.xcorr <= i/N),:);
    scatter3(electrode_cmap.mni_nonlinear_x-15-i, electrode_cmap.mni_nonlinear_y, electrode_cmap.mni_nonlinear_z,'filled',...
      'MarkerFaceAlpha',0.2+0.8*i/N,'MarkerFaceColor',cmap(round(size(cmap,1).*i./N),:))
end
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/fig/A08_06_mni_nonlin_electrodes_xcorr_overlay.png'])






















figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
scatter3(0,0,0)
scatter3(electrode.mni_nonlinear_x-15, electrode.mni_nonlinear_y, electrode.mni_nonlinear_z,'filled',...
  'MarkerFaceAlpha',0.2)
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/figures/ALL_mni_nlin_electrodes.png'])

figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceColor', [.5 .5 .6], 'EdgeColor', 'none', 'FaceAlpha',1, ...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
scatter3(0,0,0)
scatter3(0,0,0)
scatter3(electrode.fs_mni152_cvs_x-15, electrode.fs_mni152_cvs_y, electrode.fs_mni152_cvs_z,'filled',...
  'MarkerFaceAlpha',0.2)
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
saveas(gcf,[PATH_ANALYSIS '/figures/ALL_fs_cvs_electrodes.png'])

average_mni_table = table(average_mni.Vertices(:,1),average_mni.Vertices(:,2),average_mni.Vertices(:,3));
average_mni_table.Properties.VariableNames = {'coord_x','coord_y','coord_z'};
cfg=[];
cfg.atlas = 'HCPMMP1 (Glasser 2016)';
average_mni_table = bml_anat_coord2label(cfg,average_mni_table);

average_mni_table

palette=...
	[0,      0.4470, 0.7410; 
   0.8500, 0.3250, 0.0980;
   0.9290, 0.6940, 0.1250;
   0.4940, 0.1840, 0.5560;
   0.4660, 0.6740, 0.1880;
   0.3010, 0.7450, 0.9330;
   0.6350, 0.0780, 0.1840];
regions = {'1','6v','A4','OP4','PFop','55b','8C'};

CData = repmat([.5 .5 .6],size(average_mni.Vertices,1),1);
for i=1:7
  vert_sel = strcmp(average_mni_table.HCPMMP1_label_1,regions{i});
  CData(vert_sel ,:)=repmat(palette(i,:),sum(vert_sel),1);
end

figure;
hold on
patch('vertices', average_mni.Vertices, 'faces', average_mni.Faces,...
'FaceVertexCData',CData,'FaceColor', 'interp', 'EdgeColor', 'none',...
'facelighting', 'gouraud', 'specularstrength', 0, 'ambientstrength', 0.5, 'diffusestrength', 0.5)
hold on
for i=1:7
  scatter3(0,0,0,'filled','MarkerFaceColor',palette(i,:));
end
view(-90,0)
axis off; axis equal
camlight('headlight','infinite');
legend([{'MNI 152 NLIN 09b'},regions])
saveas(gcf,[PATH_ANALYSIS '/figures/HCPMMP1_highest_coverage_regions.png'])
