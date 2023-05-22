maxNumCompThreads(1);
addpath('/mnt/MD1200B/egarza/vissa/matlab/');
[status,current_sub] = system('echo $current_sub');
[status,SGE_TASK_ID] = system('echo $SGE_TASK_ID');

fileID=fopen('motor_thresholds.txt','r');
formatSpec='%f';
didt=fscanf(fileID,formatSpec);

coord = readtable(strcat('../',current_sub,'/ses-t0/anat/',current_sub,'_stim_site.csv'))

% Initialize a session
s = sim_struct('SESSION');
S.map_to_fsavg = true;
S.map_to_mni = true;
% Name of head mesh
s.fnamehead = strcat('../',current_sub,'/ses-t0/anat/',current_sub,'.msh');
% Output folder
s.pathfem = strcat('../',current_sub,'/ses-t0/anat/simulation/');

% Initialize a list of TMS simulations
s.poslist{1} = sim_struct('TMSLIST');
% Select coil
s.poslist{1}.fnamecoil = 'MagVenture_MC_B70.nii.gz';

% Select coil centre
s.poslist{1}.pos(1).centre = [coord(2),coord(3),coord(4)];
% Select coil direction
s.poslist{1}.pos(1).pos_ydir = [coord(2)+5,coord(3)+5,coord(4)];
s.poslist{1}.pos(1).didt = didt(SGE_TASK_ID-1);

run_simnibs(s)
