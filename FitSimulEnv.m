%% Clear Everything
clear;
cd('~/Dropbox/Papers/01-simrel-m/rmarkdown/simrel-m');

%% Adding Path
addpath(genpath('~/Documents/MATLAB/EnvelopeAlgorithms/'));
addpath(genpath('~/Documents/MATLAB/SDR-Test'));
addpath(genpath('~/Documents/MATLAB/EnvelopeMLM/'));

%% Load Data
load('sim-data.mat');

%% Fitting SimulEnvelope Model
for ncx = 1:10
    for ncy = 1:4
        d1Out(ncx, ncy) = SimulEnv(Train1.x, Train1.y, ncx, ncy, 1);
        d2Out(ncx, ncy) = SimulEnv(Train2.x, Train2.y, ncx, ncy, 1);
    end
end

%% Saving fitted Objects
save('sim-fit-obj.mat', 'd1Out', 'd2Out');
