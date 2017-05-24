%% Clear Everything
clear;
cd('~/Dropbox/Papers/01-simrel-m/rmarkdown/simrel-m');

%% Adding Path
addpath(genpath('~/Documents/MATLAB/EnvelopeMLM/'));

%% Get Beta from Simultanious Envelope
dgn = 4;
rep = 20;
ncomp = 10;
obj = cell(dgn, rep, ncomp);
for d = 1:dgn 
    for r = 1:rep
       disp(['Design:', int2str(d), '; Replication:', int2str(r)]);
       load(['matfiles/D-', int2str(d), '-', int2str(r), '.mat']);
       try
          [~, ncy] = tritests(data.x, data.y, 0, 0.05);
       catch
           warning('Error in determining dimension of y-envelope');
           ncy = [3 3 3];
       end
       ny = ncy(2);
       parfor nx = 1:ncomp
          disp(['ncomp:', int2str(nx)]);
          try
              obj{d, r, nx} = SimulEnv(data.x, data.y, nx, ny, 0);
          catch
              warning('Error in fitting Simultanious Envelope');
              obj{d, r, nx} = 0;
              continue;
          end
       end
    end
end

objFinal = cell(1, 4);
for d = 1:dgn
    for r = 1:rep
        for nc = 1:ncomp
            objFinal{d}{r}{nc} = obj{d, r, nc};
        end
    end
end

%% Saving Beta Array
save('output/fitted-sim-env.mat', 'objFinal')