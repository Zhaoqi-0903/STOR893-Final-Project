%circle plot
clear all;
close all;

addpath(genpath('circularGraph'))

%load the datafile from no_depression group;
load('mean.nodep.mat');
hcp_sc_68_m0 = mean_nodep;
hcp_sc_87_m0 = zeros(87,87,1);
hcp_sc_87_m0(20:87,20:87,1) = hcp_sc_68_m0;

%load the datafile from depression group;
load('mean.dep.mat');
hcp_sc_68_m1 = mean_dep;
hcp_sc_87_m1 = zeros(87,87,1);
hcp_sc_87_m1(20:87,20:87,1) = hcp_sc_68_m1;

%load the datafile from hypothesis test between the two groups;
load('neg.log10.pvalues.trait130.mat');
hcp_sc_68_nlogp = nlogpvalues;
hcp_sc_87_nlogp = zeros(87,87,1);
hcp_sc_87_nlogp(20:87,20:87,1) = hcp_sc_68_nlogp;

%we want to plot circles;
%prepare for the circle plot;
%reorgnize the nodes;
rfl = [65, 61, 60, 57, 53, 52, 51, 50, 47, 45, 37];
rsu = [68];
rlc = [59, 56, 49, 43, 36];
rpl = [64, 62, 58, 55, 41];
rtl = [67, 66, 63, 48, 42, 40, 39, 35];
rol = [54, 46, 44, 38];

lfl = [3, 11, 13, 16, 17, 18, 19, 23, 26, 27, 31];
lsu = [34];
llc = [2, 9, 15, 22, 25];
lpl = [7, 21, 24, 28, 30];
ltl = [1, 5, 6, 8, 14, 29, 32, 33];
lol = [4, 10, 12, 20];

col_index = [lfl,lsu,llc,lpl,ltl,lol,rfl,rsu,rlc,rpl,rtl,rol]+19;
left_index=[lfl,lsu,llc,lpl,ltl,lol]+19;
right_index=[rol,rtl,rpl,rlc,rsu,rfl]+19;
whole_index=1:87;
subcortical=[1:9,19,18,17,16,15,14,13,12,11,10];
final_index=[left_index,subcortical,right_index];

Nnode = length(final_index);
%plot circle
myLabel = cell(Nnode);
for i = 1:Nnode
    if(i<11.5)
        if(i==1)
            myLabel{i} = sprintf('l-FL-%d',final_index(i)-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-19);
        end
    elseif(i<12.5)
        if(i==12)
            myLabel{i} = sprintf('l-SU-%d',final_index(i)-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-19);
        end
        
    elseif(i<17.5)
        if(i==13)
            myLabel{i} = sprintf('l-LC-%d',final_index(i)-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-19);
        end
        
    elseif(i<22.5)
        if(i==18)
            myLabel{i} = sprintf('l-PL-%d',final_index(i)-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-19);
        end
        
    elseif(i<30.5)
        if(i==23)
            myLabel{i} = sprintf('l-TL-%d',final_index(i)-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-19);
        end
        
    elseif(i<34.5)
        if(i==31)
            myLabel{i} = sprintf('l-OL-%d',final_index(i)-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-19);
        end
        
    elseif(i<53.5)
        if(i==35)
            myLabel{i}=sprintf('Subcort-%d',final_index(i));
        elseif(i==53)
            myLabel{i}=sprintf('%d-Subcort',final_index(i));
        else
            myLabel{i} = sprintf('%d',final_index(i));
        end
                
    elseif(i<57.5)
        if(i==57)
            myLabel{i} = sprintf('%d-OL-r',final_index(i)-34-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-34-19);
        end
    elseif(i<65.5)
        if(i==65)
            myLabel{i} = sprintf('%d-TL-r',final_index(i)-34-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-34-19);
        end
        
    elseif(i<70.5)
        if(i==70)
            myLabel{i} = sprintf('%d-PL-r',final_index(i)-34-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-34-19);
        end
        
    elseif(i<75.5)
        if(i==75)
            myLabel{i} = sprintf('%d-LC-r',final_index(i)-34-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-34-19);
        end
        
    elseif(i<76.5)
        if(i==76)
            myLabel{i} = sprintf('%d-SU-r',final_index(i)-34-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-34-19);
        end
        
    elseif(i<87.5)
        if(i==87)
            myLabel{i} = sprintf('%d-FL-r',final_index(i)-34-19);
        else
            myLabel{i} = sprintf('%d',final_index(i)-34-19);
        end
    end
end

%plot circle of no_depression group
for ii=1
    
    brain_net_no_dep = squeeze(hcp_sc_87_m0(:,:,ii));
    for i=1:Nnode
        brain_net_no_dep(i,i) = 0;
    end
    
    %smallest and biggest value in this brain network.
    low_bound = min(min(brain_net_no_dep));
    up_bound = max(max(brain_net_no_dep));
    
    %thresholding the connections in the network
    brain_net_thrd = brain_net_no_dep.*(abs(brain_net_no_dep)>0.1*up_bound);
    
    figure(1);clf;
    circularGraph_dynamicnetwork(brain_net_thrd(final_index,final_index),'Label',myLabel,'EdgeColorMapRange',[low_bound,up_bound]);
    colorbar;
    

end

%plot circle of depression group
for ii=1
    
    brain_net_dep = squeeze(hcp_sc_87_m1(:,:,ii));
    for i=1:Nnode
        brain_net_dep(i,i) = 0;
    end
    
    %smallest and biggest value in this brain network.
    low_bound = min(min(brain_net_dep));
    up_bound = max(max(brain_net_dep));
    
    %thresholding the connections in the network
    brain_net_thrd = brain_net_dep.*(abs(brain_net_dep)>0.1*up_bound);
    
    figure(2);clf;
    circularGraph_dynamicnetwork(brain_net_thrd(final_index,final_index),'Label',myLabel,'EdgeColorMapRange',[low_bound,up_bound]);
    colorbar;
    

end

%plot circle of difference between no_depession and depression group
for ii=1
    
    brain_net_diff = squeeze(hcp_sc_87_m0(:,:,ii)-hcp_sc_87_m1(:,:,ii));
    for i=1:Nnode
        brain_net_diff(i,i) = 0;
    end
    
    %smallest and biggest value in this brain network.
    low_bound = min(min(brain_net_diff));
    up_bound = max(max(brain_net_diff));
    
    %thresholding the connections in the network
    brain_net_thrd = brain_net_diff.*(abs(brain_net_diff)>0.1*up_bound);
    
    figure(3);clf;
    circularGraph_dynamicnetwork(brain_net_thrd(final_index,final_index),'Label',myLabel,'EdgeColorMapRange',[low_bound,up_bound]);
    colorbar;
    

end

%plot connections of p-values <= 0.05 from hypothesis test
% -log10p-value >= 1.30103
for ii=1
    
    brain_net = squeeze(hcp_sc_87_nlogp(:,:,ii));
    for i=1:Nnode
        brain_net(i,i) = 0;
    end
    
    %smallest and biggest value in this brain network.
    low_bound = min(min(brain_net));
    up_bound = max(max(brain_net));
    
    %thresholding the connections in the network
    brain_net_thrd = brain_net.*(abs(brain_net)>=1.30103);
    
    figure(4);clf;
    circularGraph_dynamicnetwork(brain_net_thrd(final_index,final_index),'Label',myLabel,'EdgeColorMapRange',[low_bound,up_bound]);
    colorbar;
    

end

%plot connections of p-values <= 0.01 from hypothesis test
% -log10p-value >= 2
for ii=1
    
    brain_net = squeeze(hcp_sc_87_nlogp(:,:,ii));
    for i=1:Nnode
        brain_net(i,i) = 0;
    end
    
    %smallest and biggest value in this brain network.
    low_bound = min(min(brain_net));
    up_bound = max(max(brain_net));
    
    %thresholding the connections in the network
    brain_net_thrd = brain_net.*(abs(brain_net)>=2);
    
    figure(5);clf;
    circularGraph_dynamicnetwork(brain_net_thrd(final_index,final_index),'Label',myLabel,'EdgeColorMapRange',[low_bound,up_bound]);
    colorbar;
    

end

