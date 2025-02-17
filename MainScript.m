% Note:
% Skip the CTCS volume that has 
% Different slice thickness than 2.5mm  (TYPE 102)
% Different in-plain voxel size than 512 x 512 (any spacing is fine) (TYPE 103)
% Unreadable volume folder (TYPE 104)
% Number of slices <20 (TYPE 105)
%%%%%
% Input: FilePath   Ex: E:\TestEAT_AutoSE\Test_Subject\VN557SDC1\im_6
% Output: Structure Data
%         1. Status: Success or Not
%            Type 101: Sucesses    CTCS Volume with 512*512*56
%            Type 102: Ignore      Different slice thickness than 2.5mm  
%            Type 103: Ignore      Different in-plain voxel size than 512 x 512 
%            Type 104: Ignore      Unreadable volume folder 
%            Type 105: Ignore      Number of slices <20 
%         2. Patient Name       Ex: 'VN557SDC1'
%         3. EAT Mask           512*512*56 binary Mask
%         4. SAC Mask           512*512*56 binary Mask
%         5. Total EAT Volume   Unit: cm^3
%         6. Figure 1:          Volume(cm^3) per slice
%         7. Figure 2:          Original Volume with overlapping SAC mask


clear all
clc
Directory = dir;
oldDir = "Y:\CT Calcium Scoring\UH dataset 500";

t = readtable('clarify_500.xlsx','Sheet','Sheet1');
Volpath = [];
for i=1: height(t)
      CTdir=fullfile(t.anonAccNo{i}, t.FullCTfolder{i});
      Volpath= [Volpath;cellstr(CTdir)];
      folderPath = strcat(oldDir,'\',Volpath{i});
      Out = MainFunc(folderPath);
      %%% Save the output in paitent name with a new folder called "EAT"
      
      %%save PatientName.mat   ,CT_Vol (all),CT_Vol_trunc(all),SacMask (binary),EATmask (binary)
                                %    FirstSlice,LastSlice,VolumeEAT,meanHU,TableofSlice.... 
                                
                                % Excel file, patname, FirstSlice,LastSlice,VolumeEAT,meanHU
      
end
