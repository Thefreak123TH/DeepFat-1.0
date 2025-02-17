%% 3D Unet --- Determining the start and End slice
function [Start_Slice,End_Slice] = Unet3D_StartEnd(Vol_processed)
load('trained3DUNet-2022-05-18-00-04-24-Epoch-30.mat')  %%% AOH need to make it golbal with net2 of deepfat
pxds_Results = semanticseg(Vol_processed,net,"ExecutionEnvironment",'cpu');

% Keep the largest 3d Volume
props = regionprops3(double(pxds_Results)-1, 'Volume'); %% If you have 2 wrong volumes, keep the biggest
sortedVolumes = sort([props.Volume], 'descend');
Biggest = round(sortedVolumes(1)/2);
% Pull out 3 biggest into another 3-D logical image.
pxds_results = bwareaopen(double(pxds_Results)-1, Biggest,6);


[~,~,z] = size(pxds_results);
for j = 1:z
    sum_R(j) = sum(pxds_results(:,:,j),'all');
end
index_zero_R = find(sum_R~=0);
Start_Slice = index_zero_R(1);
End_Slice = index_zero_R(end);

end