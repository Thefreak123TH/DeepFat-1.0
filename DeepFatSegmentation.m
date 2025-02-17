function [Mask_Epicardial,Mask_SAC,TotalEATvol_cm3,TotalEATmean] = DeepFatSegmentation(Vol_processed,Start_Slice,End_Slice,pixelSpacing,sliceThickness)

[index_0,total_length] =  ImageCombine_rev(Vol_processed,Start_Slice,End_Slice);
imageDir_Test = strcat('output');
imds_Test = imageDatastore(imageDir_Test,...
    'IncludeSubfolders',true,'FileExtensions','.png','ReadFcn',@(x) imread(x));
load('Original_axial_rev_98Subject.mat')  %%%% AOH 06-08-2022 try to put in a global variables

pxds_Results = semanticseg(imds_Test,net, ...
    'MiniBatchSize',4, ...
    'WriteLocation',tempdir, ...
    'Verbose',false);


n1 = length(pxds_Results.Files);
sum_E_R = 0;
SAC_R = zeros(512,512,n1);
f_R = zeros(512,512,n1);
voxel_R = zeros(n1,1);
sum_HU =0;



for ii = 1:n1
    temp_I = Vol_processed(:,:,index_0(ii));
    temp_I(temp_I<-190)= 0;
    temp_I(temp_I>-30)= 0;
    temp_I(temp_I~=0)= 1;
    mask_R =  int16(readimage(pxds_Results,ii))-1;
    SAC_R(:,:,ii) = mask_R;
    temp_epicardial = mask_R .* temp_I;
    f_epicardial = medfilt2(temp_epicardial);
    sum_HU =sum_HU + sum(f_epicardial.* Vol_processed(:,:,index_0(ii)),'all');
    voxel_R(ii,1) = sum(f_epicardial,'all');
    sum_E_R = sum_E_R + sum(f_epicardial,'all');
    f_R(:,:,ii) = f_epicardial;
end
Mask_Epicardial =cat(3,zeros(512,512,Start_Slice-1),f_R);
Mask_Epicardial = cat(3,Mask_Epicardial,zeros(512,512,total_length-End_Slice));
Mask_SAC = cat(3,zeros(512,512,Start_Slice-1),SAC_R);
Mask_SAC = cat(3,Mask_SAC,zeros(512,512,total_length-End_Slice));
TotalEATmean = sum_HU/sum_E_R;

%% Calculated the total EAT volume and EAT volume per slice in cm^3 and Show in a Table
Vol_r_P = sliceThickness*pixelSpacing*pixelSpacing*sum(f_R,'all')/1000;
Vol_r_s = sliceThickness*pixelSpacing*pixelSpacing*voxel_R/1000;
% Present the result in one table
Spacing = pixelSpacing * ones(n1+1,1);
Thickness = sliceThickness*ones(n1+1,1);
VoxelNumber = cat(1,voxel_R,sum_E_R);
EATvolume_cm3 = cat(1,Vol_r_s,Vol_r_P);
SliceNumber = index_0';
SliceNumber = [num2cell(SliceNumber);'Total'];
TotalEATvol_cm3 = Vol_r_P;

flag=false; %default

if flag==true
    %%%%% Tao put them in diffrent testing seperate function
    T = table(SliceNumber,Thickness,Spacing,VoxelNumber,EATvolume_cm3);
    f = figure;
    data = cat(2,Thickness,Spacing,VoxelNumber,EATvolume_cm3);
    row ={'Thickness(mm)','Spacing(mm)','Voxel Number','EAT Volume(cm3)'};
    Table = uitable(f,'data',data,'rowname',SliceNumber,'columnname',row,'Position',[20 20 450 600]);
    
    %%
    figure;
    overlay1 = [];
    % overlay3 = [];
    for i = 1:56
        overlay = labeloverlay(rescale(Vol_processed(:,:,i),0,1),Mask_SAC(:,:,i));
        %     overlay2 = labeloverlay(rescale(Vol_processed(:,:,i),0,1),Mask_Epicardial(:,:,i));%%%%
        if length(size(overlay)) == 2
            overlay = cat(3,overlay,overlay,overlay);
            overlay1 = cat(4,overlay1,overlay);
            %         overlay2 = cat(3,overlay2,overlay2,overlay2); %%%
            %         overlay3 = cat(3,overlay3,overlay2); %%%%
        else
            overlay1 = cat(4,overlay1,overlay);
            %         overlay3 = cat(4,overlay3,overlay2); %%%%
        end
    end
    overlay1 = permute(overlay1,[1,2,4,3]);
    % overlay3 = permute(overlay3,[1,2,4,3]); %%%%
    imshow3D(overlay1)
    % figure;
    % imshow3D(overlay3)
end
end


%% Functions
function [index_0,total_length] =  ImageCombine_rev(Vol_processed,slice_start,slice_end)


% Create Output folder
outputFolder_Image=strcat('output');
[~,~,~]=mkdir(outputFolder_Image);
%
[~,~,total_length] = size(Vol_processed);


% Read Image
temp_image = Vol_processed;

[a,b,c] = size(temp_image);
temp_image_1 = zeros(a,b);
temp_image_2 = zeros(a,b);
temp_image_3 = zeros(a,b);
index_0 = slice_start:1:slice_end;
index_length = length(index_0);
index_middle = round(length(index_0)*0.5);
% Image combine k,k,k+1
for j = 1:length(index_0)
    if  j < index_middle
        temp_image_1(:,:) = temp_image(:,:,index_0(j));
        temp_image_2(:,:) = temp_image(:,:,index_0(j)+1);
        temp_image_3(:,:) = temp_image(:,:,index_0(j)+2);
        Image_combine = cat(3, temp_image_1, temp_image_2, temp_image_3);
        Image_new = mat2gray(Image_combine,[-135,215]);
        % Corresponding labels
        baseFileName_Image = sprintf('image %2.2d.png',index_0(j));
        outputFullFileName_Image = fullfile(outputFolder_Image, baseFileName_Image);
        imwrite(Image_new, outputFullFileName_Image,'BitDepth',16);
    else
        % Combine 3 consective slices into 1
        temp_image_1(:,:) = temp_image(:,:,index_0(j)-2);
        temp_image_2(:,:) = temp_image(:,:,index_0(j)-1);
        temp_image_3(:,:) = temp_image(:,:,index_0(j));
        Image_combine = cat(3, temp_image_1, temp_image_2, temp_image_3);
        Image_new = mat2gray(Image_combine,[-135,215]);
        % Corresponding labels
        baseFileName_Image = sprintf('image %2.2d.png',index_0(j));
        outputFullFileName_Image = fullfile(outputFolder_Image, baseFileName_Image);
        imwrite(Image_new, outputFullFileName_Image,'BitDepth',16);
        
    end
end



end