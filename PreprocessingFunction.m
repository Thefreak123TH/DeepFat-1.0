%% Preprocessing Function
function out = PreprocessingFunction(folderPath)

% Check Potential Error
% if rand<1
%     Out = bbbbbb/c; 
% end
% 

warning('off')

    inputFolder_Image = folderPath;
%     inputFolder_Image = fullfile(pwd,'Test_Subject',folderpath);      
    filePattern_Image = fullfile(inputFolder_Image, '*.dcm');
    theFiles_Image = dir(filePattern_Image);
    baseFileName_Image = theFiles_Image(5).name;
    fullFileName_Image = fullfile(inputFolder_Image, baseFileName_Image);
    Info = dicominfo(fullFileName_Image);
    if isfield(Info,'SliceThickness')==0 || isfield(Info,'RescaleSlope') == 0
        Er = strcat('Type 104: Ignore --- Unreadable volume folder ');
%         disp('Type 104: Ignore --- Unreadable volume folder ')
        out{1} = Er;
%         varargout{1}=Er;
    elseif Info.SliceThickness ~=2.5
        Er = strcat('Type 102: Ignore --- Slice thickness=',num2str(Info.SliceThickness),'mm--- Required is 2.5mm');
%         disp('Type 102: Ignore --- Different slice thickness than 2.5mm')
        out{1} = Er;
    elseif Info.Width ~= 512 || Info.Height ~= 512
        Er = strcat('Type 103: Ignore ---in-plain=',num2str(Info.Width),'x',num2str(Info.Height),'--- Required is 512x512');
%         disp('Type 103: Ignore --- Different in-plain voxel size than 512 x 512')
        out{1} = Er;
    elseif numel(theFiles_Image) <=25
        Er = strcat('Type 105: Ignore --- Number of slices=',num2str(numel(theFiles_Image)),' --- Required >25');
%         disp('Type 105: Ignore ---  Number of slices <20')
        out{1} = Er;
    else
%         disp('Type 101: Sucesses')
        Mes = strcat('Type 101: Sucesses');
        temp_image = ImageLabelProcess(inputFolder_Image,theFiles_Image);
        Vol_processed = StandardVolumeTransformation(temp_image);
        pixelSpacing = Info.PixelSpacing(1);
        sliceThickness = Info.SliceThickness;
        PatientName = Info.AccessionNumber;
        [Start_Slice,End_Slice] = Unet3D_StartEnd(Vol_processed);
        [Mask_Epicardial,Mask_SAC,TotalEATvol_cm3,TotalEATmean] = DeepFatSegmentation(Vol_processed,Start_Slice,End_Slice,pixelSpacing,sliceThickness);
        out{1} = Mes;
        out{2} = PatientName;
        out{3} = Mask_Epicardial;
        out{4} = Mask_SAC;
        out{5} = TotalEATvol_cm3;
        out{6} = TotalEATmean;
        % Delete Useless Folder
        directory   = dir;
        oldDir  	= directory(1).folder;
        rmdir(strcat(oldDir,'\output'),'s')
    end
% end
end
%% Functions
function temp_image =  ImageLabelProcess(inputFolder_Image,theFiles_Image) % Cheecking if the volume is filpped or not (Top-to-bottom)
SliceL1 = dicominfo(fullfile(inputFolder_Image, theFiles_Image(5).name)).ImagePositionPatient(3);
SliceL2 = dicominfo(fullfile(inputFolder_Image, theFiles_Image(6).name)).ImagePositionPatient(3);

for k = 1 : length(theFiles_Image)
    if SliceL1 - SliceL2 > 0
        baseFileName_Image = theFiles_Image(length(theFiles_Image)-k+1).name;
    else
        baseFileName_Image = theFiles_Image(k).name;
    end
    fullFileName_Image = fullfile(inputFolder_Image, baseFileName_Image);
    Info = dicominfo(fullFileName_Image);
    if isempty(dicomread(fullFileName_Image))
        temp_image_1 = int16(ones(512,512)*-135);
    else
        temp_image_1 = dicomread(fullFileName_Image);
        temp_image_1 = int16(temp_image_1) *Info.RescaleSlope + Info.RescaleIntercept; %% Intercept 
        temp_image_1(temp_image_1>=215) =215;  % Window-leveling
        temp_image_1(temp_image_1<=-135)= -135;
    end
    
    temp_image(:,:,k) = temp_image_1;
end
end

function Vol_processed = StandardVolumeTransformation(temp_image)  %%%% changing volume to fit Unet3d input 512x512x56
[~,~,c] = size(temp_image);
    if c > 56
        front1 = floor((c-56)/2);
        back1 = ceil((c-56)/2);
        Vol_processed = temp_image(:,:,1+front1:c-back1);
    elseif c < 56
        front2 = floor((56-c)/2);
        back2 = ceil((56-c)/2);
        Vol_processed = cat(3,int16(ones(512,512,front2)*-135),temp_image,int16(ones(512,512,back2)*-135));
    else
        Vol_processed = temp_image;
    end
end