%%
function Out = MainFunc(folderPath)
try
out = PreprocessingFunction(folderPath);
if length(out) == 1
    disp(out{1})
    Out = out;
else
    fields = {'Status','PatientName','EATMask','SACMask','TotalEATVol_cm3'};
    Out = cell2struct(out,fields,2);
    disp(out{1})
end
catch ME
    Out = ME;
    disp(ME)
end
end