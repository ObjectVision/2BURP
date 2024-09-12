set pf="Program Files"
set config=D:\ProjDir\2BURP\cfg
set geodmsversion=GeoDMS15.4.0

set studyarealist=Europe South_America Africa Asia North_America Australia_Oceania

for %%s in (%studyarealist%) do (
	echo %%s> %config%\studyarea.txt
	
	C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe %config%\main.dms /Preprocessing/DegreesOfUrbanisation/Generate_DegreesOfUrbanisation 
	C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe %config%\main.dms /Analysis/Future/Indicators/Prep/Generate_All	
	
)
pause