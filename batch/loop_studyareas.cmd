
REM Setup Chris
REM set config=D:\ProjDir\2BURP\cfg

REM Setup OVSRV06
set config=C:\ProjDir\Jip\2BURP\cfg

set geodmsversion=GeoDMS15.7.1
set exe_dir=C:\Program Files\ObjectVision\%geodmsversion%
set ProgramPath=%exe_dir%\GeoDmsRun.exe

cd ..\cfg

set studyarealist=Europe South_America Africa Asia North_America Australia_Oceania
REM set studyarealist=Netherlands 

for %%s in (%studyarealist%) do (
	echo %%s 
	
	set StudyArea=%%s
	"%ProgramPath%" %config%\main.dms /Preprocessing/DegreesOfUrbanisation/Generate_DegreesOfUrbanisation 
	"%ProgramPath%" %config%\main.dms /Analysis/Future/Indicators/Prep/Generate_All
)
pause