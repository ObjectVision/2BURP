
REM Setup Chris
set config=D:\ProjDir\2BURP\cfg

REM Setup OVSRV06
rem set config=C:\ProjDir\Jip\2BURP\cfg

set geodmsversion=GeoDMS16.0.0
set exe_dir=C:\Program Files\ObjectVision\%geodmsversion%
set ProgramPath=%exe_dir%\GeoDmsRun.exe

cd ..\cfg

rem set studyarealist=Asia Australia_Oceania
rem set studyarealist=Asia Australia_Oceania Europe North_America South_America 
set studyarealist=Africa Asia Australia_Oceania Europe North_America South_America 
rem set studyarealist=Netherlands 


for %%s in (%studyarealist%) do (
	echo %%s 
	
	set StudyArea=%%s
	set Use_TempTifFiles=FALSE
	rem "%ProgramPath%" %config%\main.dms 	/Analysis/Calibration/Export_DataForCalibration/Result /Analysis/Calibration/Export_BuiltUp_percentages_matrix/Result
	rem "%ProgramPath%" %config%\main.dms 	/MakeUnlinkedData/c1_RoundPastPop
	rem "%ProgramPath%" %config%\main.dms 	/Analysis/Calibration/Export_DataForCalibration/Result
	rem "%ProgramPath%" %config%\main.dms 	/Analysis/Calibration/Export_BuiltUp_percentages_matrix/Result
	rem "%ProgramPath%" %config%\main.dms 	/Analysis/Calibration/Export_DataForCalibration/Export_Tiffs_ForCalibration
	rem "%ProgramPath%" %config%\main.dms /Preprocessing/DegreesOfUrbanisation/Generate_DegreesOfUrbanisation /Analysis/Future/Indicators/Prep/Generate_All
	rem "%ProgramPath%" %config%\main.dms /Analysis/Future/Indicators/Prep/Generate_All
	
	set Use_TempTifFiles=TRUE
	rem "%ProgramPath%" %config%\main.dms /Analysis/Future/Indicators/Export_CSVs
	"%ProgramPath%" %config%\main.dms /Analysis/Future/Indicators/long_time_window_analyses/store_histograms
)
pause