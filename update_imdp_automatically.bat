@echo off
C:
set "PATH=C:\Users\KHACKETT\AppData\Local\Programs\Python\Python315;%PATH%"
set "PATH=C:\Users\KHACKETT\AppData\Local\Programs\R\R-4.6.1\bin\x64;%PATH%"
cd C:\Users\KHACKETT\Documents\R_Projects\Long_Term_Projects\AIS_IMDP_Figures

REM Run Python Script
python download_annual_metabase_csv.py Options.csv

REM Run the R Script
Rscript update_imdp_data_automatically.R C:\Users\KHACKETT\Documents\R_Projects\Long_Term_Projects\AIS_IMDP_Figures\Options.csv
pause