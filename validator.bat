@echo off 
echo "Validator start op.. dit duurt een paar seconden"
echo.
echo "Sluit dit scherm om de validator vervolgens weer af te sluiten"
"%~dp0..\..\synced-folder\R-4.2.3\bin\R.exe" CMD BATCH "run.r"