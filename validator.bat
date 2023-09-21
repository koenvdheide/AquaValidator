@echo off 
echo "Validator is aan het opstarten"
echo "Sluit dit scherm om de validator af te sluiten"
"%~dp0..\..\synced-folder\R-4.2.3\bin\R.exe" CMD BATCH "run.r"