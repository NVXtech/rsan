@echo off
cls 
for %%a in (*.dot) do (
    echo gerando figura para %%a
    @REM E:\Graphviz\bin\dot.exe -Tpng -o %%~na.png %%a
    E:\Graphviz\bin\dot.exe -Tsvg -o %%~na.svg %%a
)
