@REM Commands to make windows testing easier

@REM cd ".stack-work/install/98756367/bin/"
@REM start /wait /b "" haskell-student-directory-exe.exe

@echo off
set /p args="Arguments: "

stack exec -- haskell-student-directory-exe %args% --path "C:/Users/Tristan Day/Code/Haskell/BS2220 - Functional Programming/haskell-student-directory/data/"
echo %ERRORLEVEL%