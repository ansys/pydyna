@ECHO OFF

pushd %~dp0

REM Command file for Sphinx documentation

if "%SPHINXBUILD%" == "" (
	set SPHINXBUILD=sphinx-build
)
set SOURCEDIR=source
set BUILDDIR=_build
set APIDIR=source\api

REM Set parallel build option (use SPHINXJOBS environment variable or default to auto)
if "%SPHINXJOBS%" == "" (
	set SPHINXJOBS=auto
)

if "%1" == "" goto help
if "%1" == "clean" goto clean
if "%1" == "pdf" goto pdf

%SPHINXBUILD% >NUL 2>NUL
if errorlevel 9009 (
	echo.
	echo.The 'sphinx-build' command was not found. Make sure you have Sphinx
	echo.installed, then set the SPHINXBUILD environment variable to point
	echo.to the full path of the 'sphinx-build' executable. Alternatively you
	echo.may add the Sphinx directory to PATH.
	echo.
	echo.If you don't have Sphinx installed, grab it from
	echo.https://www.sphinx-doc.org/
	exit /b 1
)

echo Starting Sphinx build with -j %SPHINXJOBS%...
echo Build started at %TIME%
%SPHINXBUILD% -M %1 %SOURCEDIR% %BUILDDIR% -j %SPHINXJOBS% %SPHINXOPTS% %O%
set BUILD_EXIT_CODE=%ERRORLEVEL%
echo Build finished at %TIME%
exit /b %BUILD_EXIT_CODE%
goto end

:clean
rmdir /s /q %BUILDDIR% > /NUL 2>&1
rmdir /s /q %APIDIR% > /NUL 2>&1
goto end

:help
%SPHINXBUILD% -M help %SOURCEDIR% %BUILDDIR% %SPHINXOPTS% %O%

:pdf
%SPHINXBUILD% -M lulatex %SOURCEDIR% %BUILDDIR% %SPHINXOPTS% %O%
call %BUILDDIR%/latex/make.bat
goto end

:end
popd
