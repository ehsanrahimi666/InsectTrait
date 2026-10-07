@echo off
setlocal
REM Push the InsectTrait folder to https://github.com/ehsanrahimi666/InsectTrait
REM Needs Git for Windows (https://git-scm.com) and a GitHub sign-in.
REM Run from the InsectTrait folder after editing the package: double-click this file
REM or type upload_to_github.cmd. Works for a folder that is not yet a Git repository
REM (it is linked to the existing GitHub repository first) and for an existing clone.

cd /d "%~dp0"
set REPO=ehsanrahimi666/InsectTrait
set URL=https://github.com/%REPO%.git

where git >nul 2>nul
if errorlevel 1 (
  echo Git is not installed. Install it from https://git-scm.com and run this file again.
  pause
  exit /b 1
)

for /f "tokens=2" %%v in ('findstr /b "Version:" DESCRIPTION') do set VERSION=%%v

if not exist .git (
  git init
  git remote add origin %URL%
  git fetch origin
  git rev-parse --verify --quiet origin/main >nul
  if errorlevel 1 (git checkout -b main) else (git reset origin/main)
)

git add -A
git diff --cached --quiet
if errorlevel 1 git commit -m "Update InsectTrait (version %VERSION%)"
git branch -M main
git push -u origin main
if errorlevel 1 (
  echo.
  echo Push failed. Check that you are signed in to GitHub and can push to %URL%.
  pause
  exit /b 1
)

git ls-remote --exit-code --tags origin v%VERSION% >nul 2>nul
if errorlevel 1 (
  git tag -a v%VERSION% -m "InsectTrait %VERSION%"
  git push origin v%VERSION%
)
echo.
echo Done. Install with: remotes::install_github("%REPO%")
pause
