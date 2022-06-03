subst /D W:
subst W: %~dp0
@echo off
w:
wasm-pack build
pushd www
start npm run start
popd
set path=W:\tools;W:\tools\sjasmplus;%path%
cls
echo =============== Emulator ==============
echo Server started, use wasm-pack build
cmd /k
