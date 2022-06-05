subst /D W:
subst W: %~dp0
@echo off
w:
set path=W:\tools;W:\tools\sjasmplus;%path%
cls
echo =============== Emulator ==============
node first.js
rem echo Server started, use wasm-pack build --target web --out-dir www/emulator --out-name emulator
rem cmd /k
