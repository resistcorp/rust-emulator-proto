subst /D W:
subst W: %~dp0
@echo off
w:
wasm-pack build --target web --out-dir www/emulator --out-name emulator
start node first.js
set path=W:\tools;W:\tools\sjasmplus;%path%
cls
echo =============== Emulator ==============
echo Server started, use wasm-pack build --target web --out-dir www/emulator --out-name emulator
cmd /k
