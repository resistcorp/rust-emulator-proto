## About this project

I'm doing it essentially for fun

<img src="res/inigo.jpg" alt="FUN" title="FUN" height="125" /> 

I watched One Lone Coder's [NES emulator from scratch](https://www.youtube.com/watch?v=nViZg02IMQo&list=PLrOv9FMX8xJHqMvSGB_9G9nZZ_4IgteYf) videos, and it got me going

I do it in rust cause I want to understand the language better. Don't use my code to learn rust, it's not great

I usually do the code on stream at [my twitch channel](https://www.twitch.tv/resistcorp)

you should be able to start a live coding experience with `node first.js`, no install required. hopefully.

Only tested in windows

### premise
The SMS (as most early Sega machines) is based on the Zilog 80 processor. First point of order is t oemulate that.

Currently I'm parsing the [CSV](src/instructions.csv) (adapted from the table available [here](http://map.grauw.nl/resources/z80instr.php#iowaitnote))file and make boxed lambdas representing each variant (there's around 1000 allowed opcode of lenght 1-4 bytes)

Then we'll need to emulate the VDP but that's going to be easier

### setup 

- needs rust nightly for now until I lose the bigint dependency
- needs node >=16.0 for hot-reload during dev

again, you should be able to start everything with `node first.mjs`

on windows, [this batch](start_emulator_dev.bat) will setup a "usable" live code experience on virtual disk W:

feel free to propose similar ones for other os's

I used the wasm template to kickstart the project. You don't need to know about it but here it is

[tutorials]: https://rustwasm.github.io/docs/wasm-pack/tutorials/index.html
[template-docs]: https://rustwasm.github.io/docs/wasm-pack/tutorials/npm-browser-packages/index.html


### 🛠️ dev with `www/npm run dev` and `wasm-pack build`
### 🛠️ Build rust code with `wasm-pack build` (not autobuilt for now, but it hot-reloads)


## Licence
[WTFPL](http://www.wtfpl.net/) You're free to copy my terrible code and make it even worse. Don't use it on important things though.

If you do make it better I'd like to know, thanks

## 📚 resources
* [the opcode reference chart](http://z80-heaven.wikidot.com/opcode-reference-chart)
* [SMS POWER!](https://www.smspower.org/Development/Documents)

## 🔋 Batteries Included

* [`wasm-bindgen`](https://github.com/rustwasm/wasm-bindgen) for communicating
  between WebAssembly and JavaScript.
* [`console_error_panic_hook`](https://github.com/rustwasm/console_error_panic_hook)
  for logging panic messages to the developer console.
* [`wee_alloc`](https://github.com/rustwasm/wee_alloc), an allocator optimized
  for small code size.
