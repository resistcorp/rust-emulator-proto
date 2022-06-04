## About this project

I'm doing it essentially for fun (![inigo montoya](inigo.jpg "FUN"))

I watched One Lone Coder's [NES emulator from scratch](https://www.youtube.com/watch?v=nViZg02IMQo&list=PLrOv9FMX8xJHqMvSGB_9G9nZZ_4IgteYf) videos, and it got me going

I do it in rust cause I want to understand the language better. Don't use my code to learn rust, it's not great

I usually do the code on stream at [my twitch channel](https://www.twitch.tv/resistcorp)

### premise
The SMS (as most early Sega machines) is based on the Zilog 80 processor. First point of order is t oemulate that.
Currently I'm parsing the [CSV](src/instructions.csv) file and make boxed lambdas representing each variant (there's around 1000 allowed opcode of lenght 1-4 bytes)

Then we'll need to emulate the VDP but that's going to be easier

### setup 
I used the wasm template to kickstart the project. It is going away soon

[tutorials]: https://rustwasm.github.io/docs/wasm-pack/tutorials/index.html
[template-docs]: https://rustwasm.github.io/docs/wasm-pack/tutorials/npm-browser-packages/index.html

- needs rust nightly for now until I lose the bigint dependency
- needs npm for hot-reload during dev

you'll need to run `npm install` in the www folder until I get rid of the dependencies and write my dev server.

### 🛠️ dev with `www/npm run dev` and `wasm-pack build`
### 🛠️ Build rust code with `wasm-pack build` (not autobuilt for now, but it hot-reloads)


## Licence
[WTFPL](http://www.wtfpl.net/) You're free to copy my terrible code and make it even worse.

If you do make it better I'd like to know, thanks

## 🔋 Batteries Included

* [`wasm-bindgen`](https://github.com/rustwasm/wasm-bindgen) for communicating
  between WebAssembly and JavaScript.
* [`console_error_panic_hook`](https://github.com/rustwasm/console_error_panic_hook)
  for logging panic messages to the developer console.
* [`wee_alloc`](https://github.com/rustwasm/wee_alloc), an allocator optimized
  for small code size.
