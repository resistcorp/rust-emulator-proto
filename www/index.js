import * as wasm from "rust-emulator-proto";

const screen = document.getElementById("screen");
const emulo = wasm.init();
let screenData = emulo.take();
console.log(wasm.version());
screen.width = emulo.size.width;
screen.height = emulo.size.height;

const ctx = screen.getContext("2d");

function frame(time){
	let delta = emulo.tick(time);
	if(delta > 1000/60){
		screenData = emulo.swap(screenData);
		let data = new ImageData(new Uint8ClampedArray(screenData.buffer), screen.width, screen.height);
		ctx.putImageData(data, 0, 0);
		getMemory();
	}
	requestAnimationFrame(frame);
}
export function getMemory(){
	let buff = new Uint8Array(256);
	let pos = emulo.mem_around_pc(buff);
	console.log("we are at position", pos)
	console.log("current memory is", [...buff].map(i => "0x" + i.toString(16)))
	getState()
	document.getElementById("state").innerHTML = getState()
}
export function advance(){
	console.log(emulo.clock());
	getMemory();
}

export function pause(){
	emulo.pause();
}
export function start(){
	emulo.start();
}
export function getState(){
	let ret = "";
	let state = emulo.get_state();
	let flags = emulo.get_flags();
	ret += Object.entries(flags).map(([k, v]) => `${k} : ${v}`).join(" ");
	for(let [key, val] of Object.getOwnPropertyNames(state.__proto__)
		.map(k => [k, state[k]])
		){
		if(key && typeof val != "function")
			ret += key + " : " + val + "\n";
	}
	return `<pre>${ret}</pre>`
}

getMemory()
frame(0.0)