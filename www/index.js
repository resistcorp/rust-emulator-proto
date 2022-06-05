import {default as init, create_emulo, version} from "./pkg/emulator.js";

let emulo;
let screenData;
const screen = document.getElementById("screen");
const ctx = screen.getContext("2d");
async function startup(){
	await init();
	emulo = create_emulo();
	screenData = emulo.take();
	console.log(version());
	screen.width = emulo.size.width;
	screen.height = emulo.size.height;

	getMemory()
	frame(0.0)
}

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
	let flag_names = ["S","Z","Y","H","X","P","N","C"];
	let ret = "";
	let state = emulo.get_state();
	let flags = emulo.get_flags();
	ret += flag_names.map(n => `${n} : ${flags[n+"F"]}`).join(" ") + "\n";
	for(let [key, val] of Object.getOwnPropertyNames(state.__proto__)
		.map(k => [k, state[k]])
		){
		if(key && key != "ptr" && typeof val != "function")
			ret += key + " : " + val + "\n";
	}
	return `<pre>${ret}</pre>`
}

startup();