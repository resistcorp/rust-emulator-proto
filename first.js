import http from "http";
import fs from "fs/promises";
import paths from "path";
import {spawn, exec} from "child_process";
import readline from "readline";
import { platform } from "os";

const WINDOWS_PLATFORM = 'win32';
const osPlatform = platform(); 
const port = 8080;

//yes, it's silly to join 1 path, but when they move I'll be glad for the paths
const rust_src = paths.join("src");
const js_src = paths.join("www");
const pkg = paths.join("www", "pkg");

//used to stop the watches on folders
const ABORT = new AbortController();

let autoreload = true;
let generation = 0;

build_pkg(false, true).then(async _=>{
	let messaging = start_watch(ABORT);
	start_server(ABORT);
	await open_browser();
	start_interface(ABORT, messaging);
});

// ==================================================== cmd line interface

async function start_interface(abort_controller, messaging) {
	const {signal} = abort_controller;
  const rl = readline.createInterface({
    input: process.stdin,
  	output: process.stdout,
  	signal
  });


  const question = async prompt => new Promise((resolve, reject)=>{
  	const local_abort = new AbortController();
  	let event = reason => {
  		local_abort.abort(reason);
  		resolve({error:reason, done:false});
  	};
  	signal.addEventListener("abort", event, { once: true });
  	const signal_l = local_abort.signal;//shadow variable
  	messaging(event)
  	try{
  		rl.question("wywd ?", {signal : signal_l}, value => resolve({value, done:false}));
  	}catch(error){
  		console.log(error);
  		console.log(signal_l.reason);
  		resolve({error, done:true});
  	}
  })

  let run = true;
  // const lines = rl[Symbol.asyncIterator]();

  console.log("process started. type help for help");
  do{
  	// process.stdout.write("wyw ? >");
  	const {value, error, done} = await question("wywd ?");
  	run &&= !done;
  	if(error){//something asked us to wait
  		await Promise.resolve(error);
  		continue;
  	}
  	if(value){
	  	let [command, ...args] = value.split(" ");
	    switch(command.toLowerCase()){
	    	case "h" :
	    	case "-h" :
	    	case "?" :
	    	case "/?" :
	    	case "/H" :
	    	case "halp" :
	    	case "help" :
	    		console.log("build watcher process :: usage");
	    		console.log("");
	    		console.log("* build [arg]         : starts a build of the rust wasm module");
	    		console.log("                       (any arg will trigger verbose mode)");
	    		console.log("* auto [on/off]       : start / stop autoreloading browsers");
	    		console.log("  autoreload [on/off] : same as auto");
	    		console.log("* [exit|kill|quit]    : stops this process and closes all browsers");
	    		console.log("* reload              : triggers a reload in all browsers browsers");
	    		console.log("* open                : open dev page in default browser");
	    		console.log("* [h|-h|?|/?|/H|help] : prints this help");
	    		console.log("");
	    		break;
	    	case "build" :
	    		console.log(`"${command}" : building rust code`);
	    		if(args.length)
		    		console.log(`VERBOSE`);
		    	else
		    		console.log(`SILENT (add arg to build verbose)`);
	    		switch(await build_pkg(...args)){
	    			case 0 :
	    				console.log("OK, it worked");
	    				break;
	    			default:
	    				console.log("build failed");
	    				if(args.length == 0)
		   				console.log("build failed. add any arg to see output");
	    		}
	    		break;
	    	case "open" :
	    		await open_browser();
	    		break;
	    	case "reload" :
	    	case "gen" :
	    		console.log(`"${command}" : forcing reload.`);
	    		generation++;
	    		break;
	    	case "auto" :
	    	case "autoreload" :
	    		let arg = args[0] || "on"
	    		let val = ["on", "true", "yes", "1"].includes(arg.toLowerCase());
	    		let value = val ? "ON" : "OFF";
	    		if(val == autoreload){
	    			console.log("autoreload already", value )
	    		}else{
	    			autoreload = val;
	    			console.log("autoreload set to", value )
	    		}
	    		if(val)
	    			generation++;
	    		break;
	    	case "exit" :
	    	case "quit" :
	    	case "kill" :
	    		run = false;
	    		console.log(`"${command}" : killing process.`);
	    		break;
	    	default :
	    		console.log("unknown command", command, args);
	  	}
  	}
   }while(run);

   abort_controller.abort();
   console.log("KTHXBYE")
}

// ==================================================== rust build script
async function build_pkg(verbose, startup){
	let time = 0;
	const id = setInterval(()=>{
		let len = time++%10
		process.stdout.write("\r"+"▓".repeat(len)+"░".repeat(10-len));
	}, 200);
	return new Promise((resolve, reject) => {
		let build = spawn("wasm-pack", ["build", "--target", "web", "--out-dir", "www/pkg", "--out-name", "emulator"]);
		if(startup)
			console.log(`building package. if first time it may be long`);

		if(verbose){
			build.stdout.on('data', (data) => {
			  console.log(`stdout: ${data}`);
			});

			build.stderr.on('data', (data) => {
			  console.error(`stderr: ${data}`);
			});
		}

		build.on('close', (code) => {
			clearInterval(id)
			if(code){
				console.log("rust code build failed");
			}else{
				console.log("rust code build complete");
			}
		  resolve(code);
		});
	});
}

// ==================================================== http sever
function start_server(abort_controller) {
	const {signal} = abort_controller;
	const server = http.createServer(async (request, result)=>{
		try{
			if(request.method == "POST" && request.url == "/generation"){
			  result.writeHead(200, { 'Content-Type': "text/plain" });
			  result.end(generation.toString());
				return;
			}
			const path = getActualPath(request.url);
			const contentType = type(path);
			if(contentType){
				const contents = await fs.readFile(path);
			  result.writeHead(200, { 'Content-Type': contentType });
			  result.end(contents);
			}else{
			  result.writeHead(404, { 'Content-Type': 'text/plain' });
			  result.end('nope');
			}
		}catch(e){
			console.error("error in request", e)
		  result.writeHead(500, { 'Content-Type': 'text/plain' });
		  result.end('error');
			console.log("wyw ?>");
		}
	});
	const sockets = new Set();
	signal.addEventListener('abort', (event) => {
		console.log("closing server, removing all connections");
		for (const socket of sockets)
    	socket.destroy();
    sockets.clear();
		server.close(cb=>console.log("server closed"));
	}
	, { once: true });
	server.on('connection', socket =>{
		sockets.add(socket);
		socket.once("close", _ => sockets.delete(socket));
	});

	server.listen(port);
	console.log(`HTTP server listening on port ${port}`);
}

function getActualPath(url){
	switch (url){
		case undefined:
		case "":
		case "/":
		case "/index.html":
			return paths.join("www", "index.html");
		case "/bootstrap.js":
			if(autoreload)
				return paths.join("bootstrap_dev.js");
			return paths.join("www", "bootstrap.js");
		default :
			return paths.join("www", url);
	}
}

function type(path){
	if(!path) return undefined;

	switch(paths.extname(path).toLowerCase()){
		case ".js" :
			return "text/javascript";
		case ".htm" :
		case ".html" :
			return "text/html";
		case ".wasm" :
			return "application/wasm";
		default:
	}
}

// ====================================== misc
function start_watch(abort_controller) {
	const {signal} = abort_controller;
	let start_watcher = (async (folder, callback) => {
		  	try {
			    const watcher = fs.watch(folder, { signal });
			    for await (const event of watcher){
			  				// console.log(event)
				      callback();
				    }
			  } catch (err) {
			    if (err.name === 'AbortError')
			      return;
			    console.log("watcher error", err);
			  }
		});
	let build_callback = null;
	start_watcher(rust_src, debounce(event => {
		console.log("detected rust change. rebuilding");
		let promise = build_pkg();
		build_callback?.(promise);
	}));
	start_watcher(js_src, debounce(event => reload_browsers()));

	return (callback) => build_callback = callback;//the repl will call this
}
function reload_browsers(force){
	if(autoreload || force)
		generation++;
}
async function open_browser(){
	return new Promise((resolve, reject)=> {
	   //warning untested except windows
		const url = "http://localhost:"+port;
			let command;
			if (osPlatform === WINDOWS_PLATFORM) {
			  command = `start /B ${url}`;
			} else if (osPlatform === MAC_PLATFORM) {
			  command = `open -a "Google Chrome" ${url}`;
			} else {
			  command = `google-chrome --no-sandbox ${url}`;
			}
			exec(command, {}, ()=>{
				console.log("browser is open");
				resolve();
			});
	});
}
// ====================================== utilities
function debounce(func, time = 100) {
	let lastCallMilli = 0.0;
	return (...args) => {
		let now = performance.now();
		if(lastCallMilli + time < now){
			lastCallMilli = now;
			func(...args)
		}
	}
}