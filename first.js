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

const server = start_server();
open_browser();
build_pkg(false, true).then(_=>{
	open_browser();
	start_interface();
});
let autoreload=true;

let generation = 0;


// ==================================================== cmd line interface

async function start_interface() {
  const rl = readline.createInterface({
    input: process.stdin,
  	output: process.stdout
  });

  let run = true;
  const lines = rl[Symbol.asyncIterator]();

  do{
  	process.stdout.write("wyw ? >");
  	const {value, done} = await lines.next();
  	run &&= !done;
  	if(value){
	  	let [command, ...args] = value.split(" ");
	    switch(command){
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
	    		open_browser();
	    		break;
	    	case "gen" :
	    		console.log(`"${command}" : reloading.`);
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

   console.log("KTHXBYE")
   server?.close();
   process.exit();
}

// ==================================================== rust build script
async function build_pkg(verbose, startup){
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
		  console.log(`child process exited with code ${code}`);
		  resolve(code);
		});
	})
}

// ==================================================== http sever
function start_server(){
	const ret = http.createServer(async (request, result)=>{
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
	}).listen(port);
	console.log("server listening on", port);

	return ret;
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
function open_browser(){

   //warning untested except windows
	const url = "http://localhost:"+port;
		let command;
		if (osPlatform === WINDOWS_PLATFORM) {
		  command = `start ${url}`;
		} else if (osPlatform === MAC_PLATFORM) {
		  command = `open -a "Google Chrome" ${url}`;
		} else {
		  command = `google-chrome --no-sandbox ${url}`;
		}
		exec(command);
}