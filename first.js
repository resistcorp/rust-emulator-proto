import http from "http";
import fs from "fs/promises";
import paths from "path";

let port = 8080;
const server = http.createServer(async (request, result)=>{
	try{
		const path = getActualPath(request.url);
		console.log("requested", request.url, path)
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
	}
}).listen(port);
console.log("listening on", port);

function getActualPath(url){
	switch (url){
		case undefined:
		case "":
		case "/":
		case "index.html":
			return paths.join("www", "index.html");
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

