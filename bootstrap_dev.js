// A dependency graph that contains any wasm must all be imported
// asynchronously. This `bootstrap.js` file does the single async import, so
// that no one else needs to worry about it again.
import("./index.js")
  .then(module => {
    window.EMUL = module;
  })
  .catch(e => console.error("Error importing `index.js`:", e));

let generation;
async function check(){
  let reload = false;
  const timeout = 500;
  const abortController = new AbortController();
  const abort = setTimeout(() => abortController.abort(), timeout);
  
  try{
    let req = await fetch("/generation", { method: 'POST', signal: abortController.signal });
    let gen = parseInt(await req.text(), 10);
    reload = generation != undefined && gen != generation
    generation = gen;
  }catch(e){
    console.error(e);
    if(e.name == "TypeError"){
      document.body.innerHTML = "<H1>server out, closing</H1>"
      setTimeout(_=>window.close(), 2500);
    }
  }finally{
    clearTimeout(abort);
    if(reload){
      document.body.innerHTML = "<H1>outdated, reloading</H1>"
      setTimeout(_=>location.reload(), 1500);
    }else{
      setTimeout(check, 500);
    }
  }
}
check();
