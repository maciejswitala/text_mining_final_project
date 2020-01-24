var fs = require('fs');
fs.readFile('./Depression data2.json', "utf8", function (err, data) {
  if (err) {
    throw err; 
  }
  const dataObject = JSON.parse(data);
  const ids = [];
  const texts = [];
  const result = dataObject.forEach((x) => {
	  ids.push(x.id);
	  texts.push(x.text);
  });
  
  fs.writeFile("ids.txt", JSON.stringify(ids), ()=>{}); 
  fs.writeFile("texts.txt", JSON.stringify(texts), ()=>{}); 

}, ()=>{});