var express = require('express');
var cors = require('cors');
var crypto = require("crypto")
var clients = []
var app = express()

var whitelist = ["http://0.0.0.0:8000"];
var corsOptions = {
  origin: function(origin, callback){
    var originIsWhitelisted = whitelist.indexOf(origin) !== -1;
    callback(null, originIsWhitelisted);
  }
};

app.use(cors(corsOptions))
app.use(express.static(__dirname + '/'));
var http = require('http').Server(app)
var io = require("socket.io")(http)

io.on("connection", function(client) {
  client.id = id()
  clients.push(client)

  client.on("SELF_UPDATE", function(data) {
    clients.map(function(socket) {
      if (socket.id === client.id)
        return

      socket.emit("OPPONENT_UPDATE", data)
    })
  })
})

io.on("error", console.log)

function id() {
  return crypto.randomBytes(12).toString("hex")
}

app.get('/', function(request, response) {
  fs.readFile(__dirname + "/index.html", "utf8",  function(err, file) {
    console.log(err)
    response.send(file)
  })
});

http.listen(process.env.PORT || 3009)
