import express from "express"
import cors from "cors"
import crypto from "crypto"
const clients = []
const app = express()
import {init} from "./update"

var whitelist = ["http://0.0.0.0:8000"];
var corsOptions = {
  origin: function(origin, callback){
    var originIsWhitelisted = whitelist.indexOf(origin) !== -1;
    callback(null, originIsWhitelisted);
  }
};

app.use(cors(corsOptions))
app.use(express.static(__dirname + "/public/"));

var http = require("http").Server(app)
var io = require("socket.io")(http)
var numberOfGames = 0

io.on("connection", function(client) {
  if (!clients[numberOfGames])
    return clients.push([client])

  if (clients[numberOfGames].length === 1) {
    clients[numberOfGames].push(client)
    init(clients[numberOfGames])
    numberOfGames++
    return
  }
})

io.on("error", console.log)

function id() {
  return crypto.randomBytes(12).toString("hex")
}

app.get("/", function(request, response) {
  fs.readFile(__dirname + "/public/index.html", "utf8",  function(err, file) {
    console.log(err)
    response.send(file)
  })
});

http.listen(process.env.PORT || 3009)
