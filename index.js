import express from "express"
import cors from "cors"
import crypto from "crypto"
import {init} from "./update"

const app = express()
const whitelist = ["http://localhost:8000"]
const corsOptions = {
  origin(origin, callback) {
    const originIsWhitelisted = whitelist.indexOf(origin) !== -1
    callback(null, originIsWhitelisted)
  }
}

app.use(cors(corsOptions))
app.use(express.static(__dirname + "/public/"))

const http = require("http").Server(app)
const io = require("socket.io")(http)
const clients = []
let numberOfGames = 0

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
})

const PORT = process.env.PORT || 3009
http.listen(PORT, () => console.log("HELLO PORT : " + PORT))
