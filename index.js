var io = require("socket.io")(3009)
var crypto = require("crypto")
var clients = []

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

