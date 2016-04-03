var Bacon = require("baconjs")

const initPlayerInput = {
  x: 0,
  y: 0
}

const initState = {
  dt: 0,
  h: initPlayerInput,
  o: initPlayerInput
}

export function init(clients) {
  const p1 = Bacon.fromEvent(clients[0], "SELF_UPDATE")
    .map((json) => parseWithDefault(initPlayerInput, json))
  const p2 = Bacon.fromEvent(clients[1], "SELF_UPDATE")
    .map((json) => parseWithDefault(initPlayerInput, json))

  var previousTime = undefined

  const timer = Bacon.fromBinder((sink) => {
    var id = setInterval(function() {
      var time = new Date().getTime()
      sink(time - previousTime || 0 / 600)
      previousTime = time
    }, 1000 / 60)

    return function() {
      clearInterval(id)
    }
  })

  Bacon.update(
    initState,
    [timer, p1, p2], function(state, dt, p1, p2) {
      state.dt = dt
      state.h = p1
      state.o  = p2
      return state
    },
    [timer, p1], function(state, dt, p1) {
      state.dt = dt
      state.h = p1
      return state
    },
    [timer, p2], function(state, dt, p2) {
      state.dt = dt
      state.o = p2
      return state
    },
    [timer], function(state, dt) {
      state.dt = dt
      return state
    }
  )
  .onValue(function(update) {
    try {
      var nextUpdate = JSON.stringify(update)
    } catch (err) {
      console.log(err)
    }

    clients.map(function(client) {
      client.emit("OPPONENT_UPDATE", nextUpdate)
    })
    return nextUpdate || update
  })
}

function parseWithDefault(defaultObj, json) {
  let parsed = undefined

  try {
    parsed = JSON.parse(json)
  } catch (err) {console.log(err)}

  return parsed || defaultObj
}

