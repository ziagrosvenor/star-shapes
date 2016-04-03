var Bacon = require("baconjs")

export function init(clients) {
  const p1 = Bacon.fromEvent(clients[0], "SELF_UPDATE")
  const p2 = Bacon.fromEvent(clients[1], "SELF_UPDATE")

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
    {},
    [timer, p1, p2],
    function(state, dt, p1, p2) {
      return mergeState(
        state,
        dt,
        parseWithDefault(state.h, p1),
        parseWithDefault(state.o, p2)
      )
    },
    [timer, p1], function(state, dt, p1) {
      return {
        ...state,
        dt,
        h: parseWithDefault(state.h, p1)
      }
    },
    [timer, p2], function(state, dt, p2) {
      return {
        ...state,
        dt,
        o: parseWithDefault(state.o, p2)
      }
    },
    [timer], function(state, dt) {
      return {
        ...state,
        dt
      }
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

function mergeState(state, dt, h, o) {
  if (!o && !h) {
    return {
      ...state,
      dt
    }
  }

  if (!o) {
    return {
      ...state,
      dt,
      h
    }
  }

  if (!h) {
    return {
      ...state,
      dt,
      o
    }
  }

  return {
    ...state,
    dt,
    h,
    o
  }
}

function parseWithDefault(defaultObj, json) {
  let parsed = undefined

  try {
    parsed = JSON.parse(json)
  } catch (err) {console.log(err)}

  return parsed || defaultObj
}

