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
        safeParse(p1),
        safeParse(p2)
      )
    },
    [timer, p1], function(state, dt, p1) {
      var h
      try {
        h = JSON.parse(p1)
      } catch (err) {
        h = state.h
        console.log(err)
      }

      return {
        ...state,
        dt,
        h
      }
    },
    [timer, p2], function(state, dt, p2) {
      var o
      try {
        o = JSON.parse(p2)
      } catch (err) {
        o = state.o
        console.log(err)
      }

      return {
        ...state,
        dt,
        o
      }
    },
    [timer], function(x, y) {
      var h = x.h || {x:0,y:0}
      var o = x.o || {x:0,y:0}
      return {dt: y, h: h, o: o}
    }
  )
  .map(function(update) {
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
  .log()
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

function safeParse(json) {
  try {
    var j = JSON.parse(p1)
  } catch (err) {console.log(err)}

  return j
}

