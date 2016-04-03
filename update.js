import Bacon from "baconjs"

const initPlayerInput = {
  x: 0,
  y: 0
}

const initState = {
  dt: 0,
  h: initPlayerInput,
  o: initPlayerInput
}

const initOutput = JSON.stringify(initState)
const eventTypes = {
  SELF_UPDATE: "SELF_UPDATE",
  OPPONENT_UPDATE: "OPPONENT_UPDATE"
}

export function init(clients) {
  const p1 = Bacon.fromEvent(clients[0], eventTypes.SELF_UPDATE)
    .map((json) => parseWithDefault(initPlayerInput, json))
  const p2 = Bacon.fromEvent(clients[1], eventTypes.SELF_UPDATE)
    .map((json) => parseWithDefault(initPlayerInput, json))


  const timer = Bacon.fromBinder((sink) => {
    let previousTime

    const id = setInterval(() => {
      const time = new Date().getTime()
      sink(time - previousTime || 0 / 600)
      previousTime = time
    }, 1000 / 60)

    return () => {
      clearInterval(id)
    }
  })

  Bacon.update(
    initState,
    [timer, p1, p2], (state, dt, p1, p2) => {
      state.dt = dt
      state.h = p1
      state.o  = p2
      return state
    },
    [timer, p1], (state, dt, p1) => {
      state.dt = dt
      state.h = p1
      return state
    },
    [timer, p2], (state, dt, p2) => {
      state.dt = dt
      state.o = p2
      return state
    },
    [timer], (state, dt) => {
      state.dt = dt
      return state
    }
  )
  .map((update) => encodeWithDefault(initOutput, update))
  .onValue((json) => {
    clients.map((client) => {
      client.emit(eventTypes.OPPONENT_UPDATE, json)
    })
  })
}

function encodeWithDefault(defaultStr, obj) {
  let encoded

  try {
    encoded = JSON.stringify(obj)
  } catch (err) {console.log(err)}

  return encoded || defaultStr
}

function parseWithDefault(defaultObj, json) {
  let parsed

  try {
    parsed = JSON.parse(json)
  } catch (err) {console.log(err)}

  return parsed || defaultObj
}

