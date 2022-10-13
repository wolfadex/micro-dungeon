import * as elmAnsi from '../elm-dependencies/elm-ansi/dist/elm-ansi.js'
import { Elm } from './elm.js'

elmAnsi.init()

let app

elmAnsi.onRawData(function (data) {
  // Close on Esc or Ctrl+c
  if (data === '\x1B' || data === '\u0003') {
    process.exit(0)
  }

  app.ports.stdin.send(data)
})

elmAnsi.onKeypress(function (key) {
  app.ports.keypress.send(key)
})

app = Elm.Main.init({
  flags: Date.now(),
})

app.ports.stdout.subscribe(function (data) {
  elmAnsi.writeToStdout(data)
})
