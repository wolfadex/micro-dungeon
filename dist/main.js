import * as elmAnsi from "../elm-dependencies/elm-ansi/examples/dist/elm-ansi.js";
import { Elm } from "./elm.js";

elmAnsi.init();

let app;

elmAnsi.onRawData(function (data) {
  app.ports.stdin.send(data);
});

// elmAnsi.onKeypress(function (key) {
//   app.ports.keypress.send(key)
// })

app = Elm.Main.init({
  flags: Date.now(),
});

app.ports.stdout.subscribe(function (data) {
  elmAnsi.writeToStdout(data);
});

app.ports.exit.subscribe(function (code) {
  process.exit(code);
});
