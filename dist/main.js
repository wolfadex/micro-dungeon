import * as elmAnsi from "../elm-dependencies/elm-ansi/dist/elm-ansi.js";
import { Elm } from "./elm.js";

let app;

elmAnsi.init(function (data) {
  if (data === "\x1B" || data === "\u0003") {
    process.exit(0);
  }

  app.ports.stdin.send(data);
});

app = Elm.Main.init({
  flags: Date.now(),
});

app.ports.stdout.subscribe(function (data) {
  elmAnsi.writeToStdout(data);
});
