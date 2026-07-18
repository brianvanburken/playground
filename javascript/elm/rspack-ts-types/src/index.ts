import { Elm } from "./Main.elm";

const storedCount = Number(localStorage.getItem("count")) || 0;

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: { count: storedCount },
});

app.ports.saveCount.subscribe((count) => {
  localStorage.setItem("count", String(count));
});
