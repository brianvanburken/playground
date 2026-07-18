import { Elm } from "./Main.elm";

const node = document.getElementById("app");
if (!node) {
  throw new Error("Missing #app element");
}

const storedCount = Number(localStorage.getItem("count")) || 0;

const app = Elm.Main.init({
  node,
  flags: { count: storedCount },
});

app.ports.saveCount.subscribe(async (count) => {
  localStorage.setItem("count", String(count));
});
