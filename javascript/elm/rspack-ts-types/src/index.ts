import { Elm as MainElm } from "./Main.elm";
import { Elm as GreetingElm } from "./Greeting.elm";

const node = document.getElementById("app");
if (!node) {
  throw new Error("Missing #app element");
}

const greetingNode = document.getElementById("greeting");
if (!greetingNode) {
  throw new Error("Missing #greeting element");
}

const storedCount = Number(localStorage.getItem("count")) || 0;

const app = MainElm.Main.init({
  node,
  flags: { count: storedCount },
});

app.ports.saveCount.subscribe(async (count) => {
  localStorage.setItem("count", String(count));
});

GreetingElm.Greeting.init({
  node: greetingNode,
  flags: { name: "rspack-ts-types" },
});
