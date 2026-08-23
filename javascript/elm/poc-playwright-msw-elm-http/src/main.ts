import { Elm } from "./Main.elm";

async function main() {
  if (import.meta.env.VITE_MSW !== "false") {
    const { worker } = await import("./mocks/browser");
    // Awaited so the worker is controlling the page before Elm's init fires
    // its first request, which would otherwise escape interception.
    await worker.start({ onUnhandledRequest: "warn" });
  }

  Elm.Main.init({ node: document.getElementById("app")! });
}

main();
