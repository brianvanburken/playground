import { defineConfig, type Plugin } from "vite";
import elm from "vite-plugin-elm";

// vite-plugin-elm builds its HMR dependency list with `deps.join('", "')`, which
// renders a single empty specifier when an Elm module imports no local modules.
// Vite 8's import-analysis rejects that; older Vite tolerated it.
// Drops the call entirely, as proposed upstream in
// https://github.com/hmsk/vite-plugin-elm/issues/863
const fixEmptyElmHmrDeps = (): Plugin => ({
  name: "fix-empty-elm-hmr-deps",
  enforce: "post",
  transform(code, id) {
    if (!id.endsWith(".elm")) {
      return null;
    }

    return code.replace(
      /import\.meta\.hot\.accept\(\[\s*""\s*\],[\s\S]*?\}\)/g,
      "",
    );
  },
});

export default defineConfig({
  plugins: [elm(), fixEmptyElmHmrDeps()],
  server: {
    port: 5173,
    strictPort: true,
  },
});
