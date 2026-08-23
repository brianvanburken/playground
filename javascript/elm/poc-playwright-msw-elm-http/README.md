# poc-playwright-msw-http

Spike: does an Elm app whose HTTP goes through `elm/http` work under Playwright
when the only backend is MSW running in the browser as a service worker?

## Run it

```sh
mise install
aube install
aubx playwright install    # browser binaries
aubr dev                   # http://localhost:5173, mocked
aubr test
```

`aubr dev:nomock` starts the app with `VITE_MSW=false`, which skips the worker.
There is no backend, so the app just renders its error state.

## Shape

- Elm 0.19.2 app, `elm/http` + `elm/json`, add / toggle / delete / list
- MSW browser mode only. No `setupServer`, no stub server, no `@msw/playwright`
- Handlers live in one place, `src/mocks/handlers.ts`, over a module-level store
  seeded with 2 todos
- Playwright drives a real browser against the service worker
