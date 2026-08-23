# poc-playwright-msw-elm-graphql

Spike: does an Elm app whose GraphQL goes through `dillonkearns/elm-graphql`
work under Playwright when the only backend is MSW running in the browser as a
service worker?

Same app and same tests as `poc-playwright-msw-elm-http`, with the REST calls
replaced by GraphQL.

## Run it

```sh
mise install
aube install
aubx playwright install    # browser binaries
aubr codegen               # schema.graphql -> src/Api/**
aubr dev                   # http://localhost:5173, mocked
aubr test
```

`aubr dev:nomock` starts the app with `VITE_MSW=false`, which skips the worker.
There is no backend, so the app just renders its error state.

`dev` and `build` run `codegen` first, so a fresh clone works without running it
by hand. `src/Api/` is generated and not committed.
