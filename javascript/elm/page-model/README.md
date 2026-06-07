# page-model

An Elm SPA demonstrating a page-based architecture with per-route layouts.

## Running

```
mise run dev
```

## Architecture

The application is split into two parallel hierarchies: **pages** and **layouts**. Both follow the same pattern — an opaque model, a local `Msg` type, and the standard `init / update / view / subscriptions` interface. `Main.elm` wires them together through a shared `Model` and a two-level `Msg` type.

### Module structure

```
src/
  Main.elm          -- Browser.application entry point
  Model.elm         -- shared Model, Msg, PageModel, LayoutModel
  Init.elm          -- init, initPage (sets page + layout per route)
  Update.elm        -- update, handlePage, handleLayout
  View.elm          -- view, viewPage, applyLayout
  Subscriptions.elm -- pageSubscriptions, layoutSubscriptions
  Route.elm         -- URL parser, Route type
  Pages/
    Home.elm
    About.elm
    NotFound.elm
  Layouts/
    Base.elm        -- stateful sidebar layout
    Container.elm   -- stateless wrapper layout
```

### Page model

Each page lives in `Pages/` and exposes:

```elm
title         : String
init          : ( Model, Cmd Msg )
update        : Msg -> Model -> ( Model, Cmd Msg )
view          : Model -> List (Html Msg)
subscriptions : Model -> Sub Msg
```

The active page is held in `PageModel`, a union type in `Model.elm`:

```elm
type PageModel
    = HomePage Home.Model
    | AboutPage About.Model
    | NotFoundPage
    | Loading
```

`NotFoundPage` and `Loading` carry no model because they have no state.

### Layout model

Each layout lives in `Layouts/` and wraps page content via `List (Html msg)`. There are two kinds:

- **Container** — stateless; only holds an opaque `Config`. `view` is a pure function with no local events.
- **Base** — stateful; holds an opaque `Config` and an opaque `Model`. Renders a toggleable sidebar with navigation links.

Both expose an opaque `Config` built through builder functions:

```elm
-- Layouts/Base.elm
config : Config                          -- default
withTitle : String -> Config -> Config   -- customise
```

The active layout is held in `LayoutModel`:

```elm
type LayoutModel
    = ContainerLayout Container.Config
    | BaseLayout Base.Config Base.Model
```

`Config` is stored alongside `Model` in the variant so it is always available at render time without threading it through the opaque `Model`.

### Routing

`Route.fromUrl` parses the URL and falls back to `NotFound`:

```elm
fromUrl : Url -> Route
fromUrl =
    parse parser >> Maybe.withDefault NotFound
```

`initPage` in `Init.elm` maps each `Route` to both a `PageModel` and a `LayoutModel`, so each route controls which layout it uses:

```elm
Home    -> setPage (HomePage m)    |> withBaseLayout
About   -> setPage (AboutPage m)   |> withBaseLayout
NotFound -> setPage NotFoundPage   |> withContainerLayout
```

### Msg design

The top-level `Msg` type stays stable as pages and layouts are added. New variants go into `PageMsg` or `LayoutMsg` instead:

```elm
type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | PageMsg PageMsg     -- never grows
    | LayoutMsg LayoutMsg -- never grows

type PageMsg             -- grows with new pages
    = HomeMsg Home.Msg
    | AboutMsg About.Msg

type LayoutMsg           -- grows with new stateful layouts
    = BaseMsg Base.Msg
```

`Update.elm` dispatches through `handlePage` and `handleLayout`. Each function pattern-matches its sub-message type, extracts the matching page or layout model, runs the local `update`, and maps the resulting command through function composition:

```elm
handlePage : PageMsg -> Model -> ( Model, Cmd Msg )
handlePage msg model =
    case msg of
        HomeMsg subMsg ->
            case model.page of
                HomePage subModel ->
                    let
                        ( m, cmd ) = Home.update subMsg subModel
                    in
                    ( { model | page = HomePage m }, Cmd.map (PageMsg << HomeMsg) cmd )
                _ ->
                    ( model, Cmd.none )
        ...
```

`Cmd`, `Sub`, and `Html` mappings all use function composition to build the two-level wrapper: `PageMsg << HomeMsg`, `LayoutMsg << BaseMsg`, etc.
