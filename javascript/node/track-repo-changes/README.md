# track-repo-changes

Visualise lines of code (Elm / SCSS / TypeScript+JS) across every commit of a
git repository to showcase refactoring progress over time.

## Files

| File | Purpose |
|------|---------|
| `gather.sh` | Walks all commits and writes `commits_data.json` |
| `visualize.html` | Opens in a browser to display the interactive chart |

---

## Requirements

The following tools must be available on your `PATH`:

- `git`
- `jq`
- `rg` (ripgrep)

---

## Step 1 — gather data

Run `gather.sh` from inside the repository you want to analyse:

```sh
cd /path/to/your-repo
/path/to/gather.sh
```

This produces `commits_data.json` in the current directory.  Re-running the
script is safe: already-processed commits are cached and skipped, so only new
commits are added.

### Options (environment variables)

| Variable | Default | Description |
|----------|---------|-------------|
| `OUTPUT_FILE` | `commits_data.json` | Path to the output / cache file |
| `SRC_DIR` | `src` | Root directory to scan inside the repository |

```sh
OUTPUT_FILE=data/my-repo.json SRC_DIR=app ./gather.sh
```

### Adding excludes

Open `gather.sh` and edit the `EXCLUDES` array near the top:

```bash
EXCLUDES=(
  "src/Gql"            # entire directory
  "src/Translations.elm"
  "src/types.ts"
  "src/Generated"      # add more entries here
)
```

Each entry is matched as a path prefix against the full file path returned by
`git ls-tree`.

---

## Step 2 — visualise

The `visualize.html` file reads `commits_data.json` via `fetch`, so it must be
served over HTTP — opening it as a `file://` URL will be blocked by CORS.

The simplest way:

```sh
# From the directory that contains both files:
npx serve .
# then open http://localhost:3000/visualize.html
```

Or with Python:

```sh
python3 -m http.server 8080
# then open http://localhost:8080/visualize.html
```

If you saved the JSON under a different name/path, update the `DATA_FILE`
constant near the top of `visualize.html`:

```js
const DATA_FILE = 'commits_data.json';
```

---

## Chart interaction

- **Hover** over a bar to see the commit hash, message, date, per-language line
  counts, and the delta relative to the previous commit (green = added,
  red = removed).
- **Click** a bar to open the commit on GitHub (only available when the
  repository has a `github.com` remote).

The chart uses a stacked bar layout so the height of each bar represents total
lines, with each colour segment showing one language.
