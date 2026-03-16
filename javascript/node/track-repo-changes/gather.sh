#!/usr/bin/env bash
set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration — edit EXCLUDES to add more paths/files to ignore.
# Each entry is matched as a prefix against the full file path.
# ---------------------------------------------------------------------------
OUTPUT_FILE="${OUTPUT_FILE:-commits_data.json}"
SRC_DIR="${SRC_DIR:-src}"

EXCLUDES=(
  "src/Gql"
  "src/Translations.elm"
  "src/types.ts"
)

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------
check_prereqs() {
  local missing=()
  for cmd in git jq awk; do
    command -v "$cmd" &>/dev/null || missing+=("$cmd")
  done
  if (( ${#missing[@]} > 0 )); then
    echo "error: missing required tools: ${missing[*]}" >&2
    exit 1
  fi
}

check_git_repo() {
  if ! git rev-parse --git-dir &>/dev/null 2>&1; then
    echo "error: must be run from inside a git repository" >&2
    exit 1
  fi
}

github_base_url() {
  local remote
  remote=$(git remote get-url origin 2>/dev/null || echo "")
  [[ -z "$remote" ]] && echo "" && return
  remote=$(echo "$remote" | sed 's|git@github\.com:|https://github.com/|')
  remote=$(echo "$remote" | sed 's|\.git$||')
  echo "$remote"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
check_prereqs
check_git_repo

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[]" > "$OUTPUT_FILE"
fi

CACHED_HASHES=$(jq -r '.[].hash' "$OUTPUT_FILE" 2>/dev/null || true)
CACHED_COUNT=0
[[ -n "$CACHED_HASHES" ]] && CACHED_COUNT=$(echo "$CACHED_HASHES" | wc -l | tr -d '[:space:]')

GITHUB_BASE=$(github_base_url)
TOTAL=$(git rev-list --count HEAD 2>/dev/null || echo "0")

echo "found $TOTAL commits ($CACHED_COUNT cached)"

if (( TOTAL <= CACHED_COUNT )); then
  echo "nothing new to process"
  exit 0
fi

# ---------------------------------------------------------------------------
# Build a pipe-separated exclude regex for awk.
# ---------------------------------------------------------------------------
EXCL_PATTERN=""
for ex in "${EXCLUDES[@]}"; do
  [[ -n "$EXCL_PATTERN" ]] && EXCL_PATTERN+="|"
  EXCL_PATTERN+="^$(printf '%s' "$ex" | sed 's/[.[\*^$]/\\&/g')(/|$)"
done

TMP_CACHE=$(mktemp)
TMP_NEW=$(mktemp)

cleanup() { rm -f "$TMP_CACHE" "$TMP_NEW"; }
trap cleanup EXIT
trap 'echo ""; echo "interrupted — re-run to resume (fast)" >&2' INT TERM

printf '%s\n' "$CACHED_HASHES" > "$TMP_CACHE"

echo "processing via single git log pass..."

# ---------------------------------------------------------------------------
# Core: one git log call → awk → JSONL of new commits.
#
# Strategy: awk keeps running totals (elm/scss/ts line counts). For each
# commit it applies the numstat diff to update totals, then emits a JSON
# record for commits that are not already in the cache.
#
# --no-renames: emit delete+add pairs instead of rename notation so paths
#               are always plain file paths.
# ---------------------------------------------------------------------------
git log --reverse --no-renames --numstat --format="GATHERCOMMIT %H %at %s" \
| awk \
    -v excl_pattern="$EXCL_PATTERN" \
    -v github_base="$GITHUB_BASE" \
    -v cache_file="$TMP_CACHE" \
    -v src_dir="$SRC_DIR" \
  '
  BEGIN {
    while ((getline line < cache_file) > 0)
      if (line != "") cached[line] = 1
    close(cache_file)
    elm=0; scss=0; ts=0; hash=""; tstamp=0; msg=""
  }

  function json_escape(s,    out, i, c) {
    out = ""
    for (i = 1; i <= length(s); i++) {
      c = substr(s, i, 1)
      if      (c == "\\") out = out "\\\\"
      else if (c == "\"") out = out "\\\""
      else if (c == "\n") out = out "\\n"
      else if (c == "\r") out = out "\\r"
      else if (c == "\t") out = out "\\t"
      else                out = out c
    }
    return out
  }

  function emit(    short, url) {
    if (hash == "" || (hash in cached)) return
    short = substr(hash, 1, 7)
    url   = (github_base != "" ? github_base "/commit/" hash : "")
    # date is omitted here; jq derives it from timestamp after the fact.
    printf "{\"hash\":\"%s\",\"shortHash\":\"%s\",\"message\":\"%s\",\"timestamp\":%d,\"lines\":{\"elm\":%d,\"scss\":%d,\"typescript\":%d},\"url\":\"%s\"}\n",
      hash, short, json_escape(msg), tstamp, elm, scss, ts, url
  }

  /^GATHERCOMMIT [0-9a-f]/ {
    emit()
    hash=$2; tstamp=$3+0
    msg = ""
    for (i=4; i<=NF; i++) msg = msg (i>4 ? " " : "") $i
    next
  }

  /^$/ { next }

  {
    if ($1 == "-") next                                          # binary file
    if ($1+0 == 0 && $2+0 == 0) next                           # no change
    added=$1+0; removed=$2+0; path=$3
    if (path !~ ("^" src_dir "/")) next                        # outside SRC_DIR
    if (excl_pattern != "" && path ~ excl_pattern) next        # excluded path
    if      (path ~ /\.elm$/)     elm  += added - removed
    else if (path ~ /\.scss$/)    scss += added - removed
    else if (path ~ /\.(ts|js)$/) ts   += added - removed
  }

  END { emit() }
  ' > "$TMP_NEW"

NEW_COUNT=$(wc -l < "$TMP_NEW" | tr -d '[:space:]')

if [[ "$NEW_COUNT" -eq 0 ]]; then
  echo "nothing new to process"
  exit 0
fi

echo "merging $NEW_COUNT new entries..."

jq -s '
  (.[0] + .[1])
  | map(.date = (.timestamp | strftime("%Y-%m-%d")))
  | sort_by(.timestamp)
' \
  "$OUTPUT_FILE" \
  <(jq -s '.' "$TMP_NEW") \
  > "${OUTPUT_FILE}.tmp" \
  && mv "${OUTPUT_FILE}.tmp" "$OUTPUT_FILE"

echo "done — wrote $NEW_COUNT new entries to $OUTPUT_FILE"
