# profile-card

The Haskell program that renders the card at the top of the profile README.

It asks the GitHub GraphQL API for the account's repositories, commit
contributions and authored diffs, then writes two animated SVGs — one per colour
scheme — into `../assets/`. A [scheduled Action](../.github/workflows/profile-card.yml)
runs it every morning and commits the result.

## Layout

| Module | Responsibility |
| --- | --- |
| `ProfileCard.Config` | Everything meant to be edited by hand: handle, location, colours, output paths. |
| `ProfileCard.GitHub` | One POST to the GraphQL endpoint, with retries. |
| `ProfileCard.Stats` | The queries, and the aggregation into the numbers on the card. |
| `ProfileCard.Cache` | Per-repository line counts persisted in `../cache/loc.json`. |
| `ProfileCard.Render` | SVG output: grid layout, themes, the reveal animation. |
| `ProfileCard.Format` | Thousands separators, calendar-aware uptime, XML escaping. |

## Running it

The program writes paths relative to the working directory, so run it from the
repository root:

```sh
cd card && cabal build
cd .. && ACCESS_TOKEN="$(gh auth token)" "$(cd card && cabal list-bin profile-card)"
```

A token is required. A classic PAT with the `repo` scope also counts private
contributions; `GITHUB_TOKEN` or `gh auth token` works but only sees public
activity. The program reads `ACCESS_TOKEN`, then `GITHUB_TOKEN`, then `GH_TOKEN`.

Pass a login to render someone else's card without editing the config:

```sh
"$(cd card && cabal list-bin profile-card)" someone-else
```

## The line-count cache

Summing additions and deletions means walking every commit the user authored in
every repository, which is far too slow to redo daily. `cache/loc.json` stores a
per-repository total alongside the default-branch head it was computed at:

- head unchanged → the repository is skipped entirely, with no API call;
- head moved → only the commits newer than the last counted one are fetched;
- the last counted commit has vanished (a rebase or force-push) → that
  repository is recounted from scratch rather than double-counted.

Deleting the file is always safe; it just makes the next run slow.
