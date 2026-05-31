# BikeWise

> Cycle safely. Arrive dry.

BikeWise is an R package that helps cyclists decide when to leave. It plans
a cycling route between two locations, checks the weather forecast at
checkpoints along the way, and finds the earliest rain-free departure window
within the next 24 hours.

Your personal rain tolerance (none / light / moderate / heavy) determines
what counts as "dry enough" for your ride.

## Recommendations

After selecting your start and destination, BikeWise gives one of:

- **You're good to go!** — the route looks dry right now
- **Leave at HH:MM for a dry ride.** — a dry window is coming; leave then
- **No dry window today. Grab a raincoat** — no dry window in today's forecast
- **Built different. Just ride.** — for users who cycle in any weather

## Installation

```r
# install.packages("devtools")
devtools::install_github("sebkrbs/BikeWise")
```

## Quickstart

**Local mode** — no account or internet setup needed, stores data in local
CSV files:

```r
library(BikeWise)
StartCycling()
```

**Online mode** — stores data in Google Sheets, supports multiple devices.
Requires a one-time setup (see `vignette("online-setup", package = "BikeWise")`):

```r
library(BikeWise)
StartCyclingOnline()
```

## APIs

BikeWise uses three free, open APIs — no keys required:

- [Open-Meteo](https://open-meteo.com) — 15-minute precipitation forecasts
  over a 24-hour window
- [OSRM](https://project-osrm.org) — cycling route geometry and travel time
- [Nominatim](https://nominatim.org) — address geocoding via OpenStreetMap

An internet connection is required to fetch routes and weather data.
