# osmplaces

`get_places()` counts OpenStreetMap places (grocery stores, pharmacies, parks,
and the other SDOH place types in the bundled taxonomy) inside census tracts,
block groups, or ZIP code tabulation areas, as of a chosen date, using the
[ohsome API](https://docs.ohsome.org/).

```r
library(osmplaces)

# One row per block group, one count column per taxonomy label
get_places(
  state = "OH", county = "Cuyahoga", year = "2019",
  tags = "all", geography = "block_group"
)

# Selected tags, an exact date, a bounding box, and polygons returned as sf
get_places(
  bbox = c(-81.75, 41.45, -81.60, 41.55),   # xmin, ymin, xmax, ymax (WGS84)
  year = "2022-06-15", tags = c("grocery", "pharmacy", "bank"),
  geography = "tract", geometry = TRUE
)

places_taxonomy()   # labels, aliases, and the ohsome filter each one uses

# Map the counts (needs geometry = TRUE; install leaflet and/or ggplot2)
res <- get_places(state = "OH", county = "Cuyahoga", year = 2019,
                  tags = c("grocery", "pharmacy"), geography = "tract",
                  geometry = TRUE)
map_places(res)                                  # interactive; buttons switch tags
map_places(res, basemap = "none")                # no background tiles
map_places(res, "pharmacy", interactive = FALSE) # static ggplot2 map
```

The interactive map draws the geography once and recolours it when you pick a
tag, so maps with all tags stay small. Hover shows the selected tag's count;
click lists every non-zero tag for that geography. `basemap` can be `"esri"`
(default), `"osm"`, `"none"`, or a tile URL template. No key is needed for the
built-in options, but providers set their own terms, and some block embedded
viewers: OpenStreetMap tiles are refused when no referrer is sent, and CARTO
basemaps now require a key. Use `basemap = "none"` if tiles fail to load.

## Domain-Based Filtering

Instead of selecting individual place types, you can filter by SDOH (Social
Determinants of Health) domains. Each domain groups related place types that
together represent a health-relevant category. This simplifies queries and
aligns with public health research frameworks.

### Basic Usage: Filter by Domain

```r
# Get all food environment places (grocery, convenience, fast food, restaurants)
food_env <- get_places(
  state = "OH", county = "Cuyahoga", year = 2019,
  domains = "food_environment", geography = "tract"
)

# This is equivalent to, but simpler than:
# get_places(state = "OH", county = "Cuyahoga", year = 2019,
#   tags = c("grocery", "convenience_store", "fast_food", "restaurant_cafe"),
#   geography = "tract")

# Get all health services places
health <- get_places(
  state = "OH", county = "Cuyahoga", year = 2019,
  domains = "health_services", geography = "tract"
)
```

### Multiple Domains

Combine multiple domains in a single request:

```r
# Get food and health data together
food_health <- get_places(
  state = "OH", county = "Cuyahoga", year = 2019,
  domains = c("food_environment", "health_services"),
  geography = "tract", geometry = TRUE
)

# Result includes columns for all place types in both domains:
# geoid, geometry, grocery, convenience_store, fast_food, restaurant_cafe,
# pharmacy, healthcare
```

### Available Domains

The taxonomy includes 9 SDOH domains:

| Domain | Place Types |
| --- | --- |
| `food_environment` | Grocery stores, convenience stores, fast food, restaurants/cafes |
| `health_services` | Pharmacies, hospitals/clinics |
| `child_development` | Childcare, schools, playgrounds |
| `physical_activity` | Parks/greenspace, fitness centers |
| `social_environment` | Social services, libraries/museums, places of worship |
| `substance_environment` | Alcohol retail, bars/nightlife |
| `transportation` | Public transit stops/stations |
| `economic_environment` | Banks, ATMs, predatory financial services |
| `environmental_burden` | Industrial facilities |

### Use Cases

#### Use Case 1: Food Access Analysis
Study food environments across neighborhoods:

```r
# Get all food-related place types
food_places <- get_places(
  state = "CA", county = "Los Angeles",
  domains = "food_environment",
  geography = "block_group", geometry = TRUE,
  year = 2020
)

# Calculate accessibility with population data
pop <- data.frame(geoid = acs$GEOID, population = acs$estimate)
food_access <- calc_access(
  food_places, population = pop,
  catchment = 1  # 1-mile radius
)

# Map the results
map_places(food_access)
```

#### Use Case 2: Child Health and Development
Analyze resources available for children's development:

```r
# Get childcare, schools, and playgrounds
child_resources <- get_places(
  state = "TX", county = "Harris",
  domains = "child_development",
  geography = "tract", geometry = TRUE
)

# View distribution of resources
head(child_resources)
# geoid  childcare  education  play_child  geometry
```

#### Use Case 3: Health Service Disparities
Compare health service availability across regions:

```r
# Get pharmacies and healthcare facilities
health_places <- get_places(
  bbox = c(-87.5, 41.8, -87.6, 41.9),  # Chicago area
  domains = "health_services",
  geography = "block_group", geometry = TRUE
)

# Examine disparities
summary(health_places$pharmacy)
summary(health_places$healthcare)
```

#### Use Case 4: Physical Activity and Recreation
Study outdoor recreation access:

```r
# Get parks and fitness facilities
activity <- get_places(
  state = "CO", county = "Denver",
  domains = "physical_activity",
  geography = "tract", geometry = TRUE
)

# Calculate 10-minute accessibility
activity_access <- calc_access(
  activity, population = pop,
  catchment = 10/60  # 10 minutes straight-line distance
)
```

#### Use Case 5: Comprehensive Community Assessment
Analyze multiple health determinants simultaneously:

```r
# Get food, health, and social resources
community <- get_places(
  state = "MA", county = "Suffolk",
  domains = c("food_environment", "health_services", "social_environment"),
  geography = "block_group", geometry = TRUE
)

# Create accessibility scores for each domain
pop <- data.frame(geoid = acs$GEOID, population = acs$estimate)
community_access <- calc_access(community, population = pop, catchment = 1.5)

# Map multiple domains
map_places(community_access, "grocery")
map_places(community_access, "pharmacy")
map_places(community_access, "library")
```

### Domain Filtering Features

- **Case-insensitive**: `"Food Environment"`, `"food_environment"`, and
  `"food-environment"` all work the same way.
- **Flexible input**: Whitespace and punctuation are normalized automatically.
- **Precedence**: When both `tags` and `domains` are specified, `domains` takes
  precedence.
- **Backward compatible**: The `tags` parameter works exactly as before when
  `domains` is not specified.
- **Same output**: Results have identical structure to tag-based queries—one
  column per place type, same column naming.

## Taxonomy

The bundled taxonomy is exposed by `places_taxonomy()` and covers the following
place types:

| label | domain | description | aliases |
| --- | --- | --- | --- |
| `grocery` | `food_environment` | Grocery stores, supermarkets, and food retail outlets | `supermarket`, `food_retail` |
| `convenience_store` | `food_environment` | Small convenience and corner stores | `corner_store` |
| `pharmacy` | `health_services` | Retail pharmacies and dispensaries | — |
| `healthcare` | `health_services` | Hospitals, clinics, and primary care providers | `health_facility`, `medical` |
| `childcare` | `child_development` | Licensed childcare centers, nurseries, and kindergartens | — |
| `education` | `child_development` | Schools, colleges, and universities | `school` |
| `parks_greenspace` | `physical_activity` | Parks, gardens, and nature reserves | `park`, `greenspace`, `green_space` |
| `play_child` | `child_development` | Playgrounds and child-oriented recreation spaces | `playground` |
| `fitness` | `physical_activity` | Fitness centers and outdoor fitness stations | `gym`, `fitness_center` |
| `fast_food` | `food_environment` | Fast food restaurants and food courts | — |
| `restaurant_cafe` | `food_environment` | Sit-down restaurants and cafes | `restaurant`, `cafe` |
| `social_services` | `social_environment` | Social service facilities and community support organizations | — |
| `transit` | `transportation` | Public transit stops and stations | `transit_stop`, `bus_stop` |
| `financial_mainstream` | `economic_environment` | Banks and ATMs | `bank` |
| `financial_predatory` | `economic_environment` | Predatory financial services (payday lenders, pawnbrokers, check cashers) | `payday_loan`, `pawnbroker` |
| `alcohol_retail` | `substance_environment` | Alcohol retail stores (off-license) | `liquor_store`, `off_licence` |
| `bars_nightlife` | `substance_environment` | Bars, pubs, and nightclubs | `bar`, `pub` |
| `cultural_civic` | `social_environment` | Libraries, arts centers, theaters, and museums | `library`, `museum`, `arts` |
| `religious` | `social_environment` | Places of worship | `place_of_worship`, `church` |
| `industrial` | `environmental_burden` | Industrial land use and manufacturing facilities | `industry` |

## Accessibility: two-step floating catchment area

`calc_access()` turns counts into scores that account for how many people share
those places, using the
[accessibility](https://ipeagit.github.io/accessibility/) package. For each
geography it sums the places-per-person ratios of every geography within the
`catchment`.

```r
d <- get_places(state = "OH", county = "Cuyahoga", year = 2019,
                tags = c("grocery", "pharmacy"), geography = "block_group",
                geometry = TRUE)

# population: a column of `d`, or a data frame with `geoid` and `population`
# (for example ACS B01003 from tidycensus, using the same boundary year)
pop <- data.frame(geoid = acs$GEOID, population = acs$estimate)

a <- calc_access(d, population = pop, catchment = 1)   # 1 mile, straight-line
a$access_grocery                                        # grocery per 1,000 residents
map_places(a)                                           # maps the access scores

# Distance decay (enhanced 2SFCA), travel times from r5r, balanced FCA
calc_access(d, population = pop, catchment = 3,
            decay = accessibility::decay_exponential(0.8))
calc_access(d, population = pop, catchment = 15,
            travel_matrix = times, travel_cost = "travel_time_p50")
calc_access(d, population = pop, catchment = 1, method = "bfca")
```

Things to know:

- Distances are great-circle distances between geography centroids, and supply
  and population are both treated as sitting at the centroid. That is coarse
  for large geographies; block groups and tracts in cities work best.
- `catchment` is required because scores are very sensitive to it. Without
  `travel_matrix` it is in `distance_units` (`"miles"`, `"km"` or `"meters"`);
  with one it is in the matrix's units, such as minutes.
- `NA` means the catchments around a geography hold no people, so the ratio is
  undefined. A score of 0 means no places are reachable.
- Scores are relative to the geographies in `d`. Geographies at the edge of
  the area have neighbours that are missing, which understates their scores.
  Pull a larger area (such as the whole state) and subset afterwards.
- Match `boundary_year` in `get_places()` to the vintage of your population
  data.

| Argument | Meaning |
| --- | --- |
| `state`, `county`, `bbox` | Area of interest: a state, counties in a state, or a box. |
| `geography` | `"block_group"`, `"tract"`, or `"zip_code"` (ZCTA). |
| `year` | `2019` or `"2019"` is January 1 of that year; an ISO date or `Date` is used as given; `NULL` is January 1 of this year. |
| `tags` | `"all"`, or taxonomy labels/aliases. |
| `geometry` | `TRUE` returns an `sf` object with boundaries. |
| `boundary_year` | TIGER vintage; defaults to the snapshot year (2010 to `getOption("tigris_year", 2022)`). |
| `api`, `api_key` | `"v1"` (default, keyless) or `"v2"` (needs a key). |

## ohsome API versions

The keyless v1 API (`https://api.ohsome.org/v1`) shuts down on **2026-11-30**.
Its replacement, v2, requires a free API key
(<https://account.heigit.org/signup>) and accepts only one polygon per request,
so `api = "v2"` makes one request per geography per tag. Pass the key with
`api_key` or the `OHSOME_API_KEY` environment variable. Use
`options(osmplaces.ohsome_url = ...)` to point at another server.

## Notes

- Counts are OSM elements, not de-duplicated places. See the `notes` in
  `inst/extdata/taxonomy.yaml` for known overlaps between types.
- ZCTAs approximate ZIP codes. For a state or county area, a ZCTA is included
  when its representative point is inside the area.
- Zero-land-area geographies (e.g. Lake Erie tracts) are dropped unless
  `drop_water = FALSE`.

## Development

```r
devtools::document()     # builds NAMESPACE and man/ from the roxygen comments
devtools::load_all()
devtools::test()         # offline tests
Sys.setenv(OSMPLACES_LIVE = "true"); devtools::test()   # also runs the live API test
```

To fold this into an existing package, copy `R/*.R` and
`inst/extdata/taxonomy.yaml`, add `httr2`, `jsonlite`, `sf`, `tigris`, and
`yaml` to `Imports` (and `accessibility`, `leaflet`, `htmlwidgets`, `ggplot2`
to `Suggests`), and replace `read_taxonomy()` with the package's own
taxonomy loader if it has one.
