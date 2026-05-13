# Construct a Geography Specification for Census Data

Currently used mostly internally. Builds a Census API-formatted
specification of which geographies to download data for. State and
county names (or postal abbreviations) are partially matched to existing
tables, for ease of use. Other geographies should be specified with
Census GEOIDs. The `usgazeteer` package, available with
`remotes::install_github("bhaskarvk/usgazetteer")`, may be useful in
finding GEOIDs for other geographies. Consult the "geography" sections
of each API at <https://www.census.gov/data/developers/data-sets.html>
for information on which geographic specifiers may be provided in
combination with others.

## Usage

``` r
cens_geo(geo = NULL, ..., check = TRUE, api = "acs/acs5", year = 2019)
```

## Arguments

- geo:

  The geographic level to return. One of the machine-readable or
  human-readable names listed in the "Details" section. Will return all
  matching geographies of this level, as filtered by the further
  arguments to `...`. For example, setting `geo="tract"` is equivalent
  to setting `tract="all"`.

- ...:

  Geographies to return, as supported by the Census API. Order matters
  here—the first argument will be the geographic level to return (i.e.,
  it corresponds to the `geo` argument) and additional arguments will
  filter the results. Use `"all"`, `"*"`, `NA`, or `TRUE` to return all
  units of a particular geography. See the examples for details.

- check:

  If `TRUE`, validate the provided geographies against the available
  geographies from the relevant Census API. Requires the `api` and
  `year` arguments to be specified.

- api:

  A Census API programmatic name such as `"acs/acs5"`.

- year:

  The year for the data

## Value

A list with two elements, `region` and `regionin`, which together
specify a valid Census API geography argument.

## Details

Supported geography arguments:

- `us`

- `region`

- `division`

- `state`

- `county`

- `county_subdiv` (County Subdivision)

- `subminor_civil_division` (Subminor Civil Division)

- `place_remainder` (Place/Remainder (Or Part))

- `tract_part` (Tract (Or Part))

- `urban_rural` (Urban Rural)

- `block_group_part` (Block Group (Or Part))

- `block`

- `tract`

- `aian_area_part` (American Indian Area/Alaska Native Area/Hawaiian
  Home Land (Or Part))

- `block_group` (Block Group)

- `county_part` (County (Or Part))

- `place_part` (Place (Or Part))

- `place`

- `consolidated_city` (Consolidated City)

- `alaska_native_regional_corporation` (Alaska Native Regional
  Corporation)

- `aian_area` (American Indian Area/Alaska Native Area/Hawaiian Home
  Land)

- `tribal_subdiv` (Tribal Subdivision/Remainder)

- `aian_reserve_stat` (American Indian Area/Alaska Native Area
  (Reservation Or Statistical Entity Only))

- `ai_tribal_subdiv_part` (American Indian Tribal Subdivision (Or Part))

- `ai_off_reserve_trust` (American Indian Area (Off-Reservation Trust
  Land Only)/Hawaiian Home Land)

- `tribal_census_tract` (Tribal Census Tract)

- `tribal_census_tract_part` (Tribal Census Tract (Or Part))

- `tribal_block_group` (Tribal Block Group)

- `state_part` (State (Or Part))

- `county_subdiv_part` (County Subdivision (Or Part))

- `tribal_subdiv_part` (Tribal Subdivision/Remainder (Or Part))

- `aian_reserve_stat_part` (American Indian Area/Alaska Native Area
  (Reservation Or Statistical Entity Only) (Or Part))

- `ai_off_reserve_trust_part` (American Indian Area (Off-Reservation
  Trust Land Only)/Hawaiian Home Land (Or Part))

- `tribal_block_group_part` (Tribal Block Group (Or Part))

- `msa` (Metropolitan Statistical Area/Micropolitan Statistical Area)

- `principal_city_part` (Principal City (Or Part))

- `metro_division` (Metropolitan Division)

- `msa_part` (Metropolitan Statistical Area/Micropolitan Statistical
  Area (Or Part))

- `metro_division_part` (Metropolitan Division (Or Part))

- `combined_statistical_area` (Combined Statistical Area)

- `combined_necta` (Combined New England City And Town Area)

- `necta` (New England City And Town Area)

- `combined_statistical_area_part` (Combined Statistical Area (Or Part))

- `combined_necta_part` (Combined New England City And Town Area (Or
  Part))

- `necta_part` (New England City And Town Area (Or Part))

- `principal_city` (Principal City)

- `necta_division` (Necta Division)

- `necta_division_part` (Necta Division (Or Part))

- `urban_area` (Urban Area)

- `urban_area_part` (Urban Area (Or Part))

- `consolidated_city_part` (Consolidated City (Or Part))

- `cd` (Congressional District)

- `sld_upper` (State Legislative District (Upper Chamber))

- `sld_lower` (State Legislative District (Lower Chamber))

- `alaska_native_regional_corporation_part` (Alaska Native Regional
  Corporation (Or Part))

- `zcta` (Zip Code Tabulation Area)

- `zcta_part` (Zip Code Tabulation Area (Or Part))

- `school_district_elementary` (School District (Elementary))

- `school_district_secondary` (School District (Secondary))

- `school_district_unified` (School District (Unified))

- `congressional_district_part` (Congressional District (Or Part))

- `school_district_elementary_part` (School District (Elementary) (Or
  Part))

- `school_district_secondary_part` (School District (Secondary) (Or
  Part))

- `school_district_unified_part` (School District (Unified) (Or Part))

- `voting_district_part` (Voting District (Or Part))

- `subminor_civil_division_part` (Subminor Civil Division (Or Part))

- `state_legislative_district_upper_chamber_part` (State Legislative
  District (Upper Chamber) (Or Part))

- `state_legislative_district_lower_chamber_part` (State Legislative
  District (Lower Chamber) (Or Part))

- `vtd` (Voting District)

- `ai_tribal_subdiv` (American Indian Tribal Subdivision)

- `puma` (Public Use Microdata Area)

## Examples

``` r
if (FALSE) { # \dontrun{
cens_geo(state="WA")
cens_geo("county", state="WA") # equivalent to `cens_geo(county="all", state="WA")`
cens_geo(county="King", state="Wash")
cens_geo(zcta="02138", check=FALSE)
cens_geo(zcta=NA, state="WA", check=FALSE)
cens_geo("zcta", state="WA", check=FALSE)
cens_geo(cd="09", state="WA", check=FALSE)
cens_geo("county_part", state="WA", cd="09", check=FALSE)
} # }
```
