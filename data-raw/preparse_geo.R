geo_acs5 = censusapi::listCensusMetadata(name="acs/acs5", vintage=2019, type="geographies")
geo_acs1 = censusapi::listCensusMetadata(name="acs/acs1", vintage=2019, type="geographies")
geo_dec = censusapi::listCensusMetadata(name="dec/sf1", vintage=2010, type="geographies")

geos = dplyr::bind_rows(dec=geo_dec, acs5=geo_acs5, acs1=geo_acs1, .id="survey") |>
    as_tibble() |>
    dplyr::distinct(name) |>
    dplyr::pull()
{
    short = str_replace_all(geos, " \\(or part\\)", "_part") |>
        janitor::make_clean_names()
    short[short == "county_subdivision"] = "county_subdiv"
    short[short == "county_subdivision_part"] = "county_subdiv_part"
    short[short == "place_remainder_part"] = "place_remainder"
    short[short == "metropolitan_statistical_area_micropolitan_statistical_area"] = "msa"
    short[short == "metropolitan_statistical_area_micropolitan_statistical_area_part"] = "msa_part"
    short[short == "zip_code_tabulation_area"] = "zcta"
    short[short == "zip_code_tabulation_area_part"] = "zcta_part"
    short[short == "new_england_city_and_town_area"] = "necta"
    short[short == "new_england_city_and_town_area_part"] = "necta_part"
    short[short == "combined_new_england_city_and_town_area"] = "combined_necta"
    short[short == "combined_new_england_city_and_town_area_part"] = "combined_necta_part"
    short[short == "public_use_microdata_area"] = "puma"
    short[short == "american_indian_area_alaska_native_area_hawaiian_home_land"] = "aian_area"
    short[short == "american_indian_area_alaska_native_area_hawaiian_home_land_part"] = "aian_area_part"
    short[short == "american_indian_area_alaska_native_area_reservation_or_statistical_entity_only"] = "aian_reserve_stat"
    short[short == "american_indian_area_alaska_native_area_reservation_or_statistical_entity_only_part"] = "aian_reserve_stat_part"
    short[short == "tribal_subdivision_remainder"] = "tribal_subdiv"
    short[short == "tribal_subdivision_remainder_part"] = "tribal_subdiv_part"
    short[short == "american_indian_tribal_subdivision"] = "ai_tribal_subdiv"
    short[short == "american_indian_tribal_subdivision_part"] = "ai_tribal_subdiv_part"
    short[short == "american_indian_area_off_reservation_trust_land_only_hawaiian_home_land"] = "ai_off_reserve_trust"
    short[short == "american_indian_area_off_reservation_trust_land_only_hawaiian_home_land_part"] = "ai_off_reserve_trust_part"
    short[short == "metropolitan_division"] = "metro_division"
    short[short == "metropolitan_division_part"] = "metro_division_part"
    short[short == "state_legislative_district_upper_chamber"] = "sld_upper"
    short[short == "state_legislative_district_lower_chamber"] = "sld_lower"
    short[short == "congressional_district"] = "cd"
}

geo = as.list(geos)
names(geo) = short
geo$us = NULL


counties = tinytiger::county_fips_2020

states = data.frame(
    stringsAsFactors = FALSE,
    fips = c("01","02","04","05","06",
             "08","09","10","11","12","13","15","16","17","18",
             "19","20","21","22","23","24","25","26","27",
             "28","29","30","31","32","33","34","35","36","37",
             "38","39","40","41","42","44","45","46","47",
             "48","49","50","51","53","54","55","56","60","66",
             "69","72","74","78"),
    abbr = c("AL","AK","AZ","AR","CA",
             "CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
             "IA","KS","KY","LA","ME","MD","MA","MI","MN",
             "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC",
             "ND","OH","OK","OR","PA","RI","SC","SD","TN",
             "TX","UT","VT","VA","WA","WV","WI","WY","AS","GU",
             "MP","PR","UM","VI"),
    name = c("Alabama","Alaska","Arizona",
             "Arkansas","California","Colorado","Connecticut",
             "Delaware","District of Columbia","Florida","Georgia",
             "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
             "Kentucky","Louisiana","Maine","Maryland",
             "Massachusetts","Michigan","Minnesota","Mississippi","Missouri",
             "Montana","Nebraska","Nevada","New Hampshire",
             "New Jersey","New Mexico","New York","North Carolina",
             "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
             "Rhode Island","South Carolina","South Dakota",
             "Tennessee","Texas","Utah","Vermont","Virginia",
             "Washington","West Virginia","Wisconsin","Wyoming",
             "American Samoa","Guam","Northern Mariana Islands","Puerto Rico",
             "U.S. Minor Outlying Islands","U.S. Virgin Islands")
)


usethis::use_data(geo, states, counties, internal=TRUE, overwrite=TRUE, compress="xz")
