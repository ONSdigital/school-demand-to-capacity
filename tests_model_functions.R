test_that("Test that the number of wards in demand set is the number in GM (=215)",
          {geography = "ward"
          GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
          GM_boundaries_by_geography <- select_boundaries_by_geography(geography)
          primary_school_demand_by_geography <- calculate_demand_by_geography(geography,
                                                                              GM_Primary_school_population_by_area_and_age,
                                                                              GM_boundaries_by_geography)
          count_wards <- length (unique(primary_school_demand_by_geography$Geography))
          expect_equal(count_wards, 215)
          })

test_that("Test that the number of pupils per LA is the same in the LA data set and the ward data set",
          {# set geography to LA, make test dataframe (rename of primary_school_demand_by_geography)
            geography = "LA"
            GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
            GM_boundaries_by_geography <- select_boundaries_by_geography(geography)
            test_demand_from_LA_data <- calculate_demand_by_geography(geography,
                                                                      GM_Primary_school_population_by_area_and_age,
                                                                      GM_boundaries_by_geography)
            
            test_demand_from_LA_data <- test_demand_from_LA_data %>%
              select(LA, demand) %>% # drops LA geo code
              arrange(LA)
            # set geography to ward, make primary_school_demand_by_geography, sum by LA to make test dataframe, rename columns
            geography = "ward"
            GM_Primary_school_population_by_area_and_age <- select_primary_school_population_by_geography(geography)
            primary_school_demand_by_geography <- calculate_demand_by_geography(geography,
                                                                                GM_Primary_school_population_by_area_and_age,
                                                                                GM_boundaries_by_geography)
            
            test_demand_from_ward_data <- primary_school_demand_by_geography %>%
              group_by(Local.Authority) %>%
              summarise(demand=sum(demand)) %>%
              setnames("Local.Authority", "LA") %>%
              as.data.frame() %>% # to force into data frame only
              arrange(LA)
            # compare test dataframes, should be the same
            
            expect_equal(test_demand_from_LA_data, test_demand_from_ward_data)
          })