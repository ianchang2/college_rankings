library(tidyverse)
library(ggthemes)
library(gitcreds)
library(janitor)
library(lubridate)
library(purrr)
library(ggtext)
library(countrycode)
library(here)


##  ---------------------- read in shanghai rankings ----------------------
all_data <- list()

for (year in 2003:2024) {
  file_path <- here("data", "Shanghai Data Files", paste0(year, ".csv"))
  
  df <- read_csv(file_path) %>%
    mutate(year = year)
  
  all_data[[length(all_data) + 1]] <- df
}

shanghai_raw <- bind_rows(all_data)

# write.csv(shanghai_raw, here("data", "Shanghai Data Files", "shanghai_raw.csv"), row.names = FALSE)

shanghai_raw <- read.csv(here("data", "shanghai_raw.csv"))


shanghai <- shanghai_raw %>%
  clean_names()%>%
  mutate(
    rank = case_when(
      str_detect(rank, "-") ~ as.integer(str_extract(rank, "^[0-9]+")),
      TRUE ~ as.integer(rank)
    ),
    region_ranking = case_when(
      str_detect(region_ranking, "-") ~ as.integer(str_extract(region_ranking, "^[0-9]+")),
      TRUE ~ as.integer(region_ranking)
    ), 
    score = as.numeric(score),
    indicator_award = as.numeric(indicator_award),
    indicator_hi_ci = as.numeric(indicator_hi_ci),
    indicator_n_s = as.numeric(indicator_n_s),
    indicator_pub = as.numeric(indicator_pub),
    indicator_pcp = as.numeric(indicator_pcp),
    country = region,
    university = name,
    country_rank = region_ranking,
    country = countryname(country))%>%
  filter(university != "name")%>%
  clean_names()

shanghai_full <- shanghai %>%
  select(university, country, rank, country_rank, year)

# write_csv(shanghai_full, here("Data", arwu_full.csv"), row.names = FALSE)


##  ---------------------- read in qs rankings ----------------------
qs2023b <- read.csv(here("Data", "QS Data Files", "qs-world-university-rankings-2017-to-2022-V2.csv"))%>% #rankings before 2023
  rename(rank = rank_display)%>%
  mutate(
    rank = as.integer(str_extract(rank, "\\d+")))

qs2023 <- read.csv(here("Data", "QS Data Files", "qs-rankings-2023.csv"))%>%
  rename(university = institution,
         country = location,
         rank = Rank) %>%
  mutate(year = 2023)

qs2024 <- read_excel(here("Data", "QS Data Files", "qs-rankings-2024.xlsx"), skip = 3)%>%
  rename(university = institution,
         country = location,
         rank = 'rank display') %>%
  mutate(
    year = 2024,
    rank = as.integer(str_extract(rank, "\\d+")))

qs2025 <- read_excel(here("Data", "QS Data Files", "qs-rankings-2025.xlsx"), skip = 3)%>%
  rename(university = institution,
         country = 'location code',
         rank = 'rank display') %>%
  mutate(year = 2025,
         rank = as.integer(rank))

qs_full <- bind_rows(qs2023b, qs2023, qs2024, qs2025)%>%
  select(university, country, rank, year)%>%
  mutate(year = as.integer(year),
         country = countryname(country))%>%
  group_by(year, country) %>%
  mutate(country_rank = min_rank(rank)) %>%
  ungroup()

#write_csv(qs_full, here("Data", "qs_full.csv"), row.names = FALSE)


##  ---------------------- read in times rankings ----------------------


rankings_list <- list() #pre-allocate space

for (year in 2011:2025) {
  file_name <- paste0("Y", year, "_R.csv")
  file_path <- here("Data", "Times Rankings", file_name)
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path, na = c("", "NA", "-"), show_col_types = FALSE) %>%
      mutate(
        year = year,
        `International Outlook` = as.double(`International Outlook`)
      )
    
    rankings_list[[as.character(year)]] <- df
  }
}


times_all <- bind_rows(rankings_list)
times_countries <- times_all %>%
  mutate(
    split_index = str_locate('Name Country/Region', "[a-z][A-Z]")[,1] + 1,
    university = str_sub('Name Country/Region', 1, split_index - 1),
    country = str_sub('Name Country/Region', split_index))

times_countries <- times_all %>%
  mutate(
    full_name = str_trim(.[[2]]),
    split_index = str_locate(times_all[[2]], "[a-z][A-Z]|\\)[A-Z]")[,1] + 1, #find index where theres the pattern of lowercaseUPPERCASE or )UPPERCASE because they tend to be indicative of the start of a country name
    university = str_sub(times_all[[2]], 1, split_index - 1), #splits into university and country based on the split index
    country = str_sub(times_all[[2]], split_index)
  )%>%
  mutate(country = str_remove(country, "Explore$"))%>% #remove the explore that they weirdly have tacked on
  mutate(
    university = case_when( #manually fix the outliers
      full_name == "UCLUnited Kingdom" ~ "UCL",
      full_name == "Lomonosov Moscow State University\r\nRussian Federation" ~ "Lomonosov Moscow State University",
      full_name == "McGill UniversityCanada\r\nExplore" ~ "McGill University",
      full_name == "McMaster UniversityCanada\r\nExplore" ~ "McMaster University",
      TRUE ~ university
    ),
    country = case_when(
      full_name == "UCLUnited Kingdom" ~ "United Kingdom",
      full_name == "Lomonosov Moscow State University\r\nRussian Federation" ~ "Russia",
      full_name == "McGill UniversityCanada\r\nExplore" ~ "Canada",
      full_name == "McMaster UniversityCanada\r\nExplore" ~ "Canada",
      TRUE ~ country
    ))%>%
  mutate(
    rank = Rank,
    country = countryname(country))%>%
  select(university, country, rank, year)  %>%
  group_by(year, country) %>%
  mutate(country_rank = min_rank(rank)) %>%
  ungroup()

## write_csv(times_countries, here("Data", "the_full.csv"), row.names = FALSE)




#  ---------------------- bind all universities ----------------------

universities <- bind_rows(
  shanghai_full %>% mutate(source = "ARWU"),
  qs_full %>% mutate(source = "QS"),
  times_countries %>% mutate(source = "THE") %>%
    select(rank, country_rank, university, country, year, source)) %>%
  na.omit("rank") %>%
  mutate(university = str_trim(university, side ="both"),
         university = case_when(
           university == "California Institute of Technology (Caltech)" ~ "California Institute of Technology",
           university == "ETH Zurich - Swiss Federal Institute of Technology" ~ "ETH Zurich",
           university == "National University of Singapore (NUS)" ~ "National University of Singapore",
           university == "Massachusetts Institute of Technology (MIT)" ~ "Massachusetts Institute of Technology",
           str_detect(university, "Nanyang") ~ "Nanyang Technological University",
           university == "UCL" ~ "University College London",
           str_detect(university, "Imperial") ~ "Imperial College London",
           university == "The University of Chicago" ~ "University of Chicago",
           university == "EPFL" ~ "Swiss Federal Institute of Technology Lausanne",
           university == "Swiss Federal Institute of Technology of Lausanne" ~ "Swiss Federal Institute of Technology Lausanne",
           university == "École Polytechnique Fédérale de Lausanne" ~ "Swiss Federal Institute of Technology Lausanne",
           str_detect(university, "Berkeley") ~ "University of California - Berkeley",
           university == "University of California, Los Angeles" ~ "University of California - Los Angeles",
           university == "University of California, Los Angeles (UCLA)" ~ "University of California - Los Angeles",
           university == "University of California, San Diego" ~ "University of California - San Diego",
           university == "University of California, San Diego (UCSD)" ~ "University of California - San Diego",
           university == "University of California, Irvine" ~ "University of California - Irvine",
           str_detect(university, "Santa Barbara") ~ "University of California - Santa Barbara",
           university == "University of California, Santa Cruz" ~ "University of California - Santa Cruz",
           university == "University of California, Davis" ~ "University of California - Davis",
           university == "University of California, San Francisco" ~ "University of California - San Francisco",
           university == "University of California, Riverside" ~ "University of California - Riverside",
           university == "University of Michigan-Ann Arbor" ~ "University of Michigan - Ann Arbor",
           university == "Rutgers - The State University of New Jersey - New Brunswick" ~ "Rutgers University - New Brunswick",
           university == "Rutgers" ~ "Rutgers University - New Brunswick",
           university == "Rutgers University-New Brunswick" ~ "Rutgers University - New Brunswick",
           university == "Rutgers - The State University of New Jersey" ~ "Rutgers University - New Brunswick",
           str_detect(university, "New South Wales") ~ "University of New South Wales",
           str_detect(university, "UNSW") ~ "University of New South Wales",
           university == "Université Paris-Saclay" ~ "Paris-Saclay University",
           university == "The University of Edinburgh" ~ "University of Edinburgh",
           str_detect(university, "PSL") ~ "PSL University",
           university == "The University of Melbourne" ~ "University of Melbourne",
           university == "New York University (NYU)" ~ "New York University",
           university == "The University of Sydney" ~ "University of Sydney",
           university == "Karolinska Institutet" ~ "Karolinska Institute",
           university == "King’s College London" ~ "King's College London", #this one is evil stupid two apostrophes
           str_detect(university, "London School of Economics") ~ "London School of Economics",
           str_detect(university, "Chinese University of Hong Kong") ~ "Chinese University of Hong Kong",
           university == "Heidelberg University" ~ "University of Heidelberg",
           university == "Universität Heidelberg" ~ "University of Heidelberg",
           university == "Ruprecht-Karls-Universität Heidelberg" ~ "University of Heidelberg",
           university == "The University of Hong Kong" ~ "University of Hong Kong",
           university == "University of Illinois at Chicago (UIC)" ~ "University of Illinois at Chicago",
           university == "University of North Carolina, Chapel Hill" ~ "University of North Carolina at Chapel Hill",
           str_detect(university, "KAIST") ~ "Korea Advanced Institute of Science and Technology",
           university == "National Taiwan University (NTU)" ~ "National Taiwan University",
           university == "National University of Science and Technology (Taiwan Tech)" ~ "National Taiwan University of Science and Technology",
           str_detect(university, "Paris 11") ~ "Paris-Sud University",
           str_detect(university, "Paris-Sud") ~ "Paris-Sud University",
           university == "Pennsylvania State University - University Park" ~ "Pennsylvania State University",
           university == "Penn State (Main campus)" ~ "Pennsylvania State University",
           university == "Ohio State University (Main campus)" ~ "Ohio State University",
           university == "Yonsei University (Seoul campus)" ~ "Yonsei University",
           university == "Sungkyunkwan University (SKKU)" ~ "Sungkyunkwan University",
           university == "Pohang University of Science and Technology (POSTECH)" ~ "Pohang University of Science and Technology",
           university == "University of Virginia (Main campus)" ~ "University of Virginia",
           university == "Pontificia Universidad Católica de Chile (UC)" ~ "Pontifical Catholic University of Chile",
           university == "Tokyo Institute of Technology (Tokyo Tech)" ~ "Tokyo Institute of Technology",
           university == "Universidad de Buenos Aires (UBA)" ~ "Universidad de Buenos Aires",
           university == "Universiti Malaya (UM)" ~ "Universiti Malaya",
           university == "Pohang University of Science And Technology (POSTECH)" ~ "Pohang University of Science and Technology",
           university == "Sungkyunkwan University(SKKU)" ~ "Sungkyunkwan University",
           university == "Universidad Nacional Autónoma de México  (UNAM)" ~ "National Autonomous University of Mexico",
           str_detect(university, "Pittsburgh") ~ "University of Pittsburgh",
           str_detect(university, "India Institute of Technology") ~ str_replace(university, "\\s*\\([^\\)]+\\)", ""),
           str_detect(university, "Washington University in") ~ "Washington University in St. Louis",
           str_detect(university, "Mayo ") ~ "Mayo Clinic Alix School of Medicine",
           university == "Ecole normale supérieure, Paris" ~ "Ecole Normale Superieure, Paris",
           university == "Ecole Normale Superieure - Paris" ~ "Ecole Normale Superieure, Paris",
           str_detect(university, "Norwegian University of Science") ~ "Norwegian University of Science and Technology",
           str_detect(university, "Texas A & M") ~ "Texas A&M University",
           str_detect(university, "Pierre") ~ "Pierre and Marie Curie University",
           str_detect(university, "West Lafayette") ~ "Purdue University",
           university == "Technical University Munich" ~ "Technical University of Munich",
           university == "Ohio State University" ~ "The Ohio State University",
           university == "Universidad Nacional Autónoma de México (UNAM)" ~ "National Autonomous University of Mexico",
           str_detect(university, "College Park") ~ "University of Maryland - College Park",
           str_detect(university, "Minnesota") ~ "University of Minnesota - Twin Cities",
           university == "University of Wisconsin-Madison" ~ "University of Wisconsin - Madison",
           university == "University of Wisconsin" ~ "University of Wisconsin - Madison",
           str_detect(university, "Diderot") ~ "Paris Diderot University",
           str_detect(university, "Bruxelles") ~ "Université Libre de Bruxelles",
           university == "University of Tuebingen" ~ "University of Tübingen",
           
           TRUE ~ university
         )
  )

country_coords <- read_csv("world_country_and_usa_states_latitude_and_longitude_values.csv") %>%
  mutate(country = countryname(country))%>%
  select(country, latitude, longitude)

universities <- universities %>%
  left_join(country_coords, by = "country")


write.csv(universities, here("Data", "universities.csv"), row.names = FALSE)


#write.csv(universities, here("First", "universities.csv"), row.names = FALSE)



#unique(universities[["university"]][str_detect(universities[["university"]],  "National Taiwan University")])

