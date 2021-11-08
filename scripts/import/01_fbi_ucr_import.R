library(tidyverse)
library(fbi)
source("api_key.R")

# arrests
# employees
# demographic details by agency

set_api_key(fbi_ucr_key_here)

### updating functions ----
## updating some functions from the "fbi" package so it brings in latest data

combine_url_section <- function(data_type, ori, region_name, state_abb) {
  url_section <- paste0(data_type, "/national")
  if (!is.null(ori)) {
    url_section <- paste0(data_type, "/agencies/")
    url_section <- paste0(url_section, ori)
  } else if (!is.null(region_name)) {
    url_section <- paste0(data_type, "/regions/")
    url_section <- paste0(url_section, region_name)
  } else if (!is.null(state_abb)) {
    url_section <- paste0(data_type, "/states/")
    url_section <- paste0(url_section, state_abb)
  }
  return(url_section)
}


make_url <- function(url_section,
                     start_year,
                     key) {
  
  url <- paste0("https://api.usa.gov/crime/fbi/sapi/api/",
                url_section,
                "/",
                start_year,
                #                "/2018?API_KEY=",
                "/2020?API_KEY=",
                key)
  return(url)
}


clean_column_names <- function(.data) {
  names(.data) <- tolower(names(.data))
  names(.data) <- gsub("-", "_", names(.data))
  names(.data) <- gsub("^data_year$", "year", names(.data))
  
  
  # Fix arrest column names
  names(.data) <- gsub("^disorderly$", "disorderly_conduct", names(.data))
  names(.data) <- gsub("^driving$", "dui", names(.data))
  names(.data) <- gsub("^drug_abuse_gt$", "drug_grand_total", names(.data))
  names(.data) <- gsub("^drug_poss_m$", "drug_poss_marijuana", names(.data))
  names(.data) <- gsub("^drug_sales_m$", "drug_sales_marijuana", names(.data))
  names(.data) <- gsub("^g_all$", "gambling_all_others", names(.data))
  names(.data) <- gsub("^g_b$", "gambling_bookmaking", names(.data))
  names(.data) <- gsub("^g_n$", "gambling_numbers", names(.data))
  names(.data) <- gsub("^g_t$", "gambling_total", names(.data))
  names(.data) <- gsub("^ht_c_s_a$", "human_trafficking_commercial_sex_traffic", names(.data))
  names(.data) <- gsub("^ht_i_s$", "human_trafficking_servitude", names(.data))
  names(.data) <- gsub("^liquor$", "liquor_laws", names(.data))
  names(.data) <- gsub("^mvt$", "motor_vehicle_theft", names(.data))
  names(.data) <- gsub("^offense_family$", "offense_against_family", names(.data))
  names(.data) <- gsub("^prostitution$", "prostitution_total", names(.data))
  names(.data) <- gsub("^prostitution_a_p_p$", "prostitution_assisting", names(.data))
  names(.data) <- gsub("^prostitution_p$", "prostitution_performing", names(.data))
  names(.data) <- gsub("^prostitution_p_p$", "prostitution_purchasing", names(.data))
  names(.data) <- gsub("^sex_offense$", "other_sex_offenses", names(.data))
  
  
  
  .data$csv_header <- NULL
  return(.data)
}



url_to_dataframe <- function(url) {
  useragent <- paste0(
    "Mozilla/5.0 (compatible; a bot using the R fbi",
    " package; https://github.com/jacobkap/fbi/)")
  
  response <- httr::GET(url = url,
                        httr::user_agent(useragent))
  
  
  if (response$status_code %in% 200) {
    response <- jsonlite::fromJSON(rawToChar(response$content))
    response <- response$results
  }
  return(response)
}

get_arrest_count <- function(ori = NULL,
                             state_abb = NULL,
                             region = NULL,
                             monthly = FALSE,
                             key = get_api_key()) {
  
  url_part <- "data/arrest/agencies/offense"
  if (is.null(ori) & is.null(state_abb) & is.null(region)) {
    url_part <- "data/arrest"
  }
  url_section <- combine_url_section(url_part,
                                     ori = ori,
                                     state_abb = state_abb,
                                     region_name = region)
  if (monthly) {
    start_year <- "monthly/1985"
  } else {
    start_year <- "all/1985"
  }
  
  url <- make_url(url_section, start_year, key)
  url <- gsub("offense/agencies", "offense", url)
  url <- gsub("national", "national/offense", url)
  
  data <- url_to_dataframe(url)
  data <- clean_column_names(data)
  
  if (!is.null(ori)) {
    data$ori <- ori
    data <- data[, c("ori", "year",
                     names(data)[which(!names(data) %in% c("year", "ori"))])]
  }
  data <- data[order(data$year, decreasing = TRUE), ]
  rownames(data) <- 1:nrow(data)
  return(data)
}


agencies <- fbi_api_agencies

#### exporting agencies list -----
write_csv(agencies, "data/raw_data/ucr_agencies_list.csv", na="")

bad_array <- c("", "")

#### importing arrests data for all agencies ----
for (i in 1:nrow(agencies)) {
  
  tryCatch({
    arrests <- get_arrest_count(agencies$ori[i])
    if (i ==1) {
      arrests_df <- arrests
    } else {
      arrests_df <- rbind(arrests_df, arrests)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    bad_array <- c(agencies$ori[i], bad_array)})
  
  tryCatch({
    demo <- get_arrest_demographics(agencies$ori[i])
    if (i ==1) {
      demo_df <- demo
    } else {
      demo_df <- rbind(demo_df, demo)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    bad_array <- c(agencies$ori[i], bad_array)})
  
  print(paste0(i, " of ", nrow(agencies)))
  # Sys.sleep(runif(1, 1, 5))
}

arrests_df <- unique(arrests_df)
demo_df <- unique(demo_df)
write_csv(arrests_df, "data/raw_data/ucr_agencies_arrests.csv", na="")
write_csv(demo_df, "data/raw_data/ucr_agencies_arrests_demo.csv", na="")


#### importing employees data for all agencies ----
for (i in 1:nrow(agencies)) {

    tryCatch({
      employees <- get_police_employment(agencies$ori[i])
      if (i ==1) {
        employees_df <- employees
      } else {
        employees_df <- rbind(employees_df, employees)
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
      bad_array <- c(agencies$ori[i], bad_array)})

  
  print(paste0(i, " of ", nrow(agencies)))
  # Sys.sleep(runif(1, 1, 5))
}

employees_df <- unique(employees_df)
write_csv(employees_df, "data/raw_data/ucr_agencies_employees.csv", na="")

