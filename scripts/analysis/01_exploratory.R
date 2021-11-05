library(tidyverse)
library(readxl)

# run this script to pull data from the FBI API
#Source("scripts/import/01_fbi_ucr_import.R")

# Load data from FBI UCR

# employees data
employees <- read_csv("data/raw_data/ucr_agencies_employees.csv")
employees_last <- employees %>% 
  group_by(ori) %>% 
  arrange(desc(year)) %>% 
  ungroup()
#  slice(1) %>% 
#  mutate(year=2019)
#employees <- rbind(employees, employees_last)

# arrests data
arrests <- read_csv("data/raw_data/ucr_agencies_arrests.csv")

# load crosswalk data from Inter-university Consortium for Political and Social Research

# To proceed, download the crosswalk zip folder from the link below
# https://www.icpsr.umich.edu/web/NACJD/studies/35158
# And unzip the folder into the `data/raw_data` folder
# then you can run this successfully

load("data/raw_data/ICPSR_35158/DS0001/35158-0001-Data.rda")

# save it as crosswalk dataframe
crosswalk <- da35158.0001

# remove the preloaded data
rm(da35158.0001)



#### summarize ----


emp <- employees %>% 
  # filter on later than 2004
  filter(year>=2005)

# joining number of employees to arrests data
joined <- emp %>% 
  left_join(arrests)

joined <- joined %>% 
  left_join(crosswalk, by=c("ori"="ORI9")) %>% 
  # filtering out rows that reported no arrests
  filter(!is.na(all_other_offenses)) %>% 
  # filters out 4,990 agencies that did not submit employee count info
  filter(!is.na(officers_total))# %>% 
  # filtering out weird outlier
  #filter(ori!="IN0080100" | year!=2013)



# replacing NAs with zeroes now
joined$all_other_offenses <- ifelse(is.na(joined$all_other_offenses),  0, joined$all_other_offenses)
joined$drug_grand_total <- ifelse(is.na(joined$drug_grand_total), 0, joined$drug_grand_total)
joined$simple_assault <- ifelse(is.na(joined$simple_assault), 0, joined$simple_assault)
joined$arson <- ifelse(is.na(joined$arson), 0, joined$arson)

joined$dui <- ifelse(is.na(joined$dui), 0, joined$dui)
joined$larceny <- ifelse(is.na(joined$larceny), 0, joined$larceny)
joined$aggravated_assault <- ifelse(is.na(joined$aggravated_assault), 0, joined$aggravated_assault)
joined$drunkness <- ifelse(is.na(joined$drunkness), 0, joined$drunkness)
joined$burglary <- ifelse(is.na(joined$burglary), 0, joined$burglary)

joined$curfew <- ifelse(is.na(joined$curfew),  0, joined$curfew)
joined$disorderly_conduct <- ifelse(is.na(joined$disorderly_conduct),  0, joined$disorderly_conduct)
joined$embezzlement <- ifelse(is.na(joined$embezzlement),  0, joined$embezzlement)
joined$forgery <- ifelse(is.na(joined$forgery),  0, joined$forgery)
joined$fraud <- ifelse(is.na(joined$fraud),  0, joined$fraud)
joined$gambling_total <- ifelse(is.na(joined$gambling_total),  0, joined$gambling_total)
joined$human_trafficking_commercial_sex_traffic <- ifelse(is.na(joined$human_trafficking_commercial_sex_traffic),  0, joined$human_trafficking_commercial_sex_traffic)
joined$human_trafficking_servitude <- ifelse(is.na(joined$human_trafficking_servitude),  0, joined$human_trafficking_servitude)
joined$larceny <- ifelse(is.na(joined$larceny),  0, joined$larceny)
joined$liquor_laws <- ifelse(is.na(joined$liquor_laws ),  0, joined$liquor_laws )
joined$manslaughter <- ifelse(is.na(joined$manslaughter),  0, joined$manslaughter)
joined$motor_vehicle_theft <- ifelse(is.na(joined$motor_vehicle_theft),  0, joined$motor_vehicle_theft)
joined$murder <- ifelse(is.na(joined$murder),  0, joined$murder)
joined$offense_against_family <- ifelse(is.na(joined$offense_against_family),  0, joined$offense_against_family)
joined$prostitution_total <- ifelse(is.na(joined$prostitution_total),  0, joined$prostitution_total)
joined$rape <- ifelse(is.na(joined$rape),  0, joined$rape)
joined$robbery <- ifelse(is.na(joined$robbery),  0, joined$robbery)
joined$other_sex_offenses <- ifelse(is.na(joined$other_sex_offenses),  0, joined$other_sex_offenses)
joined$simple_assault <- ifelse(is.na(joined$simple_assault),  0, joined$simple_assault)
joined$stolen_property <- ifelse(is.na(joined$stolen_property),  0, joined$stolen_property)
joined$suspicion <- ifelse(is.na(joined$suspicion),  0, joined$suspicion)
joined$vagrancy <- ifelse(is.na(joined$vagrancy),  0, joined$vagrancy)
joined$vandalism <- ifelse(is.na(joined$vandalism),  0, joined$vandalism)
joined$weapons<- ifelse(is.na(joined$weapons),  0, joined$weapons)
joined$drug_poss_marijuana  <- ifelse(is.na(joined$drug_poss_marijuana),  0, joined$drug_poss_marijuana )


# summarizing types of arrests by agency/year
# arrests_all, arrests_old, low_level_arrests


# Low level arrests are categorized as arrests from
## vandalism
## liquor_laws
## drunkness
## gambling_total
## suspicion 
## disorderly_conduct 
## all_other_offenses
# vagrancy
# curfew
# drug_poss_marijuana

joined <- joined %>% 
  mutate(arrests_all=aggravated_assault +
           all_other_offenses +
           arson+
           burglary +
           curfew +
           disorderly_conduct +
           dui +
           drug_grand_total+
           drunkness +
           embezzlement +
           forgery+
           fraud+
           gambling_total +
           human_trafficking_commercial_sex_traffic +
           human_trafficking_servitude+
           larceny +
           liquor_laws +
           manslaughter +
           motor_vehicle_theft +
           murder +
           offense_against_family +
           prostitution_total +
           rape +
           robbery +
           other_sex_offenses +
           simple_assault +
           stolen_property +
           suspicion +
           vagrancy +
           vandalism +
           weapons,
         arrests_old=all_other_offenses+
           drug_grand_total+
           simple_assault+
           dui+ larceny+
           aggravated_assault+
           drunkness,
         low_level_arrests=
           vandalism+
           liquor_laws+
           drunkness+
           gambling_total+
           suspicion +
           disorderly_conduct +
           all_other_offenses +
           #new
           #dui +
           vagrancy +
           curfew +
           drug_poss_marijuana
  )

# load in relationship file for 287g sheriffs agencies
# has ori codes and years of participation
# breaks out agencies by year and participation
ice <- read_excel("data/raw_data/287g_curated.xlsx", sheet=1) %>% 
  filter(!is.na(ori)) %>% 
  filter(County!="Butler County Sheriff's Office") %>% 
  mutate(ori=case_when(
    nchar(ori)==7 ~ paste0(ori, "00"),
    TRUE ~ ori
  )) %>% 
  mutate(ice=T) %>%
  select(ori, ice, y2005:y2019) %>% 
  pivot_longer(cols=y2005:y2019, names_to="year", values_to="participant") %>% 
  mutate(participant=case_when(
    str_to_upper(participant)=="X" ~ T,
    TRUE ~ F
  ),
  year=as.numeric(gsub("y", "", year))) %>% 
  group_by(ori) %>% 
  arrange(ori, year) %>% 
  group_by(ori) %>% 
  mutate(lag=lag(participant)) %>% 
  mutate(what=case_when(
    participant==TRUE & lag==FALSE ~ "joined",
    participant==FALSE & lag==TRUE ~ "left"
  )) 


# loads in the same data as above but 
# marks if they've ever participated in 287g
ice2 <- read_excel("data/raw_data/287g_curated.xlsx", sheet=1) %>% 
  filter(!is.na(ori)) %>% 
  filter(County!="Butler County Sheriff's Office") %>% 
  mutate(ori=case_when(
    nchar(ori)==7 ~ paste0(ori, "00"),
    TRUE ~ ori
  )) %>% 
  mutate(ice=T) %>% 
  select(ori, ice) %>% 
  unique()


# fbi agency employment and arrests totals joined with ice relationship files
joined <- left_join(joined, ice)
joined <- left_join(joined, ice2)

## identifying and excluding outliers

# need to change classification of some departments
# from police departments to sheriff's office
# except for a couple of state police - those were outliers in number of employees

joined2 <- joined %>% 
  filter(!is.na(total_police_employees)) %>% 
  mutate(AGCYTYPE=as.character(AGCYTYPE)) %>% 
  # Reclassifying agency type based on input from relationship file
  mutate(AGCYTYPE=case_when(
    participant==T | participant==F ~ "(001) Sheriff's office",
    TRUE ~ AGCYTYPE
  )) %>% 
  # Filtering out Massachusetts State Law Enforcement Agencies
  mutate(AGCYTYPE=case_when(
    state_abbr=="MA" & grepl("SP: ", agency_name_edit) ~ "(005) State law enforcement agency",
    TRUE ~ AGCYTYPE
  )) 

# changing some NAs from the ICE joins to non-287g participants
joined2$participant <- ifelse(is.na(joined2$participant), FALSE, joined2$participant)

# calculating deviations in low-level arrests year-over-year
deviation <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  #filter(!is.na(ice)) %>% 
  filter(officers_total>3) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  group_by(ori) %>% 
  mutate(pop=median(population, na.rm=T)) %>% 
  mutate(avg_low=mean(low_level_arrests, na.rm=T)) %>% 
  mutate(deviation=round((low_level_arrests-avg_low)/avg_low*100,1)) %>% 
  mutate(diff=low_level_arrests-avg_low) %>% 
  mutate(per_pop=round(pop/low_level_arrests)) %>% 
  select(ori, agency_name_edit, year, pop, low_level_arrests, avg_low, deviation, diff, per_pop)

# 548... deviation, 544, 1462.4286

# 1745.8... diff, 4486, 159


# identify the outliers based on a deviation of 400 and raw change of 400 arrests
thou <- filter(deviation, deviation>400 & diff>400) %>% 
  #select(ori) %>% 
  #unique() 
  select(ori, year) %>% 
  mutate(outlier="ignore")



#### summary_agency_type ----
## the crux of the story

joined2_count <- joined %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  # Looking at agencies with more than 3 officers
  filter(officers_total>3) %>% 
  # bringing in outlier ids and filtering them out
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  # additional cleaning
  mutate(AGCYTYPE=as.character(AGCYTYPE)) %>% 
  mutate(AGCYTYPE=case_when(
    participant==T | participant==F ~ "(001) Sheriff's office",
    TRUE ~ AGCYTYPE
  )) %>% 
  mutate(AGCYTYPE=case_when(
    state_abbr=="MA" & grepl("SP: ", agency_name_edit) ~ "(005) State law enforcement agency",
    TRUE ~ AGCYTYPE
  )) %>% 
  # only looking at sheriffs agencies
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  select(ori, ice) %>% 
  unique() %>% 
  count(ice)

# IDing participating agencies
participated <- joined2_count %>% 
  filter(ice==TRUE) %>% 
  pull(n)

# IDing non-participating agencies
not_participated <- joined2_count %>% 
  filter(is.na(ice)) %>% 
  pull(n)

# What's the percent of agencies who participated?
participated/(participated+not_participated)*100
# 3.3 percent 


just_checking <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  #7
  group_by(AGCYTYPE, participant) %>% 
  summarize(total=n(),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            officers_total = sum(officers_total)) %>% 
  group_by(AGCYTYPE) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") 

# tried to see what the percent of officers there were
# compared to percent of low level arrests

just_checking_percents <- just_checking %>% 
  ungroup() %>% 
  mutate(total_percent=round(total/sum(total)*100,1),
         low_level_arrests_percent=round(arrests_low/sum(arrests_low)*100,1),
         officers_percent=round(officers_total/sum(officers_total)*100,1))

View(just_checking_percents)


# figuring out the average low level arrest rate for sheriff's office 
# between 2008 and 2018
# should be 7 per officer

joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  #7
  group_by(AGCYTYPE) %>% 
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  pull(arrests_low_per_officer)

# what's the rate of low level arrests for sheriff's agencies 
# who participate versus who don't
# 7.4 versus 5.4!
summarized <- joined2 %>% 
 filter(year>=2016) %>% 
  #filter(year>=2010) %>% 
  
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  group_by(AGCYTYPE, participant) %>% 
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") 
#pull(arrests_low_per_officer)

View(summarized)  

# what is the rate above but annually?
summary_agency_type_annual2 <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  group_by(AGCYTYPE, participant, year) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office")

View(summary_agency_type_annual2)

# visualizing it
summary_agency_type_annual2 %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  ggplot(aes(x=year, y=arrests_low_per_officer, group=participant, fill=participant)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(title="Low level arrests per officer by sheriff's offices since 2008", 
       y="low level arrests per officer", x="", fill="287g participant")

# bucketing up the years for sentence construction
summary_agency_type_annual2b <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  #left_join(thou) %>% 
  #filter(is.na(outlier)) %>% 
  mutate(grouped_year=case_when(
    year>=2008 & year < 2016 ~ "2008-2015",
    year>=2016 ~ "2016-2019",
    TRUE ~"other"
  )) %>% 
  group_by(AGCYTYPE, participant, grouped_year) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  select(AGCYTYPE, participant, grouped_year, arrests_low_per_officer) %>% 
  pivot_wider(names_from="participant", values_from="arrests_low_per_officer") %>% 
  rename(`Non-participant`=`FALSE`, Participant=`TRUE`)

View(summary_agency_type_annual2b) 


summary_agency_type_annual2c <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  mutate(grouped_year=case_when(
    year>=2008 & year < 2016 ~ "2008-2015",
    year>=2016 ~ "2016-2019",
    TRUE ~"other"
  )) %>% 
  filter(!is.na(total_police_employees)) %>% 
  group_by(AGCYTYPE, participant, grouped_year) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  select(AGCYTYPE, participant, grouped_year, arrests_per_officer) %>% 
  pivot_wider(names_from="participant", values_from="arrests_per_officer") %>% 
  rename(`Non-participant`=`FALSE`, Participant=`TRUE`)

View(summary_agency_type_annual2c) 

### agency

summary_agency_type_annual2ca <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  mutate(grouped_year=case_when(
    year>=2008 & year < 2016 ~ "2008-2015",
    year>=2016 ~ "2016-2019",
    TRUE ~"other"
  )) %>% 
  filter(!is.na(total_police_employees)) %>% 
  group_by(AGCYTYPE, agency_name_edit, participant, grouped_year) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  select(AGCYTYPE, agency_name_edit, participant, grouped_year, arrests_per_officer) %>% 
  pivot_wider(names_from="participant", values_from="arrests_per_officer") %>% 
  rename(`Non-participant`=`FALSE`, Participant=`TRUE`) %>% 
  select(-`Non-participant`) %>%
  filter(!is.na(Participant)) %>% 
  pivot_wider(names_from=grouped_year, values_from=Participant) %>% 
  filter(!is.na(`2008-2015`) & !is.na(`2016-2019`)) %>% 
  mutate(change=(`2016-2019`-`2008-2015`)/`2008-2015`*100)

View(summary_agency_type_annual2ca) 

summary_agency_type_annual2d <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  mutate(grouped_year=case_when(
    year>=2008 & year < 2016 ~ "2008-2015",
    year>=2016 ~ "2016-2019",
    TRUE ~"other"
  )) %>% 
  filter(!is.na(total_police_employees)) %>% 
  group_by(AGCYTYPE, participant, grouped_year) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  select(AGCYTYPE, participant, grouped_year, percent_low) %>% 
  pivot_wider(names_from="participant", values_from="percent_low") %>% 
  rename(`Non-participant`=`FALSE`, Participant=`TRUE`)

View(summary_agency_type_annual2d) 

#Comparing those two time periods, the low-level arrest rates 
# for non-participating agencies dropped 17 percent while the 
# rate for 287g participating agencies jumped 29 percent.

# identifying when agencies joined or left the program
# and seeing if arrest rates increased or not
summary_agency_type_annual6 <- joined2 %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  group_by(AGCYTYPE, ori, participant, year) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  left_join(ice) %>% 
  group_by(ori) %>% 
  arrange(ori, year) %>% 
  group_by(ori) %>% 
  mutate(lag_arrests=lag(arrests_low_per_officer),
         lead_arrests=lead(arrests_low_per_officer),
         what_year_lag=case_when(
           what=="joined" | what=="left" ~ year-1),
         what_year_lead=case_when(
           what=="joined" | what=="left" ~ year+1)
  ) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  filter(ice==T) %>% 
  #filter(!is.na(what)) %>% 
  mutate(left_change=case_when(
    what=="left" & lag(year)==what_year_lag~ round((arrests_low_per_officer-lag_arrests)/lag_arrests*100,1),
    TRUE ~  NA_real_)
  ) %>% 
  mutate(join_change=case_when(
    what=="joined" & lead(year)==what_year_lead~ round((lead_arrests-arrests_low_per_officer)/arrests_low_per_officer*100,1),
    TRUE ~  NA_real_)
  )  %>% 
  filter(year>=2008) 

View(summary_agency_type_annual6)

# what's the median rate decrease after leaving 
median(summary_agency_type_annual6$left_change, na.rm=T)
# -3 percent
# 9 agencies dropped, 7 increased

# what's the median rate increase after joining
median(summary_agency_type_annual6$join_change, na.rm=T)
# increased 1.25 percent
# 17 agencies increased, 16 decreased

# looking at agencies that ever participated
# and looking at their rate for low level arrests
# when they were participating versus when they weren't
summary_agency_type2 <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(year<2016) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(officers_total>3) %>%
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  filter(!is.na(ice)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  group_by(state_abbr, agency_name_edit, ori, AGCYTYPE, participant) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) 

View(summary_agency_type2)

# adding a flag if the agency's low level arrest rate is higher 
# than the average rate for these agencies
#arrests_low_per_officer_TRUE – rate of low level arrests while agency was participating in 287g
#arrests_low_per_officer_FALSE – rate of low level arrests while agency was not participating in 287g
#compare2 looks to see if their participation was higher than the average
#compare3 looks to see if their non participatoin rate was higher than the average
summary_agency_type3 <- summary_agency_type2 %>% 
  select(state=state_abbr, agency=agency_name_edit, participant, total, arrests_low_per_officer) %>% 
  pivot_wider(names_from="participant", values_from=c("total", "arrests_low_per_officer")) %>% 
  mutate(compare=case_when(
    arrests_low_per_officer_TRUE > arrests_low_per_officer_FALSE ~ "more",
    TRUE ~ "less"),
    compare2=case_when(
      arrests_low_per_officer_TRUE > 7 ~ TRUE,
      TRUE ~ FALSE
    ),
    compare3=case_when(
      arrests_low_per_officer_FALSE > 7 ~ TRUE,
      TRUE ~ FALSE
    ))

# replacing NAs in flag columns
# compare – flagging agencies that were always in 287g
# compare2 - flags whether arrests per officer is higher than the average of 7.1
# compare3 - flags whether arrests per officer is lower than the average of 7.1

summary_agency_type3$compare <- ifelse(is.na(summary_agency_type3$arrests_low_per_officer_FALSE), "no false", summary_agency_type3$compare )
summary_agency_type3$compare <- ifelse(is.na(summary_agency_type3$arrests_low_per_officer_TRUE), "no true", summary_agency_type3$compare )
summary_agency_type3$compare2 <- ifelse(is.na(summary_agency_type3$arrests_low_per_officer_TRUE), NA, summary_agency_type3$compare2)
summary_agency_type3$compare3 <- ifelse(is.na(summary_agency_type3$arrests_low_per_officer_FALSE), NA, summary_agency_type3$compare3)

# counting it up
# nothing really significant here
# 27 less, 28 more, no falses: 22
summary_agency_type3 %>% ungroup() %>% 
  count(compare)

# 45 agencies who did not participate had higher rates than 32 that did participate
summary_agency_type3 %>% ungroup() %>% 
  count(compare2)


# in general, did these agencies exceed the average rate?
summary_agency_type5 <- joined2 %>% 
  #filter(year>=2008) %>%
  #filter(year<2016) %>% 
  filter(year>=2016) %>% 
  
  filter(!is.na(total_police_employees)) %>% 
  #filter(!is.na(ice)) %>% 
  filter(officers_total>3) %>% 
  left_join(thou) %>% 
  filter(is.na(outlier)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  group_by(state_abbr, agency_name_edit, ori, ice, AGCYTYPE) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  mutate(higher=case_when(
    arrests_low_per_officer > 7 ~ TRUE,
    TRUE ~ FALSE)) %>% 
  ungroup() %>% 
  mutate(rank=rank(-arrests_low_per_officer)) %>% 
  arrange(rank) %>% 
  mutate(percent_low=round(arrests_low/arrests_all*100,1)) %>% 
  mutate(rank_percent=rank(-percent_low))

summary_agency_type7 <- full_join(summary_agency_type5, summary_agency_type3)
write_csv(summary_agency_type7, "agencies_ranked2008_2015.csv", na="")

deviation <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  #filter(!is.na(ice)) %>% 
  filter(officers_total>3) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  group_by(ori) %>% 
  mutate(pop=median(population, na.rm=T)) %>% 
  mutate(avg_low=mean(low_level_arrests, na.rm=T)) %>% 
  mutate(deviation=round((low_level_arrests-avg_low)/avg_low*100,1)) %>% 
  mutate(diff=low_level_arrests-avg_low) %>% 
  mutate(per_pop=round(pop/low_level_arrests)) %>% 
  select(ori, agency_name_edit, year, pop, low_level_arrests, avg_low, deviation, diff, per_pop)

solo <- joined2 %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  #filter(!is.na(ice)) %>% 
  filter(officers_total>3) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  group_by(ori) %>% 
  mutate(pop=median(population, na.rm=T)) %>% 
  mutate(avg_low=mean(low_level_arrests, na.rm=T)) %>% 
  mutate(deviation=round((low_level_arrests-avg_low)/avg_low*100,1)) %>% 
  mutate(diff=low_level_arrests-avg_low) %>% 
#  mutate(per_pop=round(pop/low_level_arrests)) %>% 
  mutate(per_pop=round(low_level_arrests/pop*1000)) %>% 
  select(ori, agency_name_edit, year, pop, low_level_arrests, avg_low, deviation, diff, per_pop) %>% 
  filter(ori=="MD0110000")

# 548... deviation, 544, 1462.4286

# 1745.8... diff, 4486, 159

View(solo)

# identify the outliers
thou <- filter(deviation, deviation>400 & diff>400) %>% 
  #select(ori) %>% 
  #unique() 
  select(ori, year) %>% 
  mutate(outlier="ignore")

joined3 <- joined2 %>% 
  filter(ori %in% thou$ori) %>% 
  filter(year>=2008) %>% 
  filter(!is.na(total_police_employees)) %>% 
  #filter(!is.na(ice)) %>% 
  filter(officers_total>3) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  group_by(ori) %>% 
  mutate(pop=median(population, na.rm=T)) %>% 
  mutate(avg_low=mean(low_level_arrests, na.rm=T)) %>% 
  mutate(deviation=round((low_level_arrests-avg_low)/avg_low*100,1)) %>% 
  mutate(diff=low_level_arrests-avg_low) %>% 
  mutate(per_pop=round(low_level_arrests/pop)) %>% 
  filter(low_level_arrests!=0) %>% 
  select(ori, agency_name_edit, year, pop, low_level_arrests, avg_low, deviation, diff, per_pop)


## clay county
filtered <- joined %>% 
  filter(ori=="MD0110000") %>% 
  group_by(year, AGCYTYPE, participant) %>%
  #group_by(AGCYTYPE, icesub) %>% 
  #group_by(AGCYTYPE) %>% 
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) #

View(filtered)
%>% 
  ggplot(aes(x=as.factor(year), y=arrests_low_per_officer)) +
  geom_bar(stat="identity", position="dodge")

##### counties by year ----

county_agency_count  <- joined %>% 
  filter(AGCYTYPE=="(000) Local police department" |
           AGCYTYPE=="(001) Sheriff's office" ) %>% 
  #  filter(year>2013) %>% 
  group_by(year, FIPS, AGCYTYPE) %>% 
  summarize(agencies=n()) %>% 
  pivot_wider(names_from="AGCYTYPE", values_from="agencies") %>% 
  rename(police_agencies=`(000) Local police department`,
         sheriff_agencies=`(001) Sheriff's office`) 

county_officer_count <- joined %>% 
  filter(AGCYTYPE=="(000) Local police department" |
           AGCYTYPE=="(001) Sheriff's office" ) %>% 
  #  filter(year>2013) %>% 
  group_by(year, FIPS, AGCYTYPE) %>% 
  summarize(total_officers=sum(officers_total, na.rm=T)) %>% 
  group_by(year, FIPS) %>% 
  mutate(percent_officers=round(total_officers/sum(total_officers, na.rm=T)*100,2)) %>% 
  pivot_wider(names_from="AGCYTYPE", values_from=c("total_officers", "percent_officers")) %>% 
  rename(total_police_officers=`total_officers_(000) Local police department`,
         total_sheriff_officers=`total_officers_(001) Sheriff's office`,
         percent_police_officers=`percent_officers_(000) Local police department`,
         percent_sheriff_officers=`percent_officers_(001) Sheriff's office`) 

county_arrests <- joined %>% 
  filter(AGCYTYPE=="(000) Local police department" |
           AGCYTYPE=="(001) Sheriff's office" ) %>% 
  #  filter(year>2013) %>% 
  group_by(year, FIPS, AGCYTYPE) %>% 
  summarize(total_arrests=sum(arrests_all, na.rm=T)) %>% 
  group_by(year, FIPS) %>% 
  mutate(percent_arrests=round(total_arrests/sum(total_arrests, na.rm=T)*100,2)) %>% 
  pivot_wider(names_from="AGCYTYPE", values_from=c("total_arrests", "percent_arrests")) %>% 
  rename(total_police_arrests=`total_arrests_(000) Local police department`,
         total_sheriff_arrests=`total_arrests_(001) Sheriff's office`,
         percent_police_arrests=`percent_arrests_(000) Local police department`,
         percent_sheriff_arrests=`percent_arrests_(001) Sheriff's office`,
  )


county_annual <- county_agency_count %>% 
  left_join(county_officer_count) %>% 
  left_join(county_arrests) %>% 
  mutate(per_police=round(total_police_arrests/total_police_officers,1),
         per_deputy=round(total_sheriff_arrests/total_sheriff_officers,1))

g287 <- read_excel("data/raw_data/287g for Andrew2.xlsx") %>% 
  filter(!is.na(ori)) %>% 
  select(FIPS) %>% 
  mutate(`287g`=T) %>% 
  filter(!is.na(FIPS))

### add fips data
library(tidycensus)
p2018 <- get_acs(geography="county", variables="B03002_001",
                 geometry=F, survey="acs5", year=2018)
p2018 <- p2018 %>% 
  select(FIPS=GEOID, NAME) %>% 
  left_join(g287)

county_annual <- left_join(county_annual, p2018)

county_annual <- county_annual %>% 
  group_by(year) %>% 
  mutate(rank_per_capita_deputies = order(order(per_deputy, decreasing=TRUE)))

write_csv(county_annual, "outputs/summarized_data/county_annual_287g.csv", na="")


county_annual_287g <- county_annual %>% 
  filter(`287g`==T) %>% 
  group_by(year) %>% 
  mutate(rank_per_capita_deputies = order(order(per_deputy, decreasing=TRUE)))

write_csv(county_annual_287g, "outputs/summarized_data/county_annual_287g.csv", na="")

#### counties_annual_rate -----

counties_annual_rate <- joined %>% 
  filter(AGCYTYPE=="(000) Local police department" |
           AGCYTYPE=="(001) Sheriff's office" ) %>% 
  #  filter(year>2013) %>% 
  group_by(year, FIPS, AGCYTYPE) %>% 
  summarize(agencies=n(),
            officers_total=sum(officers_total, na.rm=T),
            arrests_all=sum(arrests_all, na.rm=T), 
            low_level_arrests=sum(low_level_arrests, na.rm=T)) %>% 
  
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_officer=round(arrests_all/officers_total, 1),
    arrests_low_per_officer=round(low_level_arrests/officers_total, 1),
    percent_low=round(low_level_arrests/arrests_all*100, 2)) %>% 
  
  filter(officers_total!=0)


##### agencies rated by year ----

agencies_annual_rate <- joined %>% 
  #  filter(year>2013) %>% 
  select(year, ori, ncic_agency_name, agency_name_edit, agency_type_name,
         state_abbr, FCOUNTY, FPLACE, FIPS, ORI7, NAME, UA,
         STATENAME, COUNTYNAME, PARTOF, AGCYTYPE, SUBTYPE1, 
         ADDRESS_STR1, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP, 
         CSLLEA08_ID, INTPTLAT, INTPTLONG,ice,
         officers_total, arrests_all, low_level_arrests) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_officer=round(arrests_all/officers_total, 1),
    arrests_low_per_officer=round(low_level_arrests/officers_total, 1),
    percent_low=round(low_level_arrests/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(000) Local police department" |
           AGCYTYPE=="(001) Sheriff's office" ) %>% 
  filter(officers_total!=0)



##### since 2010 summarized ----

agency_summary <- joined %>% 
  #filter(year>2013) %>% 
  group_by(ori, ncic_agency_name, agency_name_edit, agency_type_name,
           state_abbr, FCOUNTY, FPLACE, FIPS, ORI7, NAME, UA,
           STATENAME, COUNTYNAME, PARTOF, AGCYTYPE, SUBTYPE1, 
           ADDRESS_STR1, ADDRESS_CITY, ADDRESS_STATE, ADDRESS_ZIP, 
           CSLLEA08_ID, INTPTLAT, INTPTLONG,ice) %>% 
  summarize(officers_total=sum(officers_total, na.rm=T),
            arrests_all=sum(arrests_all, na.rm=T), 
            arressts_old=sum(arrests_old, na.rm=T),
            low_level_arrests=sum(low_level_arrests, na.rm=T)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_officer=round(low_level_arrests/officers_total,1),
    percent_low=round(low_level_arrests/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(000) Local police department" |
           AGCYTYPE=="(001) Sheriff's office" ) %>% 
  filter(officers_total!=0)

write_csv(agency_summary, "outputs/summarized_data/agency_summary.csv", na="")

agency_summary_287g <- agency_summary %>% 
  filter(ice==T) %>% 
  ungroup() %>% 
  mutate(rank_arrests_low_per_officer = rank(-arrests_low_per_officer))

write_csv(agency_summary_287g, "outputs/summarized_data/agency_summary_287g.csv", na="")

## urban / rural

urb <- read_csv("data/raw_data/rural_urban_designations.csv") %>% 
  select(FIPS=fips, x2013_code, county_category) %>% 
  mutate(county_cat=case_when(
    x2013_code==1 | x2013_code==2 ~ "Urban",
    x2013_code==3 | x2013_code==4 ~ "Suburban",
    x2013_code==5 | x2013_code==6 ~ "Rural"
  ))



mutate_agency_type <- joined %>% 
  #filter(year>2013) %>% 
  filter(!is.na(total_police_employees)) %>% 
  #filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  #group_by(AGCYTYPE, ice) %>% 
  group_by(ori, AGCYTYPE, icesub) %>% 
  #group_by(AGCYTYPE) %>% 
  mutate(total=n(),
         arrests_all=sum(arrests_all),
         arrests_low=sum(low_level_arrests),
         population=sum(population),
         population2=sum(LG_POPULATION),
         total_police = sum(total_police_employees),
         officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(arrests_all!=0) %>% 
  group_by(ori, AGCYTYPE, ice) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(AGCYTYPE, ice)

# sheriffs agencies rural/suburban/rural
summary_agency_type3 <- joined %>% 
  left_join(urb) %>% 
  #filter(year>2013) %>% 
  filter(!is.na(total_police_employees)) %>% 
  filter(!is.na(arrests_all)) %>% 
  
  select(ori, AGCYTYPE, county_cat) %>% 
  unique() %>% 
  #filter(AGCYTYPE=="(001) Sheriff's office") %>% 
  count(AGCYTYPE, county_cat) 



summary_agency_type_year <- joined %>% 
  filter(!is.na(total_police_employees)) %>% 
  group_by(year, AGCYTYPE, ice) %>% 
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2))


summary_agency_type_year_sheriff <- joined %>% 
  filter(!is.na(total_police_employees)) %>% 
  group_by(year, AGCYTYPE, ice) %>% 
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>% 
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2)) %>% 
  filter(AGCYTYPE=="(001) Sheriff's office")


summary_agency_type_year_sheriff %>% 
  #ggplot(aes(x=year, y=arrests_per_officer)) +
  #ggplot(aes(x=year, y=arrests_low_per_officer)) +
  ggplot(aes(x=year, y=percent_low)) +
  geom_col() +
  facet_wrap(~ice)

summary_agency_type2 <- joined %>%
  left_join(urb) %>%
  #filter(year>2013) %>%
  filter(!is.na(total_police_employees)) %>%
  #filter(AGCYTYPE=="(001) Sheriff's office") %>%
  group_by(AGCYTYPE, county_cat) %>%
  #group_by(AGCYTYPE, ice) %>%
  #group_by(county_cat) %>%
  summarize(total=n(),
            arrests_all=sum(arrests_all),
            arrests_low=sum(low_level_arrests),
            population=sum(population),
            population2=sum(LG_POPULATION),
            total_police = sum(total_police_employees),
            officers_total = sum(officers_total)) %>%
  mutate(#arrests_per_pop=round(arrests/population*1000),
    #arrests_per_pop2=round(arrests/population2*1000000),
    arrests_per_police=round(arrests_all/total_police,1),
    arrests_per_officer=round(arrests_all/officers_total,1),
    arrests_low_per_police=round(arrests_low/total_police,1),
    arrests_low_per_officer=round(arrests_low/officers_total,1),
    percent_low=round(arrests_low/arrests_all*100,2))


library(tidyverse)
agencies.coded %>% 
  count(V10)


## FOR TED

summary_agency_type2 %>% ungroup() %>% summarize(sum(arrests_all)) #82300474
summary_agency_type2 %>% group_by(AGCYTYPE) %>% summarize(sum(arrests_all)) #19% 15624803/82300474
summary_agency_type2 %>% ungroup() %>% summarize(sum(arrests_low))
#35010919
summary_agency_type2 %>% group_by(AGCYTYPE) %>% summarize(sum(arrests_low))
#7550644/35010919 #22%

summary_agency_type2 %>% filter(county_cat=="Rural") %>% ungroup() %>% summarize(sum(arrests_all))
#13160392
summary_agency_type2 %>% filter(county_cat=="Rural") %>% group_by(AGCYTYPE) %>% summarize(sum(arrests_all))
#4380447/13160392
summary_agency_type2 %>% filter(county_cat=="Rural") %>% ungroup() %>% summarize(sum(arrests_low))
#13160392
summary_agency_type2 %>% filter(county_cat=="Rural") %>% group_by(AGCYTYPE) %>% summarize(sum(arrests_low))
#2217356/6157255

#33 percent of all rural arrests are made by sheriffs departments
#36 percent all low level

summary_agency_type3 %>% group_by(AGCYTYPE) %>% summarize(sum(n))
summary_agency_type3 %>% filter(county_cat=="Rural") %>% summarize(sum(n))
summary_agency_type3 %>% filter(county_cat=="Rural") %>% group_by(AGCYTYPE) %>% 
  summarize(sum(n))


