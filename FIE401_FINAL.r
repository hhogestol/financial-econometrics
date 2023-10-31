library(tidyverse)
library(stargazer)
library(DescTools)
library(plm)
library(lmtest)
library(lfe)
require(sandwich)
require(car)



house <- read_delim("1976-2020-house.tab")
emissions <- readRDS("emissions.rds")
emissions_location <- readRDS("emissions_location.rds")

year_to_congress <- function(year) {
  return(floor(116 - (2018-year)/2))
}


############################## House data ##############################
start_year = 2000
end_year = 2018
valid_election_years = c(start_year:end_year)

#Removes redundant
house2 <- 
  house %>%
  rename(election_year = year) %>%
  filter(party %in% c("REPUBLICAN", "DEMOCRAT")) %>% #Only keep relevant parties
  filter(special == FALSE) %>%# Remove special elections
  filter(stage == "GEN") %>% # Only keep general elections / This is used as a backup incase something is wrong with special data
  dplyr::select(election_year, district, party, candidate, candidatevotes, state, state_po, totalvotes) %>% #Keep relevant columns
  rename(stateAbb = state_po) #For merging purposes

# Finds winners
house_winners <-
  house2 %>%
  group_by(election_year, state, district) %>% 
  slice(which.max(candidatevotes)) %>%   #Only keep the winners (The people that are elected)
  mutate(congress = year_to_congress(election_year)) %>% #Calculate which congress
  mutate(share_highest = candidatevotes / totalvotes) %>%
  ungroup()

#Finds runner-ups
house_lowest2 <-
  house2 %>% 
  group_by(election_year, state, district) %>% 
  slice(which.min(candidatevotes)) %>% 
  mutate(congress = year_to_congress(election_year)) %>% #Calculate which congress
  ungroup() %>%
  mutate(share_lowest = candidatevotes / totalvotes) %>%
  select(election_year, state, district, candidatevotes, totalvotes,  share_lowest)


house_winners <- merge(house_winners, house_lowest2, by = c("election_year", "state", "district", "totalvotes")) #CHECK TOTALVOTES

house_winners  <-
  house_winners %>%
  mutate(vote_margin = share_highest - share_lowest)

#Check that it is correct
#filter(house_winners, congress == 115, candidate == "RUBEN KIHUEN")

house_winners <-
  house_winners %>%
  group_by(stateAbb, district) %>%
  mutate(previous_party = dplyr::lag(party, order = congress)) %>% 
  mutate(party_changed = party != previous_party) %>% #Checks whether a new party won
  mutate(dem_to_rep = ifelse(previous_party == "DEMOCRAT" & party == "REPUBLICAN", TRUE, FALSE)) %>% 
  mutate(rep_to_dem = ifelse(previous_party == "REPUBLICAN" & party == "DEMOCRAT", TRUE, FALSE)) %>%
  filter(election_year %in% valid_election_years) #Remove non-relevant years

#n <- house_winners[
#  with(house_winners, order(stateAbb, district, congress)),]

############################## Emission data ##############################

# Treats data
emissions2 <-
  emissions %>%
  filter(cleanAirActIndicator == "YES") %>% #Only keep relevant chemicals
  mutate(emissionAir = ifelse(unitOfMeasurement %in% c("Pounds", "POUNDS"), emissionAir * 453.59237, emissionAir)) %>%  #Convert pounds to grams
  mutate(unitOfMeasurement = ifelse(unitOfMeasurement %in% c("Pounds", "POUNDS"), "Grams_converted", unitOfMeasurement)) %>%  #Correct metric name (All are grams)
  mutate(congress = year_to_congress(reportingYear - 1)) %>% #Calculate which congress were in control during the year congress
  rename(stateAbb = facilityState) # For merging purposes

emissions2 <-
  emissions2 %>%
  group_by(facilityID, chemicalName) %>%
  mutate(lagged_indexLevel = dplyr::lag(prodIndexNormLevel, default = NA)) %>%
  mutate(correct_indexLevel = lagged_indexLevel * prodIndexChange) %>%
  ungroup() %>%
  filter(prodIndexNormLevel == correct_indexLevel)

############################## Merging ##############################
#For merging
temp <- 
  merge(emissions2, emissions_location, by = c("facilityID", "congress", "stateAbb")) %>%
  relocate(district, .after = facilityID) # Temporary for viewing purposes

house_winners <-
  house_winners %>%
  ungroup() %>%
  select(party, congress, stateAbb, district, party_changed, dem_to_rep, rep_to_dem, vote_margin, previous_party) # Select relevant columns

#Final merged data
merged <- merge(temp, house_winners, by = c("congress", "stateAbb", "district"))

#Testing
n <- filter(emissions2, facilityID == "00602SMRTMRD115", chemicalID == "7439921")
n <- filter(emissions, facilityID == "71730LNLRF1000M", chemicalName == "1,2,4-Trimethylbenzene")


n <- filter(emissions, facilityID == "71730LNLRF1000M", chemicalName == "1,2,4-Trimethylbenzene")

n2 <- n[with(n, order(reportingYear, chemicalID)),]


############################## Data Treatment ##############################
#merged$emissionAir <- Winsorize(merged$emissionAir, probs = c(0.005, 0.995))
#g_pdata <- pdata.frame(merged, index = c("reportingYear", "facilityID", "chemicalID"))

reg_data <-merged


reg_data[is.na(reg_data) | reg_data=="Inf"] = NA 
reg_data <-
  reg_data %>%
  mutate(prodIndexChange = replace(prodIndexChange, prodIndexChange <= 0, NA)) 

reg_data <-
  reg_data %>%
  mutate(prodIndexNormLevel = replace(prodIndexNormLevel, prodIndexNormLevel <= 0, NA)) 

reg_data <-
  reg_data %>%
  mutate(emissionAir = replace(emissionAir, emissionAir <= 0, NA)) 

reg_data <-
  reg_data %>%
  dplyr::select(facilityID, chemicalID, reportingYear, stateAbb, emissionAir, prodIndexChange, prodIndexNormLevel, party, party_changed, rep_to_dem, dem_to_rep, vote_margin, previous_party)

reg_data <-
  reg_data %>%
  na.omit()

reg_data <- 
  reg_data %>%
  mutate(emissionAir = Winsorize(emissionAir, probs = c(0.01,0.99)),
         prodIndexChange = Winsorize(prodIndexChange, probs = c(0.01, 0.99)),
         prodIndexNormLevel = Winsorize(prodIndexNormLevel, probs = c(0.01, 0.99)))
reg_data <-
  reg_data %>%
  mutate(party = ifelse(party == "REPUBLICAN", 1, 0)) %>%
  mutate(previous_party = ifelse(previous_party == "REPUBLICAN", 1, 0))
############################## Analysis ##############################


############################ Regressions ##############################
# creating temp_id
reg_data$temp_id <- paste(reg_data$facilityID, reg_data$chemicalID)



# defining panel data frame
pdata <- pdata.frame(reg_data, index=c("temp_id","reportingYear"))



#Testing
test1 <- plm(log(emissionAir) ~ party + I(party * previous_party)  + prodIndexChange, data = pdata, model = "within")
test2 <- plm(log(emissionAir) ~ party + I(party * previous_party)  + prodIndexChange, data = pdata, model = "random")

plm::phtest(test1, test2)

#table 2
#baseline
fit1 <- felm(log(emissionAir) ~ party  | 0 | 0 | temp_id, data = pdata) 

#detected changes from production
fit2 <- felm(log(emissionAir) ~ party + I(party * previous_party)  + prodIndexChange | 0 | 0 | temp_id, data = pdata)

#with state + time fixed effects
fit3 <- felm(log(emissionAir) ~ party + I(party * previous_party)  + prodIndexChange  | stateAbb +  reportingYear  | 0 | temp_id, data = pdata)

stargazer(fit1, fit2, fit3,
          title="Table 2: Political Partisanship's Effect On Pollution",
          type="html",
          out="Table2Final.html",
          keep.stat = c("n","rsq","adj.rsq"),
          report = ('vc*t'),
          dep.var.labels = "Air Pollution (log)",
          covariate.labels = c("Republican Partisanship",
                               "Republican Second Term",
                               "Change In Production"),
          omit.stat = c("adj.rsq"),
          add.lines = list(c("Fixed Effects","None","None", "Year/State"),
                           c("Sample","A", "A", "A")))

stargazer(fit1, fit2, fit3, fit4,
          type = "text",
          keep.stat=c("n", "adj.rsq"),
          report=('vc*t'),
          out = "test.html")

#table 3
data_table3 <-
  reg_data %>%
  filter(vote_margin < 0.05, vote_margin > 0)

#baseline
fit1.limited <- felm(log(emissionAir) ~ party | 0 | 0 | temp_id, data = data_table3) 

#detected changes from production
fit2.limited <- felm(log(emissionAir) ~ party + I(party * previous_party) + prodIndexChange  | 0 | 0 | temp_id, data = data_table3)

#with state + time fixed effects
fit3.limited <- felm(log(emissionAir) ~ party + I(party * previous_party) + prodIndexChange  | stateAbb +  reportingYear  | 0 | temp_id, data = data_table3)

stargazer(fit1.limited, fit2.limited, fit3.limited,
          title="Table 3: Causality",
          type="html",
          out="Table3Final.html",
          keep.stat = c("n","rsq","adj.rsq"),
          report = ('vc*t'),
          dep.var.labels = "Air Pollution (log)",
          covariate.labels = c("Republican Partisanship",
                               "Republican Second Term",
                               "Change In Production"),
          omit.stat = c("adj.rsq"),
          add.lines = list(c("Fixed Effects", "None", "None", "Year/State"),
                           c("Sample","B", "B", "B")))


##################### Task 4 #########################

merged3 <-
  merged %>%
  drop_na(dbnrParent)

merged3 <-
  merged3 %>%
  group_by(dbnrParent, reportingYear) %>%
  mutate(total_facilities = length(unique(facilityID))) %>%
  ungroup()

merged3 <-subset(merged3, total_facilities != 1) # Remove companies that only has one factory


merged3 <-
  merged3 %>%
  group_by(dbnrParent, reportingYear, district) %>%
  mutate(count_dist = length(unique(facilityID))) %>%
  mutate(count_other_dist = total_facilities - count_dist)



merged3 <-
  merged3 %>%
  group_by(dbnrParent, reportingYear,party) %>%
  mutate(fac_dist = length(unique(facilityID))) %>%
  mutate(total_reps = fac_dist * (party == "REPUBLICAN")) %>%
  mutate(total_demo = fac_dist * (party == "DEMOCRAT")) %>%
  ungroup() %>%
  group_by(dbnrParent, reportingYear) %>%
  mutate(total_reps = max(total_reps)) %>%
  mutate(total_demo = max(total_demo)) %>%
  dplyr::select(-fac_dist)
 # mutate(share_republican = reps / total_facilities)

#merged3 <-
#  merged3 %>%
 # mutate(above_median = share_republican > median(share_republican))

merged3 <-
  merged3 %>%
  group_by(dbnrParent, reportingYear, party, district) %>%
  mutate(count_party = length(unique(facilityID))) %>%
  mutate(count_demo_in_dist = count_dist * (party == "DEMOCRAT")) %>%
  mutate(count_rep_in_dist = count_dist * (party == "REPUBLICAN")) %>%
  ungroup() %>%
  group_by(district) %>%
  mutate(reps_outside = total_reps - count_rep_in_dist) %>%
  mutate(dems_outside = total_demo - count_demo_in_dist)

merged3 <- subset(merged3, count_other_dist != 0) # Remove companies that have all factories in one district

merged3 <-
  merged3 %>%
  mutate(share_republican = reps_outside / count_other_dist) %>%
  mutate(share_democrats = dems_outside / count_other_dist)
 # dplyr::select(-c(total_facilities, count_dist, reps, demo, count_party, count_rep, count_demo)) 
  

all(merged3$reps_outside + merged3$dems_outside == merged3$count_other_dist) # Test for whether calculations are correct


merged3$above_median = merged3$share_democrats > median(merged3$share_democrats)


#test1 <- filter(merged3, dbnrParent == "808957229", reportingYear == 2002) #party == "DEMOCRAT", district == "005") 
#test2 <- filter(merged3, dbnrParent == "194634457", reportingYear == 2001) #party == "REPUBLICAN", district == "001")
#test3 <- filter(merged3, dbnrParent == "968393996", reportingYear == 2003)
#test4 <- filter(merged3, dbnrParent == "968393996", reportingYear == 2003, district == "002")


reg_data <-merged3


reg_data[is.na(reg_data) | reg_data=="Inf"] = NA 

reg_data <-
  reg_data %>%
  mutate(emissionAir = replace(emissionAir, emissionAir <= 0, NA)) 

reg_data <-
  reg_data %>%
  dplyr::select(facilityID, chemicalID, reportingYear, stateAbb, emissionAir, prodIndexChange, prodIndexNormLevel, party, party_changed, rep_to_dem, dem_to_rep, vote_margin, previous_party, share_republican, above_median, share_democrats)

reg_data <-
  reg_data %>%
  na.omit()

reg_data <- 
  reg_data %>%
  mutate(emissionAir = Winsorize(emissionAir, probs = c(0.01,0.99)))
reg_data <-
  reg_data %>%
  mutate(party = ifelse(party == "REPUBLICAN", 1, 0)) %>%
  mutate(previous_party = ifelse(previous_party == "REPUBLICAN", 1, 0))

reg_data$temp_id <- paste(reg_data$facilityID, reg_data$chemicalID)


fit1.lst<- felm(log(emissionAir) ~ I(share_democrats*party) | 0 | 0 | temp_id, data = reg_data) 

fit2.lst <- felm(log(emissionAir) ~ I(share_democrats*party) |  chemicalID + reportingYear | 0 | temp_id, data = reg_data)

fit3.lst <- felm(log(emissionAir) ~ I(share_democrats*party) | chemicalID + facilityID + district | 0 | temp_id, data = reg_data)


stargazer(fit1.lst, fit2.lst, fit3.lst, 
          title="Table 4: Firms' Reallocation of Emissions",
          type="html",
          out="Table4Final.html",
          keep.stat = c("n","adj.rsq"),
          report = ('vc*t'),
          dep.var.labels = "log(Emissions)",
          covariate.labels = c("Other Facilities' Democrat Share"),
          omit.stat = c("adj.rsq"),
          add.lines = list(c("FE District", "No", "Yes", "No", "No", "Yes"),
                           c("FE Year", "No", "Yes", "Yes", "No", "No"),
                           c("FE Chemical", "No", "No", "Yes", "Yes", "Yes"),
                           c("Sample","C", "C", "C", "C", "C")))


table_A_summary <-
  merged %>%
  mutate(party = ifelse(party == "REPUBLICAN", 1, 0)) %>%
  mutate(emissionAir = emissionAir / 1000) %>%
  #mutate(party = )
  dplyr::select(emissionAir, prodIndexChange, party, vote_margin)

table_B_summary <-
  data_table3 %>%
  mutate(emissionAir = emissionAir / 1000) %>%
  select(emissionAir, prodIndexChange, party, vote_margin)

table_C_summary <-
  merged3 %>%
  mutate(party = ifelse(party == "REPUBLICAN", 1, 0)) %>%
  mutate(emissionAir = emissionAir / 1000) %>%
  select(emissionAir, share_democrats, party) %>%
  filter(share_democrats >= 0, share_democrats <= 1) 
  table_C_summary <- as.data.frame(table_C_summary)

table_temp <-
  merged %>% 
  group_by(party) %>%
  mutate(emissionAir = emissionAir / 1000) %>%
  rename("Emission" = "emissionAir") %>%
  rename("Change in Production" = "prodIndexChange") %>%
  rename("Win-Margin" = "vote_margin") %>%
  dplyr::select(-party) %>%
  summarise_at(c("Emission", "Change in Production", "Win-Margin"), mean, na.rm=TRUE) %>%
  
  
stargazer(as.data.frame(table_temp), 
          title = "Panel A: Party specific",
          type = "html", 
          flip = TRUE, 
          covariate.labels = c(" ", "Democrat", "Republican"),
        #  keep = c("Emission","Change in Production","Win-Margin"),
          summary = FALSE, 
          digits = 2,
          out = "PanelA_party.html")


stargazer(list(table_A_summary),
          type="html",
          title=c("Panel A"),
          omit.summary.stat = c("p25","p75"),
          #keep = c("turnover","bidask","Leverage","mkvalt"),
          covariate.labels = c("Emission (Thousands)","Change in Production","Republican Partisanship","Win-Margin"),
          out="PanelA.html",
          digits = 2,
          flip=FALSE)

stargazer(list(table_B_summary),
          type="html",
          title=c("Panel B"),
          omit.summary.stat = c("p25","p75"),
          #keep = c("turnover","bidask","Leverage","mkvalt"),
          covariate.labels = c("Emission (Thousands)","Change in Production","Republican Partisanship","Win-Margin"),
          out="PanelB.html",
          digits = 2,
          flip=FALSE)

stargazer(list(table_C_summary),
          type="html",
          title=c("Panel C"),
          omit.summary.stat = c("p25","p75"),
          #keep = c("turnover","bidask","Leverage","mkvalt"),
          covariate.labels = c("Emission (Thousands)","Other Facilities' Democrat Share","Republican Partisanship"),
          digits = 2,
          flip=FALSE,
          out="PanelC.html")

