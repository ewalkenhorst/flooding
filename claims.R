# import claims data
library(readr)
library(readxl)
library(dplyr)
library(SparkR)
library(lubridate)
claims <- read_csv("data/FIMA_NFIP_Redacted_Claims_Data_Set/openFEMA_claims20190831.csv")
claims_dot <- read.csv("data/FIMA_NFIP_Redacted_Claims_Data_Set/openFEMA_claims20190831.csv")
# narrow to Arkansas
ar_claims <- dplyr::filter(claims, state=="AR") %>%
  dplyr::arrange(ymd(dateofloss))
# narrow to 2019 and other dates
ar_claims_19 <- dplyr::filter(ar_claims, yearofloss=="2019")
ar_claims_19$month <- month(ar_claims_19$dateofloss, label=TRUE)
ar_claims_19$day <- day(ar_claims_19$dateofloss)
# narrow to county codes of interest and date range
# County codes: Arkansas: 05001, Conway: 05029, Crawford: 05033, Desha: 05041, Faulkner: 05045, Jefferson: 05069, Lincoln: 05079, Logan: 05083, Perry: 05105, Pope: 05115, Pulaski: 05119, Sebastian: 05131, Yell: 05149
# Dates: May 21-June 14
# Disaster (not in dataset): DR-4441-AR
ar_claims_dr4441ar <- dplyr::filter(ar_claims_19, countycode=="05001" | countycode=="05033" | countycode=="05041" | countycode=="05045" | countycode=="05069" | countycode=="05079" | countycode=="05083" | countycode=="05105" | countycode=="05115" | countycode=="05119" | countycode=="05131" | countycode=="05149") %>%
  dplyr::filter(., dateofloss >= "2019-05-21", dateofloss <= "2019-06-14") %>%
  dplyr::mutate(., amt_paid_claim=sum(amountpaidonbuildingclaim, amountpaidoncontentsclaim, amountpaidonincreasedcostofcomplianceclaim))
dplyr::group_by(ar_claims_dr4441ar, amountpaidonbuildingclaim) %>%
  dplyr::summarize(total=sum(amountpaidonbuildingclaim))
dplyr::filter(ar_claims_dr4441ar, amountpaidonbuildingclaim==!is.na(amountpaidonbuildingclaim)) %>%
  dplyr::summarize(., total=sum(amountpaidonbuildingclaim))

#import severe reptitive loss data
srp_2018 <- read_csv("hc_severe_repetitive_loss_properties_flood_games_arkansas.csv")
srp_2018_byprop <- dplyr::group_by(srp_2018, proplocatr) %>%
  dplyr::summarize(total=n())

#import public assistance data
pub_assist <- read_csv("https://www.fema.gov/api/open/v1/PublicAssistanceFundedProjectsSummaries.csv")
pub_assist_arflood19 <- dplyr::filter(pub_assist, disasterNumber=="4441")
pub_assist_apps <- read_csv("https://www.fema.gov/api/open/v1/PublicAssistanceApplicants.csv")
pub_assist_apps_arflood19 <- dplyr::filter(pub_assist_apps, disasterNumber=="4441")
pub_assist_detail <- read_csv("https://www.fema.gov/api/open/v1/PublicAssistanceFundedProjectsDetails.csv")
pub_assist_detail_arflood19 <- dplyr::filter(pub_assist_detail, disasterNumber=="4441" & state=="Arkansas") %>%
  dplyr::arrange(., -federalShareObligated)
#for Datawrapper
write.csv(pub_assist_arflood19_full, "pub_assist_arflood19_full.csv")
write.csv(pub_assist_detail_arflood19, "pub_assist_detail_arflood19.csv")

#analyze money given to Arkansas for the flood
arflood19_pub_assist_detail_projects <- unique(pub_assist_detail_arflood19$applicationTitle)
length(arflood19_pub_assist_detail_projects)
dplyr::summarize(pub_assist_detail_arflood19, total_fed_spending=sum(federalShareObligated))
pub_assist_arflood19_byapplicant <- dplyr::group_by(pub_assist_detail_arflood19, applicantId) %>%
  dplyr::summarize(., total=sum(federalShareObligated))
pub_assist_arflood19_full <- right_join(pub_assist_arflood19_byapplicant, pub_assist_apps_arflood19, by="applicantId") %>%
  dplyr::arrange(., -total)

#import housing assistance data and make it workable
library(stringr)
housing_assist_owners <- read_csv("FEMA_Housing_Assistance_Data_4_17_20_owners.csv")
names(housing_assist_owners) <- str_replace_all(names(housing_assist_owners), c(" " = "_", "," = "", "[$]" = "", "/" = "_", ">" = "morethan"))
housing_assist_owners$Total_Damage <- as.numeric(gsub("\\$|,", "", housing_assist_owners$Total_Damage))
housing_assist_owners$Average_FEMA_Inspected_Damage <- as.numeric(gsub("\\$|,", "", housing_assist_owners$Average_FEMA_Inspected_Damage))
housing_assist_owners$Total_Approved_IHP_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_owners$Total_Approved_IHP_Amount))
housing_assist_owners$Repair_Replace_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_owners$Repair_Replace_Amount))
housing_assist_owners$Rental_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_owners$Rental_Amount))
housing_assist_owners$Other_Needs_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_owners$Other_Needs_Amount))
housing_assist_renters <- read_csv("FEMA_Housing_Assistance_Data_4_17_20_renters.csv")
names(housing_assist_renters) <- str_replace_all(names(housing_assist_renters), c(" " = "_", "," = "", "[$]" = "", "/" = "_", ">" = "morethan"))
housing_assist_renters$Total_Approved_IHP_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_renters$Total_Approved_IHP_Amount))
housing_assist_renters$Repair_Replace_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_renters$Repair_Replace_Amount))
housing_assist_renters$Rental_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_renters$Rental_Amount))
housing_assist_renters$Other_Needs_Amount <- as.numeric(gsub("\\$|,", "", housing_assist_renters$Other_Needs_Amount))
#filtter to Arkansas River flooding
housing_assist_owners_arflood <- dplyr::filter(housing_assist_owners, Disaster=="4441" & State=="AR")
housing_assist_renters_arflood <- dplyr::filter(housing_assist_renters, Disaster=="4441" & State=="AR")
#analyze damage and payouts
dplyr::summarize(housing_assist_owners_arflood, total_arflood_damage=sum(Total_Damage))
dplyr::summarize(housing_assist_owners_arflood, total_arflood__owner_IHP=sum(Total_Approved_IHP_Amount))
dplyr::summarize(housing_assist_renters_arflood, total_arflood__renter_IHP=sum(Total_Approved_IHP_Amount))

#narrow buyouts to 4441 disaster
ar_buyouts_4441 <- dplyr::filter(ar_buyouts, disasterNumber==4441)

#import hgazard mitigation properties data, version 2 (newer), via https://www.fema.gov/openfema-dataset-hazard-mitigation-assistance-mitigated-properties-v2
haz_mit_props <- read_csv("https://www.fema.gov/api/open/v2/HazardMitigationAssistanceMitigatedProperties.csv")
haz_mit_props_arflood19 <- dplyr::filter(haz_mit_props, disasterNumber==4441)

#imort hazard mitigation projects data, version 2 (newer), via https://www.fema.gov/openfema-dataset-hazard-mitigation-assistance-projects-v2
haz_mit_projs <- read_csv("https://www.fema.gov/api/open/v2/HazardMitigationAssistanceProjects.csv")
haz_mit_projs_arflood19 <- dplyr::filter(haz_mit_projs, disasterNumber==4441)
haz_mit_grants <- read_csv("https://www.fema.gov/api/open/v1/HazardMitigationGrantProgramDisasterSummaries.csv")
haz_mit_grants_arflood19 <- dplyr::filter(haz_mit_grants, disasterNumber==4441)
