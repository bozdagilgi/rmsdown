#####SSD data anonymized version for public with figures


###Document below to be used an example!

##Remove past activities

rm(list = ls())

####Load libraries and data ----

# Install pacman if not already installed
if(!require(pacman)) install.packages('pacman')

# Install the unhcrthemes package from GitHub
install.packages("remotes")
remotes::install_github("unhcr/unhcrthemes")


# Load all required libraries using pacman
pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled )


##RUN below function


labelled_chr2dbl <- function(x) {
  varlab <- var_label(x)
  vallab <- val_labels(x)
  vallab <- setNames(as.numeric(vallab),
                     names(vallab))
  x <- as.numeric(as.character(x))
  var_label(x) <- varlab
  val_labels(x) <- vallab
  x
}

##Set WD

setwd("C:/Users/BOZDAG/OneDrive - UNHCR/Desktop/UNHCR/R projets/Standard report template/South Sudan")



##Load datasets -- both datasets includes indicators already calculated and cleaned ( step 1 and 2 are complete)

main<- read_csv("main.csv")
ind <- read_csv("ind.csv")


CreateTableOne(data = main) # Check sample and distribution - HH level dataset

CreateTableOne(data = ind)  # check sample and distribution - individual level dataset

### Cleaned data with indicators ----

ind$citizenship <- ifelse(ind$citizenship == "SDN", "SDN",
                          ifelse(ind$citizenship == "XXX", "SSD", "SSD")
)

main$citizenship <- ifelse(main$citizenship == "SDN", "SDN",
                           ifelse(main$citizenship == "XXX", "SSD", "SSD")
                           
                           
)
###Add labels for disaggregation variables


##For more guidance - https://unhcr365.sharepoint.com/:x:/r/teams/ResultsMonitoringSurveysRMS-RMSStandardReportTemplate/Shared%20Documents/Standard%20report%20template/RMS%20indicator%20table.xlsx?d=wf8ea2ac92eac4324bd42b5f248622cab&csf=1&web=1&e=wL2EHp



###Age - HH07_cat

table(ind$HH07_cat) # 4 categories
table(ind$HH07_cat2) # under 18 / above 18

##Disability - disability

table(ind$disability)

main<- main %>%
  mutate(disability = factor(disability, levels = c(0, 1), labels = c("Non-disabled", "Disabled")))


table(main$disability)


###Gender - HH04 -- already labelled 

table(ind$HH04)


###Population groups

table(ind$pop_groups)


#### Creating the survey design object without stratification ---- for individual and HH level datasets 
RMS_SSD_2023_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


RMS_SSD_2023_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


### 2.2 Proportion of people residing in physically safe and secure settlements with access to basic facilities -----

### Indicator calculations 

##electricity 

table(main$LIGHT01)
table(main$LIGHT02)



main <- main %>%
  mutate(electricity = ifelse(LIGHT01 == "1" & LIGHT02 == "1" & LIGHT03 != "0", 1, 0)
  ) %>%
  mutate( electricity = labelled(electricity,
                                 labels = c(
                                   "Yes" = 1,
                                   "No" = 0
                                 ),
                                 label = "Access to electricity"))

table(main$electricity)


###healthcare

###Access to healthcare if household has any facility available excluding 'don't know' and 'other' 
#within one hour distance (cannot be > 60) (walking or any other type of transport)


main <- main %>%
  mutate(healthcare = ifelse(HEA01 != "96" & HEA01 != "98" & HEA03 <= 60, 1, 0)
  ) %>%
  mutate( healthcare = labelled(healthcare,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Access to healthcare facility"))

table(main$healthcare)


###drinking water

###Convert time variable to minutes only

main <- main %>%
  mutate(time_DWA=case_when(
    DWA03a=="1"~ "1", DWA03a=="2"~"60") #convert hour into minutes
  )

main$time_DWA <- as.numeric(main$time_DWA)

table(main$time_DWA)

###Compute variable with above conditions

main <- main %>%
  mutate(time_tot=time_DWA*DWA03b
  ) %>% 
  mutate(dwa_cond1=case_when( time_tot > 30 ~ 0, 
                              TRUE ~ 1) # reachable under 30 minutes or NA
  ) %>% 
  mutate(dwa_cond2=case_when(DWA01!="7" |DWA01 !="9" |DWA01 != "13" | DWA01 != "96" |DWA01 !="98" ~ 1,
                             TRUE ~ 0) # improved source only
  ) %>%
  mutate(dwa_cond3=case_when(DWA02 == "3" ~ 0, 
                             TRUE ~ 1) # in the dwelling/yard/plot
  ) %>% 
  mutate(drinkingwater=case_when(
    ((dwa_cond1==1 | dwa_cond3==1) & dwa_cond2==1 ) ~ 1, TRUE ~ 0)
  ) %>%
  mutate(drinkingwater = labelled(drinkingwater,
                                  labels = c(
                                    "Yes" = 1,
                                    "No" = 0
                                  ),
                                  label = "Access to drinking water"))

table(main$drinkingwater)


main <-main %>%
  mutate(impact2_2=case_when(
    shelter==0 | electricity==0 | drinkingwater==0 | healthcare==0 | secure==0 ~ 0,
    shelter==1 & electricity==1 & drinkingwater==1 & healthcare==1 & secure==1 ~ 1)
  ) %>% 
  mutate(impact2_2=labelled(impact2_2,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                            label="Proportion of people residing in physically safe and secure settlements with access to basic facilities"))



###habitable shelter - updated use 3.2 and exclude SHEL05

##First check the variables
table(main$SHEL01)
table(main$SHEL02)
table(main$SHEL03)
table(main$SHEL04)
table(main$SHEL05)
table(main$SHEL06)

main$SHEL01 <- labelled_chr2dbl(main$SHEL01)
main$SHEL02 <- labelled_chr2dbl(main$SHEL02)
main$SHEL03 <- labelled_chr2dbl(main$SHEL03)
main$SHEL04 <- labelled_chr2dbl(main$SHEL04)
main$SHEL05 <- labelled_chr2dbl(main$SHEL05)
main$SHEL06 <- labelled_chr2dbl(main$SHEL06)




main <- main %>%
  mutate(across(starts_with("SHEL"), ~ifelse(. == 98, NA, .))) %>%
  mutate(housing = case_when(
    (SHEL01 == "1") & (SHEL02 == "1") & (SHEL05 == "1") & 
      (SHEL03 == "0" ) & (SHEL04 == "0" ) & (SHEL06 == "0" ) ~ 1,
    (SHEL01 == "0") | (SHEL02 == "0" ) | (SHEL05 == "0") |
      (SHEL03 == "1") | (SHEL04 == "1") | (SHEL06 == "1" ) ~ 0,
    TRUE ~ NA_integer_
  ))

table(main$housing)



##Condition 2
####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep
###Overcrowding may cause health issues, thus, not considered as physically safe


table(main$hh_size_001)
table(main$DWE05)


main <- main %>%
  mutate(crowding=hh_size_001/DWE05
  ) %>%
  mutate(dwe05_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 0)
  )


table(main$crowding)
table(main$dwe05_cat)

###Combine both conditions for habitable housing -- exclude DWE01

main <- main %>%
  mutate(shelter=case_when(
    dwe05_cat==1 & housing==1 ~ 1,
    TRUE ~ 0
  ))

table(main$shelter)




###safe environment 

##Step 5. Safe and secure settlements are those with no risks and hazards like flooding, landslides, landmines, and close proximity to military installations and hazardous zones

table(main$RISK01)
table(main$RISK02)


main <- main %>%
  mutate(secure=case_when(
    RISK01=="1" |  RISK02=="1" ~ 0,
    TRUE ~ 1
  ))

table(main$secure)



##Combine variables

###Calculate impact indicator based on electricity, healthcare, drinkingwater, shelter and secure

##Impact 2.2 is "1" if all services above are accessible

main <-main %>%
  mutate(impact2_2=case_when(
    shelter==0 | electricity==0 | drinkingwater==0 | healthcare==0 | secure==0 ~ 0,
    shelter==1 & electricity==1 & drinkingwater==1 & healthcare==1 & secure==1 ~ 1)
  ) %>% 
  mutate(impact2_2=labelled(impact2_2,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                            label="Proportion of people residing in physically safe and secure settlements with access to basic facilities"))


table(main$impact2_2)



####Standard tables 


composite_impact2_2 <- main %>%
  select(pop_groups, shelter, electricity, drinkingwater, secure, healthcare, impact2_2) %>%
  pivot_longer(cols = shelter:healthcare, names_to = "facility", values_to = "access") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = mean(access, na.rm = TRUE) * 100) %>%
  ungroup()



###Chart for above with all dimensions 


composite_impact2_2 %>%
  ggplot(aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_unhcr_d() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "Access to Facilities by Population Group",
    x = "Facility",
    y = "Percentage Access (%)",
    fill = "Population Groups"
  ) +
  theme_unhcr()

##Table by population groups

impact2_2 <- RMS_SSD_2023_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "impact2_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(impact2_2, vartype = c("ci", "se"), , na.rm = TRUE),  # indicator value ( weighted) with CI and SE
  )




###Chart of impact 2_2 by pop groups

ggplot(impact2_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Impact 2.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



### 2.3 Proportion of people with access to health services -----

##indicator calculations

ind <- ind %>%
  mutate(impact2_3=case_when(
    HACC01=="1" & HACC03=="1" ~ 1,
    HACC01=="0" ~ NA,
    HACC01=="1" & HACC03=="0" & (HACC04_1 == "1" | HACC04_2 == "1" | HACC04_4 == "1" | HACC04_7 == "1" | HACC04_10 == "1" | HACC04_11 == "1" | 
                                   HACC04_12 == "1" | HACC04_13 == "1") ~ 0 ,
    HACC01=="1" & HACC03=="0" & (HACC04_3 == "1" | HACC04_5 == "1" | HACC04_6 == "1" | HACC04_8 == "1" | 
                                   HACC04_9 == "1" | HACC04_96 == "1") ~ 1)
  ) %>%
  mutate(impact2_3=labelled(impact2_3,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                            label="Proportion of people with access to health"))




###Table by population groups

impact2_3 <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "impact2_3",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(impact2_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above




ggplot(impact2_3, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Impact 2.3",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity



impact2_3_AGD <- RMS_SSD_2023_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat)) %>%                     # Exclude if HH07_cat is NA
  group_by(HH07_cat, HH04, disability) %>%            # Group by Age, Gender, and Disability
  summarise(                                       # Summarise to compute values
    var_name = "impact2_3",                        # Name of the variable
    num_obs_uw = unweighted(n()),                  # Unweighted total count
    denominator = survey_total(),                  # Weighted total count
    mean_value = survey_mean(impact2_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


####Chart with the AGD variables 

ggplot(impact2_3_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_unhcr_d() +  # UNHCR color palette
  facet_wrap(~ HH07_cat) +  # Create facets for each age group
  labs(
    title = "Impact 2.3 by gender, age and disability status",
    x = "Gender",
    y = "Mean Value",
    fill = "Disability",
    caption = "Note: The disability module does not include children under 5."  #
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


#### The reasons for not being able to access to health services 

##add labels for not accessing the health services

reasons_mapping <- c(
  "HACC04_1" = "Health facility too far",
  "HACC04_2" = "Medicine or health facility too expensive",
  "HACC04_3" = "No treatment exists/Not necessary",
  "HACC04_4" = "Don't know where to go",
  "HACC04_5" = "No time",
  "HACC04_6" = "Prefer other options",
  "HACC04_7" = "Health facility does not accept new patients",
  "HACC04_8" = "Don't trust modern medicine",
  "HACC04_9" = "Don't trust doctors",
  "HACC04_10" = "Administrative/documentation issues (certificates, service cards, etc.)",
  "HACC04_11" = "Long waiting times",
  "HACC04_12" = "Lack of medical supplies",
  "HACC04_13" = "Health facility damaged/destroyed"
)

hacc_percentages <- ind %>%
  summarise(
    HACC04_1 = mean(HACC04_1 == 1, na.rm = TRUE) * 100,
    HACC04_2 = mean(HACC04_2 == 1, na.rm = TRUE) * 100,
    HACC04_3 = mean(HACC04_3 == 1, na.rm = TRUE) * 100,
    HACC04_4 = mean(HACC04_4 == 1, na.rm = TRUE) * 100,
    HACC04_5 = mean(HACC04_5 == 1, na.rm = TRUE) * 100,
    HACC04_6 = mean(HACC04_6 == 1, na.rm = TRUE) * 100,
    HACC04_7 = mean(HACC04_7 == 1, na.rm = TRUE) * 100,
    HACC04_8 = mean(HACC04_8 == 1, na.rm = TRUE) * 100,
    HACC04_9 = mean(HACC04_9 == 1, na.rm = TRUE) * 100,
    HACC04_10 = mean(HACC04_10 == 1, na.rm = TRUE) * 100,
    HACC04_11 = mean(HACC04_11 == 1, na.rm = TRUE) * 100,
    HACC04_12 = mean(HACC04_12 == 1, na.rm = TRUE) * 100,
    HACC04_13 = mean(HACC04_13 == 1, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = "Reason", 
               values_to = "Percentage") %>%
  mutate(Reason = reasons_mapping[Reason])  # Map column names to descriptive labels



##Chart creation


ggplot(hacc_percentages, aes(x = reorder(Reason, Percentage), y = Percentage, fill = Reason)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Reasons for Not Accessing Health Services",
    x = "Reason",
    y = "Percentage",
    caption = "Note: Percentages calculated independently for each reason."
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )

###### Empowering Communities and Achieving Gender Equality

### 3.2a Proportion of children and young people enrolled in primary education ----

###This indicator comes from the individual dataset

ind$EDU01 <- labelled_chr2dbl(ind$EDU01)
ind$EDU02 <- labelled_chr2dbl(ind$EDU02)
ind$EDU03 <- labelled_chr2dbl(ind$EDU03)



ind <- ind %>% 
  mutate(edu_primary = case_when
         ( EDU01 == 1 & EDU02 == 1 & EDU03 == 2 ~ 1, EDU01 == 0 | EDU02 == 0 ~ 0, 
           TRUE ~ 0) 
  ) %>%
  mutate(age_primary = case_when
         ( HH07 >= 6 & HH07 <=13 ~ 1, ###!!!!!ADJUST AGE GROUPS FOR EACH EDUCATIONAL LEVEL
           TRUE ~ 0) )


###Results of the indicator table



impact3_2a <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop_groups is NA
  group_by(pop_groups) %>%                          # Group by pop_groups
  summarise(
    var_name = "impact3_2a",                        # Name of the variable
    num_obs_uw = survey_total(!is.na(age_primary), na.rm = TRUE),  # Unweighted total count
    denominator = survey_total(edu_primary, na.rm = TRUE),         # Weighted total count
    edu_primary_total = survey_total(edu_primary, na.rm = TRUE),   # Total for edu_primary
    age_primary_total = survey_total(age_primary, na.rm = TRUE)    # Total for age_primary
  ) %>%
  mutate(
    mean_value = round(edu_primary_total / age_primary_total, 4)  # Calculate the mean value
  )


###Table with gender and disability included for the chart



impact3_2a_AGD <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups) & !is.na(disability) & !is.na(HH04)) %>%                     # Exclude if pop_groups is NA
  group_by(pop_groups,disability,HH04) %>%                          # Group by pop_groups
  summarise(
    var_name = "impact3_2a",                        # Name of the variable
    num_obs_uw = survey_total(!is.na(age_primary), na.rm = TRUE),  # Unweighted total count
    denominator = survey_total(edu_primary, na.rm = TRUE),         # Weighted total count
    edu_primary_total = survey_total(edu_primary, na.rm = TRUE),   # Total for edu_primary
    age_primary_total = survey_total(age_primary, na.rm = TRUE)    # Total for age_primary
  ) %>%
  mutate(
    mean_value = round(edu_primary_total / age_primary_total, 4)  # Calculate the mean value
  )



###Chart for the above table


ggplot(impact3_2a_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each pop_group
  labs(
    title = "Enrollment in primary education by Age, Gender, and Population Groups",
    x = "Gender ",
    y = "Proportion of Education Level",
    fill = "Disability Status"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )



### 3.2b Proportion of children and young people enrolled in secondary education ----



###This indicator comes from the individual dataset

###Include if they are attending secondary or secondary -technical and vocational
ind$EDU01 <- labelled_chr2dbl(ind$EDU01)
ind$EDU02 <- labelled_chr2dbl(ind$EDU02)
ind$EDU03 <- labelled_chr2dbl(ind$EDU03)

ind <- ind %>%
  mutate(edu_secondary=case_when(
    EDU01==1 & EDU02==1 & (EDU03==3 | EDU03==4) ~ 1, EDU01==0 | EDU02==0 ~ 0, 
    TRUE ~ 0)
  ) %>%
  mutate(age_secondary=case_when(
    HH07 >= 11 & HH07 <=18 ~ 1, TRUE ~ NA_real_)) ###!!!!!ADJUST AGE GROUPS FOR EACH EDUCATIONAL LEVEL   



###Results of the indicator table


impact3_2b <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop_groups is NA
  group_by(pop_groups) %>%                          # Group by pop_groups
  summarise(
    var_name = "impact3_2b",                        # Name of the variable 
    num_obs_uw = survey_total(!is.na(age_secondary), na.rm = TRUE),  # Unweighted total count for secondary age
    denominator = survey_total(edu_secondary, na.rm = TRUE),         # Weighted total count for secondary education
    edu_secondary_total = survey_total(edu_secondary, na.rm = TRUE), # Total for secondary education
    age_secondary_total = survey_total(age_secondary, na.rm = TRUE)  # Total for secondary age
  ) %>%
  mutate(
    mean_value = round(edu_secondary_total / age_secondary_total, 4)  # Calculate the mean value
  )


###Table with gender and disability included for the chart


impact3_2b_AGD <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups) & !is.na(disability) & !is.na(HH04)) %>%  # Exclude if pop_groups, disability, or HH04 is NA
  group_by(pop_groups, disability, HH04) %>%                          # Group by pop_groups, disability, and HH04
  summarise(
    var_name = "impact3_2b",                                          # Name of the variable (changed to impact3_2b)
    num_obs_uw = survey_total(!is.na(age_secondary), na.rm = TRUE),   # Unweighted total count for secondary age
    denominator = survey_total(edu_secondary, na.rm = TRUE),          # Weighted total count for secondary education
    edu_secondary_total = survey_total(edu_secondary, na.rm = TRUE),  # Total for secondary education
    age_secondary_total = survey_total(age_secondary, na.rm = TRUE)   # Total for secondary age
  ) %>%
  mutate(
    mean_value = round(edu_secondary_total / age_secondary_total, 4)  # Calculate the mean value
  )



###Chart for the above table

ggplot(impact3_2b_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each pop_group
  labs(
    title = "Enrollment in Secondary Education by Age, Gender, and Population Groups",
    x = "Gender",
    y = "Proportion of Education Level",
    fill = "Disability Status"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )




### 3.3 Proportion of PoC feeling safe walking alone in their neighborhood ----- 

###Indicator calculations

main <- main %>%
  mutate(impact3_3=case_when(
    SAF01==1 | SAF01==2 ~ 1,
    SAF01==3 | SAF01==4 ~ 0 , 
    SAF01==98 | SAF01==99 ~ NA_real_)
  ) %>% 
  mutate(impact3_3=labelled(impact3_3,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                            label="Proportion of people that feel safe walking alone in their neighbourhood after dark"))




###Table standard 


impact3_3 <- RMS_SSD_2023_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "impact3_3",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(impact3_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(impact3_3, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Impact 3.3",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

impact3_3_AGD <- RMS_SSD_2023_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "impact3_3",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(impact3_3), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(impact3_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )

#impact3_3_AGD <- impact3_3_AGD %>% ##delete unwanted age groups
 # filter(!(HH07_cat %in% c("Under 5", "5-17")))  

#impact3_3_AGD <- impact3_3_AGD  %>%
 # mutate(disability = factor(disability, levels = c(0, 1), labels = c("Non-disabled", "Disabled")))


####Chart with the AGD variables 


navy_palette <- c("#E0E9FE", "#B8C9EE", "#8395B9", "#506489", "#18375F")
red_palette <- c(unhcr_pal(12, "pal_red"))



ggplot(impact3_3_AGD, aes(x = disability, y = mean_value, fill = HH04)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Grouped bar chart
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +  # Add values on bars
  scale_fill_manual(values = c(navy_palette[4], red_palette[2])) +  # Apply custom color palette
  facet_wrap(~ HH07_cat) +  # Create facets for each age group
  labs(
    title = "Impact 3.3 by Gender, Age, and Disability Status",
    x = "Disability Status",
    y = "Mean Value",
    fill = "Gender",
    caption = "Note: Only 18 and above"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +  # Limit the y-axis from 0 to 1
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10)  # Adjust y-axis label size for readability
  )





### 1.2 Proportion of children  under 5 years whose birth have been registered with a civil authority ----
## Access to Territory, Registration and Documentation


# ind$REG03 - birth certificate
# ind$REG04 - birth has been registered

##Calculate children who has a birth certificate

ind <- ind %>%
  mutate(birthCertificate=case_when(
    REG03==0 | REG03==98 ~ 0, REG03==1 ~ 1)
  ) %>%
  mutate(birthCertificate=labelled(birthCertificate,
                                   labels=c(
                                     'Yes'=1,
                                     'No'=0
                                   ),
                                   label="Children under 5 with a birth certificate"))

##Calculate children who has been registered with civil authorities

ind <- ind %>%
  mutate(birthRegistered=case_when(
    REG04==0 | REG04==98 ~ 0, REG04==1 ~ 1, REG04==99 ~NA_real_)
  ) %>%
  mutate(birthRegistered=labelled(birthRegistered,
                                  labels=c(
                                    'Yes'=1,
                                    'No'=0
                                  ),
                                  label="Children under 5 birth registered with civil authorities"))


##if the birth is registered or child has a birth certificate


ind <- ind %>%
  mutate(outcome1_2=case_when(
    (birthRegistered==1 | birthCertificate==1) 
    & HH07 <5 ~ 1, 
    (birthRegistered==0 & birthCertificate==0) 
    & HH07 <5 ~ 0)
  ) %>%
  mutate(outcome1_2=labelled(outcome1_2,
                             labels=c(
                               'Yes'=1,
                               'No'=0
                             ),
                             label="Proportion of children under 5 years of age whose births have been registered with a civil authority"))





###Table 


outcome1_2 <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome1_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome1_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Table with disability and gender


outcome1_2_AGD <- RMS_SSD_2023_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(pop_groups) & HH07 < 5 ) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, pop_groups) %>%
  summarise(
    var_name = "outcome1_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(impact3_3), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome1_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )


##Chart with pop groups

gender_colors <- c("Male" = "#8395B9", "Female" = "#E0E9FE")


ggplot(outcome1_2_AGD, aes(x = HH04, y = mean_value, fill = HH04)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 1.2 by Population Groups and Gender",
    x = "Gender",
    y = "Proportion of Children that registered",
    caption = "Note: Only children under 5"
  ) +
  scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )



### 1.3 Proportion of PoC with legally recognized identity documents or credentials -----

###Calculate valid identity documents for under 5 with REG05 and REG06 variables

ind$REG05a <- labelled_chr2dbl(ind$REG05a) # passport
ind$REG05b <- labelled_chr2dbl(ind$REG05b) # civil/government issued ID
ind$REG05c <- labelled_chr2dbl(ind$REG05c) # residency permit
ind$REG05d <- labelled_chr2dbl(ind$REG05d) # statelessness documentation
ind$REG05e <- labelled_chr2dbl(ind$REG05e) # household card of address/family book
ind$REG05f <- labelled_chr2dbl(ind$REG05f) # social security card
ind$REG06 <- labelled_chr2dbl(ind$REG06) # any other document establishes identity

#ind$REG05a - passport
#ind$REG05b - civil/government issued ID
#ind$REG05c - residency permit
#ind$REG05d - statelessness documentation
#ind$REG05e - household card of address/family book
#ind$REG05f - social security card
#ind$REG06 - any other document establishes identity
#add birth certificate as additional document from REG03

#Make sure to delete REG05e below from the script if you don't have any stateless 

ind <- ind %>%
  mutate(document_under5=case_when(
    REG05a==1 | REG05b==1 | REG05c==1 | REG05d==1 | REG05e==1 | REG05f==1 |REG06==1 | REG03==1 ~ 1, 
    REG05a!=1 & REG05b!=1 & REG05c!=1 & REG05d!=1 & REG05e!=1 & REG05f!=1 & REG06!=1 & REG03!=1 ~ 0, TRUE ~ NA_real_
  ))

###Calculate  valid identity documents for above 5 with REG01 and REG02 variables

ind$REG01a <- labelled_chr2dbl(ind$REG01a) # passport
ind$REG01b <- labelled_chr2dbl(ind$REG01b) # birth certificate
ind$REG01c <- labelled_chr2dbl(ind$REG01c) # civil/ government issued ID
ind$REG01d <- labelled_chr2dbl(ind$REG01d) # residency permit
ind$REG01e <- labelled_chr2dbl(ind$REG01e) # statelessness documentation
ind$REG01f <- labelled_chr2dbl(ind$REG01f) # household card of address/family book
ind$REG01g <- labelled_chr2dbl(ind$REG01g) # social security card
ind$REG02 <- labelled_chr2dbl(ind$REG02) # any other document establishes identity

#ind$REG01a # passport
#ind$REG01b # birth certificate
#ind$REG01c # civil/ government issued ID
#ind$REG01d # residency permit
#ind$REG01e # statelessness documentation
#ind$REG01f # household card of address/family book
#ind$REG01g # social security card
#ind$REG02 # any other document establishes identity

#Make sure to delete REG01e below from the script if you don't have any stateless   

ind <- ind %>%
  mutate(document_above5=case_when(
    REG01a==1 | REG01b==1 | REG01c==1 | REG01d==1 | REG01e==1 | REG01f==1 | REG01g==1 |REG02==1 ~ 1,
    REG01a!=1 & REG01b!=1 & REG01c!=1 & REG01d!=1 & REG01e!=1 & REG01f!=1 & REG01g!=1 & REG02!=1 ~ 0, TRUE ~ NA_real_)
    
    ##Combine both age groups
  ) %>%
  mutate(outcome1_3=case_when(
    (document_above5==1 | document_under5==1) ~ 1,  
    (document_above5==0 | document_under5==0) ~ 0)
  ) %>%
  mutate(outcome1_3=labelled(outcome1_3,
                             labels=c(
                               'Yes'=1,
                               'No'=0
                             ),
                             label="Proportion of people with legally recognized identity documents or credentials"))


###Table by population groups

outcome1_3 <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome1_3",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome1_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above




ggplot(outcome1_3, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 1.3",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity



outcome1_3_AGD <- RMS_SSD_2023_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat)) %>%                     # Exclude if HH07_cat is NA
  group_by(HH07_cat, HH04, disability) %>%            # Group by Age, Gender, and Disability
  summarise(                                       # Summarise to compute values
    var_name = "outcome1_3",                        # Name of the variable
    num_obs_uw = unweighted(n()),                  # Unweighted total count
    denominator = survey_total(),                  # Weighted total count
    mean_value = survey_mean(outcome1_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


####Chart with the AGD variables 

ggplot(outcome1_3_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Grouped bar chart
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on top of bars
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  facet_wrap(~ HH07_cat) +  # Create facets for each age group
  labs(
    title = "Outcome 1.3 by Gender, Age, and Disability Status",
    x = "Gender",
    y = "Mean Value",
    fill = "Disability",
    caption = "Note: The disability module does not include children under 5."
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10)  # Adjust y-axis label size for readability
  )

#### The identity documents in detail 


ind$REG01a <- labelled_chr2dbl(ind$REG01a) # passport
ind$REG01b <- labelled_chr2dbl(ind$REG01b) # birth certificate
ind$REG01c <- labelled_chr2dbl(ind$REG01c) # civil/ government issued ID
ind$REG01d <- labelled_chr2dbl(ind$REG01d) # residency permit
ind$REG01e <- labelled_chr2dbl(ind$REG01e) # statelessness documentation
ind$REG01f <- labelled_chr2dbl(ind$REG01f) # household card of address/family book
ind$REG01g <- labelled_chr2dbl(ind$REG01g) # social security card
ind$REG02 <- labelled_chr2dbl(ind$REG02) # any other document establishes identity

###define labels 

identity_documents <- c(
  "REG01a" = "Passport",
  "REG01b" = "Birth certificate",
  "REG01c" = "Civil/ government issued ID",
  "REG01d" = "Residency permit",
  "REG01e" = "Statelessness documentation",
  "REG01f" = "Household card of address/family book",
  "REG01g" = "Social security card",
  "REG02" = "Any other document"
)

# Calculate the percentage of '1's for each identity document


identity_document_percentages <- RMS_SSD_2023_ind %>%
  summarise(across(c(REG01a, REG01b, REG01c, REG01d, REG01e, REG01f, REG01g, REG02),
                   ~ mean(. == 1, na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Document", 
               values_to = "Percentage") %>%
  mutate(Document = identity_documents[Document])  # Map column names to descriptive labels


# Create the bar chart
ggplot(identity_document_percentages, aes(x = reorder(Document, Percentage), y = Percentage, fill = Document)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Percentage of Individuals Holding Identity Documents",
    x = "Identity Document",
    y = "Percentage",
    caption = "Note: Percentages are calculated independently for each document for individuals 5 and above."
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )



### 4.1 Proportion of PoC who know where to access available GBV services.-----

##indicator calculation

main$GBV01a <- labelled_chr2dbl(main$GBV01a) # health services
main$GBV01b <- labelled_chr2dbl(main$GBV01b) # psycho-social services
main$GBV01c <- labelled_chr2dbl(main$GBV01c) # safety and security services
main$GBV01d <- labelled_chr2dbl(main$GBV01d) # legal assistance



main <- main %>%
  mutate(outcome4_1 = case_when(
    as.character(GBV01a) == "1" | as.character(GBV01b) == "1" ~ "1",  # If GBV01a or GBV01b is "1", then set to "1"
    all(c(GBV01a, GBV01b, GBV01c, GBV01d) == 98) ~ NA_character_,  # If all are 98, then set to NA
    TRUE ~ "0"  # For the rest, set to "0"
  )) %>%
  mutate(outcome4_1=labelled(outcome4_1,
                             labels=c(
                               'Yes'="1",
                               "No"="0"
                             ),
                             label="Proportion of people who know where to access available GBV service"
  ))

######Table standard 


outcome4_1 <- RMS_SSD_2023_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome4_1",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome4_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(outcome4_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 4.1",
    x = "Population Groups",
    y = "Mean value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

outcome4_1_AGD <- RMS_SSD_2023_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # only 18 and above 
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "outcome4_1",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome4_1), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome4_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )




####Chart with the AGD variables 


navy_palette <- c("#E0E9FE", "#B8C9EE", "#8395B9", "#506489", "#18375F")
red_palette <- c(unhcr_pal(12, "pal_red"))



ggplot(outcome4_1_AGD, aes(x = disability, y = mean_value, fill = HH04)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Grouped bar chart
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +  # Add values on bars
  scale_fill_manual(values = c(navy_palette[4], red_palette[2])) +  # Apply custom color palette
  facet_wrap(~ HH07_cat) +  # Create facets for each age group
  labs(
    title = "Outcome 4.1 by Gender, Age, and Disability Status",
    x = "Disability Status",
    y = "Mean Value",
    fill = "Gender",
    caption = "Note: Only 18 and above"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +  # Limit the y-axis from 0 to 1
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10)  # Adjust y-axis label size for readability
  )


####Simple bar chart of GBV01



###define labels 

gbv_services <- c(
  "GBV01a" = "Health services",
  "GBV01b" = "Psycho-social services",
  "GBV01c" = "safety and security services",
  "GBV01d" = "Legal assistance"
)

# Calculate the percentage of '1's for each identity document


gbv01_percentages <- RMS_SSD_2023_main %>%
  summarise(across(c(GBV01a, GBV01b, GBV01c, GBV01d),
                   ~ mean(. == 1, na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Services", 
               values_to = "Percentage") %>%
  mutate(Services = gbv_services[Services])  # Map column names to descriptive labels


# Create the bar chart

ggplot(gbv01_percentages, aes(x = reorder(Services, Percentage), y = Percentage, fill = Services)) +
  geom_bar(stat = "identity", width = 0.7) +  # Use "identity" for stat
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels inside bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Knowledge on where to access available GBV services",
    x = "GBV Services",
    y = "Percentage",
    caption = "Note: Percentages are calculated independently for each service for individuals 5 and above."
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust y-axis text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

### 4.2 Proportion of PoC who do not accept violence against women. -----


#Turn into numeric variables
main$VAW01a <- labelled_chr2dbl(main$VAW01a)
main$VAW01b <- labelled_chr2dbl(main$VAW01b)
main$VAW01c <- labelled_chr2dbl(main$VAW01c)
main$VAW01d <- labelled_chr2dbl(main$VAW01d)
main$VAW01e <- labelled_chr2dbl(main$VAW01e)

##This indicator comes from main dataset based on the respondent randomly selected for individual level

#If randomly selected adult who believes that a  husband is justified in beating his wife in various circumstances

##If yes selected for any of the circumstances
###Prefer not to respond will be put into missing
main <- main %>%
  mutate(outcome4_2=case_when(
    VAW01a==1 | VAW01b==1 |  VAW01c==1 |  VAW01d==1 | VAW01e==1 ~ 0,
    VAW01a==0 & VAW01b==0 &  VAW01c==0 &  VAW01d==0 & VAW01e==0 ~ 1,
    TRUE ~ NA_real_)
  ) %>%
  mutate(outcome4_2=labelled(outcome4_2,
                             labels=c(
                               'Yes'=1,
                               "No"=0
                             ),
                             label="Proportion of people who do not accept violence against women"
  ))




######Table standard 


outcome4_2 <- RMS_SSD_2023_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome4_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome4_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(outcome4_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 4.2",
    x = "Population Groups",
    y = "Mean value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

outcome4_2_AGD <- RMS_SSD_2023_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # only 18 and above 
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "outcome4_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome4_2), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome4_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )




####Chart with the AGD variables 


navy_palette <- c("#E0E9FE", "#B8C9EE", "#8395B9", "#506489", "#18375F")
red_palette <- c(unhcr_pal(12, "pal_red"))



ggplot(outcome4_2_AGD, aes(x = disability, y = mean_value, fill = HH04)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Grouped bar chart
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +  # Add values on bars
  scale_fill_manual(values = c(navy_palette[4], red_palette[2])) +  # Apply custom color palette
  facet_wrap(~ HH07_cat) +  # Create facets for each age group
  labs(
    title = "Outcome 4.2 by Gender, Age, and Disability Status",
    x = "Disability Status",
    y = "Mean Value",
    fill = "Gender",
    caption = "Note: Only 18 and above"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +  # Limit the y-axis from 0 to 1
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10)  # Adjust y-axis label size for readability
  )
## Gender-based Violence


vaw_options <- c(
  VAW01a = "If she goes out without telling him",
  VAW01b = "If she neglects the children",
  VAW01c = "If she argues with him",
  VAW01d = "If she refuses to have sex with him",
  VAW01e = "If she burns the food"
)

# Summarize the percentages for each question
vaw01_percentages <- main %>%
  summarise(across(c(VAW01a, VAW01b, VAW01c, VAW01d, VAW01e),
                   ~ mean(. == 1, na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Question", 
               values_to = "Percentage") %>%
  mutate(Question = vaw_options[Question])  # Map column names to descriptive labels



###Chart

ggplot(vaw01_percentages, aes(x = reorder(Question, Percentage), y = Percentage, fill = Question)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Justification for Violence Against Women",
    x = "Justification",
    y = "Percentage",
    caption = "Note: Percentages represent the proportion of respondents who agree with the justification."
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )


### 5.2 Proportion of children who participate in community-based child protection programmes -----
## Child Protection


ind$COMM01 <- labelled_chr2dbl(ind$COMM01)
ind$COMM02 <- labelled_chr2dbl(ind$COMM02)
ind$COMM03 <- labelled_chr2dbl(ind$COMM03)
ind$COMM04 <- labelled_chr2dbl(ind$COMM04)


###This indicator comes from the individual level dataset


#Children who participate in community-based programmes at least once 
##under adult supervision in a physically safe area

ind <- ind %>%
  mutate(outcome5_2=case_when(
    (COMM01==1 & ( COMM02 >=1 & COMM02!=98) & COMM03==1 & COMM04==1) ~ 1,
    (COMM01==0 | 
       (COMM02==0 | COMM02==98) | 
       (COMM03==0 | COMM03==98) |
       (COMM04==0 | COMM04==98)) ~ 0, TRUE ~ NA_real_)
  ) %>%
  mutate(outcome5_2=labelled(outcome5_2,
                             labels=c(
                               'Yes'=1,
                               "No"=0
                             ),
                             label="Proportion of children who participate in community-based child protection programmes"
  ))



###Table 


outcome5_2 <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome5_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome5_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Table with disability and gender


outcome5_2_AGD <- RMS_SSD_2023_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(pop_groups)) %>%  # Missing pipe added here
  group_by(HH04, pop_groups, disability) %>%
  summarise(
    var_name = "outcome5_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome5_2), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                  # Weighted total count
    mean_value = survey_mean(outcome5_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )



##Chart with pop groups

gender_colors <- c("Male" = "#8395B9", "Female" = "#E0E9FE")


ggplot(outcome5_2_AGD, aes(x = HH04, y = mean_value, fill = HH04)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 5.2 by Population Groups and Gender",
    x = "Gender",
    y = "Proportion of Children",
    caption = "Note: Only children between 5 to 17"
  ) +
  scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


## Well-being and Basic Needs

### 8.2 Proportion of PoC with primary reliance on clean (cooking) fuels and technology ----



###indicator calculation

main$COOK01 <- labelled_chr2dbl(main$COOK01)
main$COOK02 <- labelled_chr2dbl(main$COOK02)
main$COOK03 <- labelled_chr2dbl(main$COOK03)


###Based on MICS calculation : TC4.1

main <- main %>%
  mutate(
    outcome8_2 = case_when(
      (COOK01 == 1 & (COOK02 %in% c("1", "2", "3", "4", "5")) | (COOK02 %in% c("10") & COOK03 %in% c("1"))
      ) ~ 1, 
      (COOK01 == 1 & (COOK02 %in% c("7", "8", "9", "10", "96")) | ((COOK02 %in% c("10") & !(COOK03 %in% c("1")
      )))) ~ 0 ,
      COOK01==0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    outcome8_2 = labelled(outcome8_2,
                          labels = c(
                            "No" = 0,
                            "Yes" = 1
                          ),
                          label = "Proportion of people with primary reliance on clean (cooking) fuels and technology"
    )
  )


table(main$outcome8_2, main$pop_groups)

##Table by population groups

outcome8_2 <- RMS_SSD_2023_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome8_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome8_2, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )


###Chart of impact 2_2 by pop groups

ggplot(outcome8_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 8.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



###Show the bar chart for COOK02

table(main$COOK02)


# Define stove categories based on the provided list
stove_labels <- c(
  "1" = "Solar cooker (thermal energy from the sun)",
  "2" = "Electric stove",
  "3" = "Piped natural gas stove",
  "4" = "Biogas stove",
  "5" = "Liquefied petroleum gas (LPG)/cooking gas stove",
  "6" = "Manufactured solid fuel stove",
  "7" = "Traditional solid fuel stove (non-manufactured)",
  "8" = "Moveable firepan",
  "9" = "Three stone stove/open fire",
  "10" = "Liquid fuel stove",
  "96" = "Other, specify"
)

# Summarize the counts and percentages for each category
cook02_percentages <- main %>%
  filter(!is.na(COOK02)) %>%  # Exclude missing values
  count(COOK02) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(COOK02 = factor(COOK02, levels = names(stove_labels), labels = stove_labels))

# Create the chart

ggplot(cook02_percentages, aes(x = reorder(COOK02, Percentage), y = Percentage, fill = COOK02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Distribution of Stove Types (COOK02)",
    x = "Stove Type",
    y = "Percentage",
    caption = "Source: RMS SSD 2023"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )


## Sustainable housing and Settlements

### 9.1 Proportion of PoC living in habitable and affordable housing.----


###Indicator calculation

##Module :DWE01 – SHEL01-SHEL06 – DWE05 – DWE08-DWE09


##This indicator is calculated from the main dataset

##Condition 1

##Classify as habitable for below conditions - if 98 selected, put into missing

##First check the variables

table(main$SHEL01)
table(main$SHEL02)
table(main$SHEL03)
table(main$SHEL04)
table(main$SHEL05)
table(main$SHEL06)

main$SHEL01 <- labelled_chr2dbl(main$SHEL01)
main$SHEL02 <- labelled_chr2dbl(main$SHEL02)
main$SHEL03 <- labelled_chr2dbl(main$SHEL03)
main$SHEL04 <- labelled_chr2dbl(main$SHEL04)
main$SHEL05 <- labelled_chr2dbl(main$SHEL05)
main$SHEL06 <- labelled_chr2dbl(main$SHEL06)




main <- main %>%
  mutate(across(starts_with("SHEL"), ~ifelse(. == 98, NA, .))) %>%
  mutate(habitablehousing = case_when(
    (SHEL01 == "1") & (SHEL02 == "1") & (SHEL05 == "1") & 
      (SHEL03 == "0" ) & (SHEL04 == "0" ) & (SHEL06 == "0" ) ~ 1,
    (SHEL01 == "0") | (SHEL02 == "0" ) | (SHEL05 == "0") |
      (SHEL03 == "1") | (SHEL04 == "1") | (SHEL06 == "1" ) ~ 0,
    TRUE ~ NA_integer_
  ))

table(main$habitablehousing)


##Condition 2
####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep
###Overcrowding may cause health issues, thus not considered as physically safe


table(main$hh_size_001)
table(main$DWE05)


main <- main %>%
  mutate(crowding=hh_size_001/DWE05
  ) %>%
  mutate(dwe05_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 0)
  )


table(main$crowding)
table(main$dwe05_cat)

##Condition 3


## Add DWE08 and DWE09 to calculations - if household is paying rent, they should be able to afford to pay rent without any financial distress

table(main$DWE08)
table(main$DWE09)

main$DWE08 <- labelled_chr2dbl(main$DWE08)
main$DWE09 <- labelled_chr2dbl(main$DWE09)

main <- main %>%
  mutate(dwe09_cat=case_when( #affordable if HH pays rent and often and always without financial distress
    (DWE08==1 & (DWE09==1 | DWE09==2)) ~ 1, 
    (DWE08==1 & (DWE09==3 | DWE09==4)) ~ 0,  
    DWE08==0 ~ 1) ## if not 0, then not into missing but 1 to be able to calculate the composite indicator
  )

table(main$dwe09_cat)

###Combine all three conditions for habitable housing


main <- main %>%
  mutate(
    outcome9_1 = case_when(
      dwe05_cat == 1 & habitablehousing == 1 & dwe09_cat == 1  ~ 1,
      TRUE ~ 0
    ),
    outcome9_1 = labelled(outcome9_1,
                          labels = c("Yes" = 1, "No" = 0),
                          label = "Proportion of people living in habitable and affordable housing")
  )



table(main$outcome9_1)


####Standard tables 


composite_outcome9_1 <- main %>%
  select(pop_groups, dwe05_cat, habitablehousing, dwe09_cat) %>%
  pivot_longer(cols = c(dwe05_cat, habitablehousing, dwe09_cat),  # Pivot the three variables
               names_to = "facility", 
               values_to = "access") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = mean(access, na.rm = TRUE) * 100) %>%
  ungroup()



###Chart for above with all dimensions 


ggplot(composite_outcome9_1, aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "dwe05_cat" = "Not overcrowded",
    "habitablehousing" = "Habitable Housing",
    "dwe09_cat" = "Affordable"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Access to Housing Facilities by Population Group",
    x = "Facility",
    y = "Percentage Access",
    fill = "Population Groups"
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  )

##Table by population groups

outcome9_1 <- RMS_SSD_2023_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome9_1",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome8_2, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )



###Chart of impact 2_2 by pop groups

ggplot(outcome9_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 9.1",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )




### 9.2 Proportion of PoC that have energy to ensure lighting.----

##Module :LIGHT01-LIGHT03

main$LIGHT01 <- labelled_chr2dbl(main$LIGHT01)
main$LIGHT02 <- labelled_chr2dbl(main$LIGHT02)
main$LIGHT03 <- labelled_chr2dbl(main$LIGHT03)


###This basic service is calculated from the main dataset

### The below Calculates percentage of PoC having access to clean fuel for lighting and / or basic connectivity (9.1 Outcome Indicator)

main <- main %>% 
  mutate(outcome9_2=
           case_when(LIGHT01==1 & (LIGHT02==1 |LIGHT02==2 | LIGHT02==3 | LIGHT02==4 | LIGHT02==5 | 
                                     LIGHT02==6 |LIGHT02==7) ~ 1, TRUE ~ 0)
  ) %>%
  mutate( outcome9_2 = labelled(outcome9_2,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Proportion of people that have energy to ensure lighting"))

table(main$outcome9_2)

##Table by population groups

outcome9_2 <- RMS_SSD_2023_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome9_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome9_2, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )


###Chart of outcome 9.2 by pop groups

ggplot(outcome9_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 9.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



###Show the bar chart for COOK02

table(main$LIGH02)


# Define stove categories based on the provided list
lighting_labels <- c(
  "1" =	"Electricity",
  "2" =	"Solar home system",
  "3"	= "Solar-powered lantern or flashlight",
  "4" = "Rechargeable flashlight, mobile, torch or lantern",
  "5" = "Battery powered flashlight, torch or lantern",
  "6" =	"Biogas lamp",
  "7" =	"LPG lamp",
  "8" = "Gasoline lamp",
  "9"	= "Kerosene or paraffin lamp",
  "10" = "Oil lamp",
  "11" = "Candle",
  "12" ="Open fire",
  "96" ="Other, specify"
)

# Summarize the counts and percentages for each category
light02_percentages <- main %>%
  filter(!is.na(LIGHT02)) %>%  # Exclude missing values
  count(LIGHT02) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(LIGHT02 = factor(LIGHT02, levels = names(lighting_labels), labels = lighting_labels))

# Create the chart

ggplot(light02_percentages, aes(x = reorder(LIGHT02, Percentage), y = Percentage, fill = LIGHT02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Distribution of Energy of Lighting (LIGHT02)",
    x = "Lighting Type",
    y = "Percentage",
    caption = "Source: RMS SSD 2023"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )


## Healthy Lives

#### 10.1 Proportion of children 9mo-5years who have received measles vaccination -----

#Turn into numeric
ind$MMR03 <- labelled_chr2dbl(ind$MMR03)


ind <- ind %>%
  mutate(outcome10_1=case_when(
    MMR03==1 ~ 1, MMR03==0  | MMR03==98 ~ 0)
  ) %>%
  mutate( outcome10_1 = labelled(outcome10_1,
                                 labels = c(
                                   "Yes" = 1,
                                   "No" = 0
                                 ),
                                 label = "Proportion of children aged 9 months to five years who have received measles vaccination*"))


###Table 


outcome10_1 <- RMS_SSD_2023_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome10_1",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome10_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Table with disability and gender


outcome1_2_AGD <- RMS_SSD_2023_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(pop_groups) & HH07 < 5 ) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, pop_groups) %>%
  summarise(
    var_name = "outcome1_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(impact3_3), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(impact3_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )


##Chart with pop groups

gender_colors <- c("Male" = "#8395B9", "Female" = "#E0E9FE")


ggplot(outcome1_2_AGD, aes(x = HH04, y = mean_value, fill = HH04)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 1.2 by Population Groups and Gender",
    x = "Gender",
    y = "Proportion of Children that registered",
    caption = "Note: Only children under 5"
  ) +
  scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )



10.2 Proportion of births attended by skilled health personnel.

## Clean Water,Sanitation and Hygiene

12.1 Proportion of PoC using at least basic drinking water services.
12.2 Proportion of PoC with access to a safe household toilet.

## Self Reliance, Economic Inclusion and Livelihoods

13.1 Proportion of PoC with an account at a bank or other financial institution or with a mobile-money service provider.
13.2 Proportion of PoC who self-report positive changes in their income compared to previous year.
13.3 Proportion of PoC (working age) who are unemployed.

## Local Integration and other Local Solutions

16.1 Proportion of PoC with secure tenure rights and/or property rights to housing and/or land.
16.2 Proportion of PoC covered by social protection floors/systems.

####FINAL TABLE








