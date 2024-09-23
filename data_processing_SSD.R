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
    mean_value = survey_mean(impact2_2, vartype = c("ci", "se")),  # indicator value ( weighted) with CI and SE
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

# Get the column names for HACC04_1 to HACC04_13
reason_columns <- names(reasons_mapping)

# Calculate the percentage of '1's for each column

reasons_nohealthaccess <- RMS_SSD_2023_ind %>%
  summarise(across(all_of(reason_columns), 
                   ~ mean(. == 1, na.rm = TRUE) * 100,  # Calculate percentage of '1's ignoring NAs
                   .names = "{col}")) %>%
  pivot_longer(everything(), 
               names_to = "Reason", 
               values_to = "Percentage") %>%
  mutate(Reason = reasons_mapping[Reason])  # Map column names to descriptive labels

##Chart creation


ggplot(reasons_nohealthaccess, aes(x = reorder(Reason, Percentage), y = Percentage, fill = Reason)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5, angle = 1)  +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Percentage of Reasons for Not Accessing Health Services",
    x = "Reasons",
    y = "Percentage",
    caption = "Note: This includes those who could not access to health services in the last 3 months" 
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend
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
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) ) %>%  # Exclude HH07_cat categories 1, 2, and 5
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

## Access to Territory, Registration and Documentation

1.2 Proportion of children \<5 years whose birth have been registered with a civil authority.
1.3 Proportion of PoC with legally recognized identity documents or credentials

## Gender-based Violence

4.1 Proportion of PoC who know where to access available GBV services.
4.2 Proportion of PoC who do not accept violence against women.

## Child Protection

5.2 Proportion of children who participate in community-based child protection programmes.

## Well-being and Basic Needs

8.2 Proportion of PoC with primary reliance on clean (cooking) fuels and technology.

## Sustainable housing and Settlements

9.1 Proportion of PoC living in habitable and affordable housing.
9.2 Proportion of PoC that have energy to ensure lighting.

## Healthy Lives

10.1 Proportion of children 9mo-5years who have received measles vaccination.
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

