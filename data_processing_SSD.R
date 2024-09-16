#####SSD data anonymized version for public with figures

##Remove past activities

rm(list = ls())

####Load libraries and data ----

# Install pacman if not already installed
if(!require(pacman)) install.packages('pacman')

# Load all required libraries using pacman
pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2 )


##Set WD

setwd("C:/Users/BOZDAG/OneDrive - UNHCR/Desktop/UNHCR/R projets/Standard report template/South Sudan")



##Load datasets 

main<- read_csv("main.csv")
ind <- read_csv("ind.csv")


CreateTableOne(data = main) # Check sample and distribution


### Clean and anonymize the data ----

ind$citizenship <- ifelse(ind$citizenship == "SDN", "SDN",
                          ifelse(ind$citizenship == "XXX", "SSD", "SSD")
)

main$citizenship <- ifelse(main$citizenship == "SDN", "SDN",
                           ifelse(main$citizenship == "XXX", "SSD", "SSD")
                           
                           
)
###Add labels for disaggregation variables

###Age - HH07_cat2


##Disability - disability

ind <- ind %>%
  mutate(disability = factor(disability, levels = c(0, 1), labels = c("Non-disabled", "Disabled")))

table(ind$disability)

###Gender - HH04
##ALREADY labelled


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







#### Creating the survey design object without stratification ----
survey_design_RMS_SSD_2023 <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


##Individual level ----

###Education only indicators ----


###Impact 3_2b with percentages only

impact3_2b <- survey_design_RMS_SSD_2023 %>%
  filter(!is.na(pop_groups)) %>%
  group_by(pop_groups) %>%
  summarise(
    age_secondary_total = survey_total(age_secondary, na.rm = TRUE),
    edu_secondary_total = survey_total(edu_secondary, na.rm = TRUE)
  ) %>%
  mutate(impact3_2b = round(edu_secondary_total / age_secondary_total, 4))


##With statistical outputs


impact3_2b_stats <- survey_design_RMS_SSD_2023 %>%
  filter(!is.na(pop_groups)) %>%
  group_by(pop_groups) %>%
  summarise(
    age_secondary_total = survey_total(age_secondary, vartype = "se"),
    edu_secondary_total = survey_total(edu_secondary, vartype = "se")
  ) %>%
  mutate(
    impact3_2b = round(edu_secondary_total / age_secondary_total, 4),
    impact3_2b_se = sqrt(
      (edu_secondary_total_se / edu_secondary_total)^2 +
        (age_secondary_total_se / age_secondary_total)^2
    ) * impact3_2b,
    impact3_2b_ci = impact3_2b + c(-1.96, 1.96) * impact3_2b_se,
    impact3_2b_cv = 100 * (impact3_2b_se / impact3_2b)
  )

###Disability ----


###Impact 3_2b with percentages only


impact3_2b <- survey_design_RMS_SSD_2023 %>%
  filter(!is.na(disability)) %>%
  group_by(disability) %>%
  summarise(
    age_secondary_total = survey_total(age_secondary, na.rm = TRUE),
    edu_secondary_total = survey_total(edu_secondary, na.rm = TRUE)
  ) %>%
  mutate(impact3_2b = round(edu_secondary_total / age_secondary_total, 4))




##With statistical outputs


impact3_2b_stats <- survey_design_RMS_SSD_2023 %>%
  filter(!is.na(disability)) %>%
  group_by(disability) %>%
  summarise(
    age_secondary_total = survey_total(age_secondary, vartype = "se"),
    edu_secondary_total = survey_total(edu_secondary, vartype = "se")
  ) %>%
  mutate(
    impact3_2b = round(edu_secondary_total / age_secondary_total, 4),
    impact3_2b_se = sqrt(
      (edu_secondary_total_se / edu_secondary_total)^2 +
        (age_secondary_total_se / age_secondary_total)^2
    ) * impact3_2b,
    impact3_2b_ci = impact3_2b + c(-1.96, 1.96) * impact3_2b_se,
    impact3_2b_cv = 100 * (impact3_2b_se / impact3_2b)
  )



###Impact 3_2a with percentages only


impact3_2a <- survey_design_RMS_SSD_2023 %>%
  filter(!is.na(disability)) %>%
  group_by(disability) %>%
  summarise(
    age_primary_total = survey_total(age_primary, na.rm = TRUE),
    edu_primary_total = survey_total(edu_primary, na.rm = TRUE)
  ) %>%
  mutate(impact3_2a = round(edu_primary_total / age_primary_total, 4))




##With statistical outputs


impact3_2b_stats <- survey_design_RMS_SSD_2023 %>%
  filter(!is.na(disability)) %>%
  group_by(disability) %>%
  summarise(
    age_secondary_total = survey_total(age_secondary, vartype = "se"),
    edu_secondary_total = survey_total(edu_secondary, vartype = "se")
  ) %>%
  mutate(
    impact3_2b = round(edu_secondary_total / age_secondary_total, 4),
    impact3_2b_se = sqrt(
      (edu_secondary_total_se / edu_secondary_total)^2 +
        (age_secondary_total_se / age_secondary_total)^2
    ) * impact3_2b,
    impact3_2b_ci = impact3_2b + c(-1.96, 1.96) * impact3_2b_se,
    impact3_2b_cv = 100 * (impact3_2b_se / impact3_2b)
  )

###Randomly selected adult ----



### 2.2 Proportion of PoCs residing in physically safe and secure settlements with access to basic facilities -----

### Figure ----

impact2_2 <- main %>%
  select(pop_groups, shelter, electricity, drinkingwater, secure, healthcare) %>%
  pivot_longer(cols = shelter:healthcare, names_to = "facility", values_to = "access") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = mean(access, na.rm = TRUE) * 100) %>%
  ungroup()


##Facility labels

facility_labels <- c(
  shelter = "Shelter",
  electricity = "Electricity",
  drinkingwater = "Drinking Water",
  secure = "Security",
  healthcare = "Healthcare"
)


# Create the plot using UNHCR theme with 100% limit and labels by population group
ggplot(impact2_2) +
  geom_col(aes(
    x = percentage,
    y = fct_rev(factor(facility)),
    fill = as.character(pop_groups)
  ),
  position = position_dodge(0.7),
  width = 0.6
  ) +
  geom_text(aes(
    x = percentage,
    y = fct_rev(factor(facility)),
    label = paste0(round(percentage, 0), "%"), #Change decimals to 0 to 1 for 0.1 decimal
    group = pop_groups
  ),
  position = position_dodge(0.7),
  vjust = -0.5,
  size = 3
  ) +
  scale_fill_unhcr_d(palette = "pal_unhcr") +
  labs(
    title = "Impact 2.2 : Access to Basic Facilities by Population Groups",
    x = "Percentage (%)",
    y = "Facility",
    fill = "Population Groups",
    caption = "Source: RMS 2023 South Sudan\n© UNHCR, The UN Refugee Agency" #Change source 
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = expansion(c(0, 0.1)),
    labels = label_number_si(suffix = "%")
  ) +
  scale_y_discrete(labels = scales::label_wrap(17)) +
  theme_unhcr(
    grid = "X",
    axis = "y",
    axis_title = "x"
  ) +
  scale_y_discrete(labels = facility_labels)

### A final indicator value is called ' impact2_2' by pop_group, HH04, disability, and HH07_cat2 including percentages and CI along with the final value for impact2_2 shown as total. 

# Table- ----



# Function to calculate percentages and confidence intervals
calculate_percentage_ci <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      total = n(),
      impact2_2_yes = sum(impact2_2 == 1, na.rm = TRUE),
      percentage = 100 * impact2_2_yes / total,
      ci_lower = percentage - qnorm(0.975) * sqrt((percentage / 100) * (1 - percentage / 100) / total) * 100,
      ci_upper = percentage + qnorm(0.975) * sqrt((percentage / 100) * (1 - percentage / 100) / total) * 100,
      ci = paste0("(", round(ci_lower, 2), ", ", round(ci_upper, 2), ")")
    ) %>%
    select(!!sym(group_var), total, percentage, ci, impact2_2_yes)
}

# List of grouping variables
group_vars <- c("pop_groups", "HH04", "disability", "HH07_cat2")

# Calculate percentages and CIs for each group and combine them into one table
final_table <- bind_rows(lapply(group_vars, function(var) {
  calculate_percentage_ci(main, var) %>% mutate(group = var)
}))

# Add a total row for the overall impact2_2
total_stats <- main %>%
  summarise(
    total = n(),
    impact2_2_yes = sum(impact2_2 == 1, na.rm = TRUE),
    percentage = 100 * impact2_2_yes / total,
    ci_lower = percentage - qnorm(0.975) * sqrt((percentage / 100) * (1 - percentage / 100) / total) * 100,
    ci_upper = percentage + qnorm(0.975) * sqrt((percentage / 100) * (1 - percentage / 100) / total) * 100,
    ci = paste0("(", round(ci_lower, 2), ", ", round(ci_upper, 2), ")"),
    group = "Total"
  )


# Combine with the final table
final_table <- bind_rows(final_table, total_stats)

# Create the table using gt
gt_table <- final_table %>%
  gt() %>%
  tab_header(
    title = "Impact Analysis Table",
    subtitle = "Percentage and Confidence Intervals by Group"
  ) %>%
  fmt_number(
    columns = vars(percentage),
    decimals = 2
  ) %>%
  cols_label(
    group = "Group",
    impact2_2_yes = "Count (Yes)",
    total = "Total",
    percentage = "Percentage (%)",
    ci = "95% CI"
  )

# Export the table as a JPG file
gtsave(gt_table, "Impact_Analysis_Table.pdf")
