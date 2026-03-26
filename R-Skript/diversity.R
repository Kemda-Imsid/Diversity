# ======================================
# Survey Analysis Pipeline: Demographics + Likerts
# ======================================


#Main Questions 
#   Do attitudes differ by age?
#   
#   Do perceptions differ by gender?
#   
#   Do minority vs non-minority respondents answer differently?
#   
#   Which groups are the most positive / most critical?
#   
#   These become your analysis questions, e.g.
# 
# “Does satisfaction differ by age and gender?”


setwd("")  ### Change to your own directory

pkgs <- c("dplyr","janitor","ggplot2","likert","tidyr","forcats","scales","RColorBrewer",
          "viridis","scales","patchwork","stringr","grid","png","purrr","cowplot","plotly")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ------------------------------
# 1. Load data
# ------------------------------

path <- "../DiversittGMDS_DATA_LABELS_2026-01-12_1350.csv"# <- adjust path if needed
raw <- readr::read_csv(path, show_col_types = FALSE) %>% clean_names() 

# ------------------------------
# 2. Data cleaning and preproccesing
# ------------------------------

# Unified languages to English

raw_en <- raw %>%
  filter(please_select_a_language_sprachauswahl == "English") %>%
  select(-c("record_id" , "survey_identifier","survey_timestamp_3", "please_select_a_language_sprachauswahl", "complete_5",                                                                                                                                                                                                                        
            "survey_timestamp_6","complete_48","survey_timestamp_49" ,"complete_91"))%>%select(1:(ncol(.) %/% 2))

raw_ger <- raw %>%
  filter(please_select_a_language_sprachauswahl == "Deutsch") %>%
  select(-c("record_id" , "survey_identifier","survey_timestamp_3", "please_select_a_language_sprachauswahl", "complete_5",                                                                                                                                                                                                                        
            "survey_timestamp_6","complete_48","survey_timestamp_49" ,"complete_91"))%>%select(((ncol(.) %/% 2)+1):ncol(.))%>%setNames(colnames(raw_en))
raw<-rbind(raw_en,raw_ger)
  
raw<-raw %>%
  mutate(across(everything(), ~ case_when(
    # Demographics
    . =="weiblich" ~ "female",
    . =="männlich" ~ "male",
    
    . =="weibliche Geschlechtsidentität" ~ "Female gender identity",
    . =="männliche Geschlechtsidentität" ~ "Male gender identity",
    . =="Männlich" ~ "male",
    . =="Frau" ~ "female",
    . =="Ja" ~ "Yes",
    . =="Nein" ~ "No",
    
    . =="Häufig" ~ "Often",
    . =="Manchmal"~ "Sometimes",
    . =="Selten" ~ "Rarely",
    . =="Nie" ~ "Never",
    . =="Immer"~ "Always",
    # Likert / quality ratings
    . =="mangelhaft" ~ "Poor",
    . =="sehr mangelhaft" ~ "Very poor",
    . =="Durchschnittlich" ~ "Average",
    . =="Gut" ~ "Good",
    . =="Sehr gut" ~ "Very good",
    . =="relativ hoch" ~ "relatively high",
    # Diversity / inclusion
    . =="Überhaupt nicht vielfältig" ~ "Not diverse",
    . =="Wenig vielfältig" ~ "Slightly diverse",
    . =="Mäßig vielfältig" ~ "Moderately diverse",
    . =="Sehr vielfältig" ~ "Very diverse",
    . =="Extrem vielfältig" ~ "Extremely diverse",
    . =="Schlecht inklusiv" ~ "Bad inclusive",
    . =="Außergewöhnlich inklusiv" ~ "Exceptional inclusive",
    . =="Mäßig inklusiv" ~ "Moderately inclusive",
    . =="Gut inklusiv" ~ "Well inclusive",
    . =="Sehr inklusiv" ~ "Highly inclusive",
    # Frequency / agreement
    . =="Zustimmen" ~"Agree",
    . =="Eher nicht einverstanden" ~ "Somewhat disagree",
    . =="Stimme voll und ganz zu" ~ "Strongly agree",
    . =="Stark ablehnend" ~  "Strongly disagree",
    . =="Neutral" ~ "Neutral",
    . =="mäßig nützlich" ~ "Moderately useful",
    . =="Äußerst nützlich" ~ "Extremely useful",
    . =="sehr nützlich" ~ "Very useful",
    . =="Geringfügig nützlich" ~ "Slight useful",
    . =="Überhaupt nicht nützlich" ~ "Not at all useful",
    
    # Missing / no answer
    . =="---" ~  NA_character_,
    . =="Keine Antwort" ~ NA_character_,
    . =="Keine" ~ NA_character_,
    . =="keiner" ~ NA_character_,
    . =="keinem" ~ NA_character_,
    . =="Keine Angaben"~"Not specified",
    . =="andere"~"Other",
    . =="Keine andere"~"No other",
    . =="Ausgewählt" ~"selected",
    . =="Nicht ausgewählt" ~"Not selected",
    . =="41- 50"~"41 - 50",
    TRUE ~ .)))

# Rename columns

raw <- raw %>%
  rename(
    Age=how_old_are_you,
    Birth=what_gender_entry_do_you_have_on_your_birth_certificate,
    Birthgender=does_your_gender_identity_match_your_birth_gender,
    Gender=which_term_do_you_use_to_describe_your_gender_or_gender_identity,
    Genderother=what_other_terms_might_you_use_to_describe_your_gender_or_gender_identity, 
    Identtity=do_you_identify_as_a_member_of_a_minority_or_marginalised_group ,
    Identtitywhich=which_minority_or_marginalized_group_do_you_identify_with,
    Employement=are_you_currently_employed,
    Pastwork=have_you_worked_in_the_medical_informatics_field_in_the_last_5_to_10_years,
    Work_Academia=do_you_currently_work_in_choice_academia,
    Work_Industry=do_you_currently_work_in_choice_industry,
    Work_Authorithies=do_you_currently_work_in_choice_authorities_e_g_ministry_of_health,
    Medical_Informatics=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_medical_informatics,
    BioMedical_Informatics=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_biomedical_informatics,
    Data_Science=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_data_science,
    Epidemiology=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_epidemiology,
    Medical_Documentation=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_medical_documentation,                                                                                                                                                              
    Health_Informatics=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_health_informatics_e_health,
    Image_Processing=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_image_processing,
    Biosignal_Processing=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_biosignal_processing,
    Bioinformatics=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_bioinformatics,                                                                                                    
    Nursing_Informatics=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_nursing_informatics,
    Dental_Informatics=in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_dental_informatics,
    Medical_Biometry =in_which_areas_do_you_work_please_select_the_area_that_best_applies_choice_medical_biometry ,
    Workother=in_which_other_area_of_medical_informatics_are_you_active ,
    "Experienced unconscious bias or discrimination in  professional field"=have_you_ever_experienced_unconscious_bias_or_discrimination_regarding_diversity_in_your_professional_field,                                                                                                
    "Witnessed unconscious bias or discrimination in professional field"=have_you_ever_witnessed_unconscious_bias_or_discrimination_regarding_diversity_in_your_professional_field,                                                                                                  
    "Representation of different backgrounds and identities in the scientific field of medical computer science in germany"=how_would_you_assess_the_representation_of_different_backgrounds_and_identities_in_the_scientific_field_of_medical_computer_science_in_germany,                                                             
    "Assessment of the diversity of your leadership team management team"=what_is_your_assessment_of_the_diversity_of_your_leadership_team_management_team,                                                                                                                           
    "Valuing Diverse Perspectives in Decision-Making"=do_you_feel_that_different_perspectives_are_encouraged_and_valued_when_making_decisions_in_your_company,                                                                                                   
    "Fairness of Equal Opportunities in Medical Informatics"=would_you_describe_the_field_of_medical_informatics_as_fair_in_terms_of_equal_opportunities,                                                                                                                
    "Fairness of Research Initiatives in Medical Informatics"=would_you_describe_the_field_of_medical_informatics_as_fair_in_terms_of_research_initiatives,                                                                                                               
    "Inclusive in medical informatics"=how_inclusive_is_the_field_of_medical_informatics_in_your_opinion,                                                                                                                                         
    "Colleagues' Valuation of Diversity"=do_your_collegues_value_diversity,                                                                                                                                                                         
    "Equal Opportunities for Prosperity in the Company"=do_all_employees_in_your_company_have_the_same_chances_for_prosperity,                                                                                                                                      
    "Diversity in company team?"=do_you_feel_like_your_company_has_a_diverse_team,                                                                                                                                                           
    "Importance of Diversity & Inclusion in Workplace Conversations"=do_you_think_that_diversity_and_integration_inclusion_are_important_in_workplace_conversations_e_g_are_your_coworkers_willing_to_switch_to_english_when_people_from_other_backgrounds_join_the_conversation,
    "Membership of the german society for medical informatics biometry and epidemiology e_v"=are_you_a_member_of_the_german_society_for_medical_informatics_biometry_and_epidemiology_e_v,                                                                                                              
    "GMDS's Role in Encouraging Diversity in Member Acquisition"=how_does_gmds_encourage_diversity_in_the_acquisition_of_new_members,                                                                                                                                        
    "Personal definition diversity"=how_would_you_personally_define_diversity,                                                                                                                                                                  
    "Do you find this survey useful ?"=do_you_find_this_survey_useful,
  ) 

raw<-raw[!is.na(raw$Age),]###all NA in Age implied empty case  #nrows=96

#Age 
raw <- raw %>%
     mutate(
       Age = case_when(
         Age %in% c("< 30", "30 - 40") ~ "<= 40",
         Age %in% c("41 - 50", "> 50") ~ "> 40",
         TRUE ~ "No answer"
       )) %>%
   mutate(Age=str_pad(Age, width = 12, side = "both"),
          Age = factor(Age))



# Gender identity

raw <- raw %>%
  mutate(
    gender_category = case_when(
      Birthgender == "Yes" &
        Birth == "female" &
        Gender%in%c("Female gender identity","Not specified","Other",NA) ~ "Women",
      
      Birthgender == "Yes" &
        Birth == "male" &
        Gender%in%c("Male gender identity","Not specified","Other",NA) ~ "Men",
      Birthgender == NA &
        Birth == "male" |
        Gender=="Male gender identity"~ "Men",
      TRUE ~ "No answer")) %>%
  mutate(gender_category = sprintf("%-12s", gender_category),
         gender_category = factor(gender_category))


# Minority status

raw <- raw %>%
  mutate(
    Identtitywhich = case_when(
      Identtity == "Yes" & is.na(Identtitywhich) ~ "Yes",
      Identtity == "No" ~ "No",
      TRUE ~ "Yes"),
    minority_group = fct_lump_min(
      as.factor(Identtitywhich), min = 1))

## Employments

raw <- raw %>%
  mutate(
    employment_sector = case_when(
      Work_Authorithies == "selected" ~ "Other/None",
      Work_Academia == "selected" ~ "Academia",
      Work_Industry == "selected" ~ "Industry",
      
      TRUE ~ "Other/None"
    )
  )%>%
  mutate(employment_sector = sprintf("%-12s", employment_sector),
         employment_sector = factor(employment_sector))



# Descriptive
#  Demographics as factors
demo_vars<-raw[,c("Age","gender_category","minority_group","employment_sector")]





demo_vars[] <- lapply(demo_vars, as.factor)
demo_vars$age_class<-factor(demo_vars$Age, levels = c("    > 40    ", "   <= 40    " ))

create_demo_gender_tab <- function(data, group_var, gender_var) {
  
  # group_var and gender_var should be unquoted column names
  tab <- data %>%
    tabyl({{ group_var }}, {{ gender_var }}) %>%  # use curly-curly to evaluate
    adorn_totals("row") %>%
    adorn_totals("col") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns(position = "front")
  
  # contingency table for Fisher's test
  cont_table <- table(data[[deparse(substitute(group_var))]],
                      data[[deparse(substitute(gender_var))]])
  fisher_result <- fisher.test(cont_table)
  
  # return results
  list(
    tabyl_table = tab,
    contingency_table = cont_table,
    fisher_test = fisher_result
  )
}


# Example usage
result_Age<- create_demo_gender_tab(demo_vars, Age, gender_category)
result_minority <- create_demo_gender_tab(demo_vars, minority_group, gender_category)
result_Employement <- create_demo_gender_tab(demo_vars, employment_sector, gender_category)

# Access outputs
result_Age$tabyl_table
result_Age$contingency_table
result_Age$fisher_test

#Likerts analysis ... We focused on Experienced unconscious bias or discrimination in  professional field

likert_vars<-raw[,c("Age","gender_category","minority_group","employment_sector", "Experienced unconscious bias or discrimination in  professional field",                                                                                                
                    "Witnessed unconscious bias or discrimination in professional field",                                                                                                  
                    "Representation of different backgrounds and identities in the scientific field of medical computer science in germany",                                                             
                    "Assessment of the diversity of your leadership team management team", "Valuing Diverse Perspectives in Decision-Making",                                                                                                   
                    "Fairness of Equal Opportunities in Medical Informatics","Fairness of Research Initiatives in Medical Informatics","Inclusive in medical informatics",                                                                                                                                         
                    "Colleagues' Valuation of Diversity","Equal Opportunities for Prosperity in the Company", "Diversity in company team?",                                                                                                                                                           
                    "Importance of Diversity & Inclusion in Workplace Conversations",
                    "Membership of the german society for medical informatics biometry and epidemiology e_v",                                                                                                              
                    "GMDS's Role in Encouraging Diversity in Member Acquisition","Personal definition diversity")]

clean_likert <- function(x) {
  x <- as.character(x)                # ensure character
  x <- str_trim(x)                     # Removes leading/trailing spaces
  x <- str_remove_all(x, ":\\d+")      # Removes trailing counts like ":12"
}
likert_vars_clean <- likert_vars %>%
  mutate(across(everything(), ~ clean_likert(.x)))

likert_levels <- c("Never","Rarely","Sometimes","Often")

# function to visualize single likert 

single_likert<-function(data,likert_cols,demo,likert_levels,list_order,Limit,Lab){

  data <- data[, c(likert_cols, demo)]

  data <- data %>%
    group_by(across(all_of(c(likert_cols, demo)))) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(.data[[likert_cols]]) %>%
    mutate(percent = count / sum(count) * 100) %>%
    ungroup()
  if (!is.null(list_order)) {
    data <- data %>%
      mutate(
        !!demo := factor(.data[[demo]], levels = list_order),
      )
  }
  data <- data %>%mutate(
    !!likert_cols :=  factor(
      .data[[likert_cols]],
      levels = likert_levels
    )
  )

  positive_data <- data %>%
    filter(.data[[likert_cols]] %in% c(likert_levels[2],likert_levels[3], likert_levels[4]))%>%
    complete(!!sym(demo) := Limit,!!sym(likert_cols), fill = list(count = 0))

  negative_data <- data %>%
    filter(.data[[likert_cols]] %in% c(likert_levels[1]))%>%
    complete(!!sym(demo) := Limit,!!sym(likert_cols), fill = list(count = 0))
  # Create the divergent plot
 p<- ggplot() +
    geom_col(data = positive_data,
             aes(x = .data[[demo]],
                 y = count,
                 fill = .data[[likert_cols]]),
             position = position_stack(reverse = TRUE)) +
   geom_text(data = positive_data%>% filter(count != 0),
             aes(x = .data[[demo]],
                 y = count,
                 label = count,
                 group = .data[[likert_cols]]),
             position = position_stack(vjust = 0.5, reverse = TRUE),
             color = "black",  # text color for contrast
             size = 2) +
    # Negative side
    geom_col(data = negative_data,
             aes(x = .data[[demo]],
                 y = -count,  # negative values for left side
                 fill = .data[[likert_cols]]),
             position = "stack") +
   geom_text(data = negative_data%>% filter(count != 0),
             aes(x = .data[[demo]],
                 y = -count/2,
                 label = count,
                 group = .data[[likert_cols]]),
             position = position_stack(vjust = 0.5),
             color = "black",
             size = 2) +
  coord_flip() +
  scale_y_continuous(limits =c(-60, 60),breaks = seq(-60, 60, by = 10),
                     expand = c(0, 2),labels = abs) +
  scale_x_discrete(limits = Limit)+
   scale_fill_manual(values = c("Never"="#8B4513","Rarely"="#91bfdb","Sometimes"="#4575b4", "Often"="#2b486d"),
                               breaks = c("Never", "Rarely", "Sometimes", "Often"),  name = NULL) +
  labs(x = Lab,
       y = "Number of participants",
  ) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 10,color = "black",margin = margin(t = 8)),
    axis.title.y = element_text(size = 10,color = "black",margin = margin(r = 8)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.ticks.x = element_line(color = "black", size = 0.2),
    axis.ticks.length = unit(5, "pt"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.line.x = element_line(color = "black", size = 0.2),  # ← adds horizontal axis line
    plot.title = element_text(hjust = 0.5))
p
}

p_gender<-single_likert(likert_vars_clean,"Experienced unconscious bias or discrimination in  professional field","gender_category",likert_levels,
                        list_order=c("Women", "Men"),Limit=c("Women" ,"Men",""),Lab="Gender")
p_age<-single_likert(likert_vars_clean,"Experienced unconscious bias or discrimination in  professional field","Age",
                     likert_levels,list_order=c("<= 40", "> 40"),Limit=c("","<= 40" ,"> 40"),Lab="Age")
p_minority<-single_likert(likert_vars_clean,"Experienced unconscious bias or discrimination in  professional field","minority_group",likert_levels,list_order=NULL,
                             Limit=c("","No",  "Yes" ),Lab="Minority")
p_employement<-single_likert(likert_vars_clean,"Experienced unconscious bias or discrimination in  professional field","employment_sector",likert_levels,list_order=NULL,
                             Limit=c("Academia","Industry", "Other/None"),Lab="Sector")

legend <- cowplot::get_legend(p_age) 

## Plot

png(filename = "likert_Diversity_2sided_neutŕal_No Answer_removed.png", width = 160, height =110,units = "mm", res=300)

cowplot::plot_grid(plot_grid(
  p_gender+theme( 
    axis.title.x = element_blank(),
    plot.title = element_blank(),
    axis.text.y = element_text(margin = margin(r = 45)),
    axis.text.x = element_text(color = "transparent"),
    plot.margin  = margin(15, 0, -15, 0),
    axis.ticks.x=element_blank(),
    axis.line = element_blank(),
    legend.position = "none"), 
  p_age+theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "transparent")  ,
    plot.title = element_blank(),
    axis.title.y = element_text(margin = margin(r = 28)),
    plot.margin  = margin(15, 0, -15, 0),
    axis.ticks.x=element_blank(),
    axis.line = element_blank(),
    legend.position = "none"), 
  p_minority+theme( 
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "transparent")  ,
    plot.title = element_blank(),
    axis.title.y = element_text(margin = margin(r = 35,b=20)),
    plot.margin  = margin(15, 0, -15, 0),
    axis.ticks.x=element_blank(),
    axis.line = element_blank(),
    legend.position = "none"), 
  
  p_employement+theme( 
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 3,)),
   # axis.text.y = element_text(margin = margin(r = 4)),
    plot.title = element_blank(),
    plot.margin  = margin(0, 0, 0, 0),
    legend.position = "none"),
  ncol = 1,
  align = "v",  
  axis = "lr" ,  
  legend
)+
  draw_label("Experienced bias or discrimination in professional field ", x = 0.5, y = 1, vjust = 1.3, size = 14, fontface = "bold") +
  draw_label("Number of participants", x=0.5, y= 0.17, size = 10, angle= 0))
dev.off()


## Display the statistics fo the likert
y <- likert_vars_clean %>%
  group_by(across(all_of(c("employment_sector","Experienced unconscious bias or discrimination in  professional field")))) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(.[["Experienced unconscious bias or discrimination in  professional field"]]) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

