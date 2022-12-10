library(tidyverse)

ggplot(Deaths_Data, aes(x=mmwryear, y=Deaths_Data[,CDCvars[1]])) + 
  geom_col()

longYear<-summary_wrapper("Covid 19 Deaths", "Report Year")

longSum<-sum%>%
  pivot_longer(cols=2:7, names_to = "Summary_Type", values_to = "Number_Of_Deaths")

#Bar graphs by grouping
ggplot(Deaths_Data, aes(x=mmwryear, y=Covid_19_Deaths)) + 
  geom_col()

ggplot(longSum, aes(x=Age_Group, y=Number_Of_Deaths)) + 
  geom_col(aes(fill=Summary_Type), position="dodge")

ggplot(longYear, aes(x=mmwryear, y=Number_Of_Deaths)) + 
  geom_col(aes(fill=Summary_Type), position="dodge")


ggplot(Deaths_Data, aes(x=mmwryear, y=Covid_19_Deaths)) + 
  geom_col(aes(fill=Age_Group), position="dodge")


ggplot(Deaths_Data, aes(x=mmwryear, y=Covid_19_Deaths)) + 
  geom_col(aes(fill=mean(Covid_19_Deaths)), position="dodge")

names(CDCvars)

print(paste(c("Summary Table of", names(CDCvars[1]))))

#Histogram of deaths per week
ggplot(Deaths_Data, aes(x=Covid_19_Deaths)) + 
  geom_histogram(binwidth = 1000)


#Boxplot of deaths per week
ggplot(Deaths_Data, aes(x=mmwryear, y=Deaths_Total)) + 
  geom_boxplot()


ggplot(Deaths_Data, aes(x=mmwryear)) +
  geom_bar()

#read in data
absenteeismData<- read_csv2("Absenteeism_at_work.csv")

#Create a function to change variables, rename, create factors
cleaningData<- function(UnmodifiedAbsenteeData){
  absenteeDataClean<- absenteeismData %>%
    mutate(
      Reason_For_Absence = factor(absenteeismData$`Reason for absence`,
                                  levels = 0:28,
                                  labels = c("No Absences", 
                                             "Certain infectious and parasitic diseases", 
                                             "Neoplasms", 
                                             "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism", 
                                             "Endocrine, nutritional and metabolic diseases", 
                                             "Mental and behavioural disorders", 
                                             "Diseases of the nervous system", 
                                             "Diseases of the eye and adnexa", 
                                             "Diseases of the ear and mastoid process", 
                                             "Diseases of the circulatory system", 
                                             "Diseases of the respiratory system", 
                                             "Diseases of the digestive system", 
                                             "Diseases of the skin and subcutaneous tissue", 
                                             "Diseases of the musculoskeletal system and connective tissue", 
                                             "Diseases of the genitourinary system", 
                                             "Pregnancy, childbirth and the puerperium", 
                                             "Certain conditions originating in the perinatal period", 
                                             "Congenital malformations, deformations and chromosomal abnormalities", 
                                             "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", 
                                             "Injury, poisoning and certain other consequences of external causes", 
                                             "External causes of morbidity and mortality", 
                                             "Factors influencing health status and contact with health services", 
                                             "Patient follow-up", 
                                             "Medical consultation", 
                                             "Blood donation", 
                                             "Laboratory examination", 
                                             "Unjustified absence", 
                                             "Physiotherapy", 
                                             "Dental consultation")),
      Month_Of_Absence = factor(absenteeismData$`Month of absence`),
      Day_Of_The_Week = factor(absenteeismData$`Day of the week`,
                               levels = 2:6,
                               labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),
      Season = factor(absenteeismData$Seasons,
                       levels = 1:4,
                       labels = c("Summer", "Autumn", "Winter", "Spring")),
      Transportation_Expense = absenteeismData$`Transportation expense`,
      Distance_From_Residence_To_Work = absenteeismData$`Distance from Residence to Work`,
      Service_Time = absenteeismData$`Service time`,
      Avg_Workload_Per_Day = absenteeismData$`Work load Average/day`,
      Hit_Target = absenteeismData$`Hit target`,
      Disciplinary_Failure = factor(absenteeismData$`Disciplinary failure`),
      Education_Level = factor(absenteeismData$Education,
                               levels = 1:4,
                               labels = c("High School", "Graduate", "Postgraduate", "Master and Doctor")),
      Number_Of_Children = absenteeismData$Son,
      Social_Drinker = factor(absenteeismData$`Social drinker`),
      Social_Smoker = factor(absenteeismData$`Social smoker`),
      BMI = absenteeismData$`Body mass index`,
      Absenteeism_Time_In_Hours = absenteeismData$`Absenteeism time in hours`
    ) %>%
    select(!c(`Reason for absence`, `Month of absence`, `Day of the week`, Seasons, `Transportation expense`, `Distance from Residence to Work`, `Service time`, `Work load Average/day`, `Hit target`, `Disciplinary failure`, Education, Son, `Social drinker`, `Social smoker`, `Body mass index`, `Absenteeism time in hours`))
  
  return(absenteeDataClean)
}


cleanData<-cleaningData(absenteeismData)


#Summary data on absentee hours by education level
cleanData%>%
  group_by(Education_Level) %>%
  summarise("Mean Absentee Hours" = mean(Absenteeism_Time_In_Hours), "Median Absentee Hours" = median(Absenteeism_Time_In_Hours), SD = sd(Absenteeism_Time_In_Hours))


#Summary data on absentee hours by day of the week
cleanData%>%
  group_by(Day_Of_The_Week) %>%
  summarise("Mean Absentee Hours" = mean(Absenteeism_Time_In_Hours), "Median Absentee Hours" = median(Absenteeism_Time_In_Hours), SD = sd(Absenteeism_Time_In_Hours))


#Summary data on absentee hours by season
cleanData%>%
  group_by(Season) %>%
  summarise("Mean Absentee Hours" = mean(Absenteeism_Time_In_Hours), "Median Absentee Hours" = median(Absenteeism_Time_In_Hours), SD = sd(Absenteeism_Time_In_Hours))


cleanData%>%
  summary()


cleanData%>%
  group_by(Reason_For_Absence) %>%
  summarise("Mean Absentee Hours" = mean(Absenteeism_Time_In_Hours), "Median Absentee Hours" = median(Absenteeism_Time_In_Hours), SD = sd(Absenteeism_Time_In_Hours))%>%
  print(n=28)


# from data.frame to polygons
head(states)
## make list for input to e.g., rMaps
rMaps<-geojson_json(states[1:351, ],
             lat = "lat", lon = "long", geometry = "polygon",
             group = "group"
)



list<-geojson_list(rMaps)
