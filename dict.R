CDCvars<- c("Covid 19 Deaths" = "Covid_19_Deaths" ,
            "Total Deaths (All Causes)" = "Deaths_Total",
            "Pneumonia Deaths" = "Pneumonia_Deaths",
            "Influenza Deaths" = "Influenza_Deaths",
            "Pneumonia Or Influenza Deaths" = "Pneumonia_Or_Influenza_Deaths",
            "Pneumonia, Influenza Or Covid 19 Deaths" = "Pneumonia_Influenza_Or_Covid_Deaths")




groupings<- c("Age" = "Age_Group",
              "Jurisdiction" = "jurisdiction",
              "Report Week" = "mmwrweek",
              "Report Year" = "mmwryear")


modelVars<-c("Report Week" = "mmwrweek",
             "Report Year" = "mmwryear",
             "Jurisdiction" = "jurisdiction",
             "Age" = "Age_Group",
             "Total Deaths (All Causes)" = "Deaths_Total",
             "Pneumonia Deaths" = "Pneumonia_Deaths",
             "Influenza Deaths" = "Influenza_Deaths")

sumFormulas<-c("Minimum Deaths Per Week" = min,
               "Q1 Deaths Per Week" = quantile,
               "Median Deaths Per Week" = median,
               "Mean Deaths Per Week" = mean,
               "Q3 Deaths Per Week" = quantile,
               "Maximum Deaths Per Week" = max)

varLabels<-c("Covid 19 Deaths" = "Covid_19_Deaths" ,
             "Total Deaths (All Causes)" = "Deaths_Total",
             "Pneumonia Deaths" = "Pneumonia_Deaths",
             "Influenza Deaths" = "Influenza_Deaths",
             "Pneumonia Or Influenza Deaths" = "Pneumonia_Or_Influenza_Deaths",
             "Pneumonia, Influenza Or Covid 19 Deaths" = "Pneumonia_Influenza_Or_Covid_Deaths",
             "Age" = "Age_Group",
             "Jurisdiction" = "jurisdiction",
             "Report Week" = "mmwrweek",
             "Report Year" = "mmwryear",
             "Data As Of" = "Data_As_Of",
             "Start Week" = "Start_Week",
             "End Week" = "End_Week")


# summaryCheckBox<-c("Minimum Deaths Per Week" = "Min=min(.data[[death_variable]])",
#                    "Q1 Deaths Per Week" = "Q1 = quantile(.data[[death_variable]], 0.25)",
#                    "Median Deaths Per Week" = "Median = median(.data[[death_variable]])",
#                    "Mean Deaths Per Week" = "Mean Deaths Per Week = round(mean(.data[[death_variable]]))",
#                    "Q3 Deaths Per Week" = "Q3 = quantile(.data[[death_variable]], 0.75)",
#                    "Maximum Deaths Per Week" = "Maximum = max(.data[[death_variable]])",
#                    "Count" = "Count = n()")




