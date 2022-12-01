CDCvars<- c("Covid 19 Deaths" = "Covid_19_Deaths" ,
            "Total Deaths (All Causes)" = "Deaths_Total",
            "Pneumonia Deaths" = "Pneumonia_Deaths",
            "Influenza Deaths" = "Influenza_Deaths",
            "Pneumonia Or Influenza Deaths" = "Pneumonia_Or_Influenza_Deaths",
            "Pneumonia, Influenza Or Covid 19 Deaths" = "Pneumonia_Influenza_Or_Covid_Deaths")

getVar<- function(variable){
  death_variable<-
    CDCvars[variable]
}


groupings<- c("Age" = "Age_Group",
              "Jurisdiction" = "jurisdiction",
              "Report Week" = "mmwrweek",
              "Report Year" = "mmwryear")

getGrouping<- function(groupByVariable){
  groupsBy<-
    groupings[groupByVariable]
}

print(getGrouping("Report Week"))




getVar("Covid 19 Deaths")


death_variable<- 
  if(summary_variable=="Covid 19 Deaths"){
    "Covid_19_Deaths"
  } else if(summary_variable=="Total Deaths (All Causes)"){
    "Deaths_Total"
  } else if(summary_variable=="Pneumonia Deaths"){
    "Pneumonia_Deaths"
  } else if(summary_variable=="Influenza Deaths"){
    "Influenza_Deaths"
  } else if(summary_variable=="Pneumonia Or Influenza Deaths"){
    "Pneumonia_Or_Influenza_Deaths"
  } else if(summary_variable=="Pneumonia, Influenza Or Covid 19 Deaths"){
    "Pneumonia_Influenza_Or_Covid_Deaths"
  }






