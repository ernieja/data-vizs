library(shiny)
library(shinydashboard)
library(RCurl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)


shinyServer(function(input, output) {
  college <- read.csv("college.csv")
  
  college.data <- college[,c("UNITID","INSTNM","CITY","STABBR","HIGHDEG","CONTROL","SATVRMID","SATMTMID", "SATWRMID", "SAT_AVG","PCIP01","PCIP04","PCIP09","PCIP11","PCIP13","PCIP14","PCIP16","PCIP19","PCIP22","PCIP27","PCIP38","PCIP40","PCIP45","PCIP50","PCIP52","PCIP54","UGDS","UGDS_WHITE","UGDS_BLACK","UGDS_HISP","UGDS_ASIAN","CURROPER","NPT4_PUB","NPT4_PRIV","GRAD_DEBT_MDN_SUPP","GRAD_DEBT_MDN10YR_SUPP","md_earn_wne_p10","gt_25k_p6")]
  setnames(college.data, c("PCIP01","PCIP04","PCIP09","PCIP11","PCIP13","PCIP14","PCIP16","PCIP19","PCIP22","PCIP27","PCIP38","PCIP40","PCIP45","PCIP50","PCIP52","PCIP54"), c("AGRIC","ARCHIT","COMM","CS","EDU","ENGI","LANG","HUMANSCI","LAW","MATH","PHIL","PHYSSCI","SOCIALSCI","PERFORM","BUSI","HIST"))
  
  college.data[,c(7:31,33:38)] <- colwise(as.numeric)(colwise(as.character)(college.data[,c(7:31,33:38)]))
  college.data[,c(6,32)] <- colwise(as.factor)(college.data[,c(6,32)])
  
  # disregard institutions that aren't open or don't offer a bachelor's degree
  college.data <- filter(college.data, CURROPER == 1 & HIGHDEG > 2)
  college.data <- college.data %>% mutate(isPub=ifelse(CONTROL == "1", "1", "0"))
  college.data$isPub <- factor(college.data$isPub, labels = c("Private Institution", "Public Institution"))
  
  
  # Plot 1: comparing cost of private and public schools and graduate earnings
  df <- college.data %>% select(UNITID, isPub, md_earn_wne_p10, NPT4_PRIV, NPT4_PUB) %>% filter(!is.na(NPT4_PRIV) | !is.na(NPT4_PUB)) %>% mutate(cost=ifelse(isPub == "Public Institution", NPT4_PUB, NPT4_PRIV), earn_to_cost=md_earn_wne_p10/cost)
  

  output$distPlot1 <- renderPlot({
    cost_earnings <- ggplot(df, aes(x=cost, y=md_earn_wne_p10, col=isPub)) +
      geom_point() +
      labs(x="Average Attendence Cost ($)", y="Median Graduate Earnings ($)") +
      facet_wrap(~isPub) +
      scale_y_continuous(label=function(x){return(paste0(x/1000, "K"))}) +
      scale_x_continuous(label=function(x){return(paste0(x/1000, "K"))}) +
      theme_bw() + scale_colour_tableau() +
      theme(legend.position = "none")
    cost_earnings

  })
  
  # Plot 2
  df <- college.data %>% select(SATVRMID, SATMTMID, SAT_AVG, md_earn_wne_p10)
  
  verbal <- ggplot(df, aes(x=SATVRMID, y=md_earn_wne_p10)) + geom_point(col="firebrick")
  math <- ggplot(df, aes(x=SATVRMID, y=md_earn_wne_p10)) + geom_point()
  
  grid.arrange(verbal, math, ncol=2)
  

})
