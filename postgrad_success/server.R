library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(reshape2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(scales)


shinyServer(function(input, output) {
  
  ### Load data
  
  scorecard <- read.csv("scorecard.csv")
  
  # Extract variables of interest
  scorecard.data <- scorecard[,c("UNITID","INSTNM","CITY","STABBR","HIGHDEG","CONTROL","SAT_AVG","PCIP01","PCIP09","PCIP11","PCIP12","PCIP13","PCIP15","PCIP23","PCIP24","PCIP26","PCIP27","PCIP38","PCIP39","PCIP40","PCIP42","PCIP43","PCIP45","PCIP50","PCIP52","PCIP54","UGDS","CURROPER","NPT4_PUB","NPT4_PRIV","GRAD_DEBT_MDN_SUPP","GRAD_DEBT_MDN10YR_SUPP","md_earn_wne_p10","gt_25k_p6")]
  
  
  ### Clean data
  
  # Rename esoteric variables
  data.table::setnames(scorecard.data, c("PCIP01","PCIP09","PCIP11","PCIP12","PCIP13","PCIP15","PCIP23","PCIP24","PCIP26","PCIP27","PCIP38","PCIP39","PCIP40","PCIP42","PCIP43","PCIP45","PCIP50","PCIP52","PCIP54"), c("AGRIC","COMM","CS","CULINARY","EDU","ENGI","ENGLISH","HUMANITIES","BIOSCI","MATH","PHIL","THEO","PHYSSCI","PSYCH","LAW_ENFOR","SOCIALSCI","PERFORM","BUSI/MARKET","HIST"))
  
  scorecard.data[,c(7:27,29:34)] <- colwise(as.numeric)(colwise(as.character)(scorecard.data[,c(7:27,29:34)]))
  scorecard.data$CONTROL <- factor(scorecard.data$CONTROL, labels = c("Public Institution", "Private Nonprofit", "Private For-Profit"))
  
  #scorecard.data[,c(6,35)] <- colwise(as.factor)(scorecard.data[,c(6,35)])
  scorecard.data$BIOSCI <- scorecard.data$BIOSCI+scorecard.data$PHYSSCI
  scorecard.data$HUMANITIES <- scorecard.data$ENGLISH+scorecard.data$HUMANITIES+scorecard.data$PHIL+scorecard.data$THEO
  scorecard.data$SOCIALSCI <- scorecard.data$PSYCH+scorecard.data$SOCIALSCI
  data.table::setnames(scorecard.data, c("BIOSCI"), c("SCI"))
  
  # Format degree variables as percentages
  scorecard.data[,c(8:26)] <- numcolwise(percent)(scorecard.data[,c(8:26)])
  
  # Remove institutions that have less than 100 students, aren't open, or contain NAs
  scorecard.data <- filter(scorecard.data, CURROPER == 1 & UGDS >= 100)
  
  scorecard.data <- filter(scorecard.data, (!is.na(NPT4_PUB) | !is.na(NPT4_PRIV)) & !is.na(GRAD_DEBT_MDN_SUPP) & !is.na(md_earn_wne_p10))
  
  
  ### Analysis
  
  # Plot 1: comparing cost of schools and graduate earnings
  df <- scorecard.data %>% select(GRAD_DEBT_MDN_SUPP, CONTROL, md_earn_wne_p10, NPT4_PRIV, NPT4_PUB) %>% mutate(cost=ifelse(CONTROL == "Public Institution", NPT4_PUB, NPT4_PRIV)) %>% filter(cost > 1)

  df.rsq <- data.frame()
  institutions <- c("Public Institution", "Private Nonprofit", "Private For-Profit")
  for (i in institutions) {
    df.i <- df[df$CONTROL==i,]
    fit <- lm(md_earn_wne_p10~cost, data = df.i)
    r <- round(sqrt(summary(fit)$r.squared), 2)
    row <- data.frame(CONTROL=i, r=r)
    df.rsq <- rbind(df.rsq, row)
  }
  
  output$distPlot1 <- renderPlot({
    cost_earnings <- ggplot(df, aes(x=cost, y=md_earn_wne_p10)) +
      geom_point(alpha=.5) + geom_smooth(method = "lm") + #geom_abline(slope = 1) + 
      geom_text(aes(30000, 5000, label=paste('r==',r), group=NULL), data=df.rsq, parse = T) +
      facet_grid(.~CONTROL) +
      labs(x="Yearly Attendence Cost ($)", y="Earnings 10 Yrs Post College Entry ($)") +
      scale_y_continuous(label=function(x){return(paste0(x/1000, "K"))}, limits = c(0,100000), expand = c(0,0)) +
      scale_x_continuous(label=function(x){return(paste0(x/1000, "K"))}, expand = c(0,0)) +
      theme_bw() + scale_colour_tableau() +
      theme(legend.position = "top") 
    cost_earnings
  })
  
  # Plot 2
  df2 <- scorecard.data %>% select(GRAD_DEBT_MDN_SUPP, CONTROL, NPT4_PUB, NPT4_PRIV, md_earn_wne_p10) %>% filter(!is.na(GRAD_DEBT_MDN_SUPP) & (!is.na(NPT4_PRIV) | !is.na(NPT4_PUB)) & !is.na(md_earn_wne_p10)) %>% mutate(cost=ifelse(CONTROL=="Public Institution", NPT4_PUB, NPT4_PRIV)) %>% group_by(CONTROL) %>% summarise(avg_debt = mean(GRAD_DEBT_MDN_SUPP), avg_cost = mean(cost), avg_earn = mean(md_earn_wne_p10))
  
  df2 <- melt(df2, measure.vars =c("avg_debt","avg_cost","avg_earn"))
  
  # verbal <- ggplot(df, aes(x=SATVRMID, y=md_earn_wne_p10)) + geom_point(col="firebrick")
  # math <- ggplot(df, aes(x=SATVRMID, y=md_earn_wne_p10)) + geom_point()
  # grid.arrange(verbal, math, ncol=2)  # gridExtra

  
  ggplot(df2, aes(x=variable, y=value)) +
    facet_grid(~CONTROL) +
    geom_bar(stat = "identity", position = "dodge", width = .5) +
    labs(y="Avg. Value ($)") +
    theme_bw() + scale_colour_tableau() +
    theme(axis.title.x = element_blank(), legend.title = element_blank()) +
    scale_x_discrete(limits=c("avg_earn", "avg_debt", "avg_cost"), labels=c("Graduate Earnings", "Graduate Debt", "Cost of Attendance")) +
    scale_y_continuous(label = function(x){return(paste0(x/1000,"K"))}) 

    df2 <- scorecard.data %>% select(UNITID, AGRIC, COMM, CS, CULINARY, EDU,ENGI,HUMANITIES,SCI,MATH,LAW_ENFOR,SOCIALSCI,PERFORM,`BUSI/MARKET`,HIST) %>% filter(AGRIC != 0 | COMM!=0 | CS!=0 | CULINARY!=0 | EDU!=0 | ENGI!=0 | HUMANITIES != 0 | SCI != 0 | MATH != 0 | LAW_ENFOR != 0 | SOCIALSCI != 0 | PERFORM != 0 | `BUSI/MARKET` != 0 | HIST != 0)
    df2 <- melt(df2, measure.vars = c("AGRIC", "COMM", "CS", "CULINARY", "EDU", "ENGI", "HUMANITIES", "SCI", "MATH", "LAW_ENFOR", "SOCIALSCI", "PERFORM", "BUSI/MARKET", "HIST"), variable.name = "Degree")
    melt(df2, measure.vars = c("Degree", "UNITID"))
    
    ggplot(df2, aes(x=Degree, y=value)) +
      geom_jitter(position = position_jitter(width=.2), aes(col=Degree))
})
