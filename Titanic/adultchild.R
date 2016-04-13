library(ggplot2)
library(dplyr)
library(ggthemes)
library(reshape2)

titanic.full <- read.csv("titanic.csv")

# exclude passengers with relevant null values
titanic.data <- filter(titanic.full, sex != "" & !is.na(pclass))

titanic.data$survived <- factor(titanic.data$survived, labels = c("Perished", "Survived"))
titanic.data$sex <- factor(titanic.data$sex, levels = c("male", "female"), labels = c("Men", "Women"))
titanic.data$pclass <- factor(titanic.data$pclass, levels = c(3, 2, 1), labels = c("3rd class", "2nd class", "1st class"))


# adult passengers
titanic.adult <- filter(titanic.data, age > 12)

adult.df <- titanic.adult %>% group_by(sex, pclass, survived) %>% summarise(count = n()) 
adult.df <- dcast(adult.df, sex+pclass~survived)


adult.plot <- ggplot(adult.df, aes(x=pclass)) +
  facet_grid(sex~.) +
  labs(x="Social Class", y="Passenger Count") +
  scale_y_continuous(label = function(x){return(abs(x))}) +
  geom_bar(aes(y=Survived, fill="Survived"), width = .7, position = "dodge", stat = "identity") +
  geom_bar(aes(y=-Perished, fill="Perished"), width = .7, position = "dodge", stat = "identity") +
  geom_text(aes(y=-Perished, label=Perished, hjust=ifelse(Perished < 20,1.5,-.3))) +
  geom_text(aes(y=Survived, label=Survived, hjust=ifelse(Survived < 20,0,1.4))) +
  coord_flip() +
  theme_bw() + scale_fill_few() +
  theme(legend.title = element_blank())


# child passengers
titanic.child <- filter(titanic.data, age < 13)

child.df <- titanic.child %>% group_by(pclass, survived) %>% summarise(count = n())
child.df <- dcast(child.df, pclass~survived)
child.df$Perished[is.na(child.df$Perished)] <- 0

child.plot <- ggplot(child.df, aes(x=pclass)) +
  labs(x="Social Class", y="Passenger Count") +
  scale_y_continuous(label = function(x){return(abs(x))}) +
  geom_bar(aes(y=Survived, fill="Survived"), width = .4, position = "dodge", stat = "identity") +
  geom_bar(aes(y=-Perished, fill="Perished"), width = .4, position = "dodge", stat = "identity") +
  geom_text(aes(y=-Perished, label=Perished, hjust=ifelse(Perished > 1,-.5,1.5))) +
  geom_text(aes(y=Survived, label=Survived, hjust=1.8)) +
  coord_flip() +
  theme_bw() + scale_fill_few() +
  theme(legend.title = element_blank())

