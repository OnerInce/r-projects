
### Load packages

library(ggplot2)
library(dplyr)
library(statsr)


### Load data

load("gss.Rdata")


## Exploratory data analysis
    
# investigate the variables 

gov_opinion = gss %>% group_by(helpnot) %>% 
    filter(!is.na(helpnot)) %>% 
    summarise(freq = n())

gov_opinion

# show the results in a pie chart

govt_op_dist <- gss %>% group_by(helpnot) %>% 
    filter(helpnot != "Agree With Both", !is.na(helpnot)) %>% 
    summarise(freq = n()) %>% mutate(pct = freq/sum(freq) * 100)
  
  pie = ggplot(govt_op_dist, aes(x="", y=pct/100, fill=helpnot)) + geom_bar(stat="identity", width=1)
  
  pie = pie + coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(round(pct), "%")), position = position_stack(vjust = 0.5))
  
  pie = pie + scale_fill_manual(values=c("red", "grey"))
  
  pie = pie + labs(x = NULL, y = NULL, title = "Government Opinion")
  
  pie = pie + theme_classic() + theme(axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "#666666"))
  
  print(pie)


# investigate the other variable

family_income = gss %>% group_by(coninc) %>% 
    filter(!is.na(coninc)) %>% 
    summarise(freq = n())

family_income


p <- ggplot(gss, aes(x=coninc)) + 
  geom_histogram(color="black", fill="lightblue")
p


# calculate the summary statistics of this variable.

summary(family_income$coninc)

# standard deviation is:

sd(family_income$coninc)


# mean and standard deviation values for each opinion group

family_income_both = gss %>% group_by(helpnot) %>% 
    filter(!is.na(helpnot)) %>% 
    summarise(count = n(), mean = mean(coninc, na.rm = TRUE), sd = sd(coninc, na.rm = TRUE))

family_income_both

* * *

## Part 4: Inference

# degrees of freedom ;

family_income_both = gss %>% group_by(helpnot) %>% 
    filter(!is.na(helpnot)) %>% 
    summarise(count = n(), mean = mean(coninc, na.rm = TRUE), sd = sd(coninc, na.rm = TRUE))

family_income_both


qt(0.025, df = 4060)

pt(24.20, df = 4060, lower.tail = FALSE) * 2




