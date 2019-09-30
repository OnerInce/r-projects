
# Load packages
library(ggplot2)
library(dplyr)


# Load data

load("brfss2013.RData") 


# Research question 1:

# Create a new variable for diagnosing cancer          
brfss2013 <- brfss2013 %>% mutate(isCancer = ifelse(chcocncr=="Yes" | chcscncr =="Yes", "Yes","No"))


# Group patients by sex and age
male_cancer_age = brfss2013 %>% group_by(X_age80) %>% 
filter(sex=="Male", isCancer=="Yes", !is.na(X_age80)) %>%
summarise(freq = n())

female_cancer_age = brfss2013 %>% group_by(X_age80) %>%
filter(sex=="Female", isCancer=="Yes", !is.na(X_age80)) %>%
summarise(freq = n())

# Sort by frequencies
sorted_male <- male_cancer_age[order(male_cancer_age$freq),]
sorted_female <- female_cancer_age[order(female_cancer_age$freq),]


# Get the summary statistics
summary(sorted_female)
summary(sorted_male)


# Find the median age groups
sorted_female[[1]][which.min(abs(sorted_female$freq - 496.5))]
sorted_male[[1]][which.min(abs(sorted_male$freq - 196.0))]



# Plot for each sex
ggplot(male_cancer_age, aes(x=X_age80, y=freq)) + geom_point()

ggplot(female_cancer_age, aes(x=X_age80, y=freq)) + geom_point()


# Plot both genders in a single scatter plot 
combine <- ggplot(male_cancer_age, aes(X_age80, freq)) + geom_point(aes(colour = "Male")) + 
	geom_point(data = female_cancer_age, aes(colour = "Female")) + 
	scale_colour_manual(values = c("red", "blue"))


# Visual improvements
combine <- combine + labs(x = "Age", y = "Number of Diagnoses")
combine + 
theme(
	axis.title.x = element_text(color = "black", size = 14, face = "bold"),
	axis.title.y = element_text(color = "#993333", size = 14, face = "bold"))


# Use histogram for each genders
m <- ggplot(male_cancer_age, aes(x=X_age80, y = freq)) + geom_col(color="darkblue", fill="lightblue")
m <- m + labs(x = "Age", y = "Number of Diagnoses")
m + 
	theme(
		axis.title.x = element_text(color = "black", size = 14, face = "bold"),
		axis.title.y = element_text(color = "#993333", size = 14, face = "bold"))


m <- ggplot(female_cancer_age, aes(x=X_age80, y = freq)) + geom_col(color="darkred", fill="firebrick1")
m <- m + labs(x = "Age", y = "Number of Diagnoses")
m + 
	theme(
		axis.title.x = element_text(color = "black", size = 14, face = "bold"),
		axis.title.y = element_text(color = "#993333", size = 14, face = "bold"))


  
# Research question 2:


# Gather required data
brfss2013 %>% group_by(X_hispanc) %>% 
	filter(!is.na(X_hispanc)) %>% summarise(freq = n())


brfss2013 %>% group_by(X_hispanc) %>% 
	filter(isCancer =="Yes", !is.na(X_hispanc)) %>% summarise(freq = n())
	
2141 / 37062
79743 / 449274

# Find the ratio among male and female (non-Hispanic and Hispanic)
 
brfss2013 %>% group_by(X_hispanc) %>% 
	filter(sex=="Female", !is.na(X_hispanc)) %>% summarise(freq = n())
  
brfss2013 %>% group_by(X_hispanc) %>% 
	filter(sex=="Female", isCancer =="Yes", !is.na(X_hispanc)) %>% summarise(freq = n())
 
1442 / 21775
49166 / 265775


brfss2013 %>% group_by(X_hispanc) %>% 
	filter(sex=="Male", !is.na(X_hispanc)) %>% summarise(freq = n())

brfss2013 %>% group_by(X_hispanc) %>% 
	filter(sex=="Male", isCancer =="Yes", !is.na(X_hispanc)) %>% summarise(freq = n())

699 / 15287
30577 / 183499


# Pie charts

h1 <- brfss2013 %>% group_by(isCancer) %>% 
	filter(X_hispanc == "Hispanic, Latino/a, or Spanish origin", !is.na(X_hispanc), !is.na(isCancer)) %>% 
	summarise(freq = n()) %>% mutate(pct = freq/sum(freq) * 100)

pie = ggplot(h1, aes(x="", y=pct/100, fill=isCancer)) + geom_bar(stat="identity", width=1)

pie = pie + coord_polar("y", start=0) + 
	geom_text(aes(label = paste0(round(pct), "%")), position = position_stack(vjust = 0.5))

pie = pie + scale_fill_manual(values=c("#F26419", "#999999"))

pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Hispanic People - Having Cancer")

pie = pie + theme_classic() + 
			theme(axis.line = element_blank(),
				axis.text = element_blank(),
				axis.ticks = element_blank(),
				plot.title = element_text(hjust = 0.5, color = "#666666"))


h2 <- brfss2013 %>% group_by(isCancer) %>% 
	filter(X_hispanc == "Not of Hispanic, Latino/a, or Spanish origin", !is.na(X_hispanc), !is.na(isCancer)) %>% 
	summarise(freq = n()) %>% mutate(pct = freq/sum(freq) * 100)

pie = ggplot(h2, aes(x="", y=pct/100, fill=isCancer)) + geom_bar(stat="identity", width=1)

pie = pie + coord_polar("y", start=0) + 
			geom_text(aes(label = paste0(round(pct), "%")), position = position_stack(vjust = 0.5))

pie = pie + scale_fill_manual(values=c("#F26419", "#999999"))

pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Not Hispanic People - Having Cancer")

pie = pie + theme_classic() + 
			theme(axis.line = element_blank(),
				axis.text = element_blank(),
				axis.ticks = element_blank(),
				plot.title = element_text(hjust = 0.5, color = "#666666"))
 
  

# Research question 3:

# Gather data and group

combine <- brfss2013 %>% group_by(X_rfseat2, educa) %>% filter(!is.na(X_rfseat2),!is.na(educa)) %>% 
summarise(count=n()) %>% mutate(pct = count / sum(count) * 100)

combine <- combine %>% mutate(belt = ifelse(X_rfseat2 == "Always or almost always wear seat belt", "yes", "no")

# Bar chart
p = ggplot(combine, aes(x=forcats::fct_relabel(educa,stringr::str_wrap, width = 10), y = pct, fill=belt)) + 
			geom_col(position=position_dodge())


p = p + geom_text(aes(label=paste0(round(pct), "%"), family = "serif"), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=4.0) + 
		theme_minimal() + 
		scale_fill_brewer(palette="Dark2") + 
		labs(title = "Education Level - Seat Belt", fill = "Wearing \n Belt?")

p = p + 
	theme(
  plot.title = element_text(family = "mono", face="bold.italic", hjust = 0.5, color = "#666666"), 
  axis.title.x = element_text(family = "mono", color="blue", size=12, face="bold"), 
  axis.title.y = element_text(family = "mono", color="blue", size=12, face="bold")) + 
  xlab("Education Level") + ylab("Percentage")
