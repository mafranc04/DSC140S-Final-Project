library(readr)
stroke_data <- read_csv("C:/Users/marek/Downloads/healthcare-dataset-stroke-data.csv")
View(stroke_data)

#Omits NAN values
stroke_data <- na.omit(stroke_data)
View(stroke_data)

#Print the columns and their type then a summary of the data
names(stroke_data)
str(stroke_data)
print(summary(stroke_data))

#Removes the rows with "N/A", "unkown", and "other" as those were not removed from na.omit
stroke_data <- stroke_data[stroke_data$bmi != "N/A",]
stroke_data <- stroke_data[stroke_data$smoking_status != "Unknown",]
View(stroke_data)

print(unique(stroke_data$gender))
stroke_data <- stroke_data[stroke_data$gender != "Other",]

print(unique(stroke_data$ever_married))
print(unique(stroke_data$work_type))
print(unique(stroke_data$Residence_type))

#Made bmi numeric as it was categorical
str(stroke_data)
stroke_data$bmi <- as.numeric(stroke_data$bmi)

print(summary(stroke_data$age))
print(summary(stroke_data$avg_glucose_level))
print(summary(stroke_data$bmi))

#Finding the percentages of each level of smoking in the dataframe
smoking_percentages <- prop.table(table(stroke_data$smoking_status))
print(smoking_percentages)

library(ggplot2)
#Create a histogram of the age distribution across the dataframe
ggplot(stroke_data) + 
  geom_histogram((aes(x=age)),
  fill = "lightblue", binwidth =5) + 
  labs(title = "Age Distribution")

#Created a yes or no column for stroke to make plots nicer
stroke_data$stroke_cat <- ifelse(stroke_data$stroke == 1, "Yes", "No")
View(stroke_data)

#Created a box and whisker plot of avg glucose level compared to stroke and no stroke
ggplot(stroke_data) + 
  geom_boxplot(aes(x = stroke_cat, y =avg_glucose_level, fill = stroke_cat)) + 
  labs(title = "Glucose Level vs. Stroke") 

#Created a scatter plot of glucose level vs bmi
ggplot(stroke_data) + 
  geom_point(aes(x = bmi, y = avg_glucose_level,
  color = stroke_cat)) + labs(title = "BMI vs. Glucose Level") + 
  geom_smooth(aes(bmi, avg_glucose_level), method = "lm", se = F)

corr_bmi_gluc <- cor(stroke_data$bmi, stroke_data$avg_glucose_level)
print(corr_bmi_gluc)

#Created a scatter plot of glucose level vs age with a line of best fit
ggplot(stroke_data) + 
  geom_point(aes(x = age, y = avg_glucose_level,
                 color = stroke_cat)) + labs(title = "Age vs. Glucose Level") + 
  geom_smooth(aes(age, avg_glucose_level), method = "lm", se = F)
 
#Seeing how different variables correlate with eachother
corr_age_gluc <- cor(stroke_data$age, stroke_data$avg_glucose_level)
print(corr_age_gluc)                             

corr_hyp_stroke <- cor(stroke_data$hypertension, stroke_data$stroke)
print(corr_hyp_stroke)

corr_dis_stroke <- cor(stroke_data$heart_disease, stroke_data$stroke)
print(corr_dis_stroke)

corr_age_stroke <- cor(stroke_data$age, stroke_data$stroke)
print(corr_age_stroke)

#Created a contingency table to see how many people had strokes in each profession
stroke_work_table <- table(stroke_data$work_type, stroke_data$stroke_cat)
View(stroke_work_table)
chisq.test(stroke_work_table)  #See if work type is related to stroke

stroke_counts <- table(stroke_data$stroke_cat)
View(stroke_counts)

#Created a contingency table to see how many people had strokes for each smoking status
stroke_smoke_table <- table(stroke_data$smoking_status, stroke_data$stroke_cat)
View(stroke_smoke_table)
chisq.test(stroke_smoke_table) #See if the smoking status is related to stroke

#Created a yes or no column for hypertension
stroke_data$hypertension_cat <- ifelse(stroke_data$hypertension == 1, "Yes", "No")
View(stroke_data)

#Created a contingency table to see how many people had strokes for those who have hypertension and those who dont
stroke_hyp_table <- table(stroke_data$hypertension, stroke_data$stroke_cat)
View(stroke_hyp_table)
chisq.test(stroke_hyp_table) #See if hypertension is related to stroke

#Created a yes or no column for heart disease
stroke_data$heart_disease_cat <- ifelse(stroke_data$heart_disease == 1, "Yes", "No")
View(stroke_data)

#Created a contingency table to see how many people had strokes for those who have heart disease and those who don't
stroke_dis_table <- table(stroke_data$heart_disease_cat, stroke_data$stroke_cat)
View(stroke_dis_table)
chisq.test(stroke_dis_table) #See if heart disease is related to stroke

#Saves the datafram and its changed to be able to input into Python
write.csv(stroke_data, "stroke_data.csv", row.names = FALSE)
getwd()

#Created a contingency table to see how many people had strokes for those who have been married vs never married
stroke_mar_table <- table(stroke_data$ever_married, stroke_data$stroke_cat)
View(stroke_mar_table)
chisq.test(stroke_mar_table) #See if marriage is related to stroke

#Created a contingency table to see how many people had strokes for those who live in urban vs rural
stroke_res_table <- table(stroke_data$Residence_type, stroke_data$stroke_cat)
View(stroke_res_table)
chisq.test(stroke_res_table) #See if area of living is related to stroke

#Does avg glucose level differ between patients who had stroke vs those who didn't across professions
#Groups stroke and work type together and find the mean glucose level for each group
print(aggregate(avg_glucose_level ~ stroke_cat + work_type, data=stroke_data, FUN=mean))

#Creates a boxplot comparing glucose by profession and stroke
ggplot(stroke_data) + geom_boxplot(aes(x=work_type, y=avg_glucose_level, fill=stroke_cat)) +
  labs(title="Average Glucose by Stroke Status and Profession", x="Profession", y="Glucose Level") 
  

