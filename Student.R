library(tidyverse)
library(readxl)
library(here)
library(dplyr)
library(ggplot2)
library(stringr)
library(skimr) # install.packages('skimr')
library(kableExtra) # install.packages('kableExtra')

rm(list=ls())

setwd('~/Documents/DATA332/Projects/Student Data')
course <- read_excel('Course.xlsx', .name_repair = 'universal')
registration <- read_excel('Registration.xlsx', .name_repair = 'universal')
student <- read_excel('Student.xlsx', .name_repair = 'universal')

df <- left_join(registration, course, by = c('Instance.ID'))
df <- left_join(student, df, by =c('Student.ID'))




df_majors <- df %>%
  group_by(Title) %>%
  summarize(count = n())

majors <- ggplot(df_majors, aes(x = Title, y = count)) +
  geom_col(fill = "skyblue", color = "black", size = 0.5) +
  labs(title = "Major Distribution",
       x = "Majors",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "lightgray", linetype = "dotted"), 
        plot.title = element_text(face = "bold")) +
  coord_flip()

ggsave("~/Documents/DATA332/Projects/Student Data/Images/majors.png", majors, width = 10, height = 6, dpi = 300, bg = "white")




df[c('DOB.Year', 'DOB.Month', 'DOB.Date')] <- str_split_fixed(df$Birth.Date, '-',3)

df_birth_year <- df %>%
  group_by(DOB.Year) %>% 
  summarize(count = n())

birth_year <- ggplot(df_birth_year, aes(x = DOB.Year, y = count)) +
  geom_col(fill = "lightcoral", color = "black", size = 0.5) +
  labs(title = "Birth Year Distribution",
       x = "Birth Years",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 6), 
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold")) +
  coord_flip()

ggsave("~/Documents/DATA332/Projects/Student Data/Images/birth_year.png", birth_year, width = 10, height = 6, dpi = 300, bg = "white")




df_totalcost_major <- df %>% 
  group_by(Title, Payment.Plan) %>% 
  summarize(Total.Cost = sum(Total.Cost))

totalcost_major <- ggplot(df_totalcost_major, aes(x = Title, y = Total.Cost, fill = Payment.Plan)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.5) +
  labs(title = "Total Cost Distribution by Major and Payment Plan",
       x = "Majors",
       y = "Total Cost", 
       fill = "Payment Plan") +  # Modify legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Set3") +  
  geom_text(aes(label = scales::dollar(Total.Cost), y = Total.Cost),
            position = position_stack(vjust = 0.5), color = "black", size = 3) + 
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  coord_flip()

ggsave("~/Documents/DATA332/Projects/Student Data/Images/total_cost.png", totalcost_major, width = 10, height = 6, dpi = 300, bg = "white")




df_balance_due_major <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(Balance.Due = sum(Balance.Due))

balance_due <- ggplot(df_balance_due_major, aes(x = Title, y = Balance.Due, fill = Payment.Plan)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.5) +
  labs(title = "Balance Due Distribution by Major and Payment Plan",
       x = "Majors",
       y = "Balance Due", 
      fill = "Payment Plan") +  
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = ifelse(Balance.Due %% 1 == 0, paste0("$", format(Balance.Due, nsmall = 0)), scales::dollar(Balance.Due)), 
                y = Balance.Due),
            position = position_stack(vjust = 0.5), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold")) +
  coord_flip()

ggsave("~/Documents/DATA332/Projects/Student Data/Images/balance_due.png", balance_due, width = 10, height = 6, dpi = 300, bg = "white")


  


