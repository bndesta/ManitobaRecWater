#Manitoba data 2007 to 2020
#Data Analysis: Associations between Environmental Factors and Freshwater Recreational Microbial Water Quality in Manitoba, Wi
#Binyam Negussie Desta
#Last Updated September 29, 2023


pacman::p_load(rio,
               here,
               tidyverse,
               tsibble,
               janitor,
               dplyr,
               gtsummary,
               lubridate,
               tidybayes,
               bayesplot,
               marginaleffects,
               emmeans,
               modelr,
               base,
               sclaes,
               flextable,
               rstatix)

# Load datasets
Manitoba <- import(here("C:/~", "ManitobaMerged20220729.xlsx"))


#Plotting mean of the geomeans by year 

summary_table <- Manitoba %>%                                        
  group_by(Year,Beach) %>%                                             
  summarise(                                                         
    Ecoli_mean  = round(mean(Geomean5, na.rm=T), digits = 1)  
  )

summary_table  # print

#Plot mean of the Ecoli geomeans by year
ggplot(summary_table, aes(x = Year, y = Ecoli_mean, color = Beach)) +
  geom_line(aes(group = Beach), alpha = 1, size = 1.2) +
  theme_bw() +
  labs(,
       x = "Year",
       y = "Year Average of Daily Geometric Mean of E.coli(CFU/100ml)",
       color = NULL
  )
#Plotting the graph (bar chart)
ggplot(summary_table, aes(x=Year, y=Ecoli_mean, fill=Beach)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=""))+ scale_fill_manual(values=c('yellow','black','darkgray')) +
  labs(x = "Year", y = "Year Average of Daily Geometric Mean of E.coli(CFU/100ml)") + 
  theme_minimal()
  

#Plotting mean of temperature by year 

summary_table <- Manitoba %>%                                        
  group_by(Year,Beach2) %>%                                             
  summarise(                                                         
    Temp_mean  = round(mean(MeanTemp, na.rm=T), digits = 1)  
  )

summary_table  # print

#Plot mean of the Temp by year (line graph)
ggplot(summary_table, aes(x = Year, y = Temp_mean, color = Beach2)) +
  geom_line(aes(group = Beach2), alpha = 1, size = 1.2) +
  theme_bw() +
  labs(,
       x = "Year",
       y = "Year Average of Daily Mean of Air Temperature(oC)",
       color = NULL
  )
#Plotting the graph (bar chart)
ggplot(summary_table, aes(x=Year, y=Temp_mean, fill=Beach2)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=""))+ scale_fill_manual(values=c('black','lightgray')) +
  labs(x = "Year", y = "Year Average of Mean of Air Temperature(oC)") + 
  theme_minimal()

#Plotting mean of Rain48 by year

summary_table2 <- Manitoba %>%                                        
  group_by(Year,Beach2) %>%                                             
  summarise(                                                         
    Rain_mean  = round(mean(Rain48, na.rm=T), digits = 1)  
  )

summary_table  # print

#Plot mean of the Rain48 by year (line graph)
ggplot(summary_table2, aes(x = Year, y = Rain_mean, color = Beach2)) +
  geom_line(aes(group = Beach2), alpha = 1, size = 1.2) +
  theme_bw() +
  labs(,
       x = "Year",
       y = "Year Average of 48-hr Rainfall(mm)",
       color = NULL
  )
#Plotting the graph (bar chart)
ggplot(summary_table2, aes(x=Year, y=Rain_mean, fill=Beach2)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=""))+ scale_fill_manual(values=c('black','lightgray')) +
  labs(x = "Year", y = "Year Average of 48-hr Rainfall(mm)") + 
  theme_minimal()



#Plotting a barchart of mean of geomean E.coli by year
Manitoba |> group_by(Beach) |> 
  summarize(count = n(), 
            mean_Ec = mean(Geomean5, na.rm=TRUE))

#Inputting the data
PlotE <- data.frame(Beach=rep(c("Gimli", "Grand"), each=15),
                    Year=rep(c("2007", "2008", "2009", "2010", "2011",
                               "2012", "2013", "2014", "2015", "2016", "2017",
                               "2018", "2019","2020", "2021"),14),
                    E.coli=c(192.1, 61.8, 275.2, 87.2, 35.1, 50.4, 96.5, 47.3, 295.3,
                             369.6, 278.6, 122.7, 301.9, 143.9, 134.1, 94.9, 23.6, 39.2,
                             81.2, 29.3, 29.7, 67.5, 25.7, 111.1, 70.5, 66.9, 141.6, 403.4,
                             32.6, 56.9))
head(PlotE)
#Plotting the graph
ggplot(data=PlotE, aes(x=Year, y=E.coli, fill=Beach)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=""))+ scale_fill_manual(values=c('black','lightgray')) +
  labs(x = "Year", y = "Yearly Average of Daily Geometric Mean of E. coli (CFU/100ml)") + 
  theme_minimal()


#Plotting a barchart of exceedances by year
#Inputting the data
PlotE <- data.frame(Beach=rep(c("Gimli", "Grand"), each=15),
                  Year=rep(c("2007", "2008", "2009", "2010", "2011",
                             "2012", "2013", "2014", "2015", "2016", "2017",
                             "2018", "2019","2020", "2021"),14),
                  E.coli=c(17.7, 8.6, 24.1, 9.6, 2.9, 4.2, 18, 3.5, 27.3, 
                        32.4, 20, 26.3, 35.3, 15.8, 21.1, 3.2, 2, 7.2, 
                        5.9, 1.5, 0, 7.4, 0, 13, 8.5, 6.8, 22.9, 27.8, 
                        0, 11.8))
head(PlotE)
#Plotting the graph
ggplot(data=PlotE, aes(x=Year, y=E.coli, fill=Beach)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=""))+ scale_fill_manual(values=c('black','lightgray')) +
  labs(x = "Year", y = "Days Exceeded (%)") + 
  theme_minimal()




## Preparing the data for the marginal effects plot 

#declaring columns as a factor/numeric
Manitoba <- Manitoba %>%
  mutate(Year = as.numeric(Year))
Manitoba <- Manitoba %>%
  mutate(Beach = as.factor(Beach))
Manitoba <- Manitoba %>%
  mutate(PrevGeomean = as.numeric(PrevGeomean))
Manitoba <- Manitoba %>%
  mutate(Rain48 = as.numeric(Rain48))
Manitoba <- Manitoba %>%
  mutate(MeanTemp24 = as.numeric(MeanTemp24))
Manitoba <- Manitoba %>%
  mutate(AveUV24 = as.numeric(AveUV24))
Manitoba <- Manitoba %>%
  mutate(DaysSinceRain = as.numeric(DaysSinceRain))


#Splitting the dataset by Year

Manitoba<-subset(Manitoba,Year %in% c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))
                       
#Categorizing the E.coli conc above and below 200

Manitoba$Geomean5_c <- cut(Manitoba$Geomean5,
                           breaks=c(0, 199.99999, 7400),
                           labels=c(0, 1))
#declaring the outcome var as factor and year as label
Manitoba <- Manitoba %>%
  mutate(Geomean5_c = as.factor(Geomean5_c))

Manitoba$Year <- as_label(Manitoba$Year)
summary(Manitoba)

#Log transforming variables (Previous day Geomean E.coli)

Manitoba <- Manitoba %>%
  mutate(LogEC24 = log(Manitoba$PrevGeomean)) %>% 
  dplyr::relocate(LogEC24, .after=PrevGeomean)

#Splitting the dataset by Beach name
Gimli <- Manitoba[Manitoba$Beach == 'GIMLI BEACH', ]
Grand <- Manitoba[Manitoba$Beach == 'GRAND BEACH EAST', ]
Grand <- Manitoba[Manitoba$Beach == 'GRAND BEACH WEST', ]

#Creating a second Beach2 variable merging grand west and east
Manitoba$Beach2[Manitoba$Beach == "GRAND BEACH EAST"] <- "GRAND BEACH"
Manitoba$Beach2[Manitoba$Beach == "GRAND BEACH WEST"] <- "GRAND BEACH"
Manitoba$Beach2[Manitoba$Beach == "GIMLI BEACH"] <- "GIMLI BEACH"

summary(Gimli)
table(Grand$MeanTemp24, Grand$Year)

table(Gimli$Rain48)
Manitoba["Geomean10"][Vancouver["Geomean10"] == 0] <- 10



# estimate the model and store results in modMGi - GIMLI
# short variable label, for plot
attr(Gimli$Rain48, "label") <- "48 h Rain (mm)"
attr(Gimli$MeanTemp24, "label") <- "24 h Mean Temperature (oC)"
attr(Gimli$LogEC24, "label") <- "24-hr Log10 E. coli (CFU/100 ml)"
attr(Gimli$AveUV24, "label") <- "24-hr mean UV index"
attr(Gimli$DaysSinceRain, "label") <- "Antecedent dry days"
attr(Gimli$Geomean5_c, "label") <- "E. coli threshold exceedances (200 CFU/100 ml) "
modMGi <- glmer(Geomean5_c ~ 1 + Rain48 + MeanTemp24 + LogEC24 + AveUV24 + DaysSinceRain +
             (1|Year), data = Gimli, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)


# print the mod results without correlations among fixed effects
print(modMGi, corr = FALSE)

summary(Gimli)

plot_model(modMGi, type = "pred", terms = c("Year", "Rain48 [0, 0.5, 22.8, 45.8]"))


#GIMLI
#Plotting mariginal effect of rain on e.coli exceedances 
me <- ggpredict(modMGi, terms = c("Year","Rain48[0, 0.5, 22.8, 45.8]"), type = "re")
plot(me)

#Plotting mariginal effect of temperature on e.coli exceedances 
me2 <- ggpredict(modMGi, terms = c("Year","MeanTemp24[0, 16.7, 23.1, 25.2]"), type = "re")
plot(me2)

#Plotting mariginal effect of previous day geomean on e.coli exceedances 
me3 <- ggpredict(modMGi, terms = c("Year","LogEC24[0, 3.56, 6.49, 7.78]"), type = "re")
plot(me3)

#Plotting mariginal effect of UV index on e.coli exceedances 
me4 <- ggpredict(modMGi, terms = c("Year","AveUV24[0, 1.45, 2.03, 2.20]"), type = "re")
plot(me4)

#Plotting mariginal effect of antecedent dry days on e.coli exceedances 
me5 <- ggpredict(modMGi, terms = c("Year","DaysSinceRain[0, 1, 6, 12]"), type = "re")
plot(me5)



# estimate the model and store results in modMGr - GRAND
# short variable label, for plot
attr(Grand$Rain48, "label") <- "48 h Rain (mm)"
attr(Grand$MeanTemp24, "label") <- "24 h Mean Temperature (oC)"
attr(Grand$LogEC24, "label") <- "24-hr Log10 E. coli (CFU/100 ml)"
attr(Grand$AveUV24, "label") <- "24-hr mean UV index"
attr(Grand$DaysSinceRain, "label") <- "Antecedent dry days"
attr(Grand$Geomean5_c, "label") <- "E. coli threshold exceedances (200 CFU/100 ml)"

modMGr <- glmer(Geomean5_c ~ 1 + Rain48 + MeanTemp24 + LogEC24 + AveUV24 + DaysSinceRain +
                  (1|Year), data = Grand, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10)

# print the mod results without correlations among fixed effects
print(modMGr, corr = FALSE)

#GRAND
#Plotting mariginal effect of rain on e.coli exceedances 
me6 <- ggpredict(modMGr, terms = c("Year","Rain48[0, 0.5, 22.8, 45.8]"), type = "re")
plot(me6)

#Plotting mariginal effect of temperature on e.coli exceedances 
me7 <- ggpredict(modMGr, terms = c("Year","MeanTemp24[0, 17.1, 23.1, 25]"), type = "re")
plot(me7)

#Plotting mariginal effect of previous day geomean on e.coli exceedances 
me8 <- ggpredict(modMGr, terms = c("Year","LogEC24[0, 2.80, 5.51, 6.80]"), type = "re")
plot(me8)

#Plotting mariginal effect of UV index on e.coli exceedances 
me9 <- ggpredict(modMGr, terms = c("Year","AveUV24[0, 1.44, 2.00, 2.17]"), type = "re")
plot(me9)

#Plotting mariginal effect of antecedent dry days on e.coli exceedances 
me5 <- ggpredict(modMGi, terms = c("Year","DaysSinceRain[0, 1, 6, 12]"), type = "re")
plot(me5)