# Data Management for Project 8/2/15

library(ggplot2)
library(PDS)
library(dplyr)
DF <- tbl_df(NESARC) %>%
  filter(S3AQ1A ==1 & S3AQ3B1 == 1 & CHECK321 == 1 & AGE <= 25) %>% 
  select(contains("S3AQ8"))
DF[] <- lapply(DF, as.character)
DF[] <- lapply(DF, as.numeric)
myfix <- function(x){ifelse(x %in% c(2, 9), 0, ifelse(x == 1, 1, NA))}
DF2 <- apply(DF, 2, myfix)
DF2 <- as.data.frame(DF2)
DF2$NumberNicotineSymptoms <- apply(DF2, 1, sum, na.rm = TRUE)
nesarc <- tbl_df(NESARC) %>% 
  filter(S3AQ1A ==1 & S3AQ3B1 == 1 & CHECK321 == 1 & AGE <= 25) %>% 
  rename(Ethnicity = ETHRACE2A, Age = AGE, MajorDepression = MAJORDEPLIFE, 
         Sex = SEX, TobaccoDependence = TAB12MDX, DailyCigsSmoked = S3AQ3C1) %>% 
  select(Ethnicity, Age, MajorDepression, TobaccoDependence, DailyCigsSmoked, Sex)
nesarc <- data.frame(nesarc, NumberNicotineSymptoms = DF2$NumberNicotineSymptoms)
nesarc <- tbl_df(nesarc)
# Code 99 properly
nesarc$DailyCigsSmoked[nesarc$DailyCigsSmoked == 99] <- NA
nesarc$DCScat <- cut(nesarc$DailyCigsSmoked, breaks = c(0, 5, 10, 15, 20, 98), include.lowest = FALSE)
# Label stuff
nesarc$Ethnicity <- factor(nesarc$Ethnicity, 
                           labels = c("Caucasian", "African American", 
                                      "Native American", "Asian", "Hispanic"))
nesarc$TobaccoDependence <- factor(nesarc$TobaccoDependence, 
                                   labels = c("No Nicotine Dependence", 
                                              "Nicotine Dependence"))
nesarc$Sex <- factor(nesarc$Sex, labels =c("Female", "Male"))
nesarc$MajorDepression <- factor(nesarc$MajorDepression, 
                                 labels =c("No Depression", "Yes Depression"))
#
dim(nesarc)
####################
xtabs(~ MajorDepression +  DCScat + TobaccoDependence, data = nesarc)
prop.table(xtabs(~ TobaccoDependence +  DCScat + 
                   MajorDepression, data = nesarc), 2)

tapply(nesarc$DailyCigsSmoked, list(nesarc$MajorDepression, nesarc$TobaccoDependence), mean, na.rm = TRUE)
####################
ggplot(data = nesarc, aes(x = MajorDepression, fill = TobaccoDependence)) + 
  geom_bar(position = "fill") +
  theme_bw() + 
  labs(x = "", y = "Fraction", 
       title = "Fraction of young adult daily smokers\nwith and without nicotine addiction\nby depression status") + 
  scale_fill_manual(values = c("green", "red"), name = "Tobacco Addiction Status") + 
  guides(fill = guide_legend(reverse = TRUE))

####
mod1 <- aov(DailyCigsSmoked ~ TobaccoDependence, 
            data = nesarc[nesarc$MajorDepression == "No Depression", ])
summary(mod1)
mod2 <- aov(DailyCigsSmoked ~ TobaccoDependence, 
            data = nesarc[nesarc$MajorDepression == "Yes Depression", ])
summary(mod2)
mod3 <- aov(DailyCigsSmoked ~ TobaccoDependence + MajorDepression, data = nesarc)
summary(mod3)
mod4 <- aov(DailyCigsSmoked ~ TobaccoDependence*MajorDepression, data = nesarc)
summary(mod4)
#######################
?interaction.plot
tapply(nesarc$DailyCigsSmoked, list(nesarc$TobaccoDependence, nesarc$MajorDepression), 
       mean, na.rm = TRUE)
# Need to write function t ignore NA values
mean.rm.na <- function(x){mean(x, na.rm=TRUE)}
# Ok....note lines do not cross....but not the most beautiful graph
interaction.plot(nesarc$MajorDepression, nesarc$TobaccoDependence, 
                 nesarc$DailyCigsSmoked, fun = mean.rm.na)
###
ggplot(data = nesarc, aes(x = MajorDepression, y = DailyCigsSmoked, shape = TobaccoDependence,
                            group = TobaccoDependence, linetype = TobaccoDependence)) +
  stat_summary(fun.y = mean, na.rm = TRUE, geom = "point") +
  stat_summary(fun.y = mean, na.rm = TRUE, geom = "line") +
  labs(y = "Mean Daily Cigarettes Smoked", x = "") + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_bw()
###

###################
library(grid)
library(PDS)
library(ggplot2)
mod.lm <- lm(Distance ~ Age, data = signdist)
summary(mod.lm)
predict(mod.lm, newdata = data.frame(Age = 60))
#############################################################
PV <- predict(mod.lm, newdata = data.frame(Age = 60))
PV
ggplot(data = signdist, aes(x = Age, y = Distance)) + 
   geom_point(color = "purple") +
   theme_bw() + 
   labs(x = "Drivers Age (years)", y = "Sign Legibility Distance (feet)") + 
   geom_smooth(method = "lm", se = FALSE) + 
   geom_line(data = data.frame(Age = c(60, 60), 
                               Distance = c(280, PV)), 
             arrow = arrow(type = "closed", angle = 15), color = "red") + 
  geom_line(data = data.frame(Age = c(60, 19), Distance = c(PV, PV)),
            arrow = arrow(type = "closed", angle = 15, ends = "first"), color = "red")

### Else
library(ggplot2)
ggplot(data = nesarc[(!is.na(nesarc$TobaccoDependence) & 
                        !is.na(nesarc$DCScat)), ], 
       aes(x = DCScat, fill = TobaccoDependence)) + 
  geom_bar(position = "fill") +
  theme_bw() +
  labs(x= "Daily Smoking Frequency", y = "Fraction") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_grid(MajorDepression ~ .)
#####

# Models
mod.aov <- aov(DailyCigsSmoked ~ TobaccoDependence + MajorDepression + TobaccoDependence:MajorDepression, data = nesarc)
summary(mod.aov)
mod2.aov <- aov(DailyCigsSmoked ~ MajorDepression, data = nesarc)
summary(mod2.aov)