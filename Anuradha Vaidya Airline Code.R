# Analysis of Airline Ticket Pricing
# NAME: Anuradha Vaidya
# EMAIL: anuvaidya11@gmail.com
# COLLEGE: IIT, Madras


airline.df <- read.csv(paste("SixAirlines.csv", sep = ""))

View = airline.df

library(psych)

plot( airline.df$PriceEconomy, airline.df$PricePremium)

plot( airline.df$WidthEconomy, airline.df$WidthPremium)


plot( airline.df$SeatsEconomy, airline.df$SeatsPremium)


plot( airline.df$PitchEconomy, airline.df$PitchPremium)



plot( airline.df$PitchDifference, airline.df$WidthDifference)

library(car)

boxplot(airline.df$PricePremium ~ airline.df$PriceEconomy, data=airline.df, horizontal=TRUE, yaxt="n",
        ylab="PriceEconomy", xlab="PricePremium", 
        main="Factors affecting Premium Economy ticket") 

table(airline.df$IsInternational)

library(car)

scatterplot(PricePremium ~ PriceEconomy, data=airline.df, 
            spread=FALSE, smoother.args=list(lty=2), 
            pch=19, main="Scatterplot of PricePremium vs. PriceEconomy", 
            xlab="PriceEconomy", ylab="PricePremium")

cor.test(airline.df[,"PricePremium"], airline.df[,"PriceEconomy"])

library(car) 
scatterplotMatrix(airline.df[,c("PricePremium","PriceEconomy","PriceRelative")], 
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")


model <- lm(airline.df$PricePremium ~ airline.df$PriceEconomy , data=airline.df) #the '.' means 'all' 
summary(model)

model <- lm(airline.df$PricePremium ~ . , data=airline.df) #the '.' means 'all' 
summary(model)


model <- lm(airline.df$PricePremium ~  airline.df$PriceEconomy  + airline.df$PitchDifference + 
              airline.df$WidthDifference, data=airline.df) #the '.' means 'all' 
summary(model)







summary(airline.df)
