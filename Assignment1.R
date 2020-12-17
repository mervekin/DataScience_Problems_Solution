#assignment1 
install.packages("dplyr")
library(dplyr)
data("starwars")

#1
count(unique(starwars%>%select(homeworld)))

#2
characters<-select(starwars,name,films)
characters$film_count<-NA
for(i in 1:nrow(characters)){
  characters$film_count[i]<-length(starwars$films[[i]])
}
most_characters<-characters[which.max(characters$film_count),]
most_characters

#3
grupped_species<-group_by(starwars,species)
summarise(grupped_species,avg_height=mean(height,na.rm = TRUE),
          avg_mass=mean(mass,na.rm = TRUE))


#4
starwars2<-(starwars%>%
              add_row(name="merve",height=163,mass=60,hair_color="black",skin_color="light",
                      eye_color="black",birth_year=1903,sex="female",gender="feminine",homeworld="Corellia",
                      species="Human",films=list(c("Solo: A Star Wars Story","Star Wars: Episode IV - A New Hope")),vehicles=list(c("NA")),starships=list(c("Millennium Falcon"))))
filter(starwars2, name=="merve")

#5 
dfwith_bmi<-starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name, mass, height ,species, hair_color, skin_color, eye_color, sex, gender,bmi)

dfwith_bmi


#6
underweight<-dfwith_bmi %>% 
  select(species,bmi)%>%
  filter(bmi<18.5)%>%
  mutate(bmi_result='underweight')

underweight
count(underweight)

healthy<-dfwith_bmi%>%
  select(species,bmi)%>%
  filter(bmi>=18.5 & bmi<=24.99)%>%
  mutate(bmi_result='healthy')
healthy
count(healthy)

overweight<-dfwith_bmi%>%
  select(species,bmi)%>%
  filter(bmi>=25 & bmi<=29.99)%>%
  mutate(bmi_result='overweight')
overweight
count(overweight)

obese<-dfwith_bmi%>%
  select(species,bmi)%>%
  filter(bmi>=30)%>%
  mutate(bmi_result='obese')
count(obese)


#7

install.packages("tidyverse")
library(tidyverse)
df_gender<- na.omit(select(dfwith_bmi,gender,sex,bmi))

ggplot(data = df_gender, mapping = aes(x=gender,y=bmi))+
  geom_boxplot( color="blue", fill="blue",
                alpha=0.2,
                
                notch=TRUE,
                notchwidth = 0.8,
                
                outlier.colour="red",
                outlier.fill="red",
                outlier.size=3
  )
ggplot(data = df_gender, mapping = aes(x=sex,y=bmi))+
  geom_boxplot( color="black", fill="red", alpha=0.2,
                notch=TRUE,
                notchwidth = 0.8,
                
                outlier.colour="green",
                outlier.fill="green",
                outlier.size=3)+
  coord_flip()

