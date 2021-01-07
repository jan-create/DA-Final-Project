library(tidyverse)
library(readr)
#####MYC Dog Licensing Data Import#####
NYC_Dogs <- read_csv("~/Desktop/DA Final Project/data/NYC_Dog_Licensing_Dataset-2.csv")
View(NYC_Dogs)
#simplify column names
NYC_Dogs <- NYC_Dogs %>% 
  rename(
    `Name` = `AnimalName`,
    `Sex` = `AnimalGender`,
    `Birth Year` = `AnimalBirthMonth`,
    `Breed` =  `BreedName`,
    `Zipcode` = `ZipCode`,
    `Issue Date` = `LicenseIssuedDate`,
    `Expiration Date` = `LicenseExpiredDate`
  )
#remove Borough column
#remove columns not planning to use
NYC_Dogs = select(NYC_Dogs, -1, -6, -10) #done
View(NYC_Dogs)
#fix dogs without names listed
NYC_Dogs[NYC_Dogs == "UNKNOWN"] <- "NAME NOT PROVIDED"
NYC_Dogs[NYC_Dogs == "N.A."] <- "NAME NOT PROVIDED"
NYC_Dogs[NYC_Dogs == "-"] <- "NAME NOT PROVIDED"
NYC_Dogs[NYC_Dogs == "--"] <- "NAME NOT PROVIDED"
NYC_Dogs[NYC_Dogs == "---"] <- "NAME NOT PROVIDED"
NYC_Dogs[NYC_Dogs == "-ROY-"] <- "ROY"
NYC_Dogs[NYC_Dogs == "'RUSTY"] <- "RUSTY"
NYC_Dogs[NYC_Dogs == "(LEELA)LILA"] <- "LILA"
NYC_Dogs[NYC_Dogs == "2"] <- "NAME NOT PROVIDED"
NYC_Dogs[NYC_Dogs == "90201"] <- "NAME NOT PROVIDED"


#fix dog breeds
NYC_Dogs[NYC_Dogs == "Schnauzer, Miniature"] <- "Miniature Schnauzer"
NYC_Dogs[NYC_Dogs == "American Pit Bull Terrier/Pit Bull"] <- "American Pitbull Terrier"
#"Poodle, Standard"
#Crossbreed
#Welsh Corgi, Pembroke
#American Pit Bull Mix / Pit Bull Mix

View(NYC_Dogs)
#split up years? how? make data wider? no!

#split dog breed at /
#`Breed 2` <- 

#####Dog Bite Data Import#####
#can join dog bite data and nyc dogs with zipcodes!
Dog_Bites <- read_csv("~/Desktop/DA Final Project/data/DOHMH_Dog_Bite_Data-2.csv")
View(Dog_Bites)
#remove columns not planning to use
Dog_Bites = select(Dog_Bites, -1, -3)
#figure out how to change ages? am I going to use age? does is matter?

