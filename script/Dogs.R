library(tidyverse)
library(readr)
#####MYC Dog Licensing Data Import#####
NYC_Dogs <- read_csv("~/Desktop/DA Final Project/data/NYC_Dog_Licensing_Dataset-2.csv")

glimpse(NYC_Dogs)
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
glimpse(NYC_Dogs)
#remove columns not planning to use
NYC_Dogs = select(NYC_Dogs, -1, -6, -10)
NYC_Dogs <- distinct(NYC_Dogs) #removes duplicate rows, some dogs entered multiple times and have multiple licenses issued

#Add an age column (Age in Years)
NYC_Dogs <- NYC_Dogs %>%
  mutate(Age = 2016 - NYC_Dogs$`Birth Year`)
#used 2016 because this is the year data was entered/collected


#Dog Average Age
avg_age <- mean(NYC_Dogs$Age)



#####NYC DOG NAMES#####
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
NYC_Dogs[NYC_Dogs == "0HSO" |
           NYC_Dogs == "2003-08-13T00:00:00.000" |
           NYC_Dogs == "2003-10-22T00:00:00.000" |
           NYC_Dogs == "2004-10-22T00:00:00.000" |
           NYC_Dogs == "2005-08-19T00:00:00.000" |
           NYC_Dogs == "2008-10-22T00:00:00.000" |
           NYC_Dogs == "3010271" |
           NYC_Dogs == "40804"] <- "NAME NOT PROVIDED"
NYC_Dogs[NYC_Dogs == "A."] <- "A"
NYC_Dogs[NYC_Dogs == "AJ"|
           NYC_Dogs == "A.J"] <- "A.J."
str_to_title(NYC_Dogs$Name)
glimpse(NYC_Dogs)
#####NYC DOG BREEDS#####
NYC_Dogs$Breed

#fix dog breeds - change cross to mix;
#NYC_Dogs[NYC_Dogs == "Crossbreed"] <- "Mix" Doesn't work how I thought
NYC_Dogs[NYC_Dogs == "Afghan Hound Crossbreed"] <- "Afghan Hound Mix"
NYC_Dogs[NYC_Dogs == "Akita Crossbreed"] <- "Akita Mix"
NYC_Dogs[NYC_Dogs == "Schnauzer, Miniature"] <- "Miniature Schnauzer"
NYC_Dogs[NYC_Dogs == "American Pit Bull Terrier/Pit Bull"] <- "American Pitbull Terrier"
NYC_Dogs[NYC_Dogs == "American Pit Bull Mix / Pit Bull Mix"] <- "American Pitbull Terrier Mix"
NYC_Dogs[NYC_Dogs == "Unknown"] <- "NA"
NYC_Dogs[NYC_Dogs == "Labrador Retriever Crossbreed"] <- "Labrador Retriever Mix"
NYC_Dogs[NYC_Dogs == "Dachshund Smooth Coat" | 
           NYC_Dogs == "Dachshund Smooth Coat Minature" |
           NYC_Dogs == "Dachshund, Long Haired"  |
           NYC_Dogs == "Dachshund Smooth Coat Miniature" |
           NYC_Dogs == "Dachshund, Long Haired Miniature" |
           NYC_Dogs == "Dachshund, Wirehaired, Miniature" ] <- "Dachshund"  
NYC_Dogs[NYC_Dogs == "Bull Dog, French"] <- "French Bulldog"
NYC_Dogs[NYC_Dogs == "Bull Dog, English"] <- "English Bulldog"
NYC_Dogs[NYC_Dogs == "Poodle, Standard" | 
           NYC_Dogs == "Poodle, Miniature" | 
           NYC_Dogs =="Poodle, Toy"] <- "Poodle"
NYC_Dogs[NYC_Dogs == "Collie, Border"] <- "Border Collie"
NYC_Dogs[NYC_Dogs == "Welsh Corgi, Pembroke" |
           NYC_Dogs == "Welsh Corgi, Cardigan" ] <- "Corgi" 
NYC_Dogs[NYC_Dogs == "German Shepherd dog" |
           NYC_Dogs == "German Shepherd Dog"] <- "German Shepherd"
NYC_Dogs[NYC_Dogs == "Beagle Crossbreed"] <- "Beagle"
NYC_Dogs[NYC_Dogs == "German Shepherd Crossbreed"] <- "German Shepherd Mix"
NYC_Dogs[NYC_Dogs == "Collie, Rough Coat"|
           NYC_Dogs == "Collie, Smooth Coat"] <- "Collie"
NYC_Dogs[NYC_Dogs =="Pointer, German Shorthaired"] <- "German Shorthaired Pointer"
NYC_Dogs[NYC_Dogs == "Mastiff, Old English"] <- "Old English Mastiff"
NYC_Dogs[NYC_Dogs == "Mastiff, Bull"] <- "Bull Mastiff"  
NYC_Dogs[NYC_Dogs == "Shar-Pei, Chinese"] <- "Chinese Shar-Pei"  
NYC_Dogs[NYC_Dogs == "Schnauzer, Standard"] <- "Standard Schnauzer"
NYC_Dogs[NYC_Dogs == "Jack Russell Terrier Crossbreed"] <- "Jack Russell Mix" 
#	Terrier Crossbreed
#Collie Crossbreed
#Yorkshire Terrier Crossbreed
#Chihuahua Crossbreed

#####How many dogs are "Pitbulls"?#####
#6.26% of dogs registered in NYC are "Pitbulls"

Am_pb_tr <- NYC_Dogs %>%
  filter(Breed == 'American Pitbull Terrier'
         |Breed == 'American Pitbull Terrier Mix'
         |Breed == 'American Bully'
         |Breed == 'American Staffordshire Terrier'
         |Breed == 'American Staffordshire Terrier Mix'
         |Breed == 'Dogo Argentino'
         |Breed == 'Staffordshire Bull Terrier')

percentage_pb <- Am_pb_tr %>%
  summarize(percentage = count(Am_pb_tr)/count(NYC_Dogs)) %>%
  mutate(total_pb = percentage * 100)

#####Write CSV File#####
write.csv(NYC_Dogs,'nycdogdata.csv', 
          row.names = TRUE,
          quote = FALSE)
nycdogdata <- read_csv("data/nycdogdata.csv")

