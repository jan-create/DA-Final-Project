library(tidyverse)
library(readr)
Dog_Bites <- read_csv("~/Desktop/DA Final Project/data/DOHMH_Dog_Bite_Data-2.csv")
#simplify column names
Dog_Bites <- Dog_Bites %>% 
  rename(
    `Altered` = `SpayNeuter`,
    `Date` = `DateOfBite`,
    `Zipcode` = `ZipCode`,
  )


#####AGE#####
#change <1 year to decimal points
#Over and equal to 1 year
Dog_Bites[Dog_Bites ==  "14M" ] <- "1"
Dog_Bites[Dog_Bites == "18M"|Dog_Bites == "1 1/2 YRS"|Dog_Bites == "1 & 8"] <- "2"
Dog_Bites[Dog_Bites == "3Y"] <- "3"
Dog_Bites[Dog_Bites == "4Y"] <- "4"
Dog_Bites[Dog_Bites == "5Y"|Dog_Bites == "5YR"] <- "5"
Dog_Bites[Dog_Bites == "6Y"|Dog_Bites == "6y"|Dog_Bites == "6 YRS"|Dog_Bites == "6 & 4"] <- "6"
Dog_Bites[Dog_Bites == "7Y"|Dog_Bites =="6.5 YRS"|Dog_Bites=="6.5"] <- "7"
Dog_Bites[Dog_Bites == "8Y"|Dog_Bites == "8 YRS"] <- "8"
Dog_Bites[Dog_Bites == "9Y"|Dog_Bites == "9 YRS"|Dog_Bites == "8YRS & 8 M"] <- "9"

#Under 1 year
Dog_Bites[Dog_Bites == "2MTHS"| Dog_Bites == "2M"| Dog_Bites == "2 MTHS"|Dog_Bites =="2 M"|Dog_Bites == "2m"|Dog_Bites == "9WK"|Dog_Bites == "8WKS"|Dog_Bites == "8W"|Dog_Bites == "7W"] <- "0.167"
Dog_Bites[Dog_Bites == "3MTHS"| Dog_Bites == "3M"| Dog_Bites == "3 MTHS"|Dog_Bites =="3 M"|Dog_Bites == "2-3M"|Dog_Bites == "13 WK"] <- "0.271"
Dog_Bites[Dog_Bites == "4MTHS"| Dog_Bites == "4M"| Dog_Bites == "4 MTHS"|Dog_Bites =="4 M"] <- "0.333"
Dog_Bites[Dog_Bites == "5MTHS"| Dog_Bites == "5M"| Dog_Bites == "5 MTHS"|Dog_Bites =="5 M"|Dog_Bites == "5m"|Dog_Bites == "18w"] <- "0.417"
Dog_Bites[Dog_Bites == "6MTHS"| Dog_Bites == "6M"| Dog_Bites == "6 MTHS"|Dog_Bites =="6 M"|Dog_Bites == "6MTH"] <- "0.5"
Dog_Bites[Dog_Bites == "7MTHS"| Dog_Bites == "7M"| Dog_Bites == "7 MTHS"|Dog_Bites =="7 M"|Dog_Bites=="7m"] <- "0.583"
Dog_Bites[Dog_Bites == "9MTHS"| Dog_Bites == "9M"| Dog_Bites == "9 MTHS"|Dog_Bites =="9 M"] <- "0.75"
Dog_Bites[Dog_Bites == "8MTHS"| Dog_Bites == "8M"| Dog_Bites == "8 MTHS"|Dog_Bites =="8 M"|Dog_Bites =="8 MOS"] <- "0.667"
Dog_Bites[Dog_Bites == "10MTHS"| Dog_Bites == "10M"| Dog_Bites == "10 MTHS"|Dog_Bites =="10 M"|Dog_Bites == "10 MTHS &"] <- "0.833"
Dog_Bites[Dog_Bites == "11MTHS"| Dog_Bites == "11M"| Dog_Bites == "11 MTHS"|Dog_Bites =="11 M"|Dog_Bites == "11m"] <- "0.917"
Dog_Bites[Dog_Bites == "2018-02-03T00:00:00.000"|Dog_Bites == "1/12M" |Dog_Bites == "10 & 9"] <- "NA"

Dog_Bites$Age <- as.double(Dog_Bites$Age)


glimpse(Dog_Bites)

#####Cleaning Up Dog Breed Data#####
Dog_Bites$Breed
#Dog_Bites[Dog_Bites == "Crossbreed"] <- "Mix"
Dog_Bites[Dog_Bites == "American Pit Bull Terrier/Pit Bull" | 
            Dog_Bites == "Pit Bull" | 
            Dog_Bites == "Blue Nosed Pit Bull"|
            Dog_Bites == "BLUE NOSED PIT BULL"|
            Dog_Bites == "RED-NOSE PIT BULL"] <- "American Pitbull Terrier"
Dog_Bites[Dog_Bites == "American Pit Bull Mix / Pit Bull Mix" | 
            Dog_Bites == "Pit Bull Mixed" |
            Dog_Bites == "PIT BULL/GOLDEN RETRIVE X"|
            Dog_Bites == "PIT BULL MIXED"|
            Dog_Bites == "STAFFORDSHIRE TERRIER/PITBULL MIX"|
            Dog_Bites == "PITBULL/ROTTWEILER"|
            Dog_Bites == "LABRADOR RETR/PIT BULL X"] <- "American Pitbull Terrier Mix"
Dog_Bites[Dog_Bites == "Poodle, Standard" | 
            Dog_Bites == "Poodle, Miniature" |
            Dog_Bites == "Poodle, Toy" |
            Dog_Bites == "POODLE"] <- "Poodle"
Dog_Bites[Dog_Bites == "HUSKY"] <- "Husky"
Dog_Bites[Dog_Bites == "MORKIE"] <- "Morkie"
Dog_Bites[Dog_Bites == "Mixed/Other"|
            Dog_Bites == "MIXED"|
            Dog_Bites == "MUTT" ] <- "NA"
Dog_Bites[Dog_Bites == "Jindo Dog,"] <- "Korean Jindo"
Dog_Bites[Dog_Bites == "Jack Russ"] <- "Jack Russell Terrier"
Dog_Bites[Dog_Bites == "Beagle Crossbreed"] <- "Beagle Mix"
Dog_Bites[Dog_Bites == "COCKER/CORGI X"] <- "Corgi Mix"
Dog_Bites[Dog_Bites == "Bull Dog, English"] <- "English Bulldog"
Dog_Bites[Dog_Bites == "Mastiff, Bull"] <- "Bull Mastiff"
Dog_Bites[Dog_Bites == "Mastiff, Tibetan"] <- "Tibetan Mastiff"
Dog_Bites[Dog_Bites == "Mastiff, Old English"] <- "Old English Mastiff"
Dog_Bites[Dog_Bites == "TERRIER/ROTTWEILER X"] <- "Rottweiler Mix" 
Dog_Bites[Dog_Bites == "JACK RUSS TERR X- CHIHUAHUA"] <- "Jack Russell Terrier Mix"        
Dog_Bites[Dog_Bites == "DOGO ARGENTINO X" ] <- "Dogo Argentino Mix"  
Dog_Bites[Dog_Bites == "Pointer, German Shorthaired"] <- "German Shorthaired Pointer"
Dog_Bites[Dog_Bites == "CORGI"] <- "Corgi" 
Dog_Bites[Dog_Bites == "SHIH TZU X"|Dog_Bites=="SHIH TZU/MALTESE X" ] <- "Shih Tzu Mix"
Dog_Bites[Dog_Bites == "Pharoh Hound"|
            Dog_Bites == "Pharoh hound" ] <- "Pharoah Hound"  
Dog_Bites[Dog_Bites == "SHEPARD X"] <- "NA"
Dog_Bites[Dog_Bites == "Dachshund Smooth Coat" | 
            Dog_Bites =="Dachshund Smooth Coat Minature" |
            Dog_Bites =="Dachshund, Long Haired"|
            Dog_Bites =="Dachshund Smooth Coat Miniature"] <- "Dachshund"
Dog_Bites[Dog_Bites == "Chihuahua Crossbreed"] <- "Chihuahua Mix"
Dog_Bites[Dog_Bites == "German Shepherd Crossbreed"] <- "German Shepherd Mix"
Dog_Bites[Dog_Bites == "Cocker Spaniel Crossbreed" ] <- "Cocker Spaniel Mix"
Dog_Bites[Dog_Bites == "BULL DOG X"] <- "Bulldog Mix"
Dog_Bites[Dog_Bites == "Bull Dog, French"] <- "French Bulldog"
Dog_Bites[Dog_Bites == "BLUE HEELER X"|Dog_Bites == "AUSTRALIAN CATTLE BLUE HEELER X"] <- "Blue Heeler Mix"
Dog_Bites[Dog_Bites == "RAT TERRIER"] <- "Rat Terrier"
Dog_Bites[Dog_Bites == "Yorkshire Terrier Crossbreed"] <- "Yorkshire Terrier Mix"
Dog_Bites[Dog_Bites == "Collie, Border"] <- "Border Collie"
Dog_Bites[Dog_Bites == "Labrador Retriever Crossbreed"|Dog_Bites == "LAB/COLLIE X"|Dog_Bites == "AMER STAFF/LAB X"|Dog_Bites == "BEAGLE/LAB X"] <- "Lab Mix"
"LHASA APSO/MALTESE" 
"MALTESE X"   
"MASTIFF"  
"Schnauzer, Standard"  
"SHEPHERD MIX"
"JACK RUSSELL/CHIHUAHUA X"  
"SHILOH SHEPHERD"

"BELIGUM/GERMAN SHEP X"              
  
"Bull dog" 

"BELGIAN SHEPHERD" 
"JACK RUSSELL X" 
"PERSA CANARIO"  
#####HOW MANY DOG BITES ARE BY DOGS DEEMED "PITBULLS"#####
Am_pb_tr_bites <- Dog_Bites %>%
  filter(Breed == 'American Pitbull Terrier'
         |Breed == 'American Pitbull Terrier Mix'
         |Breed == 'American Bully'
         |Breed == 'American Staffordshire Terrier'
         |Breed == 'American Staffordshire Terrier Mix'
         |Breed == 'Dogo Argentino'
         |Breed == 'Staffordshire Bull Terrier')
"Dogo Argentino Mix"


dim(Dog_Bites) #10044 observations
write.csv(Dog_Bites,'bites.csv', 
          row.names = TRUE,
          quote = FALSE)
nycbites <- read_csv("data/bites.csv")
