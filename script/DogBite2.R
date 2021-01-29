library(tidyverse)
library(readr)
library(stringr)
Dog_Bites <- read_csv("~/Desktop/DA Final Project/data/DOHMH_Dog_Bite_Data-2.csv")
#simplify column names
Dog_Bites <- Dog_Bites %>% 
  rename(
    `Altered` = `SpayNeuter`,
    `Date` = `DateOfBite`,
    `Zipcode` = `ZipCode`,
    `Sex` = `Gender`
  )

#change dog names from UPPERCASE to Title Case
Dog_Bites$Breed<- str_to_title(Dog_Bites$Breed)

#remove duplicate rows
Dog_Bites = select(Dog_Bites, -1, -3)
Dog_Bites <- distinct(Dog_Bites)

library(tidyr)
library(lubridate)
Dog_Bites <- separate(Dog_Bites, Date, c("Month","Day","Year"))

Dog_Bites <-Dog_Bites[Dog_Bites$Year %in% c(2016), ]
#####AGE#####
#change <1 year to decimal points
#Over and equal to 1 year
Dog_Bites[Dog_Bites ==  "14M" |Dog_Bites == "1/12M"] <- "1"
Dog_Bites[Dog_Bites == "18M"|Dog_Bites == "1 1/2 YRS"|Dog_Bites == "1 & 8"] <- "2"
Dog_Bites[Dog_Bites == "3Y"] <- "3"
Dog_Bites[Dog_Bites == "4Y"] <- "4"
Dog_Bites[Dog_Bites == "5Y"|Dog_Bites == "5YR"] <- "5"
Dog_Bites[Dog_Bites == "6Y"|Dog_Bites == "6y"|Dog_Bites == "6 YRS"|Dog_Bites == "6 & 4"] <- "6"
Dog_Bites[Dog_Bites == "7Y"|Dog_Bites =="6.5 YRS"|Dog_Bites=="6.5"] <- "7"
Dog_Bites[Dog_Bites == "8Y"|Dog_Bites == "8 YRS"] <- "8"
Dog_Bites[Dog_Bites == "9Y"|Dog_Bites == "9 YRS"|Dog_Bites == "8YRS & 8 M"] <- "9"
Dog_Bites[Dog_Bites == "10 & 9"]<-"11"
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
Dog_Bites[Dog_Bites == "2018-02-03T00:00:00.000"] <- "NA"
#Change Age to number
Dog_Bites$Age <- as.double(Dog_Bites$Age)

#####Cleaning Up Dog Breed Data#####
#Akita
Dog_Bites[Dog_Bites == "Akita Crossbreed"|
            Dog_Bites == "Akita/Chow Chow"|
            Dog_Bites == "Chow Chow/Akita X"] <- "Akita Mix"
#-----------------------------------------------------------
Dog_Bites[Dog_Bites == "Cotton De Tulear"] <- "Coton De Tulear"
#Pitbull
Dog_Bites[Dog_Bites == "American Pit Bull Terrier/Pit Bull" | 
            Dog_Bites == "Pit Bull" | 
            Dog_Bites == "Blue Nosed Pit Bull"|
            Dog_Bites == "Red-Nose Pit Bull"|
            Dog_Bites == "Pit Bull"|
            Dog_Bites == "Pit Bull Terrier"] <- "American Pitbull Terrier"
#Pitbull Mix
Dog_Bites[Dog_Bites == "American Pit Bull Mix / Pit Bull Mix" |
            Dog_Bites == "American Pit Bull Mix/Pit Bull Mix" |
            Dog_Bites == "Pit Bull Mixed" |
            Dog_Bites == "Pit Bull/Golden Retrive X"|
            Dog_Bites == "Pit Bull Mixed"|
            Dog_Bites == "PITBULL/ROTTWEILER"|
            Dog_Bites == "PITBULL/LAB RETRIEVER"|
            Dog_Bites == "PIT BULL MIX"|
            Dog_Bites == "Pit Bull/LAB X"|
            Dog_Bites == "Pit Bull/Labrador Mix"|
            Dog_Bites == "Pit Bull Mix"|
            Dog_Bites == "Pit Bull / Staffordshire"|
            Dog_Bites == "American Pit Ter/Labrador Retr"|
            Dog_Bites == "American Pit Bull/Labrador"|
            Dog_Bites == "Pit Bull/ Siberian Husky"|
            Dog_Bites == "Pit Bull Terr/Alaskan Husky"|
            Dog_Bites == "Pit Bull X - Husky"|
            Dog_Bites == "Pit Bull Ter/Alaskan Husky"|
            Dog_Bites == "American Pitbull/ Labrador Retriever"|
            Dog_Bites == "Pitbull/Labrador Retriever"|
            Dog_Bites == "Pitbull/Labrador Mix"|
            Dog_Bites == "Pit Bull/Yorkie X"] <- "American Pitbull Terrier Mix"
#Staffy
Dog_Bites[Dog_Bites == "STAFFORDSHIRE TERRIER/PITBULL MIX"|
            Dog_Bites == "AMER STAFF/LAB X"|
            Dog_Bites == "Amer Staff/Am Pit Bull Terr"|
            Dog_Bites == "Amer Staff/Lab X"|
            Dog_Bites == "Amer Staffordshire X"|
            Dog_Bites == "American Bull / Stafford Terrier"|
            Dog_Bites == "American Staff Mix"|
            Dog_Bites == "American Staff/Akita"|
            Dog_Bites == "American Staff Ter X"|
            Dog_Bites == "American Stafford And Labrador Mix"|
            Dog_Bites == "American Stafford Mix"|
            Dog_Bites == "American Staffordshire / Pit Bull"|
            Dog_Bites == "American Staffordshire / Pit Bull Mix"|
            Dog_Bites == "American Staffordshire Terrier / Pit Bull"|
            Dog_Bites == "American Staffordshire X"]<- "American Staffordshire Terrier Mix"
Dog_Bites[Dog_Bites == "Staffordshire Terr"|
            Dog_Bites == "Staffordshire Terrier"] <- "American Staffordshire Terrier"
#####Husky#####
Dog_Bites[Dog_Bites == "Alaska Husky"|
            Dog_Bites == "Alaskan Husky"] <- "Alaskan Husky"
Dog_Bites[Dog_Bites == "Alaskan Husky/Labrador Retr"|
            Dog_Bites == "Alaskan Husky Mix"|
            Dog_Bites == "HUSKY/CATTLE DOG MIX"|
            Dog_Bites == "HUSKY/CATTLE MIX"|
            Dog_Bites == "Husky/Lab X"|
            Dog_Bites== "Husky -X"|
            Dog_Bites == "Husky/Cattle Mix"|
            Dog_Bites == "Husky/Cattle Dog Mix"|
            Dog_Bites == "Husky X"|
            Dog_Bites == "Husky/ Shiba Inu X"|
            Dog_Bites == "Husky Labrador"] <- "Husky Mix"

Dog_Bites[Dog_Bites == "Alaskan Malmute"] <- "Alaskan Malamute"
#Poodle
Dog_Bites[Dog_Bites == "Poodle, Standard" | 
            Dog_Bites == "Poodle, Miniature" |
            Dog_Bites == "Poodle, Toy"] <- "Poodle"
Dog_Bites[Dog_Bites == "Poodle X"] <- "Poodle Mix"

#Unknown/Unclear = Unknown
Dog_Bites[Dog_Bites == "Mixed/Other"|
            Dog_Bites == "Mix"|
            Dog_Bites == "Mutt" | 
            Dog_Bites == "SHEPARD X"|
            Dog_Bites == "2 Dogs: Terr X & Doberman"|
            Dog_Bites == "NA"|
            Dog_Bites == "Mixed Breed"|
            Dog_Bites == "Mixed"|
            Dog_Bites == "Alpaca"] <- "Unknown"

Dog_Bites[Dog_Bites == "Jindo Dog,"] <- "Korean Jindo"
#Jack Russ
Dog_Bites[Dog_Bites == "Jack Russ"] <- "Jack Russell Terrier"
Dog_Bites[Dog_Bites == "JACK RUSS TERR X- CHIHUAHUA"|
            Dog_Bites == "JACK RUSSELL/CHIHUAHUA X" |
            Dog_Bites == "Jack Russell X"|
            Dog_Bites == "Jack Russ Mix"] <- "Jack Russell Terrier Mix"  
#Beagle
Dog_Bites[Dog_Bites == "Beagle Crossbreed"|
            Dog_Bites == "Beagle/Lab X"] <- "Beagle Mix"

Dog_Bites[Dog_Bites == "Cocker/Corgi X"] <- "Corgi Mix"
Dog_Bites[Dog_Bites == "Bull Dog, English"] <- "English Bulldog"
Dog_Bites[Dog_Bites == "Mastiff, Bull"] <- "Bull Mastiff"
Dog_Bites[Dog_Bites == "Mastiff, Tibetan"] <- "Tibetan Mastiff"
Dog_Bites[Dog_Bites == "Mastiff, Old English"] <- "Old English Mastiff"
Dog_Bites[Dog_Bites == "TERRIER/ROTTWEILER X"] <- "Rottweiler Mix" 
      
Dog_Bites[Dog_Bites == "Dogo Argentino X" ] <- "Dogo Argentino Mix"

Dog_Bites[Dog_Bites == "Pointer, German Shorthaired"] <- "German Shorthaired Pointer"

#####CORGI#####
Dog_Bites[Dog_Bites == "Welsh Corgi, Cardigan"|Dog_Bites == "Welsh Corgi, Pembroke"|Dog_Bites=="Welsh/Corgi"] <- "Corgi" 
Dog_Bites[Dog_Bites == "Corgi / Beagle Mix"|Dog_Bites=="Corgi/Chihuahua X"|Dog_Bites =="Corgi / Cattle Mix"] <- "Corgi Mix"


Dog_Bites[Dog_Bites == "Shih Tzu X"|Dog_Bites=="Shih Tzu/Maltese X"|Dog_Bites=="/Shih Tzu Mix"] <- "Shih Tzu Mix"
#Pharoah Hound
Dog_Bites[Dog_Bites == "Pharoh Hound"|Dog_Bites == "Pharoh hound" ] <- "Pharoah Hound"  

#Dachshund
Dog_Bites[Dog_Bites == "Dachshund Smooth Coat" | 
            Dog_Bites =="Dachshund Smooth Coat Minature" |
            Dog_Bites =="Dachshund, Long Haired"|
            Dog_Bites =="Dachshund Smooth Coat Miniature"|
            Dog_Bites =="Dachshund, Wirehaired, Miniature"] <- "Dachshund" 
Dog_Bites[Dog_Bites == "Dachshund X"] <- "Dachshund Mix"

#####Chihuahua####
Dog_Bites[Dog_Bites == "Chihuahua/Dachschund"|Dog_Bites == "Chihuahua Crossbreed"|Dog_Bites =="Chihuahua / Corgi Mix"] <- "Chihuahua Mix"
#####Shepherd#####
Dog_Bites[Dog_Bites == "German Shepherd Crossbreed"|
            Dog_Bites == "BELIGUM/GERMAN SHEP X" ] <- "German Shepherd Mix"
Dog_Bites[Dog_Bites == "Shepherd / Labrador Mix"] <- "Shepherd Mix"

#####Cocker Spaniel####
Dog_Bites[Dog_Bites == "Cocker Spaniel Crossbreed"|Dog_Bites=="Cocker/Corgi X"] <- "Cocker Spaniel Mix"
#####Bulldog#####
Dog_Bites[Dog_Bites == "BULL DOG X"|
            Dog_Bites == "Buldog Mix/ Lab Mix"] <- "Bulldog Mix"
Dog_Bites[Dog_Bites == "Bull Dog, French"] <- "French Bulldog"
Dog_Bites[Dog_Bites == "Bull Dog"] <- "Bulldog"

Dog_Bites[Dog_Bites == "BLUE HEELER X"|Dog_Bites == "AUSTRALIAN CATTLE BLUE HEELER X"] <- "Blue Heeler Mix"
#####Yorky#####
Dog_Bites[Dog_Bites == "Yorkie"|
            Dog_Bites == "Teacup Yorkshire"] <- "Yorkshire Terrier"

Dog_Bites[Dog_Bites == "Yorkshire Terrier Crossbreed"|
            Dog_Bites == "Yorkshire/Shih Tzu Mix"|
            Dog_Bites == "Yorkshire/ Tibetan Terrier"|
            Dog_Bites == "Yorkshire Ter/Shihtzu"|
            Dog_Bites == "Yorkshire / Shih Tzu Mix"|
            Dog_Bites == "Yorkshire / Jack Russ"|
            Dog_Bites == "Yorkie/Bichon"|
            Dog_Bites == "Yorkie Mix"|
            Dog_Bites == "Yorkie / Chihuahua Mix"] <- "Yorkshire Terrier Mix"

Dog_Bites[Dog_Bites == "Yorky-Poodle"|
            Dog_Bites == "Poodle Min/ Yorkshire Ter"|
            Dog_Bites == "Yorkie Poodle"|
            Dog_Bites == "Yorkie Poo"] <- "Yorkipoo"
#####Collie#####
Dog_Bites[Dog_Bites == "Collie, Border"] <- "Border Collie"

#####Lab#####
Dog_Bites[Dog_Bites == "Labrador"]<- "Labrador Retriever"
Dog_Bites[Dog_Bites == "Labrador Retriever Crossbreed"|
            Dog_Bites == "LAB/COLLIE X"|
            Dog_Bites == "LABRADOR RETR/PIT BULL X"|
            Dog_Bites == "Labrador Mix"|
            Dog_Bites == "Labrador/Staffordshire"|
            Dog_Bites == "Yellow Lab/Beagle X"|
            Dog_Bites == "Mix Lab"|
            Dog_Bites == "Labrador/Mix"|
            Dog_Bites == "Labrador / Great Dane Mix"|
            Dog_Bites == "Lab Retriever/ Hound"|
            Dog_Bites == "Labrador Retriever Mix"|
            Dog_Bites == "Retriever/Lab X"|
            Dog_Bites == "Labrador/Pitbull"|
            Dog_Bites == "Labrador / Chow Chow Mix"|
            Dog_Bites == "Lab Retriever/Germ Shep"|
            Dog_Bites == "Labrador/Pit Bull X"|
            Dog_Bites == "Labrador/ Pit Bull"|
            Dog_Bites == "Labrador Retr/Pit Bull X"|
            Dog_Bites == "Labrador Ret / American Eskimo Mix"|
            Dog_Bites == "Labrador / Pit Bull"|
            Dog_Bites == "Lab/Rat Terrier X"|
            Dog_Bites == "Lab/Pit Bull X"|
            Dog_Bites == "Lab/Coo Hound"|
            Dog_Bites == "Lab/Collie X"|
            Dog_Bites == "Lab/Chow Chow X"|
            Dog_Bites == "Lab/ Pitbull Mix"|
            Dog_Bites == "Lab. Ret./Hound Mix"|
            Dog_Bites == "Lab- X"]<- "Lab Mix"

Dog_Bites[Dog_Bites == "LHASA APSO/MALTESE"|Dog_Bites=="MALTESE X"]<-"Maltese Mix"  
Dog_Bites[Dog_Bites == "Schnauzer, Standard"] <- "Standard Schnauzer"  
Dog_Bites[Dog_Bites == "Bull Dog"] <- "Bulldog"
Dog_Bites[Dog_Bites == "Fox Hound"] <- "Foxhound"
Dog_Bites[Dog_Bites == "HARRIER/BEAGLE"|Dog_Bites == "BEAGLE/LAB X"] <- "Beagle Mix"
Dog_Bites[Dog_Bites == "Harrier/Germ Shep"] <- "German Shepherd Mix"

#Boxer
Dog_Bites[Dog_Bites == "BOXER X W/ PIT BULL"|
            Dog_Bites == "BOXER/RHODESIAN RIDGEBACK X"|
            Dog_Bites == "BOXER/HOUND/PITBULL"] <- "Boxer Mix"
Dog_Bites[Dog_Bites == "CATAHOULA LEOPARD DOG"] <- "Catahoula Leaoard Hound"
Dog_Bites[Dog_Bites == "American Bully (Pit Bull)"|
            Dog_Bites == "American Bull"] <- "American Bully"
#Cocker Spaniel / Poodle Mix
#American Bulldog/Great Pyrenees
#AUSTRALIAN CATTLE
#BEAGLE/JACK RUSS
#PARSON RUSSELLTERR
#UNKNOWN
#POINTER/BEAGLE
#GERMAN SHEPHERD/RIDGEBACK
#STAFFORDSHIRE TERR
#CHIHUAHUA/BOSTON TERR

#BORDER COLLIE/JACK RUSSELL
#Chow Chow/Shepard X
#American Eskimo / Husky Mix
#
Dog_Bites[Dog_Bites == "American Bull Dog"] <- "American Bulldog"
Dog_Bites[Dog_Bites == "Sheperd"] <- "Shepherd"
#Welsh Corgi, Pembroke
#Boxer X W/ Pit Bull
#Dog_Bites == "BORDER COLLIE/JACK RUSSELL"|
 # Dog_Bites == "Beagle/Jack Russell"| 
  #Dog_Bites == "Beagle/Jack Russ"|
  #Dog_Bites == "Beagle/Jack Russell X"|
######Goldens#####
Dog_Bites[Dog_Bites == "Mini Golden Doddle"|
            Dog_Bites == "Golden/Doodle"|
            Dog_Bites == "Golden Doddle"|
            Dog_Bites == "Golden Poodle"]<-"Golden Doodle"
Dog_Bites[Dog_Bites == "Golden Retriver Mix"|
            Dog_Bites == "Golden Retriever/ Rottweiler"|
            Dog_Bites == "Golden Retreiver X"|
            Dog_Bites == "Golden Retr/Lab X"|
            Dog_Bites == "Golden Labrador Mix"]<- "Golden Retriever Mix"
Dog_Bites[Dog_Bites == "Aust Kelpie/Am Pit Bull X"]<-"Australian Kelpie Mix"
#####Maltese#####
Dog_Bites[Dog_Bites == "Maltese X"|
            Dog_Bites == "Maltese Poodle"|
            Dog_Bites == "Maltese Poodle Mix"]<-"Maltese Mix"
Dog_Bites[Dog_Bites == "Maltese/Yorkshire Terrier"|
            Dog_Bites == "Maltese/Yorkie"]<-"Morkie"
#Write to CSV
write.csv(Dog_Bites,'bites4.csv', 
          row.names = TRUE,
          quote = FALSE)
nycbites <- read_csv("data/bites4.csv")
