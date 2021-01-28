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
  )

#change dog names from UPPERCASE to Title Case
Dog_Bites$Breed<- str_to_title(Dog_Bites$Breed)
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
#Pitbull
Dog_Bites[Dog_Bites == "American Pit Bull Terrier/Pit Bull" | 
            Dog_Bites == "Pit Bull" | 
            Dog_Bites == "Blue Nosed Pit Bull"|
            Dog_Bites == "Red-Nose Pit Bull"] <- "American Pitbull Terrier"
#Pitbull Mix
Dog_Bites[Dog_Bites == "American Pit Bull Mix / Pit Bull Mix" | 
            Dog_Bites == "Pit Bull Mixed" |
            Dog_Bites == "Pit Bull/Golden Retrive X"|
            Dog_Bites == "Pit Bull Mixed"|
            Dog_Bites == "STAFFORDSHIRE TERRIER/PITBULL MIX"|
            Dog_Bites == "PITBULL/ROTTWEILER"|
            Dog_Bites == "LABRADOR RETR/PIT BULL X"|
            Dog_Bites == "PITBULL/LAB RETRIEVER"|
            Dog_Bites == "PIT BULL MIX"] <- "American Pitbull Terrier Mix"
#Poodle
Dog_Bites[Dog_Bites == "Poodle, Standard" | 
            Dog_Bites == "Poodle, Miniature" |
            Dog_Bites == "Poodle, Toy"] <- "Poodle"

#Unknown/Unclear
Dog_Bites[Dog_Bites == "Mixed/Other"|
            Dog_Bites == "Mix"|
            Dog_Bites == "Mutt" | 
            Dog_Bites == "SHEPARD X"|
            Dog_Bites == "2 Dogs: Terr X & Doberman"] <- "NA"

Dog_Bites[Dog_Bites == "Jindo Dog,"] <- "Korean Jindo"

#Jack Russ
Dog_Bites[Dog_Bites == "Jack Russ"] <- "Jack Russell Terrier"
Dog_Bites[Dog_Bites == "JACK RUSS TERR X- CHIHUAHUA"|
            Dog_Bites == "JACK RUSSELL/CHIHUAHUA X" |
            Dog_Bites == "Jack Russell X"|
            Dog_Bites == "BORDER COLLIE/JACK RUSSELL"|
            Dog_Bites == "Beagle/Jack Russell"| 
            Dog_Bites == "Beagle/Jack Russ"|
            Dog_Bites == "Beagle/Jack Russell X"|
            Dog_Bites == "Jack Russ Mix"] <- "Jack Russell Terrier Mix"  
#Beagle
Dog_Bites[Dog_Bites == "Beagle Crossbreed"] <- "Beagle Mix"
Dog_Bites[Dog_Bites == "Cocker/Corgi X"] <- "Corgi Mix"
Dog_Bites[Dog_Bites == "Bull Dog, English"] <- "English Bulldog"
Dog_Bites[Dog_Bites == "Mastiff, Bull"] <- "Bull Mastiff"
Dog_Bites[Dog_Bites == "Mastiff, Tibetan"] <- "Tibetan Mastiff"
Dog_Bites[Dog_Bites == "Mastiff, Old English"] <- "Old English Mastiff"
Dog_Bites[Dog_Bites == "TERRIER/ROTTWEILER X"] <- "Rottweiler Mix" 
      
Dog_Bites[Dog_Bites == "Dogo Argentino X" ] <- "Dogo Argentino Mix"  
Dog_Bites[Dog_Bites == "Pointer, German Shorthaired"] <- "German Shorthaired Pointer"
Dog_Bites[Dog_Bites == "CORGI"] <- "Corgi" 
Dog_Bites[Dog_Bites == "Shih Tzu X"|Dog_Bites=="Shih Tzu/Maltese X"|Dog_Bites=="/Shih Tzu Mix"] <- "Shih Tzu Mix"
Dog_Bites[Dog_Bites == "Pharoh Hound"|Dog_Bites == "Pharoh hound" ] <- "Pharoah Hound"  

Dog_Bites[Dog_Bites == "Dachshund Smooth Coat" | 
            Dog_Bites =="Dachshund Smooth Coat Minature" |
            Dog_Bites =="Dachshund, Long Haired"|
            Dog_Bites =="Dachshund Smooth Coat Miniature"] <- "Dachshund"
Dog_Bites[Dog_Bites == "Chihuahua Crossbreed"] <- "Chihuahua Mix"
Dog_Bites[Dog_Bites == "German Shepherd Crossbreed"|
            Dog_Bites == "BELIGUM/GERMAN SHEP X" ] <- "German Shepherd Mix"
Dog_Bites[Dog_Bites == "Cocker Spaniel Crossbreed" ] <- "Cocker Spaniel Mix"
Dog_Bites[Dog_Bites == "BULL DOG X"] <- "Bulldog Mix"
Dog_Bites[Dog_Bites == "Bull Dog, French"] <- "French Bulldog"
Dog_Bites[Dog_Bites == "BLUE HEELER X"|Dog_Bites == "AUSTRALIAN CATTLE BLUE HEELER X"] <- "Blue Heeler Mix"
Dog_Bites[Dog_Bites == "Yorkshire Terrier Crossbreed"] <- "Yorkshire Terrier Mix"
Dog_Bites[Dog_Bites == "Collie, Border"] <- "Border Collie"
Dog_Bites[Dog_Bites == "Labrador Retriever Crossbreed"|Dog_Bites == "LAB/COLLIE X"|Dog_Bites == "AMER STAFF/LAB X"|Dog_Bites == "BEAGLE/LAB X"] <- "Lab Mix"
Dog_Bites[Dog_Bites == "LHASA APSO/MALTESE"|Dog_Bites=="MALTESE X"]<-"Maltese Mix"  
Dog_Bites[Dog_Bites == "Schnauzer, Standard"] <- "Standard Schnauzer"  
Dog_Bites[Dog_Bites == "Bull Dog"] <- "Bulldog"
Dog_Bites[Dog_Bites == "Fox Hound"] <- "Foxhound"
Dog_Bites[Dog_Bites == "HARRIER/BEAGLE"] <- "Beagle Mix"
Dog_Bites[Dog_Bites == "Harrier/Germ Shep"] <- "German Shepherd Mix"
Dog_Bites[Dog_Bites == "BOXER X W/ PIT BULL"|
            Dog_Bites == "BOXER/RHODESIAN RIDGEBACK X"|
            Dog_Bites == "BOXER/HOUND/PITBULL"] <- "Boxer Mix"
Dog_Bites[Dog_Bites == "CATAHOULA LEOPARD DOG"] <- "Catahoula Leaoard Hound"

#AUSTRALIAN CATTLE
#ITALIAN MASTIFF
#BEAGLE/JACK RUSS
#PARSON RUSSELLTERR
#UNKNOWN
#POINTER/BEAGLE
#GERMAN SHEPHERD/RIDGEBACK
#STAFFORDSHIRE TERR
#2 Dogs: Terr X & Doberman
#CHIHUAHUA/BOSTON TERR
#HUSKY/CATTLE DOG MIX
#HUSKY/CATTLE MIX
#BORDER COLLIE/JACK RUSSELL
#Chow Chow/Shepard X
#	Alaskan Husky/Labrador Retr
Dog_Bites[Dog_Bites == "American Bull Dog"] <- "American Bulldog"

#Write to CSV
write.csv(Dog_Bites,'bites.csv', 
          row.names = TRUE,
          quote = FALSE)
nycbites <- read_csv("data/bites.csv")
