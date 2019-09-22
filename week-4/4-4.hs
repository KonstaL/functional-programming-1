{-
4. Use the types and functions you have created in the previous tasks in this task.

Make a record that contains simple phone book entry information:
Name
Phone

Suppose we have a list of phone book entries. Make functions to:

-Find a list of entries by a name. (All entries where the name is the given one)

-Add a new entry, given a string for the name, the three strings for the phone like in Task 4.3 and the list of phone book entries to add the new entry to.
If there already exists an entry with the given name and the given number(phoneNo field in Phone), then make no change.

Note: Make sure not to import your answer for Task4.3 or Task4.2 in your answer for this task, since in peer-reviewing the file will not be present. (You will need to copy-paste)
-}

import Data.Char

data PhoneBookEntry = PhoneBookEntry {
    name :: String,
    phone :: Phone
} deriving (Show)

getPhoneBookEntryByName :: [PhoneBookEntry] -> String -> [PhoneBookEntry]
getPhoneBookEntryByName  phonebook searchTerm = [entry | entry <- phonebook, name entry == searchTerm]

setPhoneBookEntry :: [PhoneBookEntry] -> String -> String -> String -> String -> [PhoneBookEntry]
setPhoneBookEntry phonebook owner phoneType countryCode newNum  
    | contained == True = []++phonebook
    | otherwise = PhoneBookEntry{name=owner, phone=(readPhone phoneType countryCode newNum) }:phonebook
    where
        contained = any (\x -> name x == owner && phoneNo (phone x) == makePhoneNo (read newNum :: Int)) phonebook


{-
Example commands
let luettelo = setPhoneBookEntry [] "jukka" "PrivateMoble" "+358" "582352358"
let luettelo2 = setPhoneBookEntry luettelo "toni" "PrivateMoble" "+358" "534652369"
let luettelo3 = setPhoneBookEntry luettelo2 "jukka" "WorkLandLine" "+358" "45895518"
let luettelo4 = setPhoneBookEntry luettelo3 "toni" "WorkLandLine" "+358" "534652369" -- this won't be added

getPhoneBookEntryByName luettelo4 "jukka"
getPhoneBookEntryByName luettelo4 "toni"
-}




-- 4.3
-- Note. This could contain all the country codes, but since need for ALL country codes isn't specified, it doesn't. 
legalCountryCodes = [358, 123, 81, 1, 1345]

readPhone :: String -> String -> String -> Phone
readPhone str1 str2 str3 =
    let phoneType = phoneStrToEnum str1
        countryCode = parseStrToCountryCode str2
        phoneNum = makePhoneNo (read str3 :: Int)
        in Phone {phoneType = phoneType, countryCode = countryCode, phoneNo = phoneNum}
    


phoneStrToEnum :: String -> PhoneType
phoneStrToEnum str 
        | lowerCaseStr == "worklandline" = WorkLandLine
        | lowerCaseStr == "privatemobile" = PrivateMobile
        | lowerCaseStr == "workmobile" = WorkMobile
        | otherwise = Other
        where
            lowerCaseStr = [toLower c | c <- str] 


parseStrToCountryCode :: String -> CountryCode
parseStrToCountryCode "" = error "Invalid country code!"
parseStrToCountryCode ('0':xs) = parseStrToCountryCode xs
parseStrToCountryCode ('+':xs) = parseStrToCountryCode xs
parseStrToCountryCode str = if (read str ::Int) `elem` legalCountryCodes 
    then makeCountryCode (read str :: Int)
    else error "Not an allowed country code!"


-- 4.2

data PhoneType =  WorkLandLine | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

data CountryCode = CountryCode Int deriving (Eq)
instance Show CountryCode where show (CountryCode cc) = "+" ++ show cc

data PhoneNo = PhoneNo Int  deriving (Eq)
instance Show PhoneNo where show (PhoneNo no) = show no


makeCountryCode :: Int -> CountryCode
makeCountryCode num  
    | num < 0 = error "Counry code can't be a negative integer!"
    | otherwise = CountryCode num 

makePhoneNo :: Int -> PhoneNo
makePhoneNo num  
        | num < 0 = error "Phone number can't be a negative integer!"
        | otherwise = PhoneNo num

data Phone = Phone {
    phoneType :: PhoneType,
    countryCode :: CountryCode,
    phoneNo :: PhoneNo
} deriving (Eq)
instance Show Phone where show (Phone kind code num) = show code ++ " " ++ show num ++ " " ++ "(" ++ show kind ++ ")"


makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone 
makePhone phoneType countryCode phoneNo = Phone {phoneType = phoneType, countryCode = countryCode, phoneNo = phoneNo }


