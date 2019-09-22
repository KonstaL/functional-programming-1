{-
Implement a readPhone function (String -> String -> String -> Phone) as follows:

The function
a) reads the phone type from the first string.

b) reads the country code code from the second string in the following way:
1. if the code has a '+' or "00" in the front, remove them
2. check that the code exists in a predefined list of country codes (you may define this as just a list of strings or a list of integers in your program)
3. read an integer out of the remaining string
4. call the function(that checks that the integer is >= 0) you created in Task4.2 with the integer to create the value for CountryCode.

c) reads the phone number from the third string by reading it as an integer and then calling the function you created in Task4.2.

If the input is correct, create a telephone number. Else, call error to throw an exception. Note that the read function or your functions from Task4.2 may also generate an exception.

Note: In this exercise do not try to create a custom Read instance, because it is probably unnecessarily hard at this point.

Note: Make sure not to import your answer for Task4.2 in your answer for this task, since in peer-reviewing the file will not be present. (You will need to copy-paste)
-}

import Data.Char

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


