{-
Take task 4.3 (You can use the example solution) and make the following changes to it:

-Use newtype instead of data wherever you can. (Easiest is probably to check from the dates.hs example file how to use newtype.)

-Change the definition of Phone so that country code and phone type are optional using Maybe.

-Change the Show instance of Phone so that it does not show country code or phone type if they are Nothing.

-Make the readPhone function accept empty strings for phone type and country code. If they are empty make them Nothing.
-}


import Data.Char

-- Note. This could contain all the country codes, but since need for ALL country codes isn't specified, it doesn't. 
legalCountryCodes = [358, 123, 81, 1, 1345]

readPhone :: String -> String -> String -> Phone
readPhone str1 str2 str3 =
    let phoneType = phoneStrToEnum str1
        countryCode = parseStrToCountryCode str2
        phoneNum = makePhoneNo (read str3 :: Int)
        in Phone {phoneType =  phoneType, countryCode = countryCode, phoneNo = phoneNum}
    


phoneStrToEnum :: String -> Maybe PhoneType
phoneStrToEnum "" = Nothing
phoneStrToEnum str 
        | lowerCaseStr == "worklandline" = Just WorkLandLine
        | lowerCaseStr == "privatemobile" = Just PrivateMobile
        | lowerCaseStr == "workmobile" = Just WorkMobile
        | lowerCaseStr == "other" = Just Other
        | otherwise = error "Invalid phonetype!"
        where
            lowerCaseStr = [toLower c | c <- str] 


parseStrToCountryCode :: String -> Maybe CountryCode
parseStrToCountryCode "" = Nothing
parseStrToCountryCode ('0':xs) = parseStrToCountryCode xs
parseStrToCountryCode ('+':xs) = parseStrToCountryCode xs
parseStrToCountryCode str = if (read str ::Int) `elem` legalCountryCodes 
    then Just $ makeCountryCode (read str :: Int)
    else error "Not an allowed country code!"


data PhoneType =  WorkLandLine | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

newtype CountryCode = CountryCode Int deriving (Eq)
instance Show CountryCode where show (CountryCode cc) = "+" ++ show cc

newtype PhoneNo = PhoneNo Int  deriving (Eq)
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
    phoneType :: Maybe PhoneType,
    countryCode :: Maybe CountryCode,
    phoneNo :: PhoneNo
} deriving (Eq)
instance Show Phone where 
    show (Phone (Just kind) (Just code) num) = show code ++ " " ++ show num ++ " " ++ "(" ++ show kind ++ ")"
    show (Phone Nothing (Just code) num) = show code ++ " " ++ show num ++ " " 
    show (Phone (Just kind) Nothing num) = show num ++ " " ++ "(" ++ show kind ++ ")"
    show (Phone Nothing Nothing num) = show num

-- -Change the Show instance of Phone so that it does not show country code or phone type if they are Nothing.


makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone 
makePhone phoneType countryCode phoneNo = Phone {phoneType = Just phoneType, countryCode = Just countryCode, phoneNo = phoneNo }
