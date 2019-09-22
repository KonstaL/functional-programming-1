{-
Define the PhoneType as in Task 4.1.

Now, insted of using type synonyms, define data types CountryCode and PhoneNo so that both of them have a value constructor that takes an integer.
Derive instances for Eq for them and make Show instances for them so that:
CountryCode: print '+' in front of the number.
PhoneNo: print only the number.
Make a function for both of them that takes and Integer and throws an error if the integer is negative otherwise it creates the value.

Then again, using the record syntax, define Phone type for phone numbers that has only one value constructor with fields
phoneType :: PhoneType,
countryCode :: CountryCode, (This time a type of its own)
and phoneNo :: PhoneNo. (This time a type of its own)

Derive an instance for Eq for it, but for Show make it "pretty-print" the infromation in this form:
<country code><space><phone number><space><phone type in parenthesis>
e.g. +358 123456789 (WorkLandline)

Make a function of type
:: PhoneType
-> CountryCode (This time a type of its own)
-> PhoneNo (This time a type of its own)
-> Phone
but this time do not worry about the values of CountryCode and PhoneNo, since if they are created with the functions you made before they are already correct.

Note: Another option would be to use newtype keyword instead of data keyword for CountryCode and PhoneNo, but we will get to it later.
-}

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


