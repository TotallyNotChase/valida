module Main
    ( main
    ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

import Control.Applicative (Applicative (liftA2))
import Data.Char           (isDigit, isSpace)
import Data.Ix             (Ix (inRange))

import Valida (Validation, Validator (..), failureUnless', label, lengthWithin, minLengthOf, mustContain, optionally,
               valueWithin, (-?>), (</>))

data InputForm = InpForm
  { inpName  :: String
  , inpAge   :: Int
  , inpEmail :: String
  , inpPhone :: Maybe String
  } deriving (Eq, Show)

data FormErr
  = InvalidNameLength
  | InvalidAge
  | NoAtCharInMail
  | NoPeriodInMail
  | InvalidEmailLength
  | InvalidPhLen
  | IncorrectPhFormat
  deriving (Show)

neSingleton :: a -> NonEmpty a
neSingleton = (:|[])

-- | Validator for each field in the input form - built using 'ValidationRule' combinators.
inpFormValidator :: Validator (NonEmpty FormErr) InputForm InputForm
inpFormValidator = InpForm
    -- Name should be between 1 and 20 characters long
    <$> inpName -?> lengthWithin (1, 20) InvalidNameLength
    -- Age should be between 18 and 120
    <*> inpAge -?> valueWithin (18, 120) InvalidAge
    -- Email should contain '@', and '.', and be atleast 5 characters long
    <*> inpEmail -?> (mustContain '@' NoAtCharInMail
        <> mustContain '.' NoPeriodInMail
        <> minLengthOf 5 InvalidEmailLength)
    -- Phone may not be provided, if it is - it should be 15 characters long, and correctly formatted
    <*> inpPhone -?> optionally
        ( lengthWithin (14, 15) InvalidPhLen
        <> label
            (neSingleton IncorrectPhFormat)
            -- Either Intl format or NA format
            (failureUnless' isCorrectPhIntl </> failureUnless' isCorrectPhNA)
        )
  where
    -- | Format: \+[0-9] [2-9][0-9 ]+
    isCorrectPhIntl ('+':c:' ':ac:rest) = isDigit c && inRange ('2', '9') ac && all (liftA2 (||) isSpace isDigit) rest
    isCorrectPhIntl _                   = False
    -- | Format: \([2-9][0-9]{2}\) [0-9 ]+
    isCorrectPhNA ('(':ac:c:c':')':rest) = inRange ('2', '9') ac && all (liftA2 (||) isSpace isDigit) (c:c':rest)
    isCorrectPhNA _                      = False

---------------------------------------------------------------------
-- Examples usage
---------------------------------------------------------------------

-- Failure (InvalidNameLength :| [])
emptyName :: Validation (NonEmpty FormErr) InputForm
emptyName = runValidator inpFormValidator $ InpForm
    { inpName = ""
    , inpAge = 42
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Nothing
    }

-- Failure (InvalidNameLength :| [])
longName :: Validation (NonEmpty FormErr) InputForm
longName = runValidator inpFormValidator $ InpForm
    { inpName = "an incredibly long name"
    , inpAge = 42
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Nothing
    }

-- Failure (InvalidAge :| [])
underAge :: Validation (NonEmpty FormErr) InputForm
underAge = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 0
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Nothing
    }

-- Failure (InvalidAge :| [])
overAge :: Validation (NonEmpty FormErr) InputForm
overAge = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 150
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Nothing
    }

-- Failure (NoAtCharInMail :| [])
noAtEmail :: Validation (NonEmpty FormErr) InputForm
noAtEmail = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "john.doe"
    , inpPhone = Nothing
    }

-- Failure (NoPeriodInMail :| [])
noDotEmail :: Validation (NonEmpty FormErr) InputForm
noDotEmail = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "john@doe"
    , inpPhone = Nothing
    }

-- Failure (NoAtCharInMail :| [])
noAtDotEmail :: Validation (NonEmpty FormErr) InputForm
noAtDotEmail = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "johndoeemail"
    , inpPhone = Nothing
    }

-- Failure (InvalidEmailLength :| [])
smallEmail :: Validation (NonEmpty FormErr) InputForm
smallEmail = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "@."
    , inpPhone = Nothing
    }

-- Failure (InvalidPhLen :| [])
smallPh :: Validation (NonEmpty FormErr) InputForm
smallPh = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Just "+1 421"
    }

-- Failure (IncorrectPhFormat :| [])
badPh :: Validation (NonEmpty FormErr) InputForm
badPh = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Just " c abd 452 3670"
    }

-- Success (InpForm {inpName = "John Doe", inpAge = 42, inpEmail = "johndoe@e.mail", inpPhone = Nothing})
correctWithNoPh :: Validation (NonEmpty FormErr) InputForm
correctWithNoPh = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Nothing
    }

-- Success (InpForm {inpName = "John Doe", inpAge = 42, inpEmail = "johndoe@e.mail", inpPhone = Just "+1 421 123 4567"})
correctWithPh :: Validation (NonEmpty FormErr) InputForm
correctWithPh = runValidator inpFormValidator $ InpForm
    { inpName = "John Doe"
    , inpAge = 42
    , inpEmail = "johndoe@e.mail"
    , inpPhone = Just "+1 421 123 4567"
    }

main :: IO ()
main = do
    putStrLn $ "Empty name: " ++ show emptyName
    putStrLn $ "Long name: " ++ show longName
    putStrLn $ "Age too low: " ++ show underAge
    putStrLn $ "Age too high: " ++ show overAge
    putStrLn $ "No '@' in email: " ++ show noAtEmail
    putStrLn $ "No '.' in email: " ++ show noDotEmail
    putStrLn $ "No '@' or '.' in email: " ++ show noAtDotEmail
    putStrLn $ "Too small email: " ++ show smallEmail
    putStrLn $ "Too small phone number: " ++ show smallPh
    putStrLn $ "Incorrect phone number: " ++ show badPh
    putStrLn $ "All correct (no phone): " ++ show correctWithNoPh
    putStrLn $ "All correct (with phone): " ++ show correctWithPh
