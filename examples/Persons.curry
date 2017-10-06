-- A simple example to demonstrate Web User Interfaces and
-- the Curry->JavaScript translator.
-- This example implements a dynamic web page to edit persons
-- consisting of name, email, and date of birth.
-- If JavaScript is enabled, wrong dates in input fields are immediately
-- detected and shown as error in the web page.
--
-- The background about this example and the Curry->JavaScript translator
-- are described in:
-- 
-- M. Hanus:
-- Putting Declarative Programming into the Web: Translating Curry to JavaScript
-- Proceedings of the 9th ACM SIGPLAN International Conference on
-- rinciples and Practice of Declarative Programming (PPDP'07),
-- ACM Press, 2007, pp. 155-166

module Persons where

import HTML.Base
import WUIjs
import Time(validDate)

-- a form that just shows its argument:
resultForm :: a -> IO HtmlForm
resultForm v = return $ form "Result" [htxt ("Modified value: "++show v)]

-- a date WUI:
wDate = wTriple (wSelectInt [1..31]) (wSelectInt [1..12]) wInt
           `withConditionJS` correctDate
           `withError` "Illegal date:"

correctDate (d,m,y) = validDate y m d

-- an email WUI:
wEmail = wStringSize 20 `withConditionJS` correctEmail
                        `withError` "Invalid email address:"

correctEmail s = not (null (snd (break ('@'==) s)))

-- a person WUI:
wPerson = w4Tuple (wRequiredStringSize 12) (wRequiredStringSize 12) wEmail wDate

-- a WUI for persons:
main = mainWUI (wList wPerson) persons resultForm

persons = [("Bob","Carter","bob@carter.com",(3,10,1965))
          ,("Bill","Jones","billy@acm.org",(29,2,1982))
          ,("Joe","Smith","smith.org",(20,3,1978))
          ]

-- To install this example as an interactive web page:
--
-- First, ensure that curry2js is installed, e.g.:
--
-- > cpm update && cpm installbin curry2js
--
-- Then compile and install this script into your home www directory:
--
-- > curry makecgi -cpm -wuijs -o ~/public_html/persons.cgi Persons
