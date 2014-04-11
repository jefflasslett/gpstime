module Options ( Options( .. ), defaultOptions, optionDescriptions ) where

import System.Console.GetOpt
import System.Exit

version :: String
version = "0.2.0.0"

{- These optional arguments could easily be Strings.  GetOpt handles their
 - optional nature just fine.  I like having them as Maybe values though.
 -}
data Options = Options { optHost :: String
                       , optPort :: String
                       }

-- The defaults for the parameters
defaultOptions :: Options
defaultOptions = Options { optHost = "localhost"
                         , optPort = "2947"
                         }
{-
 - Describes the valid options for the app. An array of 
 - OptDescr a, where 'a' is (Option -> IO Option).
 -
 - Each element of the array has four components that describe a single option:
 -
 - The arguments to Option (the OptDescr constructor) are:
 -
 -  * list of short option characters
 -  * list of long option strings (without "--")
 -  * argument descriptor
 -  * explanation of option for user
 -
 - Option [Char] [String] (ArgDescr a) String
 -
 - The ArgDescr (argument descriptor) is interesting in this case.
 - Note that the ReqArg and OptArg constructors take functions as
 - arguments: (String -> a) and (Maybe String -> a).
 - 
 - Here is the ArgDescr date type:
 -
      data ArgDescr a
        = NoArg                   a         -- no argument expected
        | ReqArg (String       -> a) String -- option requires argument
        | OptArg (Maybe String -> a) String -- optional argument
 -
 - For example, look at the ReqArg constructor of ArgDescr:
 -
        ReqArg (String -> a) String
 -
 - Recall that in this example 'a' is of type (Options -> IO Options).
 -
 - Substituting (Options -> IO Options) for 'a' in (String -> a)
 - we get ...
 -
    String -> (Options -> IO Options ) which is just
    String -> Options -> IO Option
 -
 - Now the types of the lambdas in the List of OptDescr below 
 - should make sense.
 -}
optionDescriptions :: [ OptDescr ( Options ->IO Options ) ]
optionDescriptions =
  [ Option "h" [ "host" ]
      ( ReqArg ( \arg opt -> return $ opt { optHost = arg } ) "<host>" )
      "The GPSd host IP"

  , Option "p" [ "port" ]
      ( ReqArg ( \arg opt -> return opt { optPort = arg } ) "<port>" )
      "The port GPSd is listening on"

  , Option "v" [ "version" ]
      ( NoArg ( \_ -> putStrLn ( "This is gpstime v" ++ version ) >>  exitSuccess ) )
      "Print version information"

  , Option "h" [ "help" ]
      ( NoArg ( \_ -> putStrLn ( usageInfo "gpstime [options]" optionDescriptions ) >>  exitSuccess ) )
      "Print this usage information"
  ]

