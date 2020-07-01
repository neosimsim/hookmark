module Options
  ( Options(..)
  , parseOptions
  )
where

import           Options.Applicative            ( Parser
                                                , auto
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , metavar
                                                , option
                                                , optional
                                                , progDesc
                                                , short
                                                , showDefault
                                                , strOption
                                                , value
                                                , (<**>)
                                                )

data Options =
  Options
    { hookmarkWebPort    :: Int
    , hookmarkWebBaseDir :: Maybe FilePath
    }

parseOptions :: IO Options
parseOptions = execParser $ info
  (optionsParser <**> helper)
  (fullDesc <> progDesc "Store, edit and search bookmarks." <> header
    "hookmark web — browser independent bookmarking"
  )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
          auto
          (short 'p' <> value 8080 <> showDefault <> metavar "PORT" <> help
            "Port to listen to."
          )
    <*> optional
          (strOption
            (  short 'b'
            <> metavar "BASEDIR"
            <> help
                 "Base directory to lookup bookmarks in. Defaults to $HOOKMARKHOME and then to $HOME/.hookmarks."
            )
          )
