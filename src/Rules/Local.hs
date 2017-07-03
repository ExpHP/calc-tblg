{-# LANGUAGE PackageImports #-}

module Rules.Local (
    localRules,
    ) where

import           "base" Control.Applicative
import           "base" Control.Monad.IO.Class
import           "text" Data.Text as Text(Text)
import qualified "text" Data.Text.IO as Text
import           "extra" Control.Monad.Extra(whenM)
import           "shake" Development.Shake.FilePath((</>))
import           "directory" System.Directory(doesPathExist,doesDirectoryExist,listDirectory,pathIsSymbolicLink)
import           "mtl" Control.Monad.Writer
import           "transformers" Control.Monad.Trans.Writer(WriterT(..))
import           "megaparsec" Text.Megaparsec
import           "megaparsec" Text.Megaparsec.Text

import           FunctorUtil
import           ShakeUtil hiding ((*>),doesDirectoryExist)

data LocalRule
    = CopyRule Pat Pat
    | LinkRule Pat Pat
    deriving (Eq, Ord, Show, Read)

type CollectLocalRulesImpl a = WriterT [LocalRule] IO a

-- | Walk the directory for "local-rules" files and add their rules
--   to the Shake dependency graph.
localRules :: App ()
localRules = do
    ruleData <- liftIO $ collectLocalRules "local-rules"
    mapM_ localRuleToRules ruleData

collectLocalRules :: FileString -> IO [LocalRule]
collectLocalRules fname = fmap snd . runWriterT $ f ""
  where
    f :: FileString -> CollectLocalRulesImpl ()
    f "comp" = pure () -- HACK to avoid searches in big directories that shouldn't contain any local rules
    f "proj" = pure ()
    f prefix = do
        let rulesPath = prefix </> fname
        whenM (liftIO $ doesPathExist rulesPath) $ do
            text <- liftIO $ Text.readFile rulesPath
            rules <- either fail pure $ parseRulesFile rulesPath text
            tell $ prefixLocalRule prefix <$> rules

        names <- liftIO . ffmap (prefix </>) $
                    listDirectory $ case prefix of "" -> "."
                                                   s  -> s

        -- We don't walk symlinks because then the same rules would be loaded under multiple prefixes,
        --  and rewrite rules would presumably cause these to conflict.
        subdirs <- liftIO $ filterM ((&&) <<$>> doesDirectoryExist
                                          <<*>> (ffmap not pathIsSymbolicLink))
                                    names

        mapM_ f subdirs

---------------------

prefixLocalRule :: Pat -> LocalRule -> LocalRule
prefixLocalRule prefix (CopyRule a b) = CopyRule (prefix </> a) (prefix </> b)
prefixLocalRule prefix (LinkRule a b) = LinkRule (prefix </> a) (prefix </> b)

localRuleToRules :: LocalRule -> App ()
localRuleToRules (CopyRule a b) = a `isCopiedFromFile` b
localRuleToRules (LinkRule a b) = a `isLinkedFromFile` b

---------------------

-- rules written in a massively dumbed down form of haskell syntax
parseRulesFile :: FileString -> Text -> Either String [LocalRule]
parseRulesFile fname = either (Left . parseErrorPretty) Right . parse rulesFile fname
  where
    rulesFile :: Parser [LocalRule]
    rulesFile = rule `sepEndBy` newlines1

    newlines1 :: Parser ()
    newlines1 = () <$ eol `sepBy1` skipInlineSpace
    skipInlineSpace :: Parser ()
    skipInlineSpace = skipMany (oneOf " \t")

    rule :: Parser LocalRule
    rule = do
        arg1 <- haskellString <* skipInlineSpace
        oper <- ruleOperator  <* skipInlineSpace
        arg2 <- haskellString
        pure $ arg1 `oper` arg2

    haskellString :: Parser Pat
    haskellString = char '"' *> many haskellStringChar <* char '"'

    identifier :: Parser String
    identifier = many (alphaNumChar <|> char '_')

    ruleOperator :: Parser (Pat -> Pat -> LocalRule)
    ruleOperator = char '`' *> fmap f identifier <* char '`'
      where
        f "isLinkedFromFile" = LinkRule
        f "isCopiedFromFile" = CopyRule
        f s = fail $ "Unrecognized rule operator: " ++ s

    haskellStringChar :: Parser Char
    haskellStringChar =   ( '"' <$ try (string "\\\""))
                      <|> ('\\' <$ try (string "\\\\"))
                      <|> noneOf "\""
