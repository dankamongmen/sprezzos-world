-- | Core parsing routines to parse various structures.
--
-- Note: various assumptions are made in this code, mainly revolving
-- around the fact that version numbers should never contain
-- metacharacters.  In the Debian archive, this is always a safe
-- assumption; if it becomes not a safe assumption, the various
-- debugging statements that aptitude prints will have to be reworked
-- to unambiguously print package and version information.
module Resolver.Parse(
                      Parser,
                      ParseState,
                      initialParseState,
                      versionWithoutTerminators,
                      version,
                      packageWithoutTerminators,
                      package,
                      promotion,
                      dep,
                      choice,
                      solution
                     )
where

import Control.Monad(unless, when)

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.Parsec hiding(choice)
import qualified Text.Parsec.ByteString()

import Resolver.Types

import qualified Data.ByteString.Char8 as BS

import Data.List(foldl')
import Data.Maybe(maybeToList)
import qualified Data.Map as Map
import qualified Data.Set as Set

type InternSet a = Map.Map a a
emptyInternSet = Map.empty
-- The two arguments should be identical, except that the second one
-- can be copied so that it shares less memory with other structures
-- (thus making it more suitable for interning).
doIntern :: (Ord a) => InternSet a -> a -> a -> (InternSet a, a)
doIntern s a1 a2 = case Map.lookup a1 s of
                     Just a' -> (s, a')
                     Nothing -> (Map.insert a2 a2 s, a2)

data ParseState = ParseState { internedStrings    :: InternSet BS.ByteString,
                               internedPackages   :: InternSet Package,
                               internedVersions   :: InternSet Version,
                               internedChoices    :: InternSet Choice,
                               internedDeps       :: InternSet Dep,
                               internedSolutions  :: InternSet Solution,
                               internedPromotions :: InternSet Promotion,
                               internedTiers      :: InternSet Tier }
                deriving(Eq, Ord, Show)

initialParseState :: ParseState
initialParseState = ParseState { internedStrings    = emptyInternSet,
                                 internedPackages   = emptyInternSet,
                                 internedVersions   = emptyInternSet,
                                 internedChoices    = emptyInternSet,
                                 internedDeps       = emptyInternSet,
                                 internedSolutions  = emptyInternSet,
                                 internedPromotions = emptyInternSet,
                                 internedTiers      = emptyInternSet }

type Parser = Parsec BS.ByteString ParseState

-- | Overloadable convenience typeclass for interning various things.
class Internable a where
    intern  :: a -> Parser a

maybeIntern :: Internable a => Maybe a -> Parser (Maybe a)
maybeIntern (Just a) = do a' <- intern a
                          return $ Just a'
maybeIntern Nothing  = return Nothing

instance Internable BS.ByteString where
    intern s      = do st <- getState
                       let strings           = internedStrings st
                           (strings', rval)  = doIntern strings s (BS.copy s)
                       putState st { internedStrings = strings' }
                       strings' `seq` rval `seq` return rval

instance Internable Tier where
    intern t      = do st <- getState
                       let tiers             = internedTiers st
                           (tiers', rval)    = doIntern tiers t t
                       putState st { internedTiers = tiers' }
                       tiers' `seq` rval `seq` return rval

instance Internable Package where
    intern p@(Package name) =
        do name'     <- intern name
           st        <- getState
           let packages          = internedPackages st
               (packages', rval) = doIntern packages p (Package name')
           putState st { internedPackages = packages' }
           packages' `seq` (pkgName rval) `seq` return rval

instance Internable Version where
    intern v@(Version pkg name) =
        do name'  <- intern name
           pkg'   <- intern pkg
           st     <- getState
           let versions          = internedVersions st
               (versions', rval) = doIntern versions v (Version pkg' name')
           putState st { internedVersions = versions' }
           versions' `seq` (pkgName $ verPkg rval) `seq` verName rval `seq` return rval

instance Internable Dep where
    intern d@(Dep source solvers isSoft) =
        do source'  <- intern source
           solvers' <- sequence $ map intern solvers
           st       <- getState
           let deps              = internedDeps st
               (deps', rval)     = doIntern deps d (Dep source' solvers' isSoft)
           putState st { internedDeps = deps' }
           deps' `seq` (pkgName $ verPkg $ depSource rval) `seq` (verName $ depSource rval)
                 `seq` (seqList $ depSolvers rval) `seq` return rval

instance Internable ChoiceDepInfo where
    intern depInfo =
        do let dep = depInfoDep depInfo
           dep' <- intern dep
           let rval = depInfo { depInfoDep = dep' }
           dep' `seq` rval `seq` return rval

instance Internable Choice where
    intern c@(InstallVersion ver depInfo) =
        do ver'     <- intern ver
           depInfo' <- maybeIntern depInfo
           st       <- getState
           let choices           = internedChoices st
               (choices', rval)  = doIntern choices c (InstallVersion ver' depInfo')
           putState st { internedChoices = choices' }
           choices' `seq` (verPkg $ choiceVer rval)
                    `seq` depInfo' `seq` return rval
    intern c@(BreakSoftDep dep) =
        do dep'     <- intern dep
           st       <- getState
           let choices           = internedChoices st
               (choices', rval)  = doIntern choices c (BreakSoftDep dep')
           putState st { internedChoices = choices' }
           choices' `seq` (depSource $ choiceDep rval)
                    `seq` return rval

instance Internable Promotion where
    intern p@(Promotion choices tier) =
        do choicesList' <- sequence $ map intern (Set.toList choices)
           tier'        <- intern tier
           st           <- getState
           let choices'            = Set.fromList choicesList'
               promotions          = internedPromotions st
               (promotions', rval) = doIntern promotions p (Promotion choices' tier')
           putState st { internedPromotions = promotions' }
           promotions' `seq` choices'
                       `seq` rval
                       `seq` return rval

instance Internable Solution where
    intern sol = do let choiceList            = Map.toList $ solChoices sol
                        forbiddenVersionList  = Set.toList $ solForbiddenVersions sol
                        brokenDepList         = Set.toList $ solBrokenDeps sol
                    choiceList'               <-
                        sequence [do k' <- intern k
                                     k' `seq` return (k', v)
                                  | (k, v) <- choiceList]
                    forbiddenVersionList'     <-
                        sequence (map intern forbiddenVersionList)
                    brokenDepList'            <-
                        sequence (map intern brokenDepList)
                    st                        <- getState
                    let solChoices'           = Map.fromList choiceList'
                        solForbiddenVersions' = Set.fromList forbiddenVersionList'
                        solBrokenDeps'        = Set.fromList brokenDepList'
                        sol'                  = solChoices' `seq`
                                                solForbiddenVersions' `seq`
                                                solBrokenDeps' `seq`
                                                sol { solChoices = solChoices',
                                                      solForbiddenVersions = solForbiddenVersions',
                                                      solBrokenDeps = solBrokenDeps' }
                        sols                  = internedSolutions st
                        (sols', rval)         = doIntern sols sol sol'
                    putState st { internedSolutions = sols' }
                    sols' `seq` rval
                          `seq` solChoices rval
                          `seq` solForbiddenVersions rval
                          `seq` solBrokenDeps rval
                          `seq` solScore rval
                          `seq` solTier rval
                          `seq` return rval

seqList :: [a] -> ()
seqList lst = foldl' (flip seq) () lst

lexeme :: Parser t -> Parser t
lexeme p = do rval <- p
              spaces
              return rval

symbol :: String -> Parser String
symbol = lexeme . string

leftParen = symbol "("
rightParen = symbol ")"
leftAngle = symbol "<"
rightAngle = symbol ">"
leftSquare = symbol "["
rightSquare = symbol "]"
leftBrace = symbol "{"
rightBrace = symbol "}"
comma = symbol ","
semicolon = symbol ";"

parens = between leftParen rightParen
angles = between leftAngle rightAngle
squares = between leftSquare rightSquare
braces = between leftBrace rightBrace


-- | Alternation that throws away the actual parse.
a <||> b = (a >> return ()) <|> (b >> return ())

readCharsTill :: [Parser t] -> Parser String
readCharsTill terminators =
    ((eof <||> Parsec.choice (map lookAhead terminators) <||> space) >> return []) <|>
    do c <- anyChar
       rest <- readCharsTill terminators
       return (c:rest)

-- | A version is any non-whitespace string not containing one of the
-- given terminators.
versionWithoutTerminators :: [Parser t] -> Parser Version
versionWithoutTerminators terminators =
    (lexeme $ do p <- packageWithoutTerminators terminators
                 v <- lexeme $ readCharsTill terminators
                 when (null v) (fail "Missing version number.")
                 let vn = BS.pack v
                 rval <- intern $ Version { verPkg = p, verName = vn }
                 p `seq` vn `seq` return rval) <?> "version"

version :: Parser Version
version = versionWithoutTerminators []


-- | A package is also any non-whitespace string.
packageWithoutTerminators :: [Parser t] -> Parser Package
packageWithoutTerminators terminators =
    (do w <- lexeme $ readCharsTill terminators
        when (null w) (fail "Missing package name.")
        p <- intern $ Package $ BS.pack w
        p `seq` return p) <?> "package name"

package :: Parser Package
package = packageWithoutTerminators []

-- | A dependency looks like this: version -> { ver ver ver }
depWithoutTerminators :: [Parser t] -> Parser Dep
depWithoutTerminators terminators =
    (do let term'     = [t >> return () | t <- terminators]
            arrow     = char '-' >> ((char '>' >> spaces >> return False) <|>
                                     (char 'S' >> char '>' >> spaces >> return True))
        source <- versionWithoutTerminators $ (try arrow >> return ()):term'
        soft   <- arrow
        leftBrace
        -- Throw away the input terminators and just use } instead,
        -- since we have a balanced group.  e.g., this means that in
        -- [a -> {b [UNINST]}] everything parses correctly.
        solvers <- lexeme $ manyTill (versionWithoutTerminators [rightBrace]) (try rightBrace)
        rval <- intern $ Dep { depSource = source, depSolvers = solvers, depIsSoft = soft }
        source `seq` seqList solvers `seq` soft `seq` return rval) <?> "dependency"

dep :: Parser Dep
dep = depWithoutTerminators []

-- | A choice is either Install(ver) or Break(dep); versions can have
-- [dep] after them (old-style from-dep-source), or <dep> (new-style
-- unscoped, or <"source:" dep> / <"scope:" dep> (new-style
-- from-dep-source or scoped).
choice :: Parser Choice
choice = (lexeme $ do installChoice <|> breakDepChoice) <?> "choice"
    where installChoice = do try (symbol "Install")
                             parens $ do
                               v <- versionWithoutTerminators [rightParen]
                               v `seq` ((do d <- squares dep
                                            let reason  = FromSource d
                                                depInfo = Just reason
                                            (d `seq` reason `seq` depInfo `seq`
                                             (intern InstallVersion { choiceVer = v,
                                                                      choiceVerDepInfo = depInfo }))) <|>
                                        (angles $ do k <- ((try (symbol "source:") >> return FromSource) <|>
                                                           (try (symbol "scope:") >> return Scoped) <|>
                                                           (return Unscoped))
                                                     d <- dep
                                                     let reason  = k d
                                                         depInfo = Just reason
                                                     d `seq` reason `seq` depInfo `seq`
                                                           (intern InstallVersion { choiceVer = v,
                                                                                    choiceVerDepInfo = depInfo })) <|>
                                        (intern InstallVersion { choiceVer = v,
                                                                 choiceVerDepInfo = Nothing }))
          breakDepChoice = do try (symbol "Break")
                              d <- parens dep
                              d `seq` intern $ BreakSoftDep { choiceDep = d }

-- | Hacky non-lexeme parser for integers.
--
-- Uses the Read instance of Integer to do the actual parsing.
integer :: Parser Integer
integer = (do sign <- optionMaybe $ oneOf "-+"
              digits <- many1 digit
              return (read (maybeToList sign ++ digits))) <?> "integer"

-- | A tier is eitiher an integer or a parenthesized list.
tier :: Parser Tier
tier = do vals <- parens (sepBy (lexeme integer) comma)
          (foldr (seq) vals vals) `seq` return $ Tier vals
       <|>
       do val <- lexeme integer
          val `seq` return $ Tier [val]

-- | A solution looks like this:
--
-- < pkg1 := ver1 , pkg2 := ver2 , ... > ;
-- <! unresolved-soft-dep1 , unresolved-soft-dep2 , ... !> ;
-- [ broken-dep1 , broken-dep2 , ... ] ;
-- !! forbidden-ver1 , forbidden-ver2 , ... !!;
-- T<tier>S<score>
solution :: Parser Solution
solution = do installVersionChoices <- solutionInstalls
              unresolvedSoftDeps <- solutionUnresolvedSoftDeps
              brokenDeps <- solutionBrokenDeps
              forbiddenVers <- solutionForbiddenVers
              char 'T'
              solTier <- tier
              char 'S'
              score <- integer
              let choices   = Map.fromList [(c, Nothing) | c <- installVersionChoices ++ unresolvedSoftDeps]
                  forbidden = Set.fromList forbiddenVers
                  broken    = Set.fromList brokenDeps
              choices `seq` forbidden `seq` broken `seq` score `seq` solTier `seq`
                      intern $ Solution { solChoices = choices,
                                          solForbiddenVersions = forbidden,
                                          solBrokenDeps = broken,
                                          solScore = score,
                                          solTier = solTier }
    where solutionInstalls =
              do bindings <- angles $ sepBy binding comma
                 spaces
                 semicolon
                 return bindings
          binding =
              choice <|>
              do p <- try $ packageWithoutTerminators [try $ symbol ":=", symbol ">"]
                 symbol ":="
                 v <- lexeme $ readCharsTill [comma, rightAngle]
                 return $ InstallVersion { choiceVer = Version { verPkg = p,
                                                                 verName = BS.pack v },
                                           choiceVerDepInfo = Nothing }
          solutionUnresolvedSoftDeps =
              do spaces
                 (do try (symbol "<!")
                     deps <- sepBy (try $ depWithoutTerminators [try $ symbol "!>"]) comma
                     symbol "!>"
                     semicolon
                     return [BreakSoftDep d | d <- deps])
                 <|> return []
          solutionBrokenDeps =
              do deps <- squares $ sepBy (try $ depWithoutTerminators [symbol "]"]) comma
                 semicolon
                 return deps
          solutionForbiddenVers =
              do (do try (symbol "!!")
                     vers <- sepBy (try $ versionWithoutTerminators [try $ symbol "!!", comma]) (comma)
                     symbol "!!"
                     semicolon
                     return vers) <|> return []

-- | A promotion looks like this: (T<tier>: {choice, ..})
promotion :: Parser Promotion
promotion = parens $ do char 'T'
                        promotionTier <- tier
                        symbol ":"
                        choices <- braces $ sepBy choice comma
                        let choiceSet = Set.fromList choices
                        promotionTier `seq` choiceSet `seq`
                             intern $ Promotion { promotionTier = promotionTier,
                                                  promotionChoices = Set.fromList choices }
