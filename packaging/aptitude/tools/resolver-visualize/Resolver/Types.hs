-- | The core data types used to represent packages, versions, and
-- other problem resolver structures.

module Resolver.Types(
                      Package(..),
                      Version(..),
                      Dep(..),
                      ChoiceDepInfo(..),
                      Choice(..),
                      Solution(..),
                      FastSolution(fastSol, fastSolHash),
                      makeFastSolution,
                      maximumTierNum,
                      alreadyGeneratedTierNum,
                      deferTierNum,
                      minimumTierNum,
                      Tier(..),
                      maximumTier,
                      conflictTier,
                      alreadyGeneratedTier,
                      deferTier,
                      minimumTier,
                      showsTierComponent,
                      Promotion(..),
                      Hashed(..),
                      makeHashed,
                      FastPromotion,
                      fastPromotionHash,
                      fastPromotion,
                      makeFastPromotion
                     ) where

import Data.ByteString.Char8(ByteString)
import Data.HashTable(hashString)
import Data.Int(Int32)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map

-- | Represents a package.
--
-- The whole resolver database is not available when parsing the log,
-- so packages can't contain the list of their versions.
data Package = Package { pkgName :: ByteString }
               deriving(Ord, Eq, Show)

-- | Represents a single version of a package.
data Version = Version { -- | The package that this version is
                         -- associated with.
                         verPkg :: Package,
                         -- | A unique string that identifies this
                         -- version among the versions of the package.
                         verName :: ByteString }
               deriving(Ord, Eq, Show)

-- | Represents a single dependency.
--
-- TODO: the resolver should state whether a dependency is "soft" when
-- logging it.  (e.g.: pkg S-> { v1 v2 })
data Dep = Dep { -- | The version that declares this dependency.
                 depSource :: Version,
                 -- | A list of versions that solve this dependency.
                 depSolvers :: [Version],
                 depIsSoft :: Bool}
         deriving(Ord, Eq, Show)

data ChoiceDepInfo = Unscoped   { depInfoDep :: Dep }
                   | Scoped     { depInfoDep :: Dep }
                   | FromSource { depInfoDep :: Dep }
              deriving(Ord, Eq, Show)

-- | Represents a choice made by the dependency resolver.
data Choice = InstallVersion { -- | The version to install.
                               choiceVer :: Version,
                               -- | The dependency that triggered this
                               -- installation.
                               --
                               -- Will be Nothing if it is not known.
                               choiceVerDepInfo :: Maybe ChoiceDepInfo }
            -- ^ Install a single version.
            -- | Leave a soft dependency unresolved.
            | BreakSoftDep { -- | The dependency that was not
                             -- resolved.
                             choiceDep :: Dep }
              deriving(Ord, Eq, Show)


-- | Represents a single solution created by the dependency resolver.
data Solution = Solution { -- | The choices made in this solution.
                           -- Each choice is mapped to an integer
                           -- representing its order in the
                           -- solution, or to Nothing if the order
                           -- is unknown.
                           solChoices :: Map Choice (Maybe Int),
                           -- | The versions that may not be
                           -- installed in this solution.
                           solForbiddenVersions :: Set Version,
                           -- | Dependencies that are broken under
                           -- this solution.
                           solBrokenDeps :: Set Dep,
                           -- | The score of this solution.
                           solScore :: Integer,
                           -- | The tier of this solution.
                           solTier  :: Tier }
              deriving(Show)

-- Custom Eq and Ord instances here to adjust the order in which the
-- parts of the Solution structure are compared.  As in the C++ code,
-- this exploits the fact that scores are very nearly unique among the
-- solutions in a particular run to avoid comparing sets most of the
-- time.
instance Eq Solution where
    sol1 == sol2 =
        solScore sol1 == solScore sol2 &&
        solTier sol1 == solTier sol2 &&
        solBrokenDeps sol1 == solBrokenDeps sol2 &&
        solForbiddenVersions sol1 == solForbiddenVersions sol2 &&
        solChoices sol1 == solChoices sol2

instance Ord Solution where
    sol1 `compare` sol2 =
        foldr combine EQ [solScore sol1 `compare` solScore sol2,
                          solTier sol1 `compare` solTier sol2,
                          Set.size (solBrokenDeps sol1) `compare` Set.size (solBrokenDeps sol2),
                          Set.size (solForbiddenVersions sol1) `compare` Set.size (solForbiddenVersions sol2),
                          Map.size (solChoices sol1) `compare` Map.size (solChoices sol2),
                          solBrokenDeps sol1 `compare` solBrokenDeps sol2,
                          solForbiddenVersions sol1 `compare` solForbiddenVersions sol2,
                          solChoices sol1 `compare` solChoices sol2]
            where combine EQ o2 = o2
                  combine o1 _  = o1

-- | Used to insert solutions into hash tables reasonably quickly.
--
-- Created by hashing the output of the Show instance.
data FastSolution = FastSolution { fastSol :: Solution, fastSolHash :: Int32 }
                  deriving(Show)

instance Eq FastSolution where
    fs1 == fs2 = fastSolHash fs1 == fastSolHash fs2 &&
                 fastSol fs1 == fastSol fs2

instance Ord FastSolution where
    fs1 `compare` fs2 = case fastSolHash fs1 `compare` fastSolHash fs2 of
                          EQ -> fastSol fs1 `compare` fastSol fs2
                          o  -> o

makeFastSolution :: Solution -> FastSolution
makeFastSolution sol = FastSolution { fastSol = sol,
                                      fastSolHash = hashString $ show sol }

maximumTierNum = 2147483647
alreadyGeneratedTierNum = maximumTierNum - 1
deferTierNum = alreadyGeneratedTierNum - 1
minimumTierNum = -2147483648
newtype Tier = Tier { tierLevels :: [Integer] } deriving(Ord, Eq)
maximumTier = Tier [maximumTierNum]
conflictTier = maximumTier
alreadyGeneratedTier = Tier [alreadyGeneratedTierNum]
deferTier = Tier [deferTierNum]
minimumTier = Tier [minimumTierNum]

-- The Show instance mainly special-cases the special tiers so they
-- get pretty-printed.
instance Show Tier where
    showsPrec _ (Tier (first:_))
        | first == maximumTierNum          = ("T(conflict)"++)
        | first == deferTierNum            = ("T(defer)"++)
        | first == alreadyGeneratedTierNum = ("T(redundant)"++)
    showsPrec _ (Tier nums)  = ("T("++) .
                               foldr (.) id (intersperse (", "++) (map showsTierComponent nums)) .
                               (')':)

-- | Display a user-friendly description of a tier number.
showsTierComponent tierNum
    | tierNum == maximumTierNum          = ("maximum"++)
    | tierNum == minimumTierNum          = ("minimum"++)
    | otherwise                          = shows tierNum

-- | Represents a promotion to a tier.
data Promotion = Promotion { -- | The choices that produced this promotion.
                             promotionChoices :: Set Choice,
                             -- | The tier of this promotion.
                             promotionTier :: Tier }
               deriving(Show)

instance Eq Promotion where
    p1 == p2 =
        promotionTier p1 == promotionTier p2 &&
        Set.size (promotionChoices p1) == Set.size (promotionChoices p2) &&
        promotionChoices p1 == promotionChoices p2

instance Ord Promotion where
    p1 `compare` p2 =
        foldr combine EQ [promotionTier p1 `compare` promotionTier p2,
                          Set.size (promotionChoices p1) `compare` Set.size (promotionChoices p2),
                          promotionChoices p1 `compare` promotionChoices p2]
        where combine EQ o2 = o2
              combine o1 _  = o1

data Hashed a = Hashed { hashedVal :: a,
                         hashedHash :: Int32 }

instance Eq a => Eq (Hashed a) where
    h1 == h2 =
        hashedHash h1 == hashedHash h2 &&
        hashedVal h1 == hashedVal h2

instance Ord a => Ord (Hashed a) where
    h1 `compare` h2 =
        case hashedHash h1 `compare` hashedHash h2 of
          EQ -> hashedVal h1 `compare` hashedVal h2
          o  -> o

-- | Use a value's Show instance to hash it.
makeHashed :: Show a => a -> Hashed a
makeHashed a = Hashed { hashedVal = a,
                        hashedHash = hashString $ show a }

type FastPromotion = Hashed Promotion

fastPromotion :: FastPromotion -> Promotion
fastPromotion = hashedVal

fastPromotionHash :: FastPromotion -> Int32
fastPromotionHash = hashedHash

makeFastPromotion :: Promotion -> FastPromotion
makeFastPromotion = makeHashed
