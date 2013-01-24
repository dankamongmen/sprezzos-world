module Resolver.PrettyPrint where

import Data.ByteString.Char8(unpack)
import Data.List
import qualified Data.Set as Set
import Resolver.Types

class PP a where
    ppS :: a -> ShowS

instance PP Version where
    ppS (Version pkg verName) = ppS pkg . (' ':) . (unpack verName++)

instance PP Package where
    ppS (Package pkgName) = (unpack pkgName++)

instance PP Dep where
    ppS (Dep src solvers isSoft) = let arrow = if isSoft
                                               then " -S> {"
                                               else " -> {" in
                                   ppS src . (arrow++) . (\x -> foldr (++) x $ intersperse ", " $ map pp solvers) . ('}':)

instance PP Choice where
    ppS (InstallVersion ver di) = ("Install "++) . case di of
                                                     Nothing             -> ppS ver
                                                     Just (Unscoped _)   -> ppS ver
                                                     Just (Scoped d)     -> ("<scope: "++) . ppS d . (">"++)
                                                     Just (FromSource d) -> ("<source: "++) . ppS d . (">"++)
    ppS (BreakSoftDep d) = ("Break "++) . ppS d

instance PP Promotion where
    ppS (Promotion choices tier) = ('(':) . shows tier . (": ("++) .
                                   (foldl' (.) id $ intersperse (", "++) $ map ppS $ Set.toList choices) .
                                   (')':)

pp x = ppS x ""
