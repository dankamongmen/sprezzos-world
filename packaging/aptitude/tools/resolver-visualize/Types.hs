module Types where

data TargetFormat = PS | PNG
    deriving(Eq, Ord, Show, Read)

-- | Parameters the user can set at the command-line.
data Params =
    Params {
      -- | The maximum number of steps to visualize (the whole file is
      -- always loaded, but only this many are rendered).
      maxSteps :: Maybe Integer,
      -- | The first step to start rendering.
      firstStep :: Maybe Integer,
      -- | Whether to display promotions as nodes.
      showPromotions :: Bool,
      -- | Where and whether to send dot output.
      dotOutput :: Maybe String,
      -- | The target output format.
      targetFormat :: Maybe TargetFormat
    } deriving(Eq, Ord, Show)
defaultParams = Params { maxSteps  = Nothing,
                         firstStep = Nothing,
                         showPromotions = False,
                         dotOutput = Nothing,
                         targetFormat = Nothing }
