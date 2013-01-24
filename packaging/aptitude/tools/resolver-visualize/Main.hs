#!/usr/bin/env runhaskell

module Main where

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Monad.Trans(liftIO)
import Data.ByteString.Char8 as ByteString(ByteString, empty, hGet, pack, unpack)
import Data.List(intersperse)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView
import Numeric
import System.Environment(getArgs)
import System.Glib.MainLoop
import System.Glib.Types
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree
import Resolver.Log
import Resolver.PrettyPrint
import Resolver.Types
import Resolver.Util
import System.IO
import System.Time
import Text.Printf
import DotRender
import Types

xmlFilename = "resolver-visualize.glade"

data MainLoopContext = MainLoopContext { mainLoop :: MainLoop,
                                         numMainWindows :: IORef Integer }

type MainM = ReaderT MainLoopContext IO

runMain :: (MainM a) -> MainLoopContext -> IO a
runMain = runReaderT

mainWindowClosed :: MainM ()
mainWindowClosed = do ctx <- ask
                      liftIO $ do modifyIORef (numMainWindows ctx) (+(-1))
                                  num <- readIORef (numMainWindows ctx)
                                  when (num == 0) (mainLoopQuit (mainLoop ctx))

mainWindowOpened :: MainM ()
mainWindowOpened = do ctx <- ask
                      liftIO $ modifyIORef (numMainWindows ctx) (+1)

-- | Information about a loaded log file.
data LoadedLogFile =
    LoadedLogFile { logFile :: LogFile }

data TextColumnInfo = TextColumnInfo { textColumn   :: TreeViewColumn,
                                       textRenderer :: CellRendererText }

textColumnNew :: String -> IO TextColumnInfo
textColumnNew name = do col      <- treeViewColumnNew
                        treeViewColumnSetTitle col name
                        renderer <- cellRendererTextNew
                        cellLayoutPackEnd col renderer True
                        return TextColumnInfo { textColumn   = col,
                                                textRenderer = renderer }

-- | Information about the columns and renderers of the tree display.
data TreeViewColumnInfo =
    TreeViewColumnInfo { treeViewText        :: TextColumnInfo,
                         treeViewNumChoices  :: TextColumnInfo,
                         treeViewBrokenDeps  :: TextColumnInfo,
                         treeViewStepNum     :: TextColumnInfo,
                         treeViewChildren    :: TextColumnInfo,
                         treeViewHeight      :: TextColumnInfo,
                         treeViewSubtreeSize :: TextColumnInfo,
                         treeViewTier        :: TextColumnInfo,
                         treeViewScore       :: TextColumnInfo,
                         treeViewDep         :: TextColumnInfo }

treeViewColumnInfoRenderers :: TreeViewColumnInfo -> [CellRenderer]
treeViewColumnInfoRenderers inf =
    [ toCellRenderer $ textRenderer $ treeViewText inf,
      toCellRenderer $ textRenderer $ treeViewNumChoices inf,
      toCellRenderer $ textRenderer $ treeViewBrokenDeps inf,
      toCellRenderer $ textRenderer $ treeViewStepNum inf,
      toCellRenderer $ textRenderer $ treeViewChildren inf,
      toCellRenderer $ textRenderer $ treeViewHeight inf,
      toCellRenderer $ textRenderer $ treeViewSubtreeSize inf,
      toCellRenderer $ textRenderer $ treeViewTier inf,
      toCellRenderer $ textRenderer $ treeViewScore inf,
      toCellRenderer $ textRenderer $ treeViewDep inf ]

treeViewColumnInfoColumns :: TreeViewColumnInfo -> [TreeViewColumn]
treeViewColumnInfoColumns inf =
    [ textColumn $ treeViewText inf,
      textColumn $ treeViewNumChoices inf,
      textColumn $ treeViewBrokenDeps inf,
      textColumn $ treeViewStepNum inf,
      textColumn $ treeViewChildren inf,
      textColumn $ treeViewHeight inf,
      textColumn $ treeViewSubtreeSize inf,
      textColumn $ treeViewTier inf,
      textColumn $ treeViewScore inf,
      textColumn $ treeViewDep inf ]

-- | Discard attribute bindings for the tree view.
treeViewColumnInfoClear :: TreeViewColumnInfo -> IO ()
treeViewColumnInfoClear inf = let cols = treeViewColumnInfoColumns inf in
                              mapM_ cellLayoutClear cols

newTreeViewColumns :: IO TreeViewColumnInfo
newTreeViewColumns =
    do text              <- textColumnNew "Description"
       numChoices        <- textColumnNew "Choices"
       brokenDeps        <- textColumnNew "Broken Deps"
       stepNum           <- textColumnNew "Order"
       children          <- textColumnNew "Children"
       height            <- textColumnNew "Height"
       subtreeSize       <- textColumnNew "Size"
       tier              <- textColumnNew "Tier"
       score             <- textColumnNew "Score"
       dep               <- textColumnNew "Dep"
       return TreeViewColumnInfo { treeViewText          = text,
                                   treeViewNumChoices    = numChoices,
                                   treeViewBrokenDeps    = brokenDeps,
                                   treeViewStepNum       = stepNum,
                                   treeViewChildren      = children,
                                   treeViewHeight        = height,
                                   treeViewSubtreeSize   = subtreeSize,
                                   treeViewTier          = tier,
                                   treeViewScore         = score,
                                   treeViewDep           = dep }

-- | Information about the columns and renderers of the chronological
-- display.
data ChronologicalViewColumnInfo =
    ChronologicalViewColumnInfo { chronViewNumChoices  :: TextColumnInfo,
                                  chronViewBrokenDeps  :: TextColumnInfo,
                                  chronViewStepNum     :: TextColumnInfo,
                                  chronViewChildren    :: TextColumnInfo,
                                  chronViewHeight      :: TextColumnInfo,
                                  chronViewSubtreeSize :: TextColumnInfo,
                                  chronViewTier        :: TextColumnInfo,
                                  chronViewScore       :: TextColumnInfo,
                                  chronViewParent      :: TextColumnInfo,
                                  chronViewChoice      :: TextColumnInfo,
                                  chronViewDep         :: TextColumnInfo
                                }

chronViewColumnInfoRenderers :: ChronologicalViewColumnInfo -> [CellRenderer]
chronViewColumnInfoRenderers inf =
    [ toCellRenderer $ textRenderer $ chronViewNumChoices inf,
      toCellRenderer $ textRenderer $ chronViewBrokenDeps inf,
      toCellRenderer $ textRenderer $ chronViewStepNum inf,
      toCellRenderer $ textRenderer $ chronViewChildren inf,
      toCellRenderer $ textRenderer $ chronViewHeight inf,
      toCellRenderer $ textRenderer $ chronViewSubtreeSize inf,
      toCellRenderer $ textRenderer $ chronViewTier inf,
      toCellRenderer $ textRenderer $ chronViewScore inf,
      toCellRenderer $ textRenderer $ chronViewParent inf,
      toCellRenderer $ textRenderer $ chronViewChoice inf,
      toCellRenderer $ textRenderer $ chronViewDep inf ]

chronViewColumnInfoColumns :: ChronologicalViewColumnInfo -> [TreeViewColumn]
chronViewColumnInfoColumns inf =
    [ textColumn $ chronViewNumChoices inf,
      textColumn $ chronViewBrokenDeps inf,
      textColumn $ chronViewStepNum inf,
      textColumn $ chronViewChildren inf,
      textColumn $ chronViewHeight inf,
      textColumn $ chronViewSubtreeSize inf,
      textColumn $ chronViewTier inf,
      textColumn $ chronViewScore inf,
      textColumn $ chronViewParent inf,
      textColumn $ chronViewChoice inf,
      textColumn $ chronViewDep inf ]

-- | Discard attribute bindings for the chronological view.
chronViewColumnInfoClear :: ChronologicalViewColumnInfo -> IO ()
chronViewColumnInfoClear inf = let cols = chronViewColumnInfoColumns inf in
                               mapM_ cellLayoutClear cols

newChronViewColumns :: IO ChronologicalViewColumnInfo
newChronViewColumns =
    do numChoices        <- textColumnNew "Choices"
       brokenDeps        <- textColumnNew "Broken Deps"
       stepNum           <- textColumnNew "Order"
       children          <- textColumnNew "Children"
       height            <- textColumnNew "Height"
       subtreeSize       <- textColumnNew "Size"
       tier              <- textColumnNew "Tier"
       score             <- textColumnNew "Score"
       parent            <- textColumnNew "Parent"
       dep               <- textColumnNew "Dep"
       choice            <- textColumnNew "Step"
       return ChronologicalViewColumnInfo {
                    chronViewNumChoices   = numChoices,
                    chronViewBrokenDeps   = brokenDeps,
                    chronViewStepNum      = stepNum,
                    chronViewChildren     = children,
                    chronViewHeight       = height,
                    chronViewSubtreeSize  = subtreeSize,
                    chronViewTier         = tier,
                    chronViewScore        = score,
                    chronViewParent       = parent,
                    chronViewDep          = dep,
                    chronViewChoice       = choice
                  }

-- | Information about the columns and renderers of the run list.
data RunListColumnInfo =
    RunListColumnInfo { runListNumber :: TextColumnInfo,
                        runListLength :: TextColumnInfo }

runListInfoRenderers :: RunListColumnInfo -> [CellRenderer]
runListInfoRenderers inf =
    [ toCellRenderer $ textRenderer $ runListNumber inf,
      toCellRenderer $ textRenderer $ runListLength inf ]

runListInfoColumns :: RunListColumnInfo -> [TreeViewColumn]
runListInfoColumns inf =
    [ textColumn $ runListNumber inf,
      textColumn $ runListLength inf ]

-- | Discard attribute bindings for the run list.
runListColumnInfoClear :: RunListColumnInfo -> IO ()
runListColumnInfoClear inf = let cols = runListInfoColumns inf in
                             mapM_ cellLayoutClear cols

newRunListColumns :: IO RunListColumnInfo
newRunListColumns =
    do number     <- textColumnNew "Run Number"
       len        <- textColumnNew "Run Length"
       return RunListColumnInfo {
                    runListNumber = number,
                    runListLength = len
                  }

type TreeViewStore  = TreeStore TreeViewEntry
type ChronViewStore = ListStore ChronViewEntry
type RunListStore   = ListStore (Integer, [ProcessingStep])


-- | Shared context for the visualizer.
data VisualizeContext =
    VisualizeContext { treeView :: TreeView,
                       treeViewColumnInfo :: TreeViewColumnInfo,
                       treeViewStore :: TreeViewStore,
                       chronologicalView :: TreeView,
                       chronologicalViewColumnInfo :: ChronologicalViewColumnInfo,
                       chronologicalViewStore :: ChronViewStore,
                       runList :: TreeView,
                       runListColumnInfo :: RunListColumnInfo,
                       runListStore :: RunListStore,
                       logView :: SourceView,
                       statusbar :: Statusbar,
                       mainStatusCtx :: ContextId, -- ^ The context ID used for the "main" messages.
                       mainWindow :: Window,
                       mainContext :: MainLoopContext,
                       params :: Params,
                       loadedFile :: IORef (Maybe LoadedLogFile),
                       activeRun  :: IORef (Maybe (Integer, [ProcessingStep])) }

-- | State monad for the visualizer.
type VisM = ReaderT VisualizeContext IO

-- Accessors for state.
runVis :: (VisM a) -> VisualizeContext -> IO a
runVis = runReaderT

getTreeView :: VisM TreeView
getTreeView = do ctx <- ask; return $ treeView ctx

getTreeViewStore :: VisM TreeViewStore
getTreeViewStore = do ctx <- ask; return $ treeViewStore ctx

getTreeViewColumnInfo :: VisM TreeViewColumnInfo
getTreeViewColumnInfo = do ctx <- ask; return $ treeViewColumnInfo ctx

getChronologicalView :: VisM TreeView
getChronologicalView = do ctx <- ask; return $ chronologicalView ctx

getChronologicalViewColumnInfo :: VisM ChronologicalViewColumnInfo
getChronologicalViewColumnInfo = do ctx <- ask; return $ chronologicalViewColumnInfo ctx

getChronologicalViewStore :: VisM ChronViewStore
getChronologicalViewStore = do ctx <- ask; return $ chronologicalViewStore ctx

getRunList :: VisM TreeView
getRunList = do ctx <- ask; return $ runList ctx

getRunListColumnInfo :: VisM RunListColumnInfo
getRunListColumnInfo = do ctx <- ask; return $ runListColumnInfo ctx

getRunListStore :: VisM RunListStore
getRunListStore = do ctx <- ask; return $ runListStore ctx

getLogView :: VisM SourceView
getLogView = do ctx <- ask; return $ logView ctx

getStatusbar :: VisM Statusbar
getStatusbar = do ctx <- ask; return $ statusbar ctx

getMainStatusCtx :: VisM ContextId
getMainStatusCtx = do ctx <- ask; return $ mainStatusCtx ctx

getMainWindow :: VisM Window
getMainWindow = do ctx <- ask; return $ mainWindow ctx

getMainCtx :: VisM MainLoopContext
getMainCtx = do ctx <- ask; return $ mainContext ctx

getParams :: VisM Params
getParams = do ctx <- ask; return $ params ctx


getLog :: VisM (Maybe LogFile)
getLog = do ctx <- ask
            maybeInf <- liftIO $ readIORef $ loadedFile ctx
            return $ fmap logFile maybeInf

getRunNumber :: VisM (Maybe Integer)
getRunNumber = do ctx <- ask
                  runInf <- liftIO $ readIORef $ activeRun ctx
                  return $ fmap fst runInf

getActiveRun :: VisM (Maybe [ProcessingStep])
getActiveRun = do ctx <- ask
                  runInf <- liftIO $ readIORef $ activeRun ctx
                  return $ fmap snd runInf

isActiveRun :: Integer -> VisM Bool
isActiveRun n = do current <- getRunNumber
                   case current of
                     Nothing   -> return False
                     (Just n') -> return (n == n')

--getTreeViewStore :: VisM (Maybe (TreeStore TreeViewEntry))
--getTreeViewStore = do ctx <- ask
--                      maybeInf <- liftIO $ readIORef $ loadedFile ctx
--                      return $ fmap treeViewStore maybeInf

-- | Stores tree-structure information about an entry.
data EntryTreeInfo = EntryTreeInfo {
      treeInfoChildren    :: Integer,
      treeInfoHeight      :: Integer,
      treeInfoSubtreeSize :: Integer
    } deriving(Ord, Eq, Show)

getTreeInfo :: ProcessingStep -> EntryTreeInfo
getTreeInfo (ProcessingStep { stepSuccessors = steps,
                              stepDepth      = height,
                              stepBranchSize = branchSize }) = EntryTreeInfo { treeInfoChildren    = toInteger $ length steps,
                                                                               treeInfoHeight      = height,
                                                                               treeInfoSubtreeSize = branchSize }

-- Each row in the tree display is either the root of the search, a
-- processing step (possibly with no parent!), a solution that was
-- enqueued but never visited, or a note about a problem rendering the
-- tree.
data TreeViewEntry =
    Root {
      entryStep         :: ProcessingStep,
      entryNumChoices   :: Integer,
      entryBrokenDeps   :: Integer,
      entryStepNum      :: Integer,
      entryTreeInfo     :: EntryTreeInfo
    }
  | Step {
      entryStep         :: ProcessingStep,
      entryChoice       :: LinkChoice,
      entryNumChoices   :: Integer,
      entryBrokenDeps   :: Integer,
      entryStepNum      :: Integer,
      entryTreeInfo     :: EntryTreeInfo
    }
  -- | The maximum number of search nodes was rendered.
  | Horizon {
      entryChoice       :: LinkChoice,
      entryStepNum      :: Integer,
      entryTreeInfo     :: EntryTreeInfo
    }
  -- | A Step node would have been produced, but the
  -- same solution was already generated as a Step
  -- node.  The attached Solution node could be used
  -- to, e.g., look up the tree node associated with
  -- the link.
  | AlreadyGeneratedStep {
      entrySol          :: Solution,
      entryChoice       :: LinkChoice,
      entryNumChoices   :: Integer,
      entryStepNum      :: Integer,
      entryBrokenDeps   :: Integer,
      entryTextStart    :: Integer,
      entryTextLength   :: Integer
    }
  | NoStep {
      entrySol          :: Solution,
      entryChoice       :: LinkChoice,
      entryBrokenDeps   :: Integer,
      entryNumChoices   :: Integer
    }
  | Error {
      entryErrorText    :: String,
      entryStepNum      :: Integer
    }

forceList :: [a] -> [a]
forceList = foldr (\a b -> a `seq` (a:b)) []

-- Each node in the tree refers either to a node's successor or to a
-- number of nodes below that node (precomputed).
data SuccessorOrHorizon = NodeSuccessor Successor
                        | NodeHorizon TreeViewEntry

-- | The tree-view building monad contains local state remembering
-- which nodes have already been incorporated into the tree, so we
-- don't display them more than once.
type BuildTreeView = State.State (Set.Set Solution)
unfoldSuccessor :: Params -> SuccessorOrHorizon -> BuildTreeView (TreeViewEntry, [SuccessorOrHorizon])
unfoldSuccessor params (NodeHorizon entry) =
    return (entry, [])
unfoldSuccessor params (NodeSuccessor (Successor step choice forced)) =
    do seen <- State.get
       let sol        = stepSol step
           stepNum    = stepOrder step
       case undefined of
         _ | maybe False (stepNum>=) (maxSteps params) ->
               let numChoices     = toInteger $ Map.size $ solChoices $ stepSol step
                   brokenDeps     = toInteger $ Set.size $ solBrokenDeps $ stepSol step
                   treeInfo       = getTreeInfo step
                   horizon        = Horizon { entryChoice     = choice,
                                              entryStepNum    = stepNum,
                                              entryTreeInfo   = treeInfo } in
               step `seq` choice `seq` stepNum `seq` treeInfo `seq`
               do return (Step { entryStep       = step,
                                 entryChoice     = choice,
                                 entryStepNum    = stepNum,
                                 entryNumChoices = numChoices,
                                 entryBrokenDeps = brokenDeps,
                                 entryTreeInfo   = treeInfo },
                          [NodeHorizon horizon])
           | sol `Set.member` seen ->
               let numChoices = toInteger $ Map.size $ solChoices sol
                   brokenDeps = toInteger $ Set.size $ solBrokenDeps sol
                   textStart  = stepTextStart step
                   textLength = stepTextLength step in
               return $ sol `seq` choice `seq` stepNum `seq` textStart `seq`
               textLength `seq` numChoices `seq` brokenDeps `seq`
               (AlreadyGeneratedStep { entrySol        = sol,
                                       entryChoice     = choice,
                                       entryStepNum    = stepNum,
                                       entryNumChoices = numChoices,
                                       entryBrokenDeps = brokenDeps,
                                       entryTextStart  = textStart,
                                       entryTextLength = textLength },
                [])
           | otherwise ->
               let newState       = Set.insert sol seen
                   numChoices     = toInteger $ Map.size $ solChoices $ stepSol step
                   brokenDeps     = toInteger $ Set.size $ solBrokenDeps $ stepSol step
                   treeInfo       = getTreeInfo step in
               step `seq` choice `seq` stepNum `seq` newState `seq`
               numChoices `seq` brokenDeps `seq` treeInfo `seq`
               do State.put newState
                  return (Step { entryStep       = step,
                                 entryChoice     = choice,
                                 entryStepNum    = stepNum,
                                 entryNumChoices = numChoices,
                                 entryBrokenDeps = brokenDeps,
                                 entryTreeInfo   = treeInfo },
                          forceList $ map NodeSuccessor $ stepSuccessors step)
unfoldSuccessor _ (NodeSuccessor (Unprocessed sol choice forced)) =
    let brokenDeps = toInteger $ Set.size $ solBrokenDeps sol
        numChoices = toInteger $ Map.size $ solChoices sol in
    sol `seq` choice `seq` brokenDeps `seq` numChoices `seq`
    return (NoStep { entrySol = sol,
                     entryChoice = choice,
                     entryBrokenDeps = brokenDeps,
                     entryNumChoices = numChoices },
            [])

runToForest :: Params -> [ProcessingStep] -> Forest TreeViewEntry
runToForest params steps =
    case steps of
      [] -> [Node (Error { entryErrorText = "No steps in this run.",
                           entryStepNum = 0 }) []]
      (first:rest) -> let hasRoot        = maybe False (==0) (firstStep params)
                          root           = if hasRoot then Just first else Nothing
                          droppedSteps   = maybe steps (\n -> genericDrop n steps) (firstStep params)
                          droppedRest    = if hasRoot then drop 1 droppedSteps else droppedSteps
                          truncatedRest  = maybe droppedRest (\n -> genericTake n droppedRest) (maxSteps params) in
                      State.evalState (makeWholeTree root truncatedRest) Set.empty
    where makeTree :: (ProcessingStep -> TreeViewEntry) -> ProcessingStep -> BuildTreeView (Maybe (Tree TreeViewEntry))
          makeTree f root =
              do seen <- State.get
                 let rootSol = stepSol root
                 (if rootSol `Set.member` seen
                  then return $ Nothing
                  else do State.put $ Set.insert rootSol seen
                          let rootSucc = stepSuccessors root
                          succ <- unfoldForestM (unfoldSuccessor params) (map NodeSuccessor rootSucc)
                          return $ f `seq` root `seq` forceList succ `seq` Just $ Node (f root) succ)
          makeWholeTree :: Maybe ProcessingStep -> [ProcessingStep] -> BuildTreeView (Forest TreeViewEntry)
          makeWholeTree root rest =
              do first <- maybe (return Nothing) (makeTree makeRootTree) root
                 rest' <- sequence $ map (makeTree makeOrphanedTree) rest
                 return $ catMaybes (first:rest')
          makeRootTree     root = let numChoices = toInteger $ Map.size $ solChoices $ stepSol root
                                      brokenDeps = toInteger $ Set.size $ solBrokenDeps $ stepSol root
                                      stepNum = stepOrder root
                                      treeInfo = getTreeInfo root in
                                  root `seq` numChoices `seq` brokenDeps `seq` stepNum `seq` treeInfo `seq`
                                  Root { entryStep       = root,
                                         entryNumChoices = numChoices,
                                         entryBrokenDeps = brokenDeps,
                                         entryStepNum    = stepOrder root,
                                         entryTreeInfo   = getTreeInfo root }
          makeOrphanedTree root = let numChoices = toInteger $ Map.size $ solChoices $ stepSol root
                                      brokenDeps = toInteger $ Set.size $ solBrokenDeps $ stepSol root
                                      stepNum = stepOrder root
                                      treeInfo = getTreeInfo root in
                                  root `seq` numChoices `seq` brokenDeps `seq` stepNum `seq` treeInfo `seq`
                                  Step { entryStep       = root,
                                         entryChoice     = Unknown,
                                         entryNumChoices = numChoices,
                                         entryBrokenDeps = brokenDeps,
                                         entryStepNum    = stepNum,
                                         entryTreeInfo   = treeInfo }

choiceText :: LinkChoice -> String
choiceText (LinkChoice c) = pp c
choiceText Unknown = "(...)"

-- Column definitions for tree view entries.
entryColumnText :: TreeViewEntry -> String
entryColumnText (Root {}) = "Root"
entryColumnText (Step { entryChoice = choice}) = choiceText choice
entryColumnText (Horizon { entryTreeInfo = inf }) = (show $ treeInfoChildren inf) ++ " search nodes..."
entryColumnText (AlreadyGeneratedStep { entryChoice = choice }) = "Already seen: " ++ choiceText choice
entryColumnText (NoStep { entryChoice = choice }) = "Never visited: " ++ choiceText choice
entryColumnText (Error { entryErrorText = err }) = err

entryColumnNumChoices :: TreeViewEntry -> String
entryColumnNumChoices (Root { entryNumChoices = n })                    = show n
entryColumnNumChoices (Step { entryNumChoices = n })                    = show n
entryColumnNumChoices (Horizon {})                                      = ""
entryColumnNumChoices (AlreadyGeneratedStep { entryNumChoices = n })    = show n
entryColumnNumChoices (NoStep { entryNumChoices = n })                  = show n
entryColumnNumChoices (Error {})                                        = ""

entryColumnBrokenDeps :: TreeViewEntry -> String
entryColumnBrokenDeps (Root { entryBrokenDeps = n })                    = show n
entryColumnBrokenDeps (Step { entryBrokenDeps = n })                    = show n
entryColumnBrokenDeps (Horizon {})                                      = ""
entryColumnBrokenDeps (AlreadyGeneratedStep { entryBrokenDeps = n })    = show n
entryColumnBrokenDeps (NoStep { entryBrokenDeps = n })                  = show n
entryColumnBrokenDeps (Error {})                                        = ""

entryColumnStepNum :: TreeViewEntry -> String
entryColumnStepNum (Root { entryStepNum = n })                          = show n
entryColumnStepNum (Step { entryStepNum = n })                          = show n
entryColumnStepNum (Horizon { entryStepNum = n })                       = show n
entryColumnStepNum (AlreadyGeneratedStep { entryStepNum = n })          = show n
entryColumnStepNum (NoStep {})                                          = ""
entryColumnStepNum (Error {})                                           = ""

entryColumnChildren :: TreeViewEntry -> String
entryColumnChildren (Root { entryTreeInfo = inf })                      = show $ treeInfoChildren inf
entryColumnChildren (Step { entryTreeInfo = inf })                      = show $ treeInfoChildren inf
entryColumnChildren (Horizon { entryTreeInfo = inf })                   = show $ treeInfoChildren inf
entryColumnChildren (AlreadyGeneratedStep {})                           = ""
entryColumnChildren (NoStep { })                                        = ""
entryColumnChildren (Error {})                                          = ""


entryColumnHeight :: TreeViewEntry -> String
entryColumnHeight (Root { entryTreeInfo = inf })                        = show $ treeInfoHeight inf
entryColumnHeight (Step { entryTreeInfo = inf })                        = show $ treeInfoHeight inf
entryColumnHeight (Horizon { entryTreeInfo = inf })                     = show $ treeInfoHeight inf
entryColumnHeight (AlreadyGeneratedStep { })                            = ""
entryColumnHeight (NoStep { })                                          = ""
entryColumnHeight (Error {})                                            = ""

entryColumnSubtreeSize :: TreeViewEntry -> String
entryColumnSubtreeSize (Root { entryTreeInfo = inf })                   = show $ treeInfoSubtreeSize inf
entryColumnSubtreeSize (Step { entryTreeInfo = inf })                   = show $ treeInfoSubtreeSize inf
entryColumnSubtreeSize (Horizon { entryTreeInfo = inf })                = show $ treeInfoSubtreeSize inf
entryColumnSubtreeSize (AlreadyGeneratedStep { })                       = ""
entryColumnSubtreeSize (NoStep { })                                     = ""
entryColumnSubtreeSize (Error {})                                       = ""

entryColumnTier :: TreeViewEntry -> String
entryColumnTier (Root { entryStep = step })                             = show $ solTier $ stepSol step
entryColumnTier (Step { entryStep = step })                             = show $ solTier $ stepSol step
entryColumnTier (Horizon {})                                            = ""
entryColumnTier (AlreadyGeneratedStep { entrySol = sol })               = show $ solTier sol
entryColumnTier (NoStep { entrySol = sol })                             = show $ solTier sol
entryColumnTier (Error {})                                              = ""

entryColumnScore :: TreeViewEntry -> String
entryColumnScore (Root { entryStep = step })                            = show $ solScore $ stepSol step
entryColumnScore (Step { entryStep = step })                            = show $ solScore $ stepSol step
entryColumnScore (Horizon {})                                           = ""
entryColumnScore (AlreadyGeneratedStep { entrySol = sol })              = show $ solScore sol
entryColumnScore (NoStep { entrySol = sol })                            = show $ solScore sol
entryColumnScore (Error {})                                             = ""

linkChoiceDepText :: LinkChoice -> String
linkChoiceDepText (LinkChoice (InstallVersion { choiceVerDepInfo = Just di })) =
    pp (depInfoDep di)
linkChoiceDepText _ = ""

entryColumnDep :: TreeViewEntry -> String
entryColumnDep (Root {}) = ""
entryColumnDep (Step { entryChoice = choice }) = linkChoiceDepText choice
entryColumnDep (Horizon { }) = ""
entryColumnDep (AlreadyGeneratedStep { entryChoice = choice }) = linkChoiceDepText choice
entryColumnDep (NoStep { entryChoice = choice }) = linkChoiceDepText choice
entryColumnDep (Error { entryErrorText = err }) = ""

renderTreeView :: Params -> [ProcessingStep] -> TreeViewStore -> IO ()
renderTreeView params steps model =
    do let forest = runToForest params steps
           taggedForest = zip [0..] forest
       treeStoreClear model
       mapM_ (\(n, tree) -> treeStoreInsertTree model [] n tree)
             taggedForest

data ChronViewEntry =
    ChronStep { chronNumChoices   :: Integer,
                chronBrokenDeps   :: Integer,
                chronStepNum      :: Integer,
                chronChildren     :: Integer,
                chronHeight       :: Integer,
                chronSubtreeSize  :: Integer,
                chronTier         :: Tier,
                chronScore        :: Integer,
                chronParent       :: Maybe Integer,
                chronDep          :: Maybe Dep,
                chronChoice       :: Maybe Choice,
                chronStep         :: ProcessingStep }

makeChronStep :: ProcessingStep -> ChronViewEntry
makeChronStep step =
    let numChoices  = toInteger $ Map.size $ solChoices $ stepSol step
        brokenDeps  = toInteger $ Set.size $ solBrokenDeps $ stepSol step
        stepNum     = stepOrder step
        children    = toInteger $ length $ stepSuccessors step
        height      = stepDepth step
        subtreeSize = stepBranchSize step
        tier        = solTier $ stepSol step
        score       = solScore $ stepSol step
        parent      = fmap (stepOrder . parentLinkParent) $ stepPredecessor step
        choice      = case stepPredecessor step of
                        Just (ParentLink { parentLinkAction = LinkChoice c }) -> Just c
                        _ -> Nothing
        dep         = case choice of
                        Just (InstallVersion { choiceVerDepInfo = maybeDi }) -> fmap depInfoDep maybeDi
                        _ -> Nothing in
    numChoices `seq` brokenDeps `seq` stepNum `seq` children `seq` height `seq` subtreeSize `seq` step `seq` tier `seq` score `seq`
    choice `seq` parent `seq` dep `seq`
    ChronStep { chronNumChoices  = numChoices,
                chronBrokenDeps  = brokenDeps,
                chronStepNum     = stepNum,
                chronChildren    = children,
                chronHeight      = height,
                chronSubtreeSize = subtreeSize,
                chronStep        = step,
                chronTier        = tier,
                chronScore       = score,
                chronParent      = parent,
                chronDep         = dep,
                chronChoice      = choice }

renderChronView :: Params -> [ProcessingStep] -> ChronViewStore -> IO ()
renderChronView params steps model =
    do let stepsAdvanced  = maybe steps (\n -> genericDrop n steps) (firstStep params)
           stepsTruncated = maybe stepsAdvanced (\n -> genericTake n stepsAdvanced) (maxSteps params)
           list           = [ makeChronStep step | step <- stepsTruncated ]
       listStoreClear model
       mapM_ (listStoreAppend model) list

showText :: Integer -> Integer -> VisM ()
showText start len =
    do log      <- getLog
       logView  <- getLogView
       liftIO $ do
         logBuffer <- textViewGetBuffer logView
         (case log of
            Nothing -> textBufferSetText logBuffer ""
            Just f  -> do let h = logFileH f
                          hSeek h AbsoluteSeek start
                          s <- ByteString.hGet h (fromInteger len)
                          textBufferSetText logBuffer $ unpack s)

stepSelected :: Maybe ProcessingStep -> VisM ()
stepSelected Nothing = return () -- Clear the log view?
stepSelected (Just step) = showText (stepTextStart step) (stepTextLength step)

setupTextColumn inf model ops =
    cellLayoutSetAttributes (textColumn inf) (textRenderer inf) model ops

-- Returns the number of *actual* solutions (search nodes with no
-- broken dependencies).
numSolutions :: [ProcessingStep] -> Integer
numSolutions steps = genericLength [ step | step <- steps,
                                     Set.null $ solBrokenDeps $ stepSol step ]

maxTier :: [ProcessingStep] -> Tier
maxTier = maximum . map (solTier . stepSol)

setRun :: Maybe (Integer, [ProcessingStep]) -> VisM ()
setRun Nothing =
    do treeView         <- getTreeView
       chronView        <- getChronologicalView

       treeStore        <- getTreeViewStore
       chronStore       <- getChronologicalViewStore

       treeViewColInfo  <- getTreeViewColumnInfo
       chronViewColInfo <- getChronologicalViewColumnInfo

       ctx <- ask

       statusbar        <- getStatusbar
       statusCtx        <- getMainStatusCtx
       log              <- getLog

       liftIO $ do statusbarPop statusbar statusCtx
                   (case log of
                      Nothing -> statusbarPush statusbar statusCtx "No file loaded."
                      Just lf -> let sourceName = logFilename lf
                                     numRuns    = length $ runs lf in
                                 statusbarPush statusbar statusCtx $
                                               printf "%s: %d runs." sourceName numRuns)
                   treeStoreClear treeStore
                   listStoreClear chronStore
                   writeIORef (activeRun ctx) Nothing

setRun (Just (n, steps)) =
    do isActive <- isActiveRun n
       unless isActive $
            do ctx <- ask
               treeView         <- getTreeView
               chronView        <- getChronologicalView

               treeStore        <- getTreeViewStore
               chronStore       <- getChronologicalViewStore

               treeViewColInfo  <- getTreeViewColumnInfo
               chronViewColInfo <- getChronologicalViewColumnInfo

               params           <- getParams

               statusbar        <- getStatusbar
               statusCtx        <- getMainStatusCtx
               log              <- getLog

               logView <- getLogView
               liftIO $ do -- Update the UI for the new log file.
                 statusbarPop statusbar statusCtx
                 (case log of
                    Nothing -> statusbarPush statusbar statusCtx "Error: bad state"
                    Just lf -> let sourceName = logFilename lf
                                   numRuns    = length $ runs lf
                                   numSteps   = length steps
                                   numSols    = numSolutions steps in
                               statusbarPush statusbar statusCtx $
                                             printf "%s: run %d/%d; %d %s, %d %s, maximum tier %s."
                                                    sourceName n numRuns
                                                    numSteps (if numSteps == 1 then "step" else "steps")
                                                    numSols (if numSols == 1 then "solution" else "solutions")
                                                    (show $ maxTier steps))
                 rawLogViewBuffer <- textViewGetBuffer logView
                 textBufferSetText rawLogViewBuffer ""

                 renderTreeView  params steps treeStore
                 renderChronView params steps chronStore
                 writeIORef (activeRun ctx) (Just (n, steps))

setLog :: LogFile -> VisM ()
setLog lf =
    do runModel <- getRunListStore
       runView  <- getRunList
       ctx      <- ask
       mainWindow <- getMainWindow
       statusbar  <- getStatusbar
       statusCtx  <- getMainStatusCtx
       liftIO $ do windowSetTitle mainWindow $
                                  printf "resolver-visualize: %s" (logFilename lf)
                   let sourceName = logFilename lf
                       numRuns    = length $ runs lf
                   statusbarPop statusbar statusCtx
                   statusbarPush statusbar statusCtx $
                                 printf "%s: %d runs." sourceName numRuns
                   writeIORef (loadedFile ctx) $ Just LoadedLogFile { logFile = lf }
                   listStoreClear runModel
                   mapM_ (listStoreAppend runModel) $ (zip [1..] $ runs lf)
                   -- Make sure the first iterator is selected.
                   firstIter <- treeModelGetIterFirst runModel
                   selection <- treeViewGetSelection runView
                   (case firstIter of
                      Nothing -> return ()
                      Just i  -> treeSelectionSelectIter selection i)
       return ()

-- | Load a widget from the Glade file by name.
loadXmlWidget :: WidgetClass a => String -> (GObject -> a) -> IO (GladeXML, a)
loadXmlWidget widgetName cast =
    do x <- xmlNewWithRootAndDomain xmlFilename (Just widgetName) Nothing
       case x of
         Nothing -> error ("Can't load " ++ (show xmlFilename))
         Just xml -> do w <- xmlGetWidget xml cast widgetName
                        return (xml, w)

-- | Load the main window from the Glade file.
loadMainWindowXML :: IO (GladeXML, Window)
loadMainWindowXML = loadXmlWidget "main_window" castToWindow

-- | Load the About box from the Glade file.
loadAboutBoxXML :: IO (GladeXML, AboutDialog)
loadAboutBoxXML = loadXmlWidget "about_box" castToAboutDialog

-- | Load the loading progress window from the Glade file.
loadLoadingProgressXML :: IO (GladeXML, Window)
loadLoadingProgressXML = loadXmlWidget "window_load_progress" castToWindow

-- | Load the export params dialog from the Glade file.
loadParamsDialogXML :: IO (GladeXML, Dialog)
loadParamsDialogXML = loadXmlWidget "params_dialog" castToDialog

setupMainWindow :: GladeXML -> IO SourceView
setupMainWindow xml =
    do sourceViewHolder <- xmlGetWidget xml castToScrolledWindow "scrolledwindow_sourceview"
       sourceView <- sourceViewNew
       sourceViewSetShowLineNumbers sourceView True
       textViewSetEditable sourceView False
       containerAdd sourceViewHolder sourceView
       return sourceView

createMainWindowColumns :: GladeXML -> MainM (TreeViewColumnInfo, ChronologicalViewColumnInfo, RunListColumnInfo)
createMainWindowColumns xml =
    liftIO $ do treeViewCols  <- newTreeViewColumns
                chronViewCols <- newChronViewColumns
                runListCols   <- newRunListColumns
                return (treeViewCols, chronViewCols, runListCols)

createMainWindowStores :: TreeViewColumnInfo -> ChronologicalViewColumnInfo -> RunListColumnInfo -> IO (TreeViewStore, ChronViewStore, RunListStore)
createMainWindowStores treeViewInf chronViewInf runListInf = do
  treeModel    <- treeStoreNew []
  chronModel   <- listStoreNew []
  runModel     <- listStoreNew []
  -- Set up column bindings.
  let makeCol info model (getCol, getText) =
          setupTextColumn (getCol info) model
                          (\row -> [cellText := getText row])
      treeCols   = [(treeViewText,         entryColumnText),
                    (treeViewNumChoices,   entryColumnNumChoices),
                    (treeViewBrokenDeps,   entryColumnBrokenDeps),
                    (treeViewStepNum,      entryColumnStepNum),
                    (treeViewChildren,     entryColumnChildren),
                    (treeViewHeight,       entryColumnHeight),
                    (treeViewSubtreeSize,  entryColumnSubtreeSize),
                    (treeViewTier,         entryColumnTier),
                    (treeViewScore,        entryColumnScore),
                    (treeViewDep,          entryColumnDep)]
      chronCols  = [(chronViewNumChoices,  show . chronNumChoices),
                    (chronViewBrokenDeps,  show . chronBrokenDeps),
                    (chronViewStepNum,     show . chronStepNum),
                    (chronViewChildren,    show . chronChildren),
                    (chronViewHeight,      show . chronHeight),
                    (chronViewSubtreeSize, show . chronSubtreeSize),
                    (chronViewTier,        show . chronTier),
                    (chronViewScore,       show . chronScore),
                    (chronViewDep,         maybe "" pp . chronDep),
                    (chronViewParent,      maybe "" show . chronParent),
                    (chronViewChoice,      maybe "" pp . chronChoice)]
      runCols = [(runListNumber,     show . fst),
                 (runListLength,     show . length . snd)]
  mapM_ (makeCol treeViewInf treeModel) treeCols
  mapM_ (makeCol chronViewInf chronModel) chronCols
  mapM_ (makeCol runListInf runModel) runCols
  return (treeModel, chronModel, runModel)

treeSelectionChanged :: VisualizeContext -> TreeSelection -> TreeViewStore -> IO ()
treeSelectionChanged ctx selection model = do
  num <- treeSelectionCountSelectedRows selection
  (if num /= 1
   then runVis (stepSelected Nothing) ctx
   else do (Just selected) <- treeSelectionGetSelected selection
           path            <- treeModelGetPath model selected
           node            <- treeStoreLookup model path
           (case node of
              Nothing   -> runVis (stepSelected Nothing) ctx
              Just (Node {rootLabel = Root {entryStep = step}}) -> runVis (stepSelected (Just step)) ctx
              Just (Node {rootLabel = Step {entryStep = step}}) -> runVis (stepSelected (Just step)) ctx
              Just (Node {rootLabel = AlreadyGeneratedStep { entryTextStart  = start,
                                                             entryTextLength = len }})
                  -> runVis (showText start len) ctx
              _ -> runVis (stepSelected Nothing) ctx))

chronSelectionChanged :: VisualizeContext -> TreeSelection -> ChronViewStore -> IO ()
chronSelectionChanged ctx selection model = do
  num <- treeSelectionCountSelectedRows selection
  (if num /= 1
   then runVis (stepSelected Nothing) ctx
   else do (Just selected) <- treeSelectionGetSelected selection
           (i:_)           <- treeModelGetPath model selected
           entry           <- listStoreGetValue model i
           runVis (stepSelected $ Just $ chronStep entry) ctx)

runSelectionChanged :: VisualizeContext -> TreeSelection -> RunListStore -> IO ()
runSelectionChanged ctx selection model = do
  num <- treeSelectionCountSelectedRows selection
  (if num /= 1
   then runVis (setRun Nothing) ctx
   else do (Just selected) <- treeSelectionGetSelected selection
           (i:_) <- treeModelGetPath model selected
           run <- listStoreGetValue model  i
           runVis (setRun (Just run)) ctx)

setupMenus :: GladeXML -> Params -> VisualizeContext -> IO ()
setupMenus xml params ctx = do
  mainWin <- liftIO $ xmlGetWidget xml castToWindow "main_window"
  menuItemGraphviz <- liftIO $ xmlGetWidget xml castToMenuItem "menuitem_export_graphviz"
  menuItemQuit <- liftIO $ xmlGetWidget xml castToMenuItem "menuitem_quit"

  afterActivateLeaf menuItemGraphviz $ runVis (doExport mainWin) ctx
  afterActivateLeaf menuItemQuit $
                    do mainLoopQuit $ mainLoop $ mainContext ctx
  return ()
    where doExport mainWin =
              do activeRun <- getActiveRun
                 liftIO $ (case activeRun of
                             Nothing    -> return ()
                             Just steps -> do dlg <- makeParamsDialog params steps (doExport' mainWin steps)
                                              windowSetTransientFor (paramsDialog dlg) mainWin
                                              return ())
                 return ()

          doExport' mainWin steps params =
              do dlg <- fileChooserDialogNew Nothing (Just mainWin)
                            FileChooserActionOpen [(stockCancel, ResponseCancel), (stockSave, ResponseOk)]
                 afterResponse dlg (\response -> do (if response /= ResponseOk
                                                     then return ()
                                                     else do maybeFilename <- fileChooserGetFilename dlg
                                                             case maybeFilename of
                                                               (Just filename) -> writeDotRun params steps filename
                                                               Nothing -> return ())
                                                    widgetDestroy dlg)
                 widgetShow dlg

newCtx :: GladeXML -> Params -> MainM VisualizeContext
newCtx xml params =
    do mainCtx <- ask
       (treeViewColInf, chronViewColumnInf, runListColumnInf) <- createMainWindowColumns xml
       liftIO $ do sourceView <- setupMainWindow xml
                   treeView <- xmlGetWidget xml castToTreeView "tree_view_search_tree"
                   chronView <- xmlGetWidget xml castToTreeView "tree_view_search_history"
                   runView <- xmlGetWidget xml castToTreeView "tree_view_run_list"
                   mapM_ (treeViewAppendColumn treeView)      (treeViewColumnInfoColumns treeViewColInf)
                   mapM_ (treeViewAppendColumn chronView)     (chronViewColumnInfoColumns chronViewColumnInf)
                   mapM_ (treeViewAppendColumn runView)   (runListInfoColumns runListColumnInf)
                   (treeModel, chronModel, runModel) <-
                       createMainWindowStores treeViewColInf chronViewColumnInf runListColumnInf

                   statusbar <- xmlGetWidget xml castToStatusbar "main_statusbar"
                   mainWindow <- xmlGetWidget xml castToWindow "main_window"

                   mainStatusCtx <- statusbarGetContextId statusbar "Status"

                   lf <- newIORef Nothing
                   runRef <- newIORef Nothing

                   let ctx = VisualizeContext { treeView = treeView,
                                                treeViewColumnInfo = treeViewColInf,
                                                treeViewStore = treeModel,
                                                chronologicalView = chronView,
                                                chronologicalViewColumnInfo = chronViewColumnInf,
                                                chronologicalViewStore = chronModel,
                                                runList = runView,
                                                runListColumnInfo = runListColumnInf,
                                                runListStore = runModel,
                                                logView = sourceView,
                                                statusbar = statusbar,
                                                mainStatusCtx = mainStatusCtx,
                                                mainWindow= mainWindow,
                                                loadedFile = lf,
                                                params = params,
                                                activeRun = runRef,
                                                mainContext = mainCtx }

                   treeSelection <- treeViewGetSelection treeView
                   treeSelectionSetMode treeSelection SelectionSingle
                   afterSelectionChanged treeSelection (treeSelectionChanged ctx treeSelection treeModel)

                   chronSelection <- treeViewGetSelection chronView
                   treeSelectionSetMode chronSelection SelectionSingle
                   afterSelectionChanged chronSelection (chronSelectionChanged ctx chronSelection chronModel)

                   runSelection <- treeViewGetSelection runView
                   treeSelectionSetMode runSelection SelectionSingle
                   afterSelectionChanged runSelection (runSelectionChanged ctx runSelection runModel)

                   treeViewSetModel treeView treeModel
                   treeViewSetModel chronView chronModel
                   treeViewSetModel runView runModel

                   setupMenus xml params ctx

                   windowSetTitle mainWindow "resolver-visualize"

                   return ctx


newMainWindow :: Params -> MainM (GladeXML, VisualizeContext)
newMainWindow params =
    do mainContext <- ask
       (xml, mainWindow) <- liftIO $ loadMainWindowXML
       ctx <- newCtx xml params
       mainWindowOpened
       liftIO $ on mainWindow deleteEvent (liftIO $ doMainWindowClosed mainContext)
       return (xml, ctx)
    where doMainWindowClosed mainContext = do runMain mainWindowClosed mainContext
                                              return True

milliToPicoseconds n = n * 1000000000

-- | Load the given log file in a background thread, displaying the
-- progress in the foreground thread.  When it's done, pop up a new
-- main window.
--
-- TODO: support displaying in an existing main window.
load :: Params -> FilePath -> MainM ()
load params fn =
    do loadedFile <- liftIO $ do (xml, win)  <- loadLoadingProgressXML
                                 title       <- xmlGetWidget xml castToLabel "label_title"
                                 progressBar <- xmlGetWidget xml castToProgressBar "load_progress"
                                 status      <- xmlGetWidget xml castToLabel "label_statistics"
                                 -- Should be done in a separate thread but GHC/Gtk2HS
                                 -- suck for threaded GUI programming.  See
                                 --
                                 -- http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/1/
                                 --
                                 -- for (very gory) details.
                                 labelSetText title ("Loading " ++ fn ++ "...")
                                 widgetShow win
                                 h   <- openFile fn ReadMode
                                 lastTime <- newIORef Nothing
                                 log <- loadLogFile h fn (showProgress progressBar lastTime)
                                 widgetDestroy win
                                 return log
       (xml, ctx) <- newMainWindow params
       liftIO $ runVis (setLog loadedFile) ctx
       liftIO $ xmlGetWidget xml castToWindow "main_window" >>= widgetShowAll
       return ()
    where showProgress :: ProgressBar -> IORef (Maybe ClockTime) -> Integer -> Integer -> IO ()
          showProgress pb lastTime cur max =
              do oldf     <- progressBarGetFraction pb
                 currTime <- getClockTime
                 last     <- readIORef lastTime
                 writeIORef lastTime (Just currTime)
                 let updateInterval = TimeDiff { tdYear = 0,
                                                 tdMonth = 0,
                                                 tdDay = 0,
                                                 tdHour = 0,
                                                 tdMin = 0,
                                                 tdSec = 0,
                                                 tdPicosec = milliToPicoseconds 100 }
                     newf           = if max == 0 then 0 else ((fromInteger cur) / (fromInteger max))
                     longUpdate     = case last of
                                        Nothing -> True
                                        Just time -> diffClockTimes currTime time >= updateInterval
                     shouldUpdate   = longUpdate || newf == 1
                 when shouldUpdate (do progressBarSetFraction pb newf
                                       progressBarSetText pb (showFFloat (Just 1) (100 * newf) "" ++ "%")
                                       while (mainContextIteration mainContextDefault False) (return ()) ()
                                       return ())

filterUserParams :: [String] -> Params -> ([String], Params)
filterUserParams [] params = ([], params)
filterUserParams ("--max-steps":(n:args)) params = let params' = params { maxSteps = Just $ read n } in
                                                   filterUserParams args params'
filterUserParams ("--first-step":(n:args)) params = let params' = params { firstStep = Just $ read n } in
                                                    filterUserParams args params'
filterUserParams ("--show-promotions":args) params = let params' = params { showPromotions = True } in
                                                     filterUserParams args params'
filterUserParams ("--dot-output":(fn:args)) params = let params' = params { dotOutput = Just $ fn } in
                                                     filterUserParams args params'
filterUserParams ("--target-format":(fmt:args)) params =
    case reads fmt of
      []             -> error (printf "Unknown target format %s" (show fmt))
      ((fmt', _):_)  -> let params' = params { targetFormat = Just fmt' } in
                        filterUserParams args params'
filterUserParams (arg:args) params = let (args', params') = filterUserParams args params in
                                     (arg:args', params')

textProgress :: IORef Integer -> Integer -> Integer -> IO ()
textProgress ref cur max =
    do lastPercent <- readIORef ref
       (if cur >= (max * (lastPercent + 10) `div` 100)
        then do let newPercent = lastPercent + 10
                print newPercent
                writeIORef ref newPercent
        else return ())

makeTextProgress :: IO (Integer -> Integer -> IO ())
makeTextProgress = do ref <- newIORef 0
                      return $ textProgress ref

writeDotOutput params logFile outputFile =
    -- TODO: show progress better.
    do progress <- makeTextProgress
       withFile logFile ReadMode $ \h ->
           do log <- loadLogFile h logFile progress
              (if null $ runs log
               then return ()
               else if (null (drop 1 $ runs log))
                    then writeDotRun params (head $ runs log) outputFile
                    else sequence_ [ writeDotRun params steps (printf "%s-%d" outputFile n)
                                         | (steps, n) <- zip (runs log) ([1..] :: [Integer]) ])

data MaybeNumberEntry = MaybeNumberEntry { mnCheckbox :: CheckButton,
                                           mnSpinner  :: SpinButton }

setMaybeNumberEntry :: MaybeNumberEntry -> Maybe Integer -> IO ()
setMaybeNumberEntry (MaybeNumberEntry { mnCheckbox = cb, mnSpinner = sb }) val =
    do (case val of
          Nothing -> toggleButtonSetActive cb False
          Just n  -> do toggleButtonSetActive cb True
                        spinButtonSetValue sb $ fromIntegral n)
       widgetSetSensitive sb (isJust val)

getMaybeNumberEntry :: MaybeNumberEntry -> IO (Maybe Integer)
getMaybeNumberEntry entry =
    do cbActive <- toggleButtonGetActive (mnCheckbox entry)
       spinButtonValue <- spinButtonGetValueAsInt (mnSpinner entry)
       (if cbActive
        then return $ Just $ toInteger spinButtonValue
        else return Nothing)

makeMaybeNumberEntry :: CheckButton -> SpinButton -> Maybe Integer -> (Integer, Integer) -> IO MaybeNumberEntry
makeMaybeNumberEntry cb sb val (min, max) =
    do let rval = MaybeNumberEntry { mnCheckbox = cb, mnSpinner = sb }
       spinButtonSetRange sb (fromIntegral min) (fromIntegral max)
       setMaybeNumberEntry rval val
       -- Update the sensitivity of the spin-box based on whether the
       -- check-button is set.
       afterToggled cb $ do cbActive <- toggleButtonGetActive cb
                            widgetSetSensitive sb cbActive
       return rval

data ParamsDialog = ParamsDialog { paramsDialog :: Dialog,
                                   paramsHboxTruncateRun :: HBox,
                                   paramsTruncateRunNumberEntry :: MaybeNumberEntry,
                                   paramsLabelTruncateRunMaxSteps :: Label,
                                   paramsHboxSkipSteps :: HBox,
                                   paramsSkipStepsNumberEntry :: MaybeNumberEntry,
                                   paramsLabelSkipStepsMaxSteps :: Label,
                                   paramsCheckboxShowPromotions :: CheckButton,
                                   paramsOkButton :: Button,
                                   paramsCancelButton :: Button }

-- Should we ever say "out of %d step"?  I think the plural is always
-- right here?
stepLimitLabelText n = printf " out of %d steps" n

getTruncateEntryLimit :: Maybe Integer -> Integer -> Integer
getTruncateEntryLimit firstStep numSteps =
    max 0 $ numSteps - maybe 0 id firstStep

skipStepsLabelText :: Integer -> String
skipStepsLabelText numSteps = stepLimitLabelText numSteps

makeParamsDialog :: Params -> [ProcessingStep] -> (Params -> IO ()) -> IO ParamsDialog
makeParamsDialog params steps callback =
    do (xml, dialog) <- loadParamsDialogXML
       hboxTruncate <- xmlGetWidget xml castToHBox "hbox_truncate_run"
       cbTruncateRun <- xmlGetWidget xml castToCheckButton "checkbutton_do_truncate_run"
       sbTruncateRun <- xmlGetWidget xml castToSpinButton "spinbutton_truncate_run"
       labelTruncateRun <- xmlGetWidget xml castToLabel "label_truncate_run_max_steps"

       hboxSkip <- xmlGetWidget xml castToHBox "hbox_skip_first_steps"
       cbSkip <- xmlGetWidget xml castToCheckButton "checkbutton_do_skip_steps"
       sbSkip <- xmlGetWidget xml castToSpinButton "spinbutton_skip_steps"
       labelSkip <- xmlGetWidget xml castToLabel "label_skip_max_steps"

       cbShowPromotions <- xmlGetWidget xml castToCheckButton "checkbutton_show_promotions"

       ok <- xmlGetWidget xml castToButton "params_ok"
       cancel <- xmlGetWidget xml castToButton "params_cancel"

       -- Set some initial parameters.
       let numSteps = genericLength steps
           initialTruncateLimit = getTruncateEntryLimit (firstStep params) numSteps
       truncateRunEntry <- makeMaybeNumberEntry cbTruncateRun sbTruncateRun
                                                (maxSteps params) (0, initialTruncateLimit)
       skipEntry <- makeMaybeNumberEntry cbSkip sbSkip (firstStep params) (0, numSteps)
       labelSetText labelTruncateRun (stepLimitLabelText initialTruncateLimit)
       labelSetText labelSkip (skipStepsLabelText $ numSteps)
       toggleButtonSetActive cbShowPromotions (showPromotions params)

       -- When the number of skipped steps is changed, we have to
       -- update the range and text of the max-steps box.
       afterValueSpinned sbSkip (do firstStep <- getMaybeNumberEntry skipEntry
                                    let truncateLimit = getTruncateEntryLimit firstStep numSteps
                                    labelSetText labelTruncateRun (stepLimitLabelText truncateLimit)
                                    spinButtonSetRange sbTruncateRun 0 (fromIntegral truncateLimit))

       -- TODO: should pass in the ParamsDialog if we add any more
       -- widgets, rather than just tacking on parameters.
       afterResponse dialog (handleResponse dialog truncateRunEntry skipEntry cbShowPromotions)

       widgetShow dialog

       return $ ParamsDialog { paramsDialog = dialog,
                               paramsHboxTruncateRun = hboxTruncate,
                               paramsTruncateRunNumberEntry = truncateRunEntry,
                               paramsLabelTruncateRunMaxSteps = labelTruncateRun,
                               paramsHboxSkipSteps = hboxSkip,
                               paramsSkipStepsNumberEntry = skipEntry,
                               paramsLabelSkipStepsMaxSteps = labelSkip,
                               paramsCheckboxShowPromotions = cbShowPromotions,
                               paramsOkButton = ok,
                               paramsCancelButton = cancel }
  where handleResponse dialog truncateRunEntry skipEntry showPromotionsButton ResponseOk =
            do maxSteps <- getMaybeNumberEntry truncateRunEntry
               firstStep <- getMaybeNumberEntry skipEntry
               showPromotions <- toggleButtonGetActive showPromotionsButton
               let params' = params { maxSteps = maxSteps,
                                      firstStep = firstStep,
                                      showPromotions = showPromotions }
               callback params'
               widgetDestroy dialog
               return ()
        handleResponse dialog _ _ _ _ =
            do widgetDestroy dialog
               return ()

main :: IO ()
main = do -- Gtk2Hs whines loudly if it gets loaded into a threaded
          -- runtime, but runhaskell always loads a threaded runtime,
          -- so we have to call this to be a script:
          unsafeInitGUIForThreadedRTS
          mainWindows <- newIORef 0
          mainLoop <- mainLoopNew Nothing False
          let mainLoopContext = MainLoopContext { numMainWindows = mainWindows,
                                                  mainLoop = mainLoop }
          args <- getArgs
          let (args', params) = filterUserParams args defaultParams
          case args' of
            [] -> do (xml, ctx) <- runMain (newMainWindow params) mainLoopContext
                     mainWin <- xmlGetWidget xml castToWindow "main_window"
                     widgetShow (toWidget mainWin)
                     mainLoopRun mainLoop
            [filename] ->
                case params of
                  Params { dotOutput = Just output } -> writeDotOutput params filename output
                  _ -> runMain (load params filename) mainLoopContext >> mainLoopRun mainLoop
            otherwise -> error "Too many arguments; expected at most one (the log file to load)."

