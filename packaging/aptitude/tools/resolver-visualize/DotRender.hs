module DotRender(
                 writeDotRun
                ) where

import Data.List
import Data.Maybe
import Dot
import Resolver.Log
import Resolver.PrettyPrint
import Resolver.Types
import System.IO
import Text.Printf
import Types
import qualified Data.Set as Set


inBounds :: Params -> Integer -> Bool
inBounds params n = let first = maybe 0 id (firstStep params) in
                    n >= first && maybe True (\max -> n < first + max) (maxSteps params)

choiceText :: LinkChoice -> String
choiceText (LinkChoice (InstallVersion ver _)) = "Install " ++ pp ver
choiceText (LinkChoice (BreakSoftDep d)) = "Break " ++ pp d
choiceText Unknown = "(...)"

dotChoiceLabel :: LinkChoice -> String
dotChoiceLabel lc@(LinkChoice c) = choiceText lc
dotChoiceLabel Unknown           = ""

inferTargetFormat :: Params -> TargetFormat
inferTargetFormat (Params { targetFormat = fmt,
                            dotOutput    = output }) =
    case fmt of
      Nothing -> PS
      Just fmt' -> fmt'

cloudImage :: Params -> String
cloudImage params =
    case inferTargetFormat params of
      PS -> "cloud.eps"
      PNG -> "cloud.png"

dotStepNode :: Params -> ProcessingStep -> Node
dotStepNode params step = node (name $ printf "step%d" (stepOrder step))
                          <<< set "label" (printf "Step: %d\nScore: %d\nTier: %s"
                                           (stepOrder step)
                                           (solScore $ stepSol step)
                                           (show $ solTier $ stepSol step))
                          <<< Set.null (solBrokenDeps (stepSol step)) `thenDo`
                              set "style" "filled" `andAlso`
                              set "peripheries" "2" `andAlso`
                              set "fillColor" "lightgrey"

-- Generate nodes for any successors that were not processed in the
-- render.
dotUnprocessedSuccs :: Params -> ProcessingStep -> [Node]
dotUnprocessedSuccs params step = unprocessed ++ excluded
    where unprocessed = [ node (name $ printf "step%dunproc%d" (stepOrder step) stepNum)
                          <<< set "label" (printf "Unprocessed\nScore: %d\nTier: %s"
                                           (solScore succSol)
                                           (show $ solTier succSol))
                          <<< set "style" "dashed"
                          <<< Set.null (solBrokenDeps (stepSol step)) `thenDo`
                              set "style" "dashed,filled" `andAlso`
                              set "peripheries" "2" `andAlso`
                              set "fillcolor" "lightgrey"
                          | ((Unprocessed { successorChoice    = succChoice,
                                            successorSolution  = succSol }),
                             stepNum)
                          <- zip (stepSuccessors step) ([0..] :: [Integer]) ]
          excluded    = [ node (name $ printf "step%d" (stepOrder step))
                          <<< set "label" (printf "Step %d+\n%d nodes..." (stepOrder step) (stepBranchSize step))
                          <<< set "shape" "plaintext"
                          <<< set "image" (cloudImage params)
                          | (Successor { successorStep = step }) <- stepSuccessors step,
                            not $ inBounds params (stepOrder step) ]

-- | If the parent of the given step (or of one of its
-- backpropagations) was excluded from the render, build and return a
-- node for it.
--
-- TODO: should show links between excluded nodes, etc...that will
-- need a bit of an overhaul though.
dotExcludedIndices :: Params -> ProcessingStep -> [Integer]
dotExcludedIndices params step =
    (maybeToList $ do (ParentLink {parentLinkParent = parentStep}) <- stepPredecessor step
                      (if inBounds params $ stepOrder parentStep
                       then fail "Not an excluded step."
                       else return $ stepOrder parentStep))
    ++
    [ parentStepNum
      | Backpropagation {
          backpropagationStep =
              ProcessingStep {
                stepOrder = parentStepNum
          } } <- stepBackpropagations step,
        not $ inBounds params parentStepNum ]

dotExcludedParentNode :: Params -> Integer -> Node
dotExcludedParentNode params stepNum = node (name $ printf "step%d" stepNum)
                                       <<< set "label" (printf "Step %d" stepNum)
                                       <<< set "shape" "plaintext"
                                       <<< set "image" (cloudImage params)

dotPromotions params step =
    if not $ showPromotions params
    then []
    else [ node (name $ printf "step%dpromotion%d" (stepOrder step) promotionNum)
           <<< set "label" (makeLabel promotion)
           <<< set "shape" "oval"
           | (promotion, promotionNum) <- zip (Set.toList $ stepPromotions step) ([0..] :: [Integer]) ]
         ++
         [ node (name $ printf "step%dbackprop%d" (stepOrder step) backpropNum)
           <<< set "label" (makeLabel $ backpropagationPromotion backprop)
           <<< set "shape" "oval"
           <<< backpropagationRedundant backprop `thenDo`
               set "style" "dashed"
           | (backprop, backpropNum) <- zip (stepBackpropagations step) ([0..] :: [Integer]) ]
    where makeLabel p = if Set.size (promotionChoices p) <= 5
                        then printf "%s\n%s"
                                 (show $ promotionTier p)
                                 (concat $ intersperse "\n"
                                             [pp c | c <- Set.toList $ promotionChoices p])
                        else printf "%s\n%d choices..."
                                 (show $ promotionTier p)
                                 (Set.size $ promotionChoices p)

dotEdges params step = cutIncoming ++ processed ++ unprocessed ++ promotions ++ backprops
    where processed   = [ edge (node (name $ printf "step%d" (stepOrder step)))
                               (node (name $ printf "step%d" (stepOrder step')))
                          <<< set "label" (dotChoiceLabel succChoice)
                          <<< forced `thenDo`
                              -- This gives us an arrow drawn with two
                              -- parallel lines.
                              set "color" "black:black"
                          | Successor { successorStep   = step',
                                        successorChoice = succChoice,
                                        successorForced = forced } <- stepSuccessors step ]
          unprocessed = [ edge (node (name $ printf "step%d" (stepOrder step)))
                               (node (name $ printf "step%dunproc%d" (stepOrder step) stepNum))
                          <<< set "label" (dotChoiceLabel succChoice)
                          <<< forced `thenDo` set "color" "black:black"
                          | ((Unprocessed { successorChoice = succChoice,
                                            successorForced = forced  }), stepNum)
                              <- zip (stepSuccessors step) ([0..] :: [Integer]) ]
          promotions  = if (not $ showPromotions params) || (Set.null $ stepPromotions step)
                        then []
                        else [ edge (node (name $ printf "step%d" (stepOrder step)))
                                    (node (name $ printf "step%dpromotion%d" (stepOrder step) promotionNum))
                               | promotionNum <- [0..((Set.size $ stepPromotions step) - 1)] ]
          backprops   = let attrs = set "color" "red" `andAlso`
                                    set "style" "dashed" `andAlso`
                                    set "constraint" "false" in
                        if (not $ showPromotions params) || (null $ stepBackpropagations step)
                        then []
                        -- Temporal edges to backpropagations.
                        else [edge (node (name $ printf "step%d" (stepOrder step)))
                                   (node (name $ printf "step%dbackprop0" (stepOrder step)))
                              <<< attrs]
                             ++
                             [edge (node (name $ printf "step%dbackprop%d" (stepOrder step) backpropNum))
                                   (node (name $ printf "step%dbackprop%d" (stepOrder step) (backpropNum + 1)))
                              <<< attrs
                              | backpropNum <- [0..((length $ stepBackpropagations step) - 2)] ]
                             ++
                             -- Structural edges to backpropagations.
                             [ edge (node (name $ printf "step%d" (stepOrder $ backpropagationStep backprop)))
                                    (node (name $ printf "step%dbackprop%d" (stepOrder step) backpropNum))
                               | (backprop, backpropNum) <- zip (stepBackpropagations step) ([0..] :: [Integer]) ]

          cutIncoming = [ edge (node (name $ printf "step%d" (stepOrder parentStep)))
                               (node (name $ printf "step%d" (stepOrder step)))
                          <<< set "label" (dotChoiceLabel choice)
                          <<< forced `thenDo` set "color" "black:black"
                          | ParentLink { parentLinkAction = choice,
                                         parentLinkForced = forced,
                                         parentLinkParent = parentStep }
                              <- maybeToList $ stepPredecessor step,
                            not $ inBounds params $ stepOrder parentStep ]

dotOrderEdges steps =
    [ edge (node (name $ printf "step%d" (stepOrder step1)))
           (node (name $ printf "step%d" (stepOrder step2)))
      <<< set "constraint" "false"
      <<< set "style" "dotted"
      <<< set "color" "blue"
      | (step1, step2) <- zip steps (drop 1 steps) ]

renderDot :: Params -> [ProcessingStep] -> Digraph
renderDot params steps =
    let droppedSteps   = maybe steps (\n -> genericDrop n steps) (firstStep params)
        truncatedSteps = maybe droppedSteps (\n -> genericTake n droppedSteps) (maxSteps params) in
    if null truncatedSteps
    then error "No steps to render."
    else let stepNodes          = map (dotStepNode params) truncatedSteps
             unprocessed        = concat $ map (dotUnprocessedSuccs params) truncatedSteps
             excludedParentIndices = concat $ map (dotExcludedIndices params) truncatedSteps
             excludedParentIndicesUnique = Set.toList $ Set.fromList excludedParentIndices
             excludedParents    = map (dotExcludedParentNode params) excludedParentIndicesUnique
             promotions         = concat $ map (dotPromotions params) truncatedSteps
             stepEdges          = concat $ map (dotEdges params) truncatedSteps
             orderEdges         = dotOrderEdges truncatedSteps in
         digraph (stepNodes ++ excludedParents ++
                  unprocessed ++ promotions) (stepEdges ++ orderEdges)

writeDotRun params steps outputFile =
    do let dot = renderDot params steps
       withFile outputFile WriteMode $ \h ->
           hPutStrLn h (show dot)
