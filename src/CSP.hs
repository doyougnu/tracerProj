module CSP where

import Data.Map as M
import Data.List as L
import Data.Ord as O
import Control.Monad.State
import Control.Arrow (second)

-- | Domains are a list of possible values for a particular variable
type Domain var val = Map var [val]

-- | A Successor is a list of neighboring variables give a variable
type Neighbors var = Map var [var]

-- | An Assignment is a value for a given variable
type Assignments var val = Map var val

class (Ord var, Show var, Show val, Eq val) => CSP csp var val where
  vars :: csp var val -> [var]                    -- all the variables for CSP
  domains :: csp var val -> Domain var val        -- The values the vars can take
  neighbors :: csp var val -> Neighbors var       -- Other variables that react in constraints

  -- representing constrains as a function, given a csp problem this takes a var
  -- x, a value x_val, a var y, a value y_val, and returns a bool if the
  -- constraint is satisfied or not, the
  constraint :: csp var val -> var -> val -> var -> val -> Bool

-- | A CSP State hold the current Domains of the variables, a Map of variables to
--   a list of pairs that denotes the pruned (vars, values) for that var
--   and an Assignments map
type CSPState var val = (Domain var val, Map var [(var, val)], Assignments var val)

-- | Just lifting the CSP state into the state monad
type CSPStateM var val res = State (CSPState var val) res

-- | Getters, Setters, Modifiers
getDomain :: MonadState (a, b, c) m => m a
--functions are functors so we can map on them!
getDomain = fmap (\(x,_,_) -> x) get 

modifyDomain :: MonadState (a, b, c) m => (a -> a) -> m ()
modifyDomain f = modify $ \(a, b, c) -> (f a, b, c)

getPruned :: MonadState (a, b, c) m => m b
getPruned = fmap (\(_,b,_) -> b) get

putPruned :: MonadState (a, b, c) m => b -> m ()
putPruned p = get >>= (\(a,_,c) -> put (a,p,c))

modifyPruned :: MonadState (a, b, c) m => (b -> b) -> m ()
modifyPruned f = modify (\(a,b,c) -> (a, f b, c))

getAssigned :: MonadState (a, b, c) m => m c
getAssigned = fmap (\(_,_,c) -> c) get

putAssigned :: MonadState (a, b, c) m => c -> m ()
putAssigned p = get >>= (\(a,b,_) -> put (a,b,p))

modifyAssigned :: MonadState (a, b, c) m => (c -> c) -> m ()
modifyAssigned f = modify (\(a,b,c) -> (a, b, f c))

-- | Assignment, Unassignment
assign :: CSP csp var val => csp var val -> var -> val -> CSPStateM var val ()
assign csp var val = do modifyAssigned $ M.insert var val

-- | remove an assigned variable from assignment map, add variable back to domain
-- | with all values considered
unAssign :: CSP csp var val => csp var val -> var -> CSPStateM var val ()
unAssign csp var = do
  modifyAssigned $ M.delete var
  modifyDomain $ M.insert var (domains csp ! var)

-- | filter all values that don't satisfy constraints from assignment list, the
-- | Check that assignment list is empty
hasConflicts :: CSP csp var val => csp var val
             -> var -> val
             -> Assignments var val
             -> Bool
hasConflicts csp var val assgn =
  not . isEmpty $ M.filterWithKey conflictPredicate assgn
  where conflictPredicate y yVal = not $ constraint csp var val y yVal
        isEmpty = M.null

-- | Necessary Helpers
allDone :: CSP csp var val => csp var val -> Assignments var val -> Bool
allDone csp dict = length (vars csp) == length (M.keys dict)

goalTest :: CSP csp var val => csp var val -> Assignments var val -> Bool
goalTest csp assgn = allDone csp assgn && all noConflicts (vars csp)
  where noConflicts var = not $ hasConflicts csp var val assgn
          where val = assgn ! var

-- | Return the most constrained variable based on number of values left in its
-- Domain
getMostConstrained :: (Ord var, Show var) => [var] -> CSPStateM var val var
getMostConstrained vars =
  do domain <- getDomain
     assigned <- getAssigned
     let possibleVars = [posVar | posVar <- vars, M.notMember posVar assigned]
         mostConVar = fst . minimumBy (O.comparing snd) .
                           L.filter (flip L.elem possibleVars . fst) .
                           fmap (second length) $ M.toList domain
     return mostConVar

-- | Backtrace search
backtrace :: (CSP csp var val, Ord val) => csp var val ->
             CSPStateM var val (Maybe (Assignments var val))
backtrace csp = do
  assignment <- getAssigned
  if goalTest csp assignment
    then return $ Just assignment
    else do
    newVar <- getMostConstrained $ vars csp
    domain <- getDomain
    let varVals = domain ! newVar
    recur newVar varVals

  where recur _    []        = return Nothing
        recur var (val:vals) = do
          savedState <- get
          assignment <- getAssigned
          if noConflicts var val assignment
            then do assign csp var val
                    res <- backtrace csp
                    case res of
                      Nothing -> unAssign csp var >>
                                 put savedState >>
                                 recur var vals
                      res -> return res
            else recur var vals

        noConflicts var val assgn = not $ hasConflicts csp var val assgn

runStateM :: Ord var =>
             CSPStateM var val res ->
             Domain var val ->
             (res, CSPState var val)
runStateM st domain = runState st (domain, pruned, assigns)
  where
    pruned = M.fromList $ zip (M.keys domain) (repeat [])
    assigns = M.empty

runBacktrace :: (CSP csp var val, Ord val) => csp var val -> (Maybe (Assignments var val), CSPState var val)
runBacktrace csp = runStateM (backtrace csp) (domains csp)
