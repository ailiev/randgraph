-- parameters:
-- - number of vertices,
-- - out-degree (uniform distribution on [0..d]?)
-- - weight range

module Main where

import List
import Random
import System
import Text.Printf

import qualified Data.Graph.Inductive.Graph         as Gr
import qualified Data.Graph.Inductive.Graphviz      as GViz
import qualified Data.Graph.Inductive.Tree          as TrGr
import           Data.Graph.Inductive.Query.SP      (sp, spLength)

import SashoLib
import MonadRandom


usage =         do progname <- getProgName
                   putStrLn $ "Usage: " ++ progname ++ " <command>"
                   putStrLn "Commands:"
                   putStrLn "generate <V> <D> <w_min> <w_max> > graph-file"
                   putStrLn "c < graph-file"
                   putStrLn "sfdl < graph-file"
                   putStrLn "gviz < graph-file"
                   putStrLn "sp <src> <dest> < graph-file"

main =
    do cmd_args <- getArgs
       if null cmd_args
        then usage
        else
         let (action:args) = cmd_args
         in
           case action of
            "generate"  -> 
                do randgen   <- newStdGen
                   let [v,d,wmin,wmax] = map read args
                       params    = GParams { v = v, d = d, w_range = (wmin,wmax) }
                       graph     = randGraph params randgen
                   print (params,graph)

            "c" ->              -- make input for the C program
                   do (_ :: GParams, g)    <- getContents >>= readIO
                      putStr $ printGraph_C g

            "sfdl" ->           -- make an input for the sfdl Dijkstra program
                   do (params, g)           <- getContents >>= readIO
                      putStr $ printGraph_SFDL params g

            "gviz" ->
                do (_ :: GParams, g)        <- getContents >>= readIO
                   putStr $ printGraph g

            "sp" ->
                do (_ :: GParams, g)        <- getContents >>= readIO
                   let gr       = g2g g
                       [s,t]    = map read args -- the source and target
                       path     = sp s t gr
                       w        = spLength s t gr
                   -- print the path in reverse
                   putStrLn $ concat $ intersperse " " $ map show $ reverse path
                   putStrLn $ "weight: " ++ show w

            "help"      -> usage
            _           -> usage

                   


data GParams = GParams { v :: Int,      -- number of vertices
                         d :: Int,      -- max out-degree
                         w_range :: (Int,Int) -- range of edge weights
                       }
               deriving (Show,Read)

type Node = Int
type Edge = (Node, Int)         -- destination and weight


-- a list of adjacency lists
type Graph = [[Edge]]


-- generate a random list of out-edges.
-- WANT: number of edges normally distributed with mean = D, and P[x > 4D/3] < 10%
-- From tables, 1.3*sigma = D/3, so set sigma to D/4 (approx)
--
-- weights and destinations uniformly chosen.
outEdges :: (MonadRandom m) => GParams -> m [Edge]
outEdges params   = do let u        = fromIntegral $ d params -- mean
                           s        = u/4 -- deviation
                           (wmin,wmax) = w_range params
                       z            <- randNormal -- standard normal
                       -- apply our mean and std. deviation
                       let x        = s*z + u
                       -- just truncate values which exceed (4/3)*mean
                       -- should be about 10% of values, which will be truncated.
                       -- IMPORTANT: the 4/3 constant appears in dijkstra.sfdl too; and in
                       -- printGraph_SFDL here.
                           numEs    = floor $ min x ((4/3)*u)
                       -- The destinations should be unique.
                       --
                       -- BUMMER: cannot create a stream, extract it from the monad, and then
                       -- take the needed number of elements. apparently the laziness
                       -- does not cross monad extraction?
                       dest_stream  <- (replicateM (numEs*3) $ getRandomR (0,v params - 1))
                                       >>== (take numEs . nub)
                       w_stream     <- (replicateM numEs $ getRandomR (wmin,wmax))
                       return $ zip dest_stream w_stream


randGraphMR :: (MonadRandom m) => GParams -> m Graph
randGraphMR   params    = do outEdges <- replicateM (v params) (outEdges params)
                             return outEdges
                               
randGraph :: (RandomGen g) => GParams -> g -> Graph
randGraph params rand = evalRand (randGraphMR params) rand



randomGenList :: (RandomGen g) => g -> [g]
randomGenList g = iterateList (tuple2list2 . split) g

-- a Normally distributed random variable, with mean=0 and sigma=1
randNormal :: (MonadRandom m) => m Float
randNormal        = do [u,v] <- replicateM 2 getRandom -- uniform randoms on [0,1)
                       -- and now use the Box-Muller Transformation; x and y are
                       -- independent and with a standard normal density. we just use x
                       -- which wastes randomness a bit.
                       let x = sqrt (-2 * log u) * cos (2*pi*v)
                           y = sqrt (-2 * log u) * sin (2*pi*v)
                       return x


-- *****************
-- serializing the graph to be given to the SFDL Dijkstra program
-- *****************

printGraph_SFDL params ees =
    let -- a flat list of edge weights, of a fixed length, padded with (-1,-1) pairs
        -- at the end; then same thing for edge destinations.
        -- IMPORTANT: the 4/3 constant is also used in dijkstra.sfdl, and in the
        -- 'outEdges' function above.
        max_edges   = ((d params) * 4) `div` 3
        (dests, ws) = unzip $ concatMap (padWithTo (-1,-1) max_edges) ees
        myShow = (++ "\n") . show
    in
      concat $ map myShow ws ++ map myShow dests

-- interleave dests and weights, and mark the end of an edge list with a (-1,-1) pair
printGraph_C ees =
    let (dests, ws) = unzip $ concatMap (++ [(-1,-1)]) ees
        myShow i = let s = show i
                   -- HACK: padding with space to 10 chars here, as expected in read_num()
                   -- in the ORAM "operating system"
                   in s ++ (replicate (9 - length s) ' ') ++ "\n"
    in
      concatMap myShow $ interleave dests ws


padWithTo pad len l = l ++ (replicate (len - length l) pad)


{-    
-- *****************
-- serializing the graph to C code for our graph implementation
-- *****************

data CTempls = CTempls { toplevel, edges, edge :: String }


printGraph_C :: CTempls -> Graph -> String
printGraph_C templs ees =
    let numV        = length ees
        args        = zip [0..numV - 1]
                          ees
        edges_stms  = concatMap (printEdges_C templs) args
    in
      printf (toplevel templs) numV edges_stms
          


printEdges_C :: CTempls -> (Int,[Edge]) -> String
printEdges_C templs (v,es) =
    let edge_stms = concatMap (printEdge_C templs) es
    in
      printf (edges templs) v edge_stms


printEdge_C :: CTempls -> Edge -> String
printEdge_C templs (dest,w) = printf (edge templs) dest w

-}


-- ********************
-- producing a Data.Graph.Inductive.Graph.Graph from one of our Graph's
-- ********************

g2g :: Graph -> TrGr.Gr () Int
g2g ees = let numV  = length ees
              nodes = [(i,()) | i <- [0 .. numV-1]]
              foos  = [(u,es) | u      <- [0 .. numV-1]
                              | es     <- ees]
              edges = concat $ map (\(u,es) -> [(u,v,w) | (v,w) <- es]) foos

          in
            Gr.mkGraph nodes edges


printGraph :: Graph -> String
printGraph = GViz.graphviz' . g2g
