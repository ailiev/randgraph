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



main = do (action:args) <- getArgs
          case action of
            "generate"  -> 
                do randgen   <- newStdGen
                   let [v,d,wmin,wmax] = map read args
                       params    = GParams { v = v, d = d, w_range = (wmin,wmax) }
                       graph     = randGraph randgen params
                   print graph

            "c" ->              -- make input for the C program
                   do g <- getContents >>= readIO
                      putStr $ printGraph_C g

            "sfdl" ->           -- make an input for the sfdl Dijkstra program
                   do g <- getContents >>= readIO
                      putStr $ printGraph_SFDL g

            "gviz" ->
                do g <- getContents >>= readIO
                   putStr $ printGraph g

            "sp" ->
                do g <- getContents >>= readIO
                   let gr       = g2g g
                       [s,t]    = map read args -- the source and target
                       path     = sp s t gr
                       w        = spLength s t gr
                   -- print the path in reverse
                   putStrLn $ concat $ intersperse " " $ map show $ reverse path
                   putStrLn $ "weight: " ++ show w
                   


data GParams = GParams { v :: Int,      -- number of vertices
                         d :: Int,      -- max out-degree
                         w_range :: (Int,Int) -- range of edge weights
                       }

type Node = Int
type Edge = (Node, Int)         -- destination and weight


-- a list of adjacency lists
type Graph = [[Edge]]

outEdges :: (RandomGen g) => GParams -> g -> [Edge]
outEdges params g = let (numEs, g2) = randomR (1,d params) g
                        (g3, g4)    = split g2
                        (wmin,wmax) = w_range params
                    in
                      take numEs $ zip (nub $ randomRs (0,v params - 1) g3)
                                       (randomRs (wmin,wmax) g4)

randGraph :: (RandomGen g) => g -> GParams -> Graph
randGraph g params = let gs     = take (v params) $ randomGenList g
                     in
                       map (outEdges params) gs



randomGenList :: (RandomGen g) => g -> [g]
randomGenList g = iterateList (tuple2list2 . split) g


-- *****************
-- serializing the graph to be given to the SFDL Dijkstra program
-- *****************

-- cSFDL_V             = 7
cSFDL_MAX_OUT_DEG   = 5

printGraph_SFDL ees =
    let -- a flat list of edges, cSFDL_MAX_OUT_DEG per vertex, padded with (-1,-1) pairs
        --    at the end
        (dests, ws) = unzip $ concatMap (padWithTo (-1,-1) cSFDL_MAX_OUT_DEG) ees
        myShow = (++ "\n") . show
    in
      concat $ map myShow ws ++ map myShow dests

-- interleave dests and weights, and mark the end of an edge list with a (-1,-1) pair
printGraph_C ees =
    let (dests, ws) = unzip $ concatMap (++ [(-1,-1)]) ees
        myShow i = let s = show i
                   -- HACK: padding to 10 chars here, as expected in read_num()
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
