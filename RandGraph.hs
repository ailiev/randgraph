-- code to randomly generate and manipulate graphs, for the graph-search SFDL and C
-- programs in Faerieplay.

-- parameters:
-- - number of vertices,
-- - max out-degree
-- - weight range

-- REQUIRES:    MonadRandom.hs
--              Faerieplay.UDraw
--              Faerieplay.SashoLib
--              Json library

module Main where

import List
import Random
import System
import Text.Printf

import qualified Data.Graph.Inductive.Graph         as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Graphviz      as GViz
import qualified Data.Graph.Inductive.Tree          as TrGr
import           Data.Graph.Inductive.Query.SP      (sp, spLength)
import qualified Data.Graph.Inductive.Query.BCC     as GrBCC

import              Text.PrettyPrint                as PP

import Maybe                                        (fromJust)

import Faerieplay.UDraw                             as UDraw
import Faerieplay.SashoLib

import MonadRandom

import Json.Abs
import Json.Lex
import Json.Par
import Json.ErrM


usage =         do progname <- getProgName
                   putStrLn $ "Usage: " ++ progname ++ " <command>"
                   putStrLn "******Commands"
                   putStrLn "generate <V> <max out degree> <w_min> <w_max> > graph-file"
                   putStrLn "from-json < jsonfile > graph-file"
                   putStrLn "from-manual < manual-file > graph-file"
                   putStrLn "***Shortest path"
                   putStrLn "sp <src> <dest> < graph-file"
                   putStrLn "***Convert to other formats"
                   putStrLn "c < graph-file			[convert to input for C/ORAM dijkstra porgram]"
                   putStrLn "json < graph-file		[convert to C-Json format for SFDL dijsktra]"
                   putStrLn "***Visualizations"
                   putStrLn "gviz < graph-file"
                   putStrLn "udg < graph-file"

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
                       params    = GParams { v = v, deg = d, w_range = (wmin,wmax) }
                       graph     = randGraph params randgen
                   print (params,graph)
            "from-json"  ->
                do json     <- getContents
                   let (params,graph) = jsonstr2gr json
                   print (params,graph)
            "from-manual"   ->
                do man_gr   <- getContents >>= readIO
                   let (params,graph) = manual2gr man_gr
                   print (params,graph)
            "bcc"           ->
                do (_ :: GParams, g)        <- getContents >>= readIO
                   print $ map Gr.nodes $ GrBCC.bcc (g2g g)

            "c" ->              -- make input for the C program
                   do (_ :: GParams, g)    <- getContents >>= readIO
                      putStr $ printGraph_C g

            "sfdl" ->           -- make an input for the sfdl Dijkstra program
                   do (_ :: GParams, g)           <- getContents >>= readIO
                      putStr $ printGraph_SFDL g

            "gviz" ->
                do (_ :: GParams, g)        <- getContents >>= readIO
                   putStr $ printGraph g

            "udg"   ->
                do (_ :: GParams, g)        <- getContents >>= readIO
                   let g' = g2g g
                       -- starts = map Gr.node' $ GrBas.gsel ((== 0) . Gr.indeg') g'
                       starts = [0..7]
                   putStr $ PP.render $ doc $ UDraw.makeTerm (show)
                                                             (\v -> [("OBJECT",
                                                                      show v)]
                                                             )
                                                             starts
                                                             g'

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

-- has the same fields as the Vertex struct in dijkstra.sfdl
data SFDLVertex =   SFDLVertex { -- we will fill these in:
                                 num, edge_list_head, num_out_edges :: Int,

                                 -- and leave these at default values, to be filled in
                                 -- by the SFDL code.
                                 d, pi_idx, heap_idx :: Int
                                 }
                                 deriving (Show)

data SFDLEdge =     SFDLEdge { -- fill both in
                               dest_idx, w :: Int
                             }
                    deriving (Show)

data SFDLGraph =    SFDLGraph { --  
                                vs  :: [SFDLVertex],
                                es  :: [SFDLEdge]
                              }
                    deriving (Show)


data GParams = GParams { v :: Int,      -- number of vertices
                         deg :: Int,      -- max out-degree
                         w_range :: (Int,Int) -- range of edge weights
                       }
               deriving (Show,Read)



jsonstr2gr :: String -> (GParams, Graph)
jsonstr2gr str  = let ts        = myLexer str
                      parse     = pTopLevelT ts
                      toplevel  = case parse of (Ok p)  -> p
                                                (Bad s) -> error $ "Parse error: " ++ s
                      in json2gr toplevel

-- extract a Graph from a parsed C-Json TopLevel
json2gr :: TopLevelT -> (GParams, Graph)
json2gr tl      = let fields    = simplifyFields tl
                      vs        = fromJustMsg "Vs" $ lookup "Vs" fields
                      es        = fromJustMsg "Es" $ lookup "Es" fields
                      out_edge_counts   = map (extrIntField "num_out_edges") vs
                      -- simplify the edges to just pairs.
                      edges     = [ (dest, w) | dest    <- map (extrIntField "dest_idx") es
                                              | w       <- map (extrIntField "w") es]
                      -- split edge list into list of edge-lists, one for each vertex.
                      edge_list = guidedSplit out_edge_counts edges
                  in  manual2gr [(v_nums, es) | es <- edge_list
                                              | v_nums <- map (extrIntField "num") vs]


-- split a list into pieces, the size of each being dictated by the guide-list
guidedSplit :: [Int]            -- ! The guide list - how large is each split
            -> [a]              -- ! The list
            -> [[a]]            -- ! The split list.
guidedSplit (t:ts) xs   = let (head, tail)  = splitAt t xs
                          in  head : guidedSplit ts tail
guidedSplit [] _        = []

                      
                      

simplifyFields :: TopLevelT -> [(String, [Value])]
simplifyFields (TopLevel objs)
    = let (Object tl1)  = head objs -- the first toplevel
          [Assoc (Ident "G") val] = tl1 -- (Gr field (the only one)
          (ObjectVal (Object assocs))   = val
      in [(name, vals) | (Assoc (Ident name) (SListVal (SList vals)))   <- assocs]


-- extract an integer field from an object value
extrIntField :: String -> Value -> Int
extrIntField name (ObjectVal (Object assocs)) =
    let (Assoc _ (NumVal (NumInt i)))   = fromJustMsg ("getting field " ++ name) $
                                          find (\x -> case x of
                                                        (Assoc kuku _)
                                                            | kuku == (Ident name)  -> True
                                                        _                           -> False)
                                                assocs
    in  fromInteger i                                 
                                                      


-- class for types which can be printed to a C-Json string.
class CJsonDocable a where
    docCJson :: a -> PP.Doc



-- quick kludges.
-- TODO: provide functions to serialize an Object, Array, Assoc, etc.
instance CJsonDocable SFDLVertex where
    docCJson x = PP.braces (PP.hcat
                                  (PP.punctuate PP.comma
                                         [ text "num=" <> int (num x)
                                         , text "edge_list_head=" <> int (edge_list_head x)
                                         , text "num_out_edges=" <> int (num_out_edges x)
                                         , text "d=" <> int (d x)
                                         , text "pi_idx=" <> int (pi_idx x)
                                         , text "heap_idx=" <> int (heap_idx x)
                                         ]
                                  )
                           )

instance CJsonDocable SFDLEdge where
    docCJson x = PP.braces (PP.hcat [
                                     text "dest_idx=" , int (dest_idx x)
                                    , text ",w=" , int (w x)
                                    ]
                           )

-- this serialization is not same as automated one would be, as we named
-- the SFDL fields capitalized, which is not allowed in Haskell.
instance CJsonDocable SFDLGraph where
    docCJson x = PP.braces (PP.vcat
                              (PP.punctuate PP.comma
                               [text "Vs=" <> PP.brackets (PP.vcat
                                                           (PP.punctuate PP.comma
                                                            (map docCJson (vs x)))),
                                text "Es=" <> PP.brackets (PP.vcat
                                                           (PP.punctuate PP.comma
                                                            (map docCJson (es x))))
                               ]
                              )
                           )
                            

type Node = Int
type Edge = (Node, Int)         -- destination and weight


-- a list of adjacency lists
type Graph = [[Edge]]


-- convert a "manual" representation of a graph to a Graph object.
manual2gr :: [(Int,             -- ! vertex number
               [(Int,           -- ! edge destination vertex number
                 Int)           -- ! edge weight
               ])
             ] -> (GParams, Graph)
manual2gr edgelist = let (vs, ees) = unzip edgelist
                         v         = length edgelist
                         d         = maximum $ map length ees
                         ws        = concatMap (map snd) ees
                         w_min     = minimum ws
                         w_max     = maximum ws
                      in (GParams { v = v,
                                    deg = d,
                                    w_range = (w_min, w_max) },
                          ees)

-- generate a random list of out-edges.
-- WANT: number of edges x normally distributed, with P[x > D] < 10%. This can be achieved
-- with:
-- mean = (3/4)D
-- sigma = mean/4
--
-- weights and destinations uniformly chosen.
outEdges :: (MonadRandom m) => GParams -> m [Edge]
outEdges params   = do let max_d    = (fromIntegral $ deg params)
                           u        = (3/4) * max_d -- mean
                           s        = u/4 -- deviation
                           (wmin,wmax) = w_range params
                       z            <- randNormal -- standard normal
                       -- apply our mean and std. deviation
                       let x        = s*z + u
                       -- just truncate values which exceed max_d
                       -- should be about 10% of values, which will be truncated.
                           numEs    = floor $ min x (max_d)
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
randomGenList g = iterateList (tuple2list2 . Random.split) g

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

-- first convert to the SFDL types
graph2sfdl :: Graph -> SFDLGraph
graph2sfdl ees =
    let num_out_edges's  = map length ees
        vs          = [ SFDLVertex num edge_list_head num_out_edges
                                   0 0 0
                        | num            <- [0..length ees - 1]
                        | num_out_edges  <- num_out_edges's
                        | edge_list_head <- runSumFrom0 num_out_edges's
                      ]
        es          = [ SFDLEdge dest_idx w | (dest_idx,w) <- concat ees ]
    in SFDLGraph vs es


printGraph_SFDL :: Graph -> String
printGraph_SFDL ees = let gr_doc    = docCJson $ graph2sfdl ees
                          param_doc = PP.braces (text "G=" <> gr_doc)
                      in  PP.render param_doc


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
