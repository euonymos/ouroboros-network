{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'AdjacencyMap' data type for edge-labelled graphs, as
-- well as associated operations and algorithms. 'AdjacencyMap' is an instance
-- of the 'C.Graph' type class, which can be used for polymorphic graph
-- construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, adjacencyMap,

    -- * Basic graph construction primitives
    empty, vertex, edge, (-<), (>-), overlay, connect, vertices, edges,
    overlays, fromAdjacencyMaps,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, edgeLabel, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, edgeSet, preSet, postSet,

    -- * Graph transformation
    removeVertex, modifyEdge, removeEdge, replaceVertex, replaceEdge, transpose, gmap,
    emap, induce, induceJust,

    -- * Relational operations
    {- closure, reflexiveClosure, symmetricClosure, transitiveClosure, -}

    -- * Miscellaneous
    consistent
    ) where

import Control.DeepSeq
import Data.Maybe
import Data.Map (Map)
import Data.Monoid (Sum (..))
import Data.Set (Set, (\\))
import GHC.Generics

import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

-- | Edge-labelled graphs, where the type variable @e@ stands for edge labels.
-- For example, 'AdjacencyMap' @Bool@ @a@ is isomorphic to unlabelled graphs
-- defined in the top-level module "Algebra.Graph.AdjacencyMap", where @False@
-- and @True@ denote the lack of and the existence of an unlabelled edge,
-- respectively.
newtype AdjacencyMap e a = AM {
    -- | The /adjacency map/ of an edge-labelled graph: each vertex is
    -- associated with a map from its direct successors to the corresponding
    -- edge labels.
    adjacencyMap :: Map a (Map a e) } deriving (Eq, Generic, NFData)

instance (Ord a, Show a, Show e) => Show (AdjacencyMap e a) where
    showsPrec p lam@(AM m)
        | Set.null vs = showString "empty"
        | null es     = showParen (p > 10) $ vshow vs
        | vs == used  = showParen (p > 10) $ eshow es
        | otherwise   = showParen (p > 10) $
                            showString "overlay (" . vshow (vs \\ used) .
                            showString ") ("       . eshow es . showString ")"
      where
        vs   = vertexSet lam
        es   = edgeList lam
        used = referredToVertexSet m
        vshow vs = case Set.toAscList vs of
            [x] -> showString "vertex "   . showsPrec 11 x
            xs  -> showString "vertices " . showsPrec 11 xs
        eshow es = case es of
            [(e, x, y)] -> showString "edge "  . showsPrec 11 e .
                           showString " "      . showsPrec 11 x .
                           showString " "      . showsPrec 11 y
            xs          -> showString "edges " . showsPrec 11 xs

instance (Eq e, Semigroup e, Ord a) => Ord (AdjacencyMap e a) where
    compare x y = mconcat
        [ compare (vertexCount x) (vertexCount y)
        , compare (vertexSet   x) (vertexSet   y)
        , compare (edgeCount   x) (edgeCount   y)
        , compare (eSet        x) (eSet        y)
        , cmp ]
      where
        eSet = Set.map (\(_, x, y) -> (x, y)) . edgeSet
        cmp | x == y               = EQ
            | overlays [x, y] == y = LT
            | otherwise            = compare x y

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: AdjacencyMap e a
empty = AM Map.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap e a
vertex x = AM $ Map.singleton x Map.empty

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory.
--
-- @
-- edge e    x y              == 'connect' e ('vertex' x) ('vertex' y)
-- edge 'zero' x y              == 'vertices' [x,y]
-- 'hasEdge'   x y (edge e x y) == (e /= 'zero')
-- 'edgeLabel' x y (edge e x y) == e
-- 'edgeCount'     (edge e x y) == if e == 'zero' then 0 else 1
-- 'vertexCount'   (edge e 1 1) == 1
-- 'vertexCount'   (edge e 1 2) == 2
-- @
edge :: (Ord a) => e -> a -> a -> AdjacencyMap e a
-- right-bias of fromList ensures that if x == y then the edge is retained.
edge e x y = AM $ Map.fromList [(y, Map.empty), (x, Map.singleton y e)]

-- | The left-hand part of a convenient ternary-ish operator @x-\<e\>-y@ for
-- creating labelled edges.
--
-- @
-- x -\<e\>- y == 'edge' e x y
-- @
(-<) :: a -> e -> (a, e)
g -< e = (g, e)

-- | The right-hand part of a convenient ternary-ish operator @x-\<e\>-y@ for
-- creating labelled edges.
--
-- @
-- x -\<e\>- y == 'edge' e x y
-- @
(>-) :: (Ord a) => (a, e) -> a -> AdjacencyMap e a
(x, e) >- y = edge e x y

infixl 5 -<
infixl 5 >-

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
--
-- Note: 'overlay' composes edges in parallel using the operator '<+>' with
-- 'zero' acting as the identity:
--
-- @
-- 'edgeLabel' x y $ overlay ('edge' e x y) ('edge' 'zero' x y) == e
-- 'edgeLabel' x y $ overlay ('edge' e x y) ('edge' f    x y) == e '<+>' f
-- @
--
-- Furthermore, when applied to transitive graphs, 'overlay' composes edges in
-- sequence using the operator '<.>' with 'one' acting as the identity:
--
-- @
-- 'edgeLabel' x z $ 'transitiveClosure' (overlay ('edge' e x y) ('edge' 'one' y z)) == e
-- 'edgeLabel' x z $ 'transitiveClosure' (overlay ('edge' e x y) ('edge' f   y z)) == e '<.>' f
-- @
overlay :: (Semigroup e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
overlay (AM x) (AM y) = AM $ Map.unionWith nonZeroUnion x y

-- Union maps, removing zero elements from the result.
nonZeroUnion :: (Semigroup e, Ord a) => Map a e -> Map a e -> Map a e
nonZeroUnion x y = Map.unionWith (<>) x y

-- | /Connect/ two graphs with edges labelled by a given label. When applied to
-- the same labels, this is an associative operation with the identity 'empty',
-- which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'isEmpty'     (connect e x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect e x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect e x y) >= 'vertexCount' x
-- 'vertexCount' (connect e x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect e x y) <= 'vertexCount' x * 'vertexCount' y + 'edgeCount' x + 'edgeCount' y
-- 'vertexCount' (connect e 1 2) == 2
-- 'edgeCount'   (connect e 1 2) == if e == 'zero' then 0 else 1
-- @
connect :: (Semigroup e, Ord a) => e -> AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
connect e (AM x) (AM y) = AM $ Map.unionsWith nonZeroUnion $ x : y :
    [ Map.fromSet (const targets) (Map.keysSet x) ]
  where
    targets = Map.fromSet (const e) (Map.keysSet y)

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Ord a => [a] -> AdjacencyMap e a
vertices = AM . Map.fromList . map (, Map.empty)

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []        == 'empty'
-- edges [(e,x,y)] == 'edge' e x y
-- edges           == 'overlays' . 'map' (\\(e, x, y) -> 'edge' e x y)
-- @
edges :: (Semigroup e, Ord a) => [(e, a, a)] -> AdjacencyMap e a
edges es = fromAdjacencyMaps [ (x, Map.singleton y e) | (e, x, y) <- es ]

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: (Semigroup e, Ord a) => [AdjacencyMap e a] -> AdjacencyMap e a
overlays = AM . Map.unionsWith nonZeroUnion . map adjacencyMap

-- | Construct a graph from a list of adjacency sets.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyMaps []                                  == 'empty'
-- fromAdjacencyMaps [(x, Map.'Map.empty')]                    == 'vertex' x
-- fromAdjacencyMaps [(x, Map.'Map.singleton' y e)]            == if e == 'zero' then 'vertices' [x,y] else 'edge' e x y
-- 'overlay' (fromAdjacencyMaps xs) (fromAdjacencyMaps ys) == fromAdjacencyMaps (xs '++' ys)
-- @
fromAdjacencyMaps :: (Semigroup e, Ord a) => [(a, Map a e)] -> AdjacencyMap e a
fromAdjacencyMaps xs = AM $ Map.unionWith (<>) vs es
  where
    vs = Map.fromSet (const Map.empty) . Set.unions $ map (Map.keysSet . snd) xs
    es = Map.fromListWith (Map.unionWith (<>)) xs

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- isSubgraphOf 'empty'      x     ==  True
-- isSubgraphOf ('vertex' x) 'empty' ==  False
-- isSubgraphOf x y              ==> x <= y
-- @
isSubgraphOf :: (Eq e, Semigroup e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a -> Bool
isSubgraphOf (AM x) (AM y) = Map.isSubmapOfBy (Map.isSubmapOfBy le) x y
  where
    le x y = (x <> y) == y

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                         == True
-- isEmpty ('overlay' 'empty' 'empty')         == True
-- isEmpty ('vertex' x)                    == False
-- isEmpty ('removeVertex' x $ 'vertex' x)   == True
-- isEmpty ('removeEdge' x y $ 'edge' e x y) == False
-- @
isEmpty :: AdjacencyMap e a -> Bool
isEmpty = Map.null . adjacencyMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Ord a => a -> AdjacencyMap e a -> Bool
hasVertex x = Map.member x . adjacencyMap

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' e x y)     == (e /= 'zero')
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'not' . 'null' . 'filter' (\\(_,ex,ey) -> ex == x && ey == y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap e a -> Bool
hasEdge x y (AM m) = fromMaybe False (Map.member y <$> Map.lookup x m)

-- | Extract the label of a specified edge in a graph, or Nothing if there
-- is no edge between these vertices.
-- Complexity: /O(log(n))/ time.
--
-- @
-- edgeLabel x y 'empty'         == 'zero'
-- edgeLabel x y ('vertex' z)    == 'zero'
-- edgeLabel x y ('edge' e x y)  == e
-- edgeLabel s t ('overlay' x y) == edgeLabel s t x <+> edgeLabel s t y
-- @
edgeLabel :: (Ord a) => a -> a -> AdjacencyMap e a -> Maybe e
edgeLabel x y (AM m) = Map.lookup x m >>= Map.lookup y

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: AdjacencyMap e a -> Int
vertexCount = Map.size . adjacencyMap

-- | The number of (non-'zero') edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'        == 0
-- edgeCount ('vertex' x)   == 0
-- edgeCount ('edge' e x y) == if e == 'zero' then 0 else 1
-- edgeCount              == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyMap e a -> Int
edgeCount = getSum . foldMap (Sum . Map.size) . adjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: AdjacencyMap e a -> [a]
vertexList = Map.keys . adjacencyMap

-- | The list of edges of a graph, sorted lexicographically with respect to
-- pairs of connected vertices (i.e. edge-labels are ignored when sorting).
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'        == []
-- edgeList ('vertex' x)   == []
-- edgeList ('edge' e x y) == if e == 'zero' then [] else [(e,x,y)]
-- @
edgeList :: AdjacencyMap e a -> [(e, a, a)]
edgeList (AM m) =
    [ (e, x, y) | (x, ys) <- Map.toAscList m, (y, e) <- Map.toAscList ys ]

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: AdjacencyMap e a -> Set a
vertexSet = Map.keysSet . adjacencyMap

-- | The set of edges of a given graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'        == Set.'Set.empty'
-- edgeSet ('vertex' x)   == Set.'Set.empty'
-- edgeSet ('edge' e x y) == if e == 'zero' then Set.'Set.empty' else Set.'Set.singleton' (e,x,y)
-- @
edgeSet :: (Eq a, Eq e) => AdjacencyMap e a -> Set (e, a, a)
edgeSet = Set.fromAscList . edgeList

-- | The /preset/ of an element @x@ is the set of its /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preSet x 'empty'        == Set.'Set.empty'
-- preSet x ('vertex' x)   == Set.'Set.empty'
-- preSet 1 ('edge' e 1 2) == Set.'Set.empty'
-- preSet y ('edge' e x y) == if e == 'zero' then Set.'Set.empty' else Set.'Set.fromList' [x]
-- @
preSet :: Ord a => a -> AdjacencyMap e a -> Set a
preSet x (AM m) = Set.fromAscList
    [ a | (a, es) <- Map.toAscList m, Map.member x es ]

-- | The /postset/ of a vertex is the set of its /direct successors/.
-- Complexity: /O(log(n))/ time and /O(1)/ memory.
--
-- @
-- postSet x 'empty'        == Set.'Set.empty'
-- postSet x ('vertex' x)   == Set.'Set.empty'
-- postSet x ('edge' e x y) == if e == 'zero' then Set.'Set.empty' else Set.'Set.fromList' [y]
-- postSet 2 ('edge' e 1 2) == Set.'Set.empty'
-- @
postSet :: Ord a => a -> AdjacencyMap e a -> Set a
postSet x = Map.keysSet . Map.findWithDefault Map.empty x . adjacencyMap

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' e x x)     == 'empty'
-- removeVertex 1 ('edge' e 1 2)     == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> AdjacencyMap e a -> AdjacencyMap e a
removeVertex x = AM . Map.map (Map.delete x) . Map.delete x . adjacencyMap

-- | Remove an edge from a given graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge x y ('edge' e x y)     == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
--
-- NB: when you add edges, you do a semigroup append, but when you remove,
-- you dump the entire appended thing. That's bizarre. Surely, in keeping
-- with the analogy, the monoid-backed treatment should require that the
-- edges are a _group_ if you want to remove one.
--
removeEdge :: Ord a => a -> a -> AdjacencyMap e a -> AdjacencyMap e a
removeEdge = modifyEdge (const Nothing)

-- The idea here is that, when working with an adjacency map graph, the edges
-- are a semigroup @e@, but the graph deals with the monoid @Maybe e@. Even if
-- @e@ has a zero element, it's not considered the zero in @Maybe e@.
--
modifyEdge :: Ord a => (Maybe e -> Maybe e) -> a -> a -> AdjacencyMap e a -> AdjacencyMap e a
modifyEdge f x y = AM . Map.adjust (Map.alter f y) x . adjacencyMap

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'gmap' (\\v -> if v == x then y else v)
-- @
replaceVertex :: (Semigroup e, Ord a) => a -> a -> AdjacencyMap e a -> AdjacencyMap e a
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Replace an edge from a given graph. If it doesn't exist, it will be created.
-- Complexity: /O(log(n))/ time.
--
-- @
-- replaceEdge e x y z                 == 'overlay' (removeEdge x y z) ('edge' e x y)
-- replaceEdge e x y ('edge' f x y)      == 'edge' e x y
-- 'edgeLabel' x y (replaceEdge e x y z) == e
-- @
replaceEdge :: (Ord a) => e -> a -> a -> AdjacencyMap e a -> AdjacencyMap e a
replaceEdge e x y = AM . addY . Map.alter replace x . adjacencyMap
  where
    addY             = Map.alter (Just . fromMaybe Map.empty) y
    replace (Just m) = Just $ Map.insert y e m
    replace Nothing  = Just $ Map.singleton y e

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose 'empty'        == 'empty'
-- transpose ('vertex' x)   == 'vertex' x
-- transpose ('edge' e x y) == 'edge' e y x
-- transpose . transpose  == id
-- @
transpose :: (Semigroup e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a
transpose (AM m) = AM $ Map.foldrWithKey combine vs m
  where
    -- No need to use @nonZeroUnion@ here, since we do not add any new edges
    combine v es = Map.unionWith (Map.unionWith (<>)) $
        Map.fromAscList [ (u, Map.singleton v e) | (u, e) <- Map.toAscList es ]
    vs = Map.fromSet (const Map.empty) (Map.keysSet m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'        == 'empty'
-- gmap f ('vertex' x)   == 'vertex' (f x)
-- gmap f ('edge' e x y) == 'edge' e (f x) (f y)
-- gmap 'id'             == 'id'
-- gmap f . gmap g     == gmap (f . g)
-- @
gmap :: (Semigroup e, Ord a, Ord b) => (a -> b) -> AdjacencyMap e a -> AdjacencyMap e b
gmap f = AM . Map.map (Map.mapKeysWith (<>) f) .
    Map.mapKeysWith (Map.unionWith (<>)) f . adjacencyMap

-- | Transform a graph by applying a function @h@ to each of its edge labels.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- The function @h@ is required to be a /homomorphism/ on the underlying type of
-- labels @e@. At the very least it must preserve 'zero' and '<+>':
--
-- @
-- h 'zero'      == 'zero'
-- h x '<+>' h y == h (x '<+>' y)
-- @
--
-- If @e@ is also a semiring, then @h@ must also preserve the multiplicative
-- structure:
--
-- @
-- h 'one'       == 'one'
-- h x '<.>' h y == h (x '<.>' y)
-- @
--
-- If the above requirements hold, then the implementation provides the
-- following guarantees.
--
-- @
-- emap h 'empty'           == 'empty'
-- emap h ('vertex' x)      == 'vertex' x
-- emap h ('edge' e x y)    == 'edge' (h e) x y
-- emap h ('overlay' x y)   == 'overlay' (emap h x) (emap h y)
-- emap h ('connect' e x y) == 'connect' (h e) (emap h x) (emap h y)
-- emap 'id'                == 'id'
-- emap g . emap h        == emap (g . h)
-- @
emap :: (e -> f) -> AdjacencyMap e a -> AdjacencyMap f a
emap h = AM . Map.map (Map.map h) . adjacencyMap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(n + m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> AdjacencyMap e a -> AdjacencyMap e a
induce p = AM . Map.map (Map.filterWithKey (\k _ -> p k)) .
    Map.filterWithKey (\k _ -> p k) . adjacencyMap

-- | Construct the /induced subgraph/ of a given graph by removing the vertices
-- that are 'Nothing'.
-- Complexity: /O(n + m)/ time.
--
-- @
-- induceJust ('vertex' 'Nothing')                               == 'empty'
-- induceJust ('edge' ('Just' x) 'Nothing')                        == 'vertex' x
-- induceJust . 'gmap' 'Just'                                    == 'id'
-- induceJust . 'gmap' (\\x -> if p x then 'Just' x else 'Nothing') == 'induce' p
-- @
induceJust :: Ord a => AdjacencyMap e (Maybe a) -> AdjacencyMap e a
induceJust = AM . Map.map catMaybesMap . catMaybesMap . adjacencyMap
  where
    catMaybesMap = Map.mapKeysMonotonic fromJust . Map.delete Nothing

{-
-- | Compute the /reflexive and transitive closure/ of a graph over the
-- underlying star semiring using the Warshall-Floyd-Kleene algorithm.
--
-- @
-- closure 'empty'         == 'empty'
-- closure ('vertex' x)    == 'edge' 'one' x x
-- closure ('edge' e x x)  == 'edge' 'one' x x
-- closure ('edge' e x y)  == 'edges' [('one',x,x), (e,x,y), ('one',y,y)]
-- closure               == 'reflexiveClosure' . 'transitiveClosure'
-- closure               == 'transitiveClosure' . 'reflexiveClosure'
-- closure . closure     == closure
-- 'postSet' x (closure y) == Set.'Set.fromList' ('Algebra.Graph.ToGraph.reachable' x y)
-- @
closure :: (Eq e, Ord a, StarSemiring e) => AdjacencyMap e a -> AdjacencyMap e a
closure = goWarshallFloydKleene . reflexiveClosure

-- | Compute the /reflexive closure/ of a graph over the underlying semiring by
-- adding a self-loop of weight 'one' to every vertex.
-- Complexity: /O(n * log(n))/ time.
--
-- @
-- reflexiveClosure 'empty'              == 'empty'
-- reflexiveClosure ('vertex' x)         == 'edge' 'one' x x
-- reflexiveClosure ('edge' e x x)       == 'edge' 'one' x x
-- reflexiveClosure ('edge' e x y)       == 'edges' [('one',x,x), (e,x,y), ('one',y,y)]
-- reflexiveClosure . reflexiveClosure == reflexiveClosure
-- @
reflexiveClosure :: (Ord a, Semiring e) => AdjacencyMap e a -> AdjacencyMap e a
reflexiveClosure (AM m) = AM $ Map.mapWithKey (\k -> Map.insertWith (<+>) k one) m

-- | Compute the /symmetric closure/ of a graph by overlaying it with its own
-- transpose.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- symmetricClosure 'empty'              == 'empty'
-- symmetricClosure ('vertex' x)         == 'vertex' x
-- symmetricClosure ('edge' e x y)       == 'edges' [(e,x,y), (e,y,x)]
-- symmetricClosure x                  == 'overlay' x ('transpose' x)
-- symmetricClosure . symmetricClosure == symmetricClosure
-- @
symmetricClosure :: (Semigroup e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a
symmetricClosure m = overlay m (transpose m)

-- | Compute the /transitive closure/ of a graph over the underlying star
-- semiring using a modified version of the Warshall-Floyd-Kleene algorithm,
-- which omits the reflexivity step.
--
-- @
-- transitiveClosure 'empty'               == 'empty'
-- transitiveClosure ('vertex' x)          == 'vertex' x
-- transitiveClosure ('edge' e x y)        == 'edge' e x y
-- transitiveClosure . transitiveClosure == transitiveClosure
-- @
transitiveClosure :: (Eq e, Ord a, StarSemiring e) => AdjacencyMap e a -> AdjacencyMap e a
transitiveClosure = goWarshallFloydKleene

-- The iterative part of the Warshall-Floyd-Kleene algorithm
goWarshallFloydKleene :: (Eq e, Ord a, StarSemiring e) => AdjacencyMap e a -> AdjacencyMap e a
goWarshallFloydKleene (AM m) = AM $ foldr update m vs
  where
    vs = Set.toAscList (Map.keysSet m)
    update k cur = Map.fromAscList [ (i, go i (get i k <.> starkk)) | i <- vs ]
      where
        get i j = edgeLabel i j (AM cur)
        starkk  = star (get k k)
        go i ik = Map.fromAscList
            [ (j, e) | j <- vs, let e = get i j <+> ik <.> get k j, e /= zero ]
-}

-- | Check that the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. The released algebraic-graphs version
-- also checks that there are no edge labels equal to the zero in its monoid,
-- but with the representation in this version, the edge labels are a semigroup
-- completed to a monoid by Maybe with Nothing as the zero. So, we don't need
-- an Eq or Monoid instance on the edge labels.
consistent :: (Ord a) => AdjacencyMap e a -> Bool
consistent (AM m) = referredToVertexSet m `Set.isSubsetOf` Map.keysSet m

-- The set of vertices that are referred to by the edges in an adjacency map
referredToVertexSet :: Ord a => Map a (Map a e) -> Set a
referredToVertexSet m = Set.fromList $ concat
    [ [x, y] | (x, ys) <- Map.toAscList m, (y, _) <- Map.toAscList ys ]
