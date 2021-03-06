module Math.Group.Stabilizer where

import qualified Data.Graph.Inductive as G
import qualified Data.Set as S
import Data.Graph.Inductive.Tree (Gr)
import Data.Maybe (isJust)
import Data.List (find)
import Data.Tuple (swap)

import Math.Group
import Math.Group.Orbit

--
-- Schreier Tree Construction
--

-- A schreier tree is a structure rooted at a given element `a`
-- where the nodes of the tree are in the orbit of `a`
-- to construct the tree, we perform a Breadth/Depth first search starting
-- from a, and applying each of the generators, being sure to not create
-- a cycle


-- Generates a schreierTreee for the group generated by `gens` rooted at `a`
schreierTree :: (Enum a, GroupAction g a) => S.Set g -> a -> Gr a g
schreierTree gens a = tree gens [a] initialTree
 where initialTree = G.insNode (fromEnum a, a) G.empty


-- keep track of our generators, the list of non-exhausted elements in the
-- search, and the tree as we build it up
--
-- once we exhaust all elements from the graph search, we are done.
-- otherwise, for each element in our list of focused nodes, we apply all the
-- generators, and see if we get something new, if so, we update the list
-- of focused nodes, as well as the tree
tree _ [] t = t
tree gens as t = tree gens as' t'
 where (as', t') = foldl (applyGens gens) ([],t) as


-- given a node that is currently the focus of the search
-- for each generator, we see if we can use that node/generator combo
-- to get a new node, and update the tree/search list if so
applyGens gens newAndTree n = S.foldl (addNode n) newAndTree gens


-- compute the action of a group element on the given node.
-- if this results in a node that is not already in the tree,
-- we update the tree and search list
--
-- n is already a node in the graph
-- we attempt to add n' = action g n to the graph
-- Nodes must have type int, so we make a node which is the enum of Node
--   labeled with the actual value
-- This graph library uses directed edges, so we add the edge in both
--   directions
-- The edge is labeled with the group element used to get from n to n'
--   as dictated by the algorithm
addNode n (ns,t) g
  | (fromEnum n') `G.gelem` t = (ns, t)
  | otherwise                 = (ns ++ [n'], newT)
 where n' = action g n
       nd newNode = (fromEnum newNode, newNode)
       newT = G.insEdge (fromEnum n', fromEnum n, g)
            . G.insEdge (fromEnum n, fromEnum n', g)
            . G.insNode (nd n')
            $ t


--
-- Operations on Schreier Trees
--


-- given a tree, i and ib nodes in the tree, determine if
--   there is an edge between i and ib with label b
isEdge t i b ib = isJust $ find match es
 where es = G.labEdges t -- :: [(Int, Int, b)]
       match (a,c,perm) = perm == b && ((e i == a && e ib == c) || (e ib == a && e i == c))
       e n = fromEnum n


-- finds the path path between the two given nodes
--   the resultant value is the product of the group elements that make up
--   the edges of the path
path :: (Enum a, GroupAction g a, G.Graph gr) => gr a g -> a -> a -> g
path t start end = foldl times identity . reverse . map snd . tail $ ns
 where G.LP ns = G.lesp (fromEnum start) (fromEnum end) t

--
-- Stabilizer
--

stabilizer :: (Enum a, Ord g, Ord a, Eq g, GroupAction g a)
  => S.Set g -> a -> S.Set g
stabilizer gens a = foldl checkAdd S.empty l
 where l = [ (i, b, action b i) | b <- S.toList gens, i <- S.toList (orbit gens a)]
       checkAdd s (i,b,ib) = if isEdge t i b ib
                               then s
                               else S.insert (e i b ib) s
       e i b ib = inverse (path t a ib) `times` b `times` path t a i
       t = schreierTree gens a
