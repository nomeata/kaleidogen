module PrettyRna (prettyRNA) where

import RNA
import Text.Printf
import Data.Colour.SRGB
import Data.List

prettyRNA :: RNA -> String
prettyRNA = assemble . go


type Node = (Int,[String])

go :: RNA -> Node
go (Solid (RGB r g b)) = (0, [printf "─ solid(%s)" (sRGB24show (sRGB r g b))])
go (Blend x a b) = binary (printf "blend(%f)" x) (go a) (go b)
go (Checker x a b) = binary (printf "checker(%f)" x) (go a) (go b)
go (Ontop x a b) = binary (printf "ontop(%f)" x) (go a) (go b)
go (Rays x a b) = binary (printf "rays(%d)" x) (go a) (go b)
go (Gradient a b) = binary (printf "gradient") (go a) (go b)
go (Rotate x a) = unary (printf "rotate(%f)" x) (go a)
go (Dilated x a) = unary (printf "dilated(%d)" x) (go a)
go (Swirl x a) = unary (printf "swirl(%f)" x) (go a)
go (Invert a) = unary (printf "invert") (go a)

binary :: String -> Node -> Node -> Node
binary s (x1,l1) (x2,l2)
 = (length l1, lineDown (x1, l1) ++ ["┤ " ++s ] ++ lineUp (x2,l2))

lineDown, lineUp :: Node -> [String]
lineDown (x,l) = zipWith (:) (replicate x ' ' ++ ['┌'] ++ repeat '│') l
lineUp (x,l) = zipWith (:) (replicate x '│' ++ ['└'] ++ repeat ' ') l

unary :: String -> Node -> Node
unary s (x2,l2) = (0, ("┐ " ++s ) : lineUp (x2,l2))


assemble :: Node -> String
assemble (_, x) = intercalate "\n" x
