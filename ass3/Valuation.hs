module Valuation(Valuation, valuations) where
import Types

type Valuation = [(Name, Integer)]

valuations :: [(Name, Domain)] -> [Valuation]
valuations [] = []
valuations [(v, lst)] = [[(v, x)] | x <- lst]
valuations ((v, xs):rest) = [(v, x):ys | x <- xs, ys <- valuations rest]
