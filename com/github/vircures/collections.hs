-- Whereas map maps a function to each entry in the list: map f list
-- funmap maps a list of functions to one variable: map var flist
-- Note: My initial intention is to allow a list of functions that
--       each have their own type (all take argument of type a): [(a -> b), (a -> c), ...]
--       However I have no clue yet how I could do that with the constraints of the haskell 
--       type system.
funmap :: a -> [(a -> b)] -> [b]
funmap _ []       = []
funmap x (fn:fns) = fn x : funmap x fns