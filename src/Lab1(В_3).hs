--- Вариант -3 
prime_factor = prime_factor' 3 [] 600851475143 
  where
    prime_factor' n pfs target 
        | product (n:pfs) == target    = n
        | 0 == target `mod` n &&
          all (\x-> 0 /= mod n x) pfs = prime_factor' (n+2) (n:pfs) target
        | otherwise                   = prime_factor' (n+2) pfs target
