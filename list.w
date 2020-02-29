module List

fun length x =
    if x == [] then
        0
    else
        1 + length (tl x)

fun map f x =
    if x == [] then []
    else f (hd x) : map f (tl x)

fun append x y =
    if x == [] then y
    else (hd x) : append (tl x) y

fun reverse x =
    if x == [] then []
    else append (reverse (tl x)) [hd x]

fun filter p x =
    if x == [] then []
    else if p (hd x) then
        (hd x) : filter p (tl x)
    else filter p (tl x)

