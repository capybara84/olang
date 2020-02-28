fun append x y =
    if x == [] then y
    else (hd x) : append (tl x) y

fun filter p x =
    if x == [] then []
    else if p (hd x) then
        (hd x) : filter p (tl x)
    else filter p (tl x)


fun qsort lst = {
    fun take n lst p = filter (fn item -> p item n) lst
    fun take_less n lst = take n lst (fn x y -> x < y)
    fun take_greater n lst = take n lst (fn x y -> x >= y)
    if lst == [] then []
    else {
        let first = hd lst
        let rest = tl lst
        append (
            append (
                qsort (take_less first rest),
                [first]),
                qsort (take_greater first rest))

    }
}

fun show_lst lst = {
    if lst == [] then
        nl ()
    else {
        let first = hd lst
        let rest = tl lst
        putn first
        putc ','
        show_lst rest
    }
}

let _ = {
    //let lst = [5,3,9,7,2,8,1,4,6]
    let lst = [4,3]
    show_lst lst
    show_lst (qsort lst)
}

