import List

fun qsort lst = {
    fun take n lst p = List.filter (fn item -> p item n) lst
    fun take_less n lst = take n lst (fn x y -> x < y)
    fun take_greater n lst = take n lst (fn x y -> x >= y)
    if lst == [] then []
    else {
        let first = hd lst
        let rest = tl lst
        List.append (
            List.append (
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

/*
let () = {
    let lst = [5,3,2,1,4,6]
    show_lst lst
    show_lst (qsort lst)
}
*/

