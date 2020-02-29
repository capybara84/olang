import List

let lt = fn x y -> x < y
let ge = fn x y -> x >= y

fun qsort lst = {
    fun take n lst p = List.filter (fn item -> p item n) lst
    fun take_less n lst = take n lst lt
    fun take_greater n lst = take n lst ge
    if lst == [] then
        []
    else {
        let first = hd lst
        let rest = tl lst
        List.append (List.append (qsort (take_less first rest)) [first]) (qsort (take_greater first rest))
    }
}

fun show_lst lst = {
    if lst == [] then
        nl ()
    else {
        putn (hd lst)
        putc ','
        show_lst (tl lst)
    }
}

let () = {
    let lst = [5,3,9,7,2,8,1,4,6]
    show_lst lst
    show_lst (qsort lst)
}

