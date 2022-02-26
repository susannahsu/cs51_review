(*======================================================================
Challenge Problem 6: Faster bignum multiplication 
......................................................................*)

(* times_faster b1 b2 -- Returns a bignum representing the product of
   `b1` and `b2`, making use of the Karatsuba algorithm for
   multiplication. *)

(* I tried this challenge but don't have time to improve the style *)
let rec sub_list (min_index: int) 
                 (max_index: int) 
                 (array: 'a list) 
                : 'a list =
  if (min_index > max_index) then []
  else (List.nth array min_index) :: (sub_list (min_index + 1) max_index array) ;;

let rec karatsuba (b1: int list) (b2: int list): int list =
  
  let min_max_length (x: int list) (y: int list): (int * int) =
    if (List.length x <= List.length y)
    then (List.length x, List.length y)
    else (List.length y, List.length x) in
  let (min_length, _max_length) = min_max_length b1 b2 in
  if min_length <= 3
  then
    let result = times {neg = false; coeffs = b1} 
                       {neg = false; coeffs = b2} in
    result.coeffs
  else
    let m = Int.of_float (Float.floor (Int.to_float min_length /. 2.)) in
    let (high1, low1) = 
      (sub_list 0 (m - 1) b1, sub_list m (List.length b1 - 1) b1) in
    let (high2, low2) = 
      (sub_list 0 (m - 1) b2, sub_list m (List.length b2 - 1) b2) in
    let sum1 = plus {neg = false; coeffs = low1} 
                    {neg = false; coeffs = high1} in
    let sum2 = plus {neg = false; coeffs = low2} 
                    {neg = false; coeffs = high2} in
    let z0 = karatsuba low1 low2 in
    let z1 = karatsuba sum1.coeffs sum2.coeffs in
    let z2 = karatsuba high1 high2 in
    let {neg = _; coeffs = temp1} = plus
        {neg = false; coeffs = z2 @ (List.init ((List.length z1) - ((List.length z2) - m)) 
                                               (fun _x -> 0))}
        {neg = false; coeffs = z1} in
    let {neg = _; coeffs = temp2} = plus
        {neg = false; coeffs = temp1}
        {neg = true; coeffs = z2} in
    let {neg = _; coeffs = temp3} = plus
        {neg = false; coeffs = temp2}
        {neg = true; coeffs = z0} in
    let {neg = _; coeffs = result} = plus
        {neg = false; coeffs = temp3 @ (List.init m (fun _x -> 0))}
        {neg = false; coeffs = z0} in
    result ;;

let times_faster (b1: bignum) (b2: bignum): bignum = 
  let result = karatsuba b1.coeffs b2.coeffs
  in {neg = b1.neg != b2.neg; coeffs = result} ;;