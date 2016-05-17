type t = string;;
  
let create = String.create;;
  
let sub_string = String.sub;;
  
let set = String.set;;
  
let extend s left right =
  let len = String.length s + left + right in
  let r = create len in
  let (srcoff, dstoff) = if left < 0 then -left, 0 else 0, left in
  let cpylen = min (String.length s - srcoff) (len - dstoff) in
  if cpylen > 0 then String.blit s srcoff r dstoff cpylen;
  r;;
