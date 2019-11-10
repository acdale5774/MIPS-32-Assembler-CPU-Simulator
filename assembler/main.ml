open String
open Hello

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode c_l =
    let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) c_l;
        Buffer.contents buf;;

let rec printStringList = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; printStringList l;;

let rec parseToTokensInner l token =
	match l with
		[] -> 
			if (token != [])
				then
					[token]
				else
					[]
		| h::t -> 
			if (h = ' ')
				then
					begin
						if (token != [])
							then
								[token] @ parseToTokensInner t []
							else
								parseToTokensInner t []
					end
				else
					parseToTokensInner t (token @ [h]);;

let parseToTokensMiddle s = parseToTokensInner (explode s) [];;

let rec parseToTokensOuter ll = 
	match ll with
		[] -> []
        | h::t -> [implode h] @ parseToTokensOuter t;;

let rec parseToTokens s = parseToTokensOuter (parseToTokensMiddle s);;

(* let main = printStringList (parseToTokens Sys.argv.(1));; *)
let main = Hello.hello ()
