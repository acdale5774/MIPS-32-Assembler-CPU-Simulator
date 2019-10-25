open String
open Char



let r_type = "R-TYPE";;
let load_or_store = "LOAD/STORE";;
let branch = "BRANCH";;

let rType_pad = [0;0;0;0;0;0];;  
let shamt = [0;0;0;0;0];;



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





(* assumes non-empty token list *)
let matchInsToType ins = 
	match ins with
		(*[] -> raise Invalid_argument "empty instruction" *)
		(* | opcode::rest -> *)
		opcode::rest ->
			match opcode with
				(* "" -> raise Invalid_argument "empty string in tokens" *)
				(* | "add" -> "RTYPE" *)
				"add" -> r_type
				| "addi" -> r_type
				| "and" -> r_type
				| "div" -> r_type 
				| "mult" -> r_type 
				| "nor" -> r_type 
				| "or" -> r_type 
				| "sub" -> r_type 
				| "xor" -> r_type 
				| "beq" -> branch
				| "j" -> branch 
				| "lw" -> load_or_store
				| "sw" -> load_or_store;;

let encodeOp op =
	match op with
		"add" -> [1;0;0;0;0;0]
		| "addi" -> [0;0;1;0;0;0]
		| "and" -> [1;0;0;1;0;0]
		| "div" -> [0;1;1;0;1;0]
		| "mult" -> [0;1;1;0;0;0]
		| "nor" -> [1;0;0;1;1;1]
		| "or" -> [1;0;0;1;0;1]
		| "sub" -> [1;0;0;0;1;0]
		| "xor" -> [1;0;0;1;1;0]
		| "beq" -> [0;0;0;1;0;0]
		| "j" -> [0;0;0;0;1;0]
		| "lw" -> [1;0;0;0;1;1]
		| "sw" -> [1;0;1;0;1;1];;

let encodeReg reg =
	match reg with
		"$s0" -> [1;0;0;0;0] (* saved *)
		| "$s1" -> [1;0;0;0;1]
		| "$s2" -> [1;0;0;1;0]
		| "$s3" -> [1;0;0;1;1]
		| "$s4" -> [1;0;1;0;0]
		| "$s5" -> [1;0;1;0;1]
		| "$s6" -> [1;0;1;1;0]
		| "$s7" -> [1;0;1;1;1]
		| "$t0" -> [0;1;0;0;0] (* temporary *)
		| "$t1" -> [0;1;0;0;1]
		| "$t2" -> [0;1;0;1;0]
		| "$t3" -> [0;1;0;1;1]
		| "$t4" -> [0;1;1;0;0]
		| "$t5" -> [0;1;1;0;1]
		| "$t6" -> [0;1;1;1;0]
		| "$t7" -> [0;1;1;1;1]
		| "$t8" -> [1;1;0;0;0] (* different as in worksheet *)
		| "$t9" -> [1;1;0;0;1] (* different as in worksheet *)
		
let rec encodeRegs regs =
	match regs with
		[] -> []
		| h::t -> (encodeReg h) @ encodeRegs t;;


(*
 *
 * R-TYPE
 *
 *)


let rec extractRTypeRegs tokens =
	match tokens with
		[] -> []
		| h::t ->
			if ((String.get h 0) == '$') then
				[h] @ extractRTypeRegs t
			else
				extractRTypeRegs t;;
let rType ins = 
	match ins with
		opcode::rest -> rType_pad @ encodeRegs (extractRTypeRegs rest) @ shamt @ (encodeOp opcode);;
