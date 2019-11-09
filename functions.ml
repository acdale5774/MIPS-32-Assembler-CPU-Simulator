open String
open Char



let r_type = "R-TYPE";;
let load_or_store = "LOAD/STORE";;
let branch = "BRANCH";;

let rTypeHeader = [0;0;0;0;0;0];;  
let loadOrStoreHeader = [1;0;0;0;1;1];;
let shamt = [0;0;0;0;0];;

let instructionLen = 32;;


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
		| "$t8" -> [1;1;0;0;0]   (* different as in worksheet *)
		| "$t9" -> [1;1;0;0;1];; (* different as in worksheet *)
		
let rec encodeRegs regs =
	match regs with
		[] -> []
		| h::t -> (encodeReg h) @ encodeRegs t;;

let rec fixLengthTo32 bin sizeOfBin =
	if(sizeOfBin > instructionLen)
		raise (Invalid_argument "ERROR: field is greater than 32 bits")
	else
		match bin with
			[] -> []
			

let convHexDigitToBin hexD =
	match hexD with
		'0' -> [0;0;0;0]
		| '1' -> [0;0;0;1]
		| '2' -> [0;0;1;0]
		| '3' -> [0;0;1;1]
		| '4' -> [0;1;0;0]
		| '5' -> [0;1;0;1]
		| '6' -> [0;1;1;0]
		| '7' -> [0;1;1;1]
		| '8' -> [1;0;0;0]
		| '9' -> [1;0;0;1]
		| 'a' -> [1;0;1;0]
		| 'b' -> [1;0;1;1]
		| 'c' -> [1;1;0;0]
		| 'd' -> [1;1;0;1]
		| 'e' -> [1;1;1;0]
		| 'f' -> [1;1;1;1];;

let rec removeHexPrefix hex inside =
	match hex,inside with
		[],false -> raise (Invalid_argument "ERROR: malformatted convHexToBin input")
		| [],true -> []
		| h::t,false -> 
				if( (h == 'x') || (h == 'X') )
				then
					[] @ removeHexPrefix t true
				else
					[] @ removeHexPrefix t false
		| h::t,true -> [h] @ removeHexPrefix t true;;

let rec convHexToBinInner hex = 
	match hex with
		[] -> []
		| h::t -> (convHexDigitToBin h) @ convHexToBinInner t;;

let convHexToBin hex =
	let withoutPrefix = (removeHexPrefix hex false) in
		let unformatted = convHexToBinInner withoutPrefix in
			fixLengthTo32 unformatted;;


(*
 *
 * R-TYPE
 *
 *)

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

let rec reorderRTypeRegs regs =
	match regs with
		h::t -> t @ [h];;

let rType ins = 
	match ins with
		opcode::rest -> rTypeHeader @ encodeRegs (reorderRTypeRegs (extractRTypeRegs rest)) @ shamt @ (encodeOp opcode);;


(*
 *
 * LOAD/STORE 
 *
 *)

(* lw $dest offset($base) *)
(* sw $to offset($base) *)
(* $rt is destination for loaded value *)

(* (35 or 46) rs rt address *)
(* opcode::rest -> loadOrStoreHeader @ rs @ rt @ address *)

(*
let extractDest tokens =
	match tokens with
		h::t -> h;;

let extractSource tokens =
	match tokens with
		h::t -> match t 

let extractOffset tokens = 

let loadOrStoreInner tokens = (encodeRegs ([extractDest tokens] @ [extractSource tokens])) @ (convHexToBin ([extractOffset tokens]));;

let loadOrStore ins = 
	match ins with
		opcode::rest -> loadOrStoreHeader @ loadOrStoreInner rest;;
*)
