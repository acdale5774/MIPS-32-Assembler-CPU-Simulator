open String
open Char


let file = "input.asm";;
let ic = open_in file;;

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

let rec printIntList = function 
[] -> ()
| e::l -> print_int e ; printIntList l;;


let rec sizeOf l =
	match l with
		[] -> 0
		| h::t -> 1 + (sizeOf t);;


let rec parseToTokensInner l token =
	match l with
		[] -> 
			if (token != [])
			then
				[token]
			else
				[]
		| h::t -> 
			if ( (h==' ') || (h==',') || (h =='(') || (h ==')') )
			then
				begin
					if (token != [])
					then
						[token] @ parseToTokensInner t []
					else
						parseToTokensInner t []
				end
			else
                begin
                    if(h == '\r')
                    then
                        []
                    else
				        parseToTokensInner t (token @ [h])
                end;;

let parseToTokensMiddle s = parseToTokensInner (explode s) [];;

let rec parseToTokensOuter ll = 
	match ll with
		[] -> []
        | h::t -> [implode h] @ parseToTokensOuter t;;

let rec parseToTokens s = parseToTokensOuter (parseToTokensMiddle s);;

let rec build_list l =
    match input_line ic with
        line -> build_list (line :: l)
        | exception End_of_file -> close_in ic; List.rev l;;

let rec extractListOfInsTokenListsInner ins_list =
        match ins_list with
            [] -> []
            | h::t -> [parseToTokens h] @ extractListOfInsTokenListsInner t;;


(* let extractListOfInsTokenLists = extractListOfInsTokenLists (build_list []);; *)

(*
let encodeToBinaryIntList t_l_l =
	match t_l_l with
		[] -> []
		| token_list::rest -> 
			match token_list with
				[] -> []
				opcode::rest -> 
					if( (matchInsToType token_list) = r_type )
					then
						(rType token_list) @
					else
*)


(* assumes non-empty token list *)
let matchInsToType ins = 
	match ins with
		[] -> raise (Invalid_argument "empty instruction passed to matchInsToType")
		| opcode::rest ->
			match opcode with
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
				| "sw" -> load_or_store
				| _ -> raise (Invalid_argument "invalid opcode passed to matchInsToType");;

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
		| "sw" -> [1;0;1;0;1;1]
		| _ -> raise (Invalid_argument "invalid opcode passed to encodeOp");;

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
		| _ -> raise (Invalid_argument "Invalid register passed to encodeReg");;
		
let rec encodeRegs regs =
	match regs with
		[] -> []
		| h::t -> (encodeReg h) @ encodeRegs t;;

let rec padN n =
	if(n == 0)
	then
		[]
	else
		[0] @ padN (n-1);;

let fixLengthTo16 bin sizeOfBin =
	if(sizeOfBin > (instructionLen/2))
	then
		raise (Invalid_argument "ERROR: field is greater than 16 bits")
	else
		(padN ((instructionLen/2)-sizeOfBin)) @ bin;;

let fixLengthTo32 bin sizeOfBin =
	if(sizeOfBin > instructionLen)
	then
		raise (Invalid_argument "ERROR: field is greater than 32 bits")
	else
		(padN (instructionLen-sizeOfBin)) @ bin;;

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
		| 'f' -> [1;1;1;1]
		| _ -> raise (Invalid_argument "not hexDigit in convHexDigitToBin");;

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
			fixLengthTo16 unformatted (sizeOf unformatted);;


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
		[] -> raise (Invalid_argument "invalid regs in reorderRTypeRegs")
		| h::t -> t @ [h];;

let rTypeInsInner tokens = 
	match tokens with
		[] -> raise (Invalid_argument "invalid token list in rTypeInsInner")
		| opcode::rest -> rTypeHeader @ encodeRegs (reorderRTypeRegs (extractRTypeRegs rest)) @ shamt @ (encodeOp opcode);;

let rTypeIns ins = rTypeInsInner (parseToTokens ins);;


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

let lw = [1;0;0;0;1;1];;
let sw = [1;0;1;0;1;1];;

let loadOrStoreHeader header =
	if(header = "lw")
	then lw
	else sw;;

let rec extractToken tokens i =
	match tokens with
		[] -> raise (Invalid_argument "invalid token list in extractToken")
		| h::t -> if(i==0)
			then h
			else extractToken t (i-1);;

let loadOrStoreInner tokens = 
	match tokens with
		[] -> raise (Invalid_argument "invalid token list in loadOrStoreInner")
		| opcode::rest -> 
			let header = loadOrStoreHeader opcode
			and rs = encodeReg (extractToken tokens 3)
			and rt = encodeReg (extractToken tokens 1)
			and offset = convHexToBin (explode (extractToken tokens 2)) in 
			header @ rs @ rt @ offset;;

let loadOrStoreIns ins = loadOrStoreInner (parseToTokens ins);;


(*
 *
 * BRANCH
 *
 *)

let beq = [0;0;0;1;0;0];;

let rec branchInsInner tokens i =
	match tokens with
		[] -> []
		| h::t ->
			match i with
				0 -> beq @ branchInsInner t (i+1)
				| 1 -> (encodeReg h) @ branchInsInner t (i+1)
				| 2 -> (encodeReg h) @ branchInsInner t (i+1)
				| 3 -> (convHexToBin (explode h)) @ branchInsInner t (i+1)
				| _ -> [];;

let branchIns ins = branchInsInner (parseToTokens ins) 0;;


let rec temp ins_list =
	match ins_list with
		[] -> []
		| ins::rest -> let insType = matchInsToType (parseToTokens ins) in
			match insType with
				r_type -> (rTypeIns ins) @ temp rest
				| load_or_store -> (loadOrStoreIns ins) @ temp rest
				| branch -> (branchIns ins) @ temp rest
				| _ -> raise (Invalid_argument "not valid ins type in FUNCTION");;

let mainI = 
	let listIns = (build_list []) in
	let intsOfIns = (temp listIns) in
	printIntList intsOfIns;;
