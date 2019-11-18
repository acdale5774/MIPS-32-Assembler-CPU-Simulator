open String
open Char
open Bytes

(* let file = "input.asm";; *)

let file = Sys.argv.(1);;
let ic = open_in file;;
let oc = open_out_bin "out.bin";;

(*let bytes = *)

let r_type = "R-TYPE";;
let load_or_store = "LOAD/STORE";;
let branch = "BRANCH";;
let jump = "JUMP";;

let rTypeHeader = [0;0;0;0;0;0];;  
let loadOrStoreHeader = [1;0;0;0;1;1];;
let jumpHeader = [0;0;0;0;1;0];;
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

let rec printIntList l = 
	match l with
		[] -> ()
		| h::t -> print_int h ; print_string " " ; printIntList t;;

let rec printIntLists l_l = 
	match l_l with
		[] -> ()
		| h::t -> print_string "\n" ; printIntList h ; printIntLists t;;

let rec printIntListByN l i n = 
	match l with
		[] -> ()
		| h::t -> 
			if(i mod n = 0) then
				print_string "\n";
			print_int h ; printIntListByN t (i+1) n;;

let rec sizeOf l =
	match l with
		[] -> 0
		| h::t -> 1 + (sizeOf t);;

let rec reverse l =
	match l with
		[] -> []
		| h::t -> (reverse t) @ [h];;

let rec map f l =
	match l with
		[] -> []
		| h::t -> [f h] @ map f t;;

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
						parseToTokensInner t token
				end
			else
                		begin
                    			if(h == '\r')
                			then
						begin
							if (token != [])
							then
								[token]
							else
								[]
						end
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
				| "j" -> jump 
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
		| _ -> raise (Invalid_argument "Invalid register passed to encodeReg : OUTER");;

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

let rec convDecToBinInner d =
	if(d = 0) then []
	else
		(d mod 2) :: convDecToBinInner (d / 2);;

let convDecToBin d =
	if (d = 0) then [0]
	else
		List.rev (convDecToBinInner d);;

let rec pow base exp =
	if (exp = 0) then 1
	else 
		base * (pow base (exp-1));;

let rec convBinToDecInner r_bin i =
	match r_bin with
		[] -> 0
		| h::t -> (h * (pow 2 i)) + (convBinToDecInner t (i+1));;

let convBinToDec bin = convBinToDecInner (reverse bin) 0;;


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

(*
 *
 * JUMP
 *
 *)

let jump_bin = [0;0;0;0;1;0];;

let rec jumpInsInner tokens =
	match tokens with
		[] -> []
		| h::t -> 
			if (String.contains (List.nth tokens 1) 'x') 
			|| (String.contains (List.nth tokens 1) 'X') then
				jump_bin @ convHexToBin (explode (List.nth tokens 1))
			else
				jump_bin @ convDecToBin (int_of_string (List.nth tokens 1));;

let jumpIns ins = jumpInsInner (parseToTokens ins);;


let rec asciiRep ins_list =
	match ins_list with
		[] -> []
		| ins::rest -> let insType = matchInsToType (parseToTokens ins) in
			match insType with
				"R-TYPE" -> let result = rTypeIns ins in 
					result @ asciiRep rest
				| "LOAD/STORE" -> (loadOrStoreIns ins) @ asciiRep rest
				| "BRANCH" -> (branchIns ins) @ asciiRep rest
				| "JUMP" -> (jumpIns ins) @ asciiRep rest
				| _ -> raise (Invalid_argument "not valid ins type in FUNCTION");;

let rec chunkBy8Inner l in_l i =
	match l with
		[] -> [] (* [in_l] *)
		| h::t -> 
			if(i = 7) then
				[in_l @ [h]] @ chunkBy8Inner t [] 0
			else
				chunkBy8Inner t (in_l @ [h]) (i+1);;

let rec chunkBy8 l = chunkBy8Inner l [] 0;;

let rec byteMapping byte_l char_l i =
	match char_l with
		[] -> byte_l
		(* | c::t -> (byteMapping (Bytes.set byte_l i c) t (i+1));; *)
		| c::t -> (Bytes.set byte_l i c) ; (byteMapping byte_l t (i+1));;

let rec writeBytes out_channel finalBytes = 
	match finalBytes with
		[] -> close_out out_channel
		| hd::tl -> output_byte out_channel hd; writeBytes out_channel tl;;

(*
let main = 
	let listIns = (build_list []) in
	let binIntsOfIns = (asciiRep listIns) in
	let binIntsOfInsBy8 = (chunkBy8 binIntsOfIns) in
	let decIntsOfInsBy8 = (map convBinToDec binIntsOfInsBy8) in
	let charsOfDecInts = (map Char.chr decIntsOfInsBy8) in
	let bytes = (byteMapping (Bytes.create (sizeOf charsOfDecInts)) charsOfDecInts 0) in
	writeBytes oc bytes;;
*)

let main = 
	let listIns = (build_list []) in
	let binIntsOfIns = (asciiRep listIns) in
	let binIntsOfInsBy8 = (chunkBy8 binIntsOfIns) in
	let decIntsOfInsBy8 = (map convBinToDec binIntsOfInsBy8) in
	(* let charsOfDecInts = (map Char.chr decIntsOfInsBy8) in *)
	(* let bytes = (byteMapping (Bytes.create (sizeOf charsOfDecInts)) charsOfDecInts 0) in *)
	writeBytes oc decIntsOfInsBy8;;
