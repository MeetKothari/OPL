(* sandbox-interp.ml - T. Wilkes, Spring Semester 2022                *)
(*                                                                    *)
(* The code in this file provides a "sandbox" environment for         *)
(* developing your interpreter functions for the Extended Calculator  *)
(* Language in a step-by-step manner.                                 *)
(* This file is self-contained, i.e., you won't need to use any other *)
(* files while experimenting here.                                    *)
(* When you get a function working, copy-and-paste it into your       *)
(* interpreter.ml file.                                               *)
(* To open this file in ocaml or utop, at the toplevel prompt type:   *)
(*   #use "sandbox-interp.ml";;                                       *)

#load "str.cma";;

open List;;
open Str;;          (* for split *)

(* AST types from interpreter.ml *)
type ast_sl = ast_s list
and ast_s =
| AST_error
| AST_assign of (string * ast_e)
| AST_read of string
| AST_write of ast_e
| AST_if of (ast_c * ast_sl)
| AST_while of (ast_c * ast_sl)
and ast_e =
| AST_binop of (string * ast_e * ast_e)
| AST_id of string
| AST_num of string
and ast_c = (string * ast_e * ast_e);;

    
(*******************************************************************
    Interpreter
 *******************************************************************)

type memory = (string * int) list;;
(*             name   * val         *)

type value =    (* an integer or an error message *)
| Value of int
| Error of string;;

(* concatenate strings, with a space in between if both were nonempty *)
let str_cat sep a b =
  match (a, b) with
  | (a, "") -> a
  | ("", b) -> b
  | (_, _) -> a ^ sep ^ b;;

(* Input to a calculator program is just a sequence of numbers.  We use
   the standard Str library to split the single input string into
   whitespace-separated words, each of which is subsequently checked
   for valid integer format.
*)
let rec interpret (ast:ast_sl) (full_input:string) : string =
  let inp = split (regexp "[ \t\n\r]+") full_input in
  let (_, _, _, outp) = interpret_sl ast [] inp [] in
    (fold_left (str_cat " ") "" outp) ^ "\n"

and interpret_sl (sl:ast_sl) (mem:memory)
                 (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
    (* ok?   new_mem  new_input     new_output *)
  match sl with
  | [] -> (true, mem, inp, outp)
  | head :: rest
  (* your code should replace the following line *)
    -> (true, mem, inp, outp @ ["interpret_sl code not written yet"])  

(* NB: the following routine is complete.  You can call it on any
   statement node and it figures out what more specific case to invoke.
*)
and interpret_s (s:ast_s) (mem:memory)
                (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  match s with
  | AST_assign(id, expr) -> interpret_assign id expr mem inp outp
  | AST_read(id)         -> interpret_read id mem inp outp
  | AST_write(expr)      -> interpret_write expr mem inp outp
  | AST_if(cond, sl)     -> interpret_if cond sl mem inp outp
  | AST_while(cond, sl)  -> interpret_while cond sl mem inp outp
  | AST_error            -> raise (Failure "cannot interpret erroneous tree")

and interpret_assign (lhs:string) (rhs:ast_e) (mem:memory)
                     (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  let (rhsv, mem2) = interpret_expr rhs mem in
  match rhsv with
  | Error (msg) -> (false, mem2, inp, outp @ [msg])
  | Value (rhs_val)
    -> let mem3 = List.remove_assoc lhs mem2 in
       (true, (lhs, rhs_val) :: mem3, inp, outp)

and interpret_read (id:string) (mem:memory)
                   (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  (* your code should replace the following line *)
  (true, mem, inp, outp @ ["interpret_read code not written yet"])

and interpret_write (expr:ast_e) (mem:memory)
                    (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  (* your code should replace the following line *)
  (true, mem, inp, outp @ ["interpret_write code not written yet"])

and interpret_if (cond:ast_c) (sl:ast_sl) (mem:memory)
                 (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  (* your code should replace the following line *)
  (true, mem, inp, outp @ ["interpret_if code not written yet"])

and interpret_while (cond:ast_c) (sl:ast_sl) (mem:memory)
                    (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  let (op, lo, ro) = cond in
  let (cond_val, mem2) = interpret_cond (op, lo, ro) mem in
  match cond_val with
  | Error (msg) -> (false, mem2, inp, outp @ [msg])
  | Value (continue)
    -> if continue = 0 then (true, mem2, inp, outp)
       else let (ok, new_mem, new_input, new_output) = interpret_sl sl mem2 inp outp in
            interpret_while cond sl new_mem new_input new_output

and interpret_expr (expr:ast_e) (mem:memory) : value * memory =
  match expr with
  | AST_num num
    -> ( try (Value (int_of_string num), mem)
         with Failure ("int_of_string") -> (Error ("Non-numeric literal: " ^ num), mem) )
         
  | AST_id id
    (* your code should replace the following line *)
    -> (Error("interpret_expr AST_id code not written yet"), mem)
    
  | AST_binop (op, lo, ro)
    (* your code should replace the following line *)
    -> (Error("interpret_expr AST_binop code not written yet"), mem)
    
  | _ -> (Error ("Invalid AST in interpret_expr"), mem)

and interpret_cond ((op:string), (lo:ast_e), (ro:ast_e)) (mem:memory)
    : value * memory =
  let int_of_bool b = if b then 1 else 0 in
  let (lov, mem1) = interpret_expr lo mem in
  let (rov, mem2) = interpret_expr ro mem1 in
  match lov with
  | Error (msg) -> (Error (msg), mem)
  | Value (lovi)
    -> ( match rov with
         | Error (msg) -> (Error (msg), mem)
         | Value (rovi)
           -> ( match op with
                | "="  -> (Value (int_of_bool (lovi =  rovi)), mem2)
                | "<>" -> (Value (int_of_bool (lovi <> rovi)), mem2)
                | "<"  -> (Value (int_of_bool (lovi <  rovi)), mem2)
                | ">"  -> (Value (int_of_bool (lovi >  rovi)), mem2)
                | "<=" -> (Value (int_of_bool (lovi <= rovi)), mem2)
                | ">=" -> (Value (int_of_bool (lovi >= rovi)), mem2)
                | _ -> (Error ("Invalid relational operator: " ^ op), mem2)
              )
       )
;;


(*******************************************************************
    Testing
 *******************************************************************)

(* "Hand build" abstract syntax trees for part of the sum_ave_prog *)

(* result of ast_ize_S stmt1;; *)
let stmt1_AST = AST_read "a";;

(* result of ast_ize_S stmt3;; *)
let stmt3_AST = AST_assign ("sum", AST_binop ("+", AST_id "a", AST_id "b"));;


(* A simple assignment statement *)
let assign_AST = AST_assign ("x", AST_num "2");;


(* A simple program using the if statement and a conditional expression *)
let if_prog = "
read x
if x > 0
  y := 100
end";;

(* AST for if_prog: *)
let if_prog_AST =
  [AST_read "x";
   AST_if ((">", AST_id "x", AST_num "0"), [AST_assign ("y", AST_num "100")])]


(* A simple program using the while statement and a conditional expression *)
let while_prog = "
x := 0
while x <= 100
  x := x + 1
end";;

(* AST for while_prog: *)
let while_prog_AST =
  [AST_assign ("x", AST_num "0");
   AST_while
    (("<=", AST_id "x", AST_num "100"),
     [AST_assign ("x", AST_binop ("+", AST_id "x", AST_num "1"))])];;
