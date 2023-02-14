(* Here it defines the 3 permissions: Read, Write and Send *)

type permission =
 | Read
 | Write
 | Send;;

(*Just defining the new expressions I invented. This is also the AST *)

type expression =
  | EInt of int
  | EFloat of float 
  | MySum of expression * expression
  | MyMinus of expression * expression 
  | MyMul of expression * expression 
  | Execute of expression * permission list
  | MyCall of expression
  | MyPin of expression 
  | MyDiv of expression * expression 
  | EBool of bool
  | And of expression * expression 
  | Or of expression * expression 
  | Equal of expression * expression
  | EString of string ;; 

type iexp = | Inspect of expression;;
type 'v env = (string * 'v) list;;

(* Here the language types are defined *)

type value = 
 | Int of int
 | Float of float 
 | String of string 
 | MyPin of int 
 | Closure of expression * (value) env 
 | Bool of bool;;

(* This checks permissions*)

let rec isPartOfPerm (per : permission) (p_list : permission list) : bool = 
  match p_list with
  | [] -> false
  | perm::l -> if (per=perm) then true else isPartOfPerm perm l;;

let rec reviewPermission (fList : permission list) (permList : permission list) : bool = 
  match fList with
  | [] -> true
  | p1::l1 -> if (isPartOfPerm p1 permList) = true then reviewPermission l1 permList else false;;

(*This defines the Inspection mechanism (type iexp) and verifies permissions *)

let rec evaluate (iex : iexp) (perList : permission list) (env : 'v env) : value =
  match iex with
  | Inspect(e1) -> begin match e1 with
                        | Execute(fbody, flist) -> if (reviewPermission flist perList) = true then Closure(fbody, env) 
                                                   else failwith("ERROR: Forbidden operation!. You do not have the necessary permsission to do this operation. Entire process cancelled. Please change the permissions")
                        | _ -> failwith("ERROR: Not a Executection!")
                      end;; 

(*this is the actual interpreter. It has a variety of functionality, Like Execute, MySum, MyPin etc*)

let rec interpret (e : expression) (perList: permission list) (env : 'v env) : value =
  match e with
  | EInt i -> Int i
  | EFloat f -> Float f 
  | EBool b -> Bool b
  | EString s -> String s 
  | MySum(x1, x2) ->    let y1 = interpret x1 perList env in
                        let y2 = interpret x2 perList env in
                          begin match (y1, y2) with 
                            | (Int e1, Int e2) -> Int(e1 + e2)
                            | (Float f1, Float f2) -> Float(f1 +. f2)
                            | (_, _) -> failwith("Exception: Operands are not of type float or type integer!")
                          end
   | MyMinus(x1, x2) -> let y1 = interpret x1 perList env in
                        let y2 = interpret x2 perList env in
                          begin match (y1, y2) with 
                            | (Int e1, Int e2) -> Int(e1 - e2)
                            | (Float f1, Float f2) -> Float(f1 -. f2)
                            | (_, _) -> failwith("Exception: Operands are not of type float or type integer!")
                         end
   | MyMul(x1, x2) ->   let y1 = interpret x1 perList env in
                        let y2 = interpret x2 perList env in
                        begin match (y1, y2) with 
                          | (Int e1, Int e2) -> Int(e1 * e2)
                          | (Float f1, Float f2) -> Float(f1 *. f2)
                          | (_, _) -> failwith("Exception: Operands are not of type float or type integer!")
                        end
   | MyDiv(x1, x2) ->   let y1 = interpret x1 perList env in 
                        let y2 = interpret x2 perList env in 
                      begin match (y1,y2) with 
                          | (Float f1, Float f2) -> Float(f1 /. f2)
                          | (_, _) -> failwith("Exception: Operands are not of type float")
                      end
   | MyPin(x1) ->       let y1 = interpret x1 perList env in
                        begin match (y1) with 
                          | (Int e1) -> Int(e1)
                          | (_) -> failwith("Exception: Operand is not integer")
                        end
   | Execute(body, plist) -> evaluate (Inspect(e)) perList env
   | MyCall(func) -> let closure = interpret func perList env in 
                          begin match closure with
                            | Closure(fbody, fenv) ->  let new_env = fenv in 
                                                                interpret fbody perList new_env 
                            | _ -> failwith("Exception: Not a closure!")
                          end
  
  (* Additional - just for fun functions *)
  | And(x1, x2) -> let y1 = interpret x1 perList env in
                   let y2 = interpret x2 perList env in
                        begin match (y1, y2) with 
                          | (Bool e1, Bool e2) -> Bool(e1 && e2)
                          | (_, _) -> failwith("Exception: Operands are not boolean!")
                        end 

   | Or (x1, x2) -> let y1 = interpret x1 perList env in
                     let y2 = interpret x2 perList env in
                        begin match (y1, y2) with 
                          | (Bool e1, Bool e2) -> Bool(e1 || e2)
                          | (_, _) -> failwith("Exception: Operands are not boolean!")
                        end 
 
   | Equal(x1, x2) -> let y1 = interpret x1 perList env in
                        let y2 = interpret x2 perList env in
                          begin match (y1, y2) with 
                            | (Int e1, Int e2) -> Bool(e1 = e2)
                            | (String e1, String e2) -> Bool(e1 = e2)
                            | (_, _) -> failwith("Exception: Operands are not Integer or String !")
                          end ;;



(*These are some manual tests. They make a calculation or operation defined above (e.g. MySum) Afterwards it gets evaluated
and the result is printed to the terminal. *)

(*This just creates a new test Environment*)

let testEnv : (value) env = []  ;;

(*This defines the active permissions. E.g In a real world scenario the user has the permission write. This "User" permission gets evaluated against the permission of the function, which is defined via Execute(body, plist) *)
let pList : (permission list) = [Read] ;;


let sum1 = Execute(MySum(EInt(1), EInt(2)), [Write]);;
let sum2 = MyCall(sum1);;
let review = if interpret sum2 pList testEnv = Int(3) then print_endline "Test 1: MySum with integers successfull. Result is 3" else print_endline ("Test 1: MySum with integers Failed. Result does not equal 3");;
interpret sum2 pList testEnv;;

let sum_float1 = Execute(MySum(EFloat(7.5), EFloat(2.5)), [Write]);;
let sum_float2 = MyCall(sum_float1);;
let review = if interpret sum_float2 pList testEnv = Float(10.0) then print_endline "Test 2: MySum with floats successfull. Result is 10.0" else print_endline("Test 2: MySum with floats Failed. Result does not equal 3");;
interpret sum_float2 pList testEnv;;

let minus1 = Execute(MyMinus(EInt(7), EInt(3)), [Write]);; 
let minus2 = MyCall(minus1);;
let review = if interpret minus2 pList testEnv = Int(5) then print_endline "Test 3: MyMinus with integers successfull. Result is 5" else print_endline("Test 3: MyMinus with integers Failed. Result does not equal 5");;
interpret minus2 pList testEnv;;

let sum_float1 = Execute(MyMinus(EFloat(7.5), EFloat(2.5)), [Write]);;
let sum_float2 = MyCall(sum_float1);;
let review = if interpret sum_float2 pList testEnv = Float(5.0) then print_endline "Test 4: MyMinus with floats successfull. Result is 5.0" else print_endline("Test 4: MyMinus with floats Failed. Result does not equal 5.0");;
interpret sum_float2 pList testEnv;;

let mul1 = Execute(MyMul(EInt(5), EInt(2)), [Write]);; 
let mul2 = MyCall(mul1);;
let review = if interpret mul2 pList testEnv = Int(10) then print_endline "Test 5: MyMul with integers successfull. Result is 10" else print_endline ("Test 5: MyMul with integers Failed. Result does not equal 10");;
interpret mul2 pList testEnv;;

let div1 = Execute(MyDiv(EFloat(15.0), EFloat(3.0)), [Write]);;
let div2 = MyCall(div1);;
let review = if interpret div2 pList testEnv = Float(5.0) then print_endline "Test 6: MyDiv with floats successfull. Result is 5.0" else print_endline ("Test 6: MyDiv with integers Failed. Result does not equal 5.0");;
interpret div2 pList testEnv;;

let and1 = Execute(And(EBool(true), EBool(false)), [Write]);; 
let and2 = MyCall(and1);;
let review = if interpret and2 pList testEnv = Bool(false) then print_endline "Test 7: And with floats successfull. Result is false" else print_endline ("Test 7: And Failed.");;
interpret and2 pList testEnv;;

let and3 = Execute(And(EBool(true), EBool(true)), [Write]);; 
let and4 = MyCall(and3);;
let review = if interpret and4 pList testEnv = Bool(true) then print_endline "Test 8: And with floats successfull. Result is true" else print_endline ("Test 8: And Failed.");;
interpret and4 pList testEnv;;

let or1 = Execute(Or(EBool(true), EBool(false)), [Write]);; 
let or2 = MyCall(or1);;
let review = if interpret or2 pList testEnv  = Bool(true) then print_endline "Test 9: Or successfull. Result is true" else print_endline ("Test 9: Or Failed.");;
interpret or2 pList testEnv;;

let equal1 = Execute(Equal(EInt(7), EInt(7)), [Write]);; 
let equal2 = MyCall(equal1);;
let review = if interpret equal2 pList testEnv = Bool(true) then print_endline "Test 10: Equal with integers successfull. Result is true. Both integers are 7" else print_endline ("Test 10: Equal with integers Failed.");;
interpret equal2 pList testEnv;;

let equal3 = Execute(Equal(EString("Hello"), EString("Hello")), [Write]);; 
let equal4 = MyCall(equal3);;
let review = if interpret equal4 pList testEnv = Bool(true) then print_endline "Test 11: Equal with string successfull. Result is true. Words are both Hello" else print_endline ("Test 11: Equal Failed.");;
interpret equal4 pList testEnv;;

let equal5 = Execute(Equal(EString("Goodbye"), EString("Hello")), [Write]);; 
let equal6 = MyCall(equal5);;
let review = if interpret equal6 pList testEnv = Bool(false) then print_endline "Test 12: Equal with string successfull. Result is false. Words are not the same" else print_endline ("Test 12: Equal Failed.");;
interpret equal6 pList testEnv;;

let mul11 = Execute(MyMul(EInt(5), EInt(2)), [Write]);; 
let mul22 = MyCall(mul1);;
let review = if interpret mul2 pList testEnv = Int(10) then print_endline "Test 5: MyMul with integers successfull. Result is 10" else print_endline ("Test 5: MyMul with integers Failed. Result does not equal 10");;
interpret mul2 pList testEnv;;


(*In this case, the Executation of the MyPin function does not happen. The MyPin Function requires [Send], but the user has only [Write]. Therefore the execution is stopped in line 210, and all following commands (pin2, review) are 
stopped. In order for this function to work, the user needs to change his permission to [Send] in Line 146, and disable all other tests, other wise this pin1 function can never be reached *)

let pin1 = Execute(MyPin(EInt(12345)), [Write]);;
let pin2 = MyCall(pin1);;
let review = if interpret pin2 pList testEnv = Int(12345) then print_endline "Test 13: MyPin successfull. Result is 12345" else print_endline ("Test 13: MyPin failed. Result does not equal 12345");;
interpret pin2 pList testEnv;;

         