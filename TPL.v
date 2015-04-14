(* Tiny Programming Language *)

(* TODO : Split in Modules. *)
(* Curry or Uncurry, it does not matter. *)

Definition Curry (T : Type) (op : T * T -> T) : T -> T -> T :=
  fun a => fun b => op (a, b).

Definition UnCurry (T : Type) (op : T -> T -> T) : T * T -> T :=
  fun p => match p with
             | (a, b) => op a b
           end.    

(* TODO : Prove the Dual of Curry and Uncurry *)

Theorem UnCurry_is_the_inverse_of_Curry  :
  forall S : Set,
  forall op : S -> S -> S,
  forall a d : S,
    (Curry S (UnCurry S op)) a d = op a d.
Proof.
  intros S op.
  intros a d.
  unfold Curry.
  unfold UnCurry.
  reflexivity.
Qed.

Theorem Curry_is_the_inverse_of_UnCurry  :
  forall S : Set,
  forall op : S * S -> S,
  forall a d : S,
    (UnCurry S (Curry S op)) (a, d) = op (a, d).
Proof.
  intros S op.
  intros a b.
  unfold Curry.
  unfold UnCurry.
  reflexivity.
Qed.

(* What is the benefits of abstracting away a number? *)

(* TODO: Answer the question.*)

Inductive Number : Set :=
  | num : Number.               (* Natural Numbers, Integers, Ratrional Numbers, ... *)

(* TODO: Fill in different properties about numbers.    *)

(* Abstracting away how what defines an ID. *)

(* Type or Set please explain... *)


Inductive ID : Set :=
| id : ID.

(* Type annotating pointers; I think it would be beneficial to  *)
(* annotate type in a live runtime enviroment, where any possible  *)
(* input would  *)

Inductive P : Type :=
  | ampID : P
  | alloc : P 
  | astE  : P
  | null  : P.                (* null is a really bad NONE! *)

Inductive ROV (T : Type) : Type :=
  | ref : ROV T -> ROV T
  | val : Number -> ROV T.      (* Right now I am assuming all values are numbers *)
(* What is the size of val? *)

Inductive AE : Type :=
  | plus   : (ROV Number) * (ROV Number) 
             -> AE
  | minus  : (ROV Number) * (ROV Number)
             -> AE
  | times  : (ROV Number) * (ROV Number) 
             -> AE
  | divide : (ROV Number) * (ROV Number) 
             -> AE.

Inductive RE (T : Type) : Type :=
  | lt : (ROV T) * (ROV T) -> RE T
  | gt : (ROV T) * (ROV T) -> RE T
  | eq : (ROV T) * (ROV T) -> RE T.

Inductive BINOP : Type :=
  | ae : AE -> BINOP
  | re : AE -> BINOP.              

Inductive E : Type :=
  | const : Number -> E
  | ref'  : ID     -> E
  | op    : BINOP  -> E * E -> E
  | input : forall T : Type,
              ROV T -> E
  | call  : ID -> list ID -> E.   
                         
Inductive S : Type :=
  | assign : ID -> E -> S
  | func   : ID -> list ID -> list ID 
             -> S -> E -> S
  | output : E -> S
  | seq    : S -> S -> S
  | if'    : E -> S -> S -> S
  | while  : E -> S -> S.

Inductive PROC : Type :=
  | proc : list S -> PROC.

(* TODO : Convert from amoller and make unit tests. We need a Parser! *)
(* But we always have lisp style :) *)

CoInductive Language : Type :=
  | defref : Language -> Language
  | nouns  : Language
  | verbs  : list Language -> Language.

(* Phefffffffff, this is difficult to reason about. Maybe something else. *)
(* Nouns is finite, but there is really many!!! Expecially when we have a  *)
(* type input which could be anything!!!! *)

(* So much explaining to do, TODO! *)

(* Thought is to annotate the variables and pointers with their type on runtime. 
   Explain more? *)

(* Coq is a nice type checker in itself, *)
(* What is interesting is the input, *)
(* so that the inputs types with something *)
(* The analyzer says that we can take appropriate action.  *)
Definition TypeConstraints (T : Type)
                           (eq_T : T -> T -> Prop) 
                           (constraint : E -> ROV T -> Prop) :=
  False.






