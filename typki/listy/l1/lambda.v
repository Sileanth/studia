

Inductive term (X:Set) : Set :=
  | var : X -> term X
  | lambda : term (option X) -> term X
  | app : term X -> term X -> term X.

Arguments var {X}.
Arguments lambda {X}.
Arguments app {X}.



Definition option_fmap {X Y : Set} (f : X -> Y) (xo : option X) : option Y :=
  match xo with
  | None => None
  | Some x => Some (f x)
  end.

Fixpoint term_fmap {X Y:Set} (f : X -> Y) (t : term X) : term Y :=
  match t with
  | var x => var (f x)
  | lambda ox => lambda (term_fmap (option_fmap f) ox)
  | app t1 t2 => app (term_fmap f t1) (term_fmap f t2)
  end. 

Definition fatarrow {X Y:Set} (f : X -> term Y) (ox : option X) : term (option Y) :=
  match ox with
  | None => var None
  | Some x => term_fmap Some (f x)
  end.

Fixpoint bind {X Y : Set} (f : X -> term Y) (t : term X) : term Y :=
  match t with
  | var x => f x
  | lambda t => lambda (bind (fatarrow f) t)
  | app t1 t2 => app (bind f t1) (bind f t2)
  end.



Definition podstaw {X: Set} (to : term (option X)) (t : term X) : term X :=
  bind 
  (fun x => match x with
  | None => t
  | Some x => var x  end) to.





Definition id {A : Set} (x : A) : A := x.

Theorem p1 : forall (X : Set) (ox : option X), id ox = option_fmap id ox.
Proof. 
  intros X ox. destruct ox. reflexivity. reflexivity.
Qed.


Definition compose {X Y Z : Set} (f1 : Y -> Z) (f2 : X -> Y) (x : X) : Z :=
  f1 (f2 x).
Theorem p2 : forall (X Y Z : Set) (f1 : Y -> Z) (f2 : X -> Y) (x : option X), 
  option_fmap (compose f1 f2) x = compose (option_fmap f1) (option_fmap f2) x.
Proof. intros X Y Z f1 f2 x.
  destruct x. reflexivity. reflexivity.
Qed.




Theorem p3_problematic : forall (X : Set) (t : term X),
  term_fmap id t = id t.
Proof. intros X t.
  induction t.
  { reflexivity. }
  {
    simpl.
    admit.
  }
  {
    simpl. rewrite IHt1. rewrite IHt2. reflexivity.
  }
Admitted.
 





Theorem p3 : forall (X : Set) (t : term X)  (idd : X -> X) ,

  (forall (x : X), idd x = x) -> term_fmap (idd) t = id t.
  intros X t. induction t.
  {
     intros idd idp.  simpl. rewrite idp. simpl. reflexivity.
  }
  {
    intros idd idp. simpl.  
    assert (H: term_fmap (option_fmap idd) t = id t). {
      apply IHt. intro x. destruct x. 
      +simpl. rewrite idp. reflexivity.
      +reflexivity.
    }
    rewrite H. reflexivity.
  }
  {
    intros idd idp. simpl. 
    assert (H1 : term_fmap idd t1 = id t1). {
      apply IHt1. apply idp.
    }
    assert (H2 : term_fmap idd t2 = id t2). {
      apply IHt2. apply idp.
    }
    rewrite H1. rewrite H2. reflexivity.
  }
Qed.
 
Theorem maybe_tbetter_3theorem : forall (X : Set) (t : term X) (idd : forall Y, Y -> Y) , (forall (Y:Set) (y: Y), idd Y y = y) -> term_fmap (idd X) t = idd (term X) t. 
Proof.
   intros X t idd idp. induction t.
  { 
    simpl.

  }




Theorem p4: forall (X Y Z : Set) (f1 : Y -> Z) (f2 : X -> Y) (t: term X) , term_fmap (compose f1 f2) t = compose (term_fmap f1) (term_fmap f2) t.
Proof.
  intros X Y Z f1 f2 t. induction t. 
  {
   simpl. reflexivity. 
  } 
  {
    simpl. unfold compose. unfold compose in IHt.
    

  }











