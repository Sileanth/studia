include module type of Kind.Datatypes
include module type of TVar.Datatypes
include module type of Type.Datatypes
include module type of TypeDef.Datatypes
include module type of Var.Datatypes

module Kind    : Kind.S
module TVar    : TVar.S
module Type    : Type.S
module TypeDef : TypeDef.S
module Var     : Var.S

module Env : module type of Env with type t = Env.t

module Subtyping : module type of Subtyping
