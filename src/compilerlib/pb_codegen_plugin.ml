(** Plugin architecture.

    OCaml-protoc generates code based on a number of plugins,
    each of which can contribute code to the output files (.ml and .mli).
*)

type codegen_f =
  ?and_:unit ->
  Pb_codegen_ocaml_type.type_ ->
  Pb_codegen_formatting.scope ->
  bool
(** A code generation function *)

module type S = sig
  val gen_sig : codegen_f
  (** Generate a signature file (.mli) *)

  val gen_struct : codegen_f
  (** Generate the implementation (.ml) *)

  val ocamldoc_title : string
  (** OCamldoc title *)

  val requires_mutable_records : bool
  (** Does this record depend on mutable records being defined? *)
end

type t = (module S)
(** A plugin is a code-generator respecting the signature {!S}. *)

let requires_mutable_records (module P : S) = P.requires_mutable_records
