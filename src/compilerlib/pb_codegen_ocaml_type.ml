(*
  The MIT License (MIT)

  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)

(** OCaml type representation *)

module Pt = Pb_parsing_parse_tree

type payload_kind =
  | Pk_varint of bool  (** zigzag *)
  | Pk_bits32
  | Pk_bits64
  | Pk_bytes

and user_defined_type = {
  udt_module_prefix: string option;
      (** since code generated is split in multiple file (type, binary, json, ..)
     this defines the prefix for the given type, the suffix will
     be defined by each generator *)
  udt_type_name: string;
      (** OCaml type name ie not the type name in proto file *)
  udt_type: [ `Message | `Enum ];
      (** Need to keep track of this since encoding logic in binary
     format is quite different *)
}

and basic_type =
  | Bt_string
  | Bt_float
  | Bt_int
  | Bt_int32
  | Bt_uint32
  | Bt_int64
  | Bt_uint64
  | Bt_bytes
  | Bt_bool

and wrapper_type = {
  wt_type: basic_type; (* basic type being wrapped *)
  wt_pk: payload_kind; (* encoding used for the basic type *)
}

and field_type =
  | Ft_unit
  | Ft_basic_type of basic_type
  | Ft_user_defined_type of user_defined_type
  | Ft_wrapper_type of wrapper_type
      (** New wrapper type which indicates that the corresponding ocaml
     Type should be an `option` along with the fact that it is encoded with
     special rules *)

and default_value = Pb_option.constant option

and associative_type =
  | At_list
  | At_hashtable
(* Future work can include the following OCaml associative containers
   | Al_map *)

and repeated_type =
  | Rt_list
  | Rt_repeated_field

and encoding_number = int
and is_packed = bool

and record_field_type =
  | Rft_nolabel of (field_type * encoding_number * payload_kind)
      (** no default values in proto3 no label fields *)
  | Rft_required of
      (field_type * encoding_number * payload_kind * default_value)
  | Rft_optional of
      (field_type * encoding_number * payload_kind * default_value)
  | Rft_repeated of
      (repeated_type * field_type * encoding_number * payload_kind * is_packed)
  | Rft_associative of
      (associative_type
      * encoding_number
      * (basic_type * payload_kind)
      * (field_type * payload_kind))
  | Rft_variant of variant

and variant_constructor = {
  vc_constructor: string;
  vc_field_type: variant_constructor_type;
  vc_encoding_number: encoding_number;
  vc_payload_kind: payload_kind;
  vc_options: Pb_option.set;
}

and variant_constructor_type =
  | Vct_nullary
  | Vct_non_nullary_constructor of field_type

and variant = {
  v_name: string;
  v_constructors: variant_constructor list;
}

and record_field = {
  rf_label: string;
  rf_field_type: record_field_type;
  rf_mutable: bool;
  rf_options: Pb_option.set;
}

and record = {
  r_name: string;
  r_fields: record_field list;
}

and const_variant_constructor = {
  cvc_name: string;
  cvc_binary_value: int;
  cvc_string_value: string;
  cvc_options: Pb_option.set;
}

and const_variant = {
  cv_name: string;
  cv_constructors: const_variant_constructor list;
}

and empty_record = { er_name: string }

and type_spec =
  | Record of record
  | Variant of variant
  | Const_variant of const_variant
  | Unit of empty_record

and type_ = {
  module_prefix: string;
      (** code generation leads to several file/module being generated for
        a given [type_]. [module_prefix] is the common prefix for all those
        generated module and it is based on the `.proto` filename. *)
  spec: type_spec;
  type_level_ppx_extension: string option;
  type_options: Pb_option.set;
}

(** RPC argument or return type. We require message types in RPC. *)
and rpc_type =
  | Rpc_scalar of field_type
  | Rpc_stream of field_type

and rpc = {
  rpc_name: string;
  rpc_req: rpc_type;
  rpc_res: rpc_type;
}
(** A RPC specification, ie the signature for one remote procedure. *)

and service = {
  service_name: string;
  service_packages: string list;  (** Package in which this belongs *)
  service_body: rpc list;
}
(** A service, composed of multiple RPCs. *)

and proto = {
  proto_types: type_ list list;
      (** List of strongly connected type definitions *)
  proto_services: service list;
}
[@@deriving_inline
  traverse_map,
    traverse_iter,
    traverse_fold,
    traverse_fold_map,
    traverse_map_with_context]
(** A proto file is composed of a list of types and a list of services. *)

let _ = fun (_ : payload_kind) -> ()
let _ = fun (_ : user_defined_type) -> ()
let _ = fun (_ : basic_type) -> ()
let _ = fun (_ : wrapper_type) -> ()
let _ = fun (_ : field_type) -> ()
let _ = fun (_ : default_value) -> ()
let _ = fun (_ : associative_type) -> ()
let _ = fun (_ : repeated_type) -> ()
let _ = fun (_ : encoding_number) -> ()
let _ = fun (_ : is_packed) -> ()
let _ = fun (_ : record_field_type) -> ()
let _ = fun (_ : variant_constructor) -> ()
let _ = fun (_ : variant_constructor_type) -> ()
let _ = fun (_ : variant) -> ()
let _ = fun (_ : record_field) -> ()
let _ = fun (_ : record) -> ()
let _ = fun (_ : const_variant_constructor) -> ()
let _ = fun (_ : const_variant) -> ()
let _ = fun (_ : empty_record) -> ()
let _ = fun (_ : type_spec) -> ()
let _ = fun (_ : type_) -> ()
let _ = fun (_ : rpc_type) -> ()
let _ = fun (_ : rpc) -> ()
let _ = fun (_ : service) -> ()
let _ = fun (_ : proto) -> ()

class virtual map =
  object (self)
    method virtual bool : bool -> bool
    method virtual int : int -> int
    method virtual list : 'a. ('a -> 'a) -> 'a list -> 'a list
    method virtual option : 'a. ('a -> 'a) -> 'a option -> 'a option
    method virtual string : string -> string

    method virtual pb_option__constant
        : Pb_option.constant -> Pb_option.constant

    method virtual pb_option__set : Pb_option.set -> Pb_option.set

    method payload_kind : payload_kind -> payload_kind =
      fun x ->
        match x with
        | Pk_varint a ->
          let a = self#bool a in
          Pk_varint a
        | Pk_bits32 -> Pk_bits32
        | Pk_bits64 -> Pk_bits64
        | Pk_bytes -> Pk_bytes

    method user_defined_type : user_defined_type -> user_defined_type =
      fun { udt_module_prefix; udt_type_name; udt_type } ->
        let udt_module_prefix = self#option self#string udt_module_prefix in
        let udt_type_name = self#string udt_type_name in
        let udt_type = (fun x -> x) udt_type in
        { udt_module_prefix; udt_type_name; udt_type }

    method basic_type : basic_type -> basic_type = fun x -> x

    method wrapper_type : wrapper_type -> wrapper_type =
      fun { wt_type; wt_pk } ->
        let wt_type = self#basic_type wt_type in
        let wt_pk = self#payload_kind wt_pk in
        { wt_type; wt_pk }

    method field_type : field_type -> field_type =
      fun x ->
        match x with
        | Ft_unit -> Ft_unit
        | Ft_basic_type a ->
          let a = self#basic_type a in
          Ft_basic_type a
        | Ft_user_defined_type a ->
          let a = self#user_defined_type a in
          Ft_user_defined_type a
        | Ft_wrapper_type a ->
          let a = self#wrapper_type a in
          Ft_wrapper_type a

    method default_value : default_value -> default_value =
      self#option self#pb_option__constant

    method associative_type : associative_type -> associative_type = fun x -> x
    method repeated_type : repeated_type -> repeated_type = fun x -> x
    method encoding_number : encoding_number -> encoding_number = self#int
    method is_packed : is_packed -> is_packed = self#bool

    method record_field_type : record_field_type -> record_field_type =
      fun x ->
        match x with
        | Rft_nolabel a ->
          let a =
            (fun (a, b, c) ->
              let a = self#field_type a in
              let b = self#encoding_number b in
              let c = self#payload_kind c in
              a, b, c)
              a
          in
          Rft_nolabel a
        | Rft_required a ->
          let a =
            (fun (a, b, c, d) ->
              let a = self#field_type a in
              let b = self#encoding_number b in
              let c = self#payload_kind c in
              let d = self#default_value d in
              a, b, c, d)
              a
          in
          Rft_required a
        | Rft_optional a ->
          let a =
            (fun (a, b, c, d) ->
              let a = self#field_type a in
              let b = self#encoding_number b in
              let c = self#payload_kind c in
              let d = self#default_value d in
              a, b, c, d)
              a
          in
          Rft_optional a
        | Rft_repeated a ->
          let a =
            (fun (a, b, c, d, e) ->
              let a = self#repeated_type a in
              let b = self#field_type b in
              let c = self#encoding_number c in
              let d = self#payload_kind d in
              let e = self#is_packed e in
              a, b, c, d, e)
              a
          in
          Rft_repeated a
        | Rft_associative a ->
          let a =
            (fun (a, b, c, d) ->
              let a = self#associative_type a in
              let b = self#encoding_number b in
              let c =
                (fun (a, b) ->
                  let a = self#basic_type a in
                  let b = self#payload_kind b in
                  a, b)
                  c
              in
              let d =
                (fun (a, b) ->
                  let a = self#field_type a in
                  let b = self#payload_kind b in
                  a, b)
                  d
              in
              a, b, c, d)
              a
          in
          Rft_associative a
        | Rft_variant a ->
          let a = self#variant a in
          Rft_variant a

    method variant_constructor : variant_constructor -> variant_constructor =
      fun {
            vc_constructor;
            vc_field_type;
            vc_encoding_number;
            vc_payload_kind;
            vc_options;
          } ->
        let vc_constructor = self#string vc_constructor in
        let vc_field_type = self#variant_constructor_type vc_field_type in
        let vc_encoding_number = self#encoding_number vc_encoding_number in
        let vc_payload_kind = self#payload_kind vc_payload_kind in
        let vc_options = self#pb_option__set vc_options in
        {
          vc_constructor;
          vc_field_type;
          vc_encoding_number;
          vc_payload_kind;
          vc_options;
        }

    method variant_constructor_type
        : variant_constructor_type -> variant_constructor_type =
      fun x ->
        match x with
        | Vct_nullary -> Vct_nullary
        | Vct_non_nullary_constructor a ->
          let a = self#field_type a in
          Vct_non_nullary_constructor a

    method variant : variant -> variant =
      fun { v_name; v_constructors } ->
        let v_name = self#string v_name in
        let v_constructors =
          self#list self#variant_constructor v_constructors
        in
        { v_name; v_constructors }

    method record_field : record_field -> record_field =
      fun { rf_label; rf_field_type; rf_mutable; rf_options } ->
        let rf_label = self#string rf_label in
        let rf_field_type = self#record_field_type rf_field_type in
        let rf_mutable = self#bool rf_mutable in
        let rf_options = self#pb_option__set rf_options in
        { rf_label; rf_field_type; rf_mutable; rf_options }

    method record : record -> record =
      fun { r_name; r_fields } ->
        let r_name = self#string r_name in
        let r_fields = self#list self#record_field r_fields in
        { r_name; r_fields }

    method const_variant_constructor
        : const_variant_constructor -> const_variant_constructor =
      fun { cvc_name; cvc_binary_value; cvc_string_value; cvc_options } ->
        let cvc_name = self#string cvc_name in
        let cvc_binary_value = self#int cvc_binary_value in
        let cvc_string_value = self#string cvc_string_value in
        let cvc_options = self#pb_option__set cvc_options in
        { cvc_name; cvc_binary_value; cvc_string_value; cvc_options }

    method const_variant : const_variant -> const_variant =
      fun { cv_name; cv_constructors } ->
        let cv_name = self#string cv_name in
        let cv_constructors =
          self#list self#const_variant_constructor cv_constructors
        in
        { cv_name; cv_constructors }

    method empty_record : empty_record -> empty_record =
      fun { er_name } ->
        let er_name = self#string er_name in
        { er_name }

    method type_spec : type_spec -> type_spec =
      fun x ->
        match x with
        | Record a ->
          let a = self#record a in
          Record a
        | Variant a ->
          let a = self#variant a in
          Variant a
        | Const_variant a ->
          let a = self#const_variant a in
          Const_variant a
        | Unit a ->
          let a = self#empty_record a in
          Unit a

    method type_ : type_ -> type_ =
      fun { module_prefix; spec; type_level_ppx_extension; type_options } ->
        let module_prefix = self#string module_prefix in
        let spec = self#type_spec spec in
        let type_level_ppx_extension =
          self#option self#string type_level_ppx_extension
        in
        let type_options = self#pb_option__set type_options in
        { module_prefix; spec; type_level_ppx_extension; type_options }

    method rpc_type : rpc_type -> rpc_type =
      fun x ->
        match x with
        | Rpc_scalar a ->
          let a = self#field_type a in
          Rpc_scalar a
        | Rpc_stream a ->
          let a = self#field_type a in
          Rpc_stream a

    method rpc : rpc -> rpc =
      fun { rpc_name; rpc_req; rpc_res } ->
        let rpc_name = self#string rpc_name in
        let rpc_req = self#rpc_type rpc_req in
        let rpc_res = self#rpc_type rpc_res in
        { rpc_name; rpc_req; rpc_res }

    method service : service -> service =
      fun { service_name; service_packages; service_body } ->
        let service_name = self#string service_name in
        let service_packages = self#list self#string service_packages in
        let service_body = self#list self#rpc service_body in
        { service_name; service_packages; service_body }

    method proto : proto -> proto =
      fun { proto_types; proto_services } ->
        let proto_types = self#list (self#list self#type_) proto_types in
        let proto_services = self#list self#service proto_services in
        { proto_types; proto_services }
  end

class virtual iter =
  object (self)
    method virtual bool : bool -> unit
    method virtual int : int -> unit
    method virtual list : 'a. ('a -> unit) -> 'a list -> unit
    method virtual option : 'a. ('a -> unit) -> 'a option -> unit
    method virtual string : string -> unit
    method virtual pb_option__constant : Pb_option.constant -> unit
    method virtual pb_option__set : Pb_option.set -> unit

    method payload_kind : payload_kind -> unit =
      fun x ->
        match x with
        | Pk_varint a -> self#bool a
        | Pk_bits32 -> ()
        | Pk_bits64 -> ()
        | Pk_bytes -> ()

    method user_defined_type : user_defined_type -> unit =
      fun { udt_module_prefix; udt_type_name; udt_type } ->
        self#option self#string udt_module_prefix;
        self#string udt_type_name;
        (fun _ -> ()) udt_type

    method basic_type : basic_type -> unit = fun _ -> ()

    method wrapper_type : wrapper_type -> unit =
      fun { wt_type; wt_pk } ->
        self#basic_type wt_type;
        self#payload_kind wt_pk

    method field_type : field_type -> unit =
      fun x ->
        match x with
        | Ft_unit -> ()
        | Ft_basic_type a -> self#basic_type a
        | Ft_user_defined_type a -> self#user_defined_type a
        | Ft_wrapper_type a -> self#wrapper_type a

    method default_value : default_value -> unit =
      self#option self#pb_option__constant

    method associative_type : associative_type -> unit = fun _ -> ()
    method repeated_type : repeated_type -> unit = fun _ -> ()
    method encoding_number : encoding_number -> unit = self#int
    method is_packed : is_packed -> unit = self#bool

    method record_field_type : record_field_type -> unit =
      fun x ->
        match x with
        | Rft_nolabel a ->
          (fun (a, b, c) ->
            self#field_type a;
            self#encoding_number b;
            self#payload_kind c)
            a
        | Rft_required a ->
          (fun (a, b, c, d) ->
            self#field_type a;
            self#encoding_number b;
            self#payload_kind c;
            self#default_value d)
            a
        | Rft_optional a ->
          (fun (a, b, c, d) ->
            self#field_type a;
            self#encoding_number b;
            self#payload_kind c;
            self#default_value d)
            a
        | Rft_repeated a ->
          (fun (a, b, c, d, e) ->
            self#repeated_type a;
            self#field_type b;
            self#encoding_number c;
            self#payload_kind d;
            self#is_packed e)
            a
        | Rft_associative a ->
          (fun (a, b, c, d) ->
            self#associative_type a;
            self#encoding_number b;
            (fun (a, b) ->
              self#basic_type a;
              self#payload_kind b)
              c;
            (fun (a, b) ->
              self#field_type a;
              self#payload_kind b)
              d)
            a
        | Rft_variant a -> self#variant a

    method variant_constructor : variant_constructor -> unit =
      fun {
            vc_constructor;
            vc_field_type;
            vc_encoding_number;
            vc_payload_kind;
            vc_options;
          } ->
        self#string vc_constructor;
        self#variant_constructor_type vc_field_type;
        self#encoding_number vc_encoding_number;
        self#payload_kind vc_payload_kind;
        self#pb_option__set vc_options

    method variant_constructor_type : variant_constructor_type -> unit =
      fun x ->
        match x with
        | Vct_nullary -> ()
        | Vct_non_nullary_constructor a -> self#field_type a

    method variant : variant -> unit =
      fun { v_name; v_constructors } ->
        self#string v_name;
        self#list self#variant_constructor v_constructors

    method record_field : record_field -> unit =
      fun { rf_label; rf_field_type; rf_mutable; rf_options } ->
        self#string rf_label;
        self#record_field_type rf_field_type;
        self#bool rf_mutable;
        self#pb_option__set rf_options

    method record : record -> unit =
      fun { r_name; r_fields } ->
        self#string r_name;
        self#list self#record_field r_fields

    method const_variant_constructor : const_variant_constructor -> unit =
      fun { cvc_name; cvc_binary_value; cvc_string_value; cvc_options } ->
        self#string cvc_name;
        self#int cvc_binary_value;
        self#string cvc_string_value;
        self#pb_option__set cvc_options

    method const_variant : const_variant -> unit =
      fun { cv_name; cv_constructors } ->
        self#string cv_name;
        self#list self#const_variant_constructor cv_constructors

    method empty_record : empty_record -> unit =
      fun { er_name } -> self#string er_name

    method type_spec : type_spec -> unit =
      fun x ->
        match x with
        | Record a -> self#record a
        | Variant a -> self#variant a
        | Const_variant a -> self#const_variant a
        | Unit a -> self#empty_record a

    method type_ : type_ -> unit =
      fun { module_prefix; spec; type_level_ppx_extension; type_options } ->
        self#string module_prefix;
        self#type_spec spec;
        self#option self#string type_level_ppx_extension;
        self#pb_option__set type_options

    method rpc_type : rpc_type -> unit =
      fun x ->
        match x with
        | Rpc_scalar a -> self#field_type a
        | Rpc_stream a -> self#field_type a

    method rpc : rpc -> unit =
      fun { rpc_name; rpc_req; rpc_res } ->
        self#string rpc_name;
        self#rpc_type rpc_req;
        self#rpc_type rpc_res

    method service : service -> unit =
      fun { service_name; service_packages; service_body } ->
        self#string service_name;
        self#list self#string service_packages;
        self#list self#rpc service_body

    method proto : proto -> unit =
      fun { proto_types; proto_services } ->
        self#list (self#list self#type_) proto_types;
        self#list self#service proto_services
  end

class virtual ['acc] fold =
  object (self)
    method virtual bool : bool -> 'acc -> 'acc
    method virtual int : int -> 'acc -> 'acc
    method virtual list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc

    method virtual option
        : 'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc

    method virtual string : string -> 'acc -> 'acc
    method virtual pb_option__constant : Pb_option.constant -> 'acc -> 'acc
    method virtual pb_option__set : Pb_option.set -> 'acc -> 'acc

    method payload_kind : payload_kind -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Pk_varint a -> self#bool a acc
        | Pk_bits32 -> acc
        | Pk_bits64 -> acc
        | Pk_bytes -> acc

    method user_defined_type : user_defined_type -> 'acc -> 'acc =
      fun { udt_module_prefix; udt_type_name; udt_type } acc ->
        let acc = self#option self#string udt_module_prefix acc in
        let acc = self#string udt_type_name acc in
        let acc = (fun _ acc -> acc) udt_type acc in
        acc

    method basic_type : basic_type -> 'acc -> 'acc = fun _ acc -> acc

    method wrapper_type : wrapper_type -> 'acc -> 'acc =
      fun { wt_type; wt_pk } acc ->
        let acc = self#basic_type wt_type acc in
        let acc = self#payload_kind wt_pk acc in
        acc

    method field_type : field_type -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Ft_unit -> acc
        | Ft_basic_type a -> self#basic_type a acc
        | Ft_user_defined_type a -> self#user_defined_type a acc
        | Ft_wrapper_type a -> self#wrapper_type a acc

    method default_value : default_value -> 'acc -> 'acc =
      self#option self#pb_option__constant

    method associative_type : associative_type -> 'acc -> 'acc =
      fun _ acc -> acc

    method repeated_type : repeated_type -> 'acc -> 'acc = fun _ acc -> acc
    method encoding_number : encoding_number -> 'acc -> 'acc = self#int
    method is_packed : is_packed -> 'acc -> 'acc = self#bool

    method record_field_type : record_field_type -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Rft_nolabel a ->
          (fun (a, b, c) acc ->
            let acc = self#field_type a acc in
            let acc = self#encoding_number b acc in
            let acc = self#payload_kind c acc in
            acc)
            a acc
        | Rft_required a ->
          (fun (a, b, c, d) acc ->
            let acc = self#field_type a acc in
            let acc = self#encoding_number b acc in
            let acc = self#payload_kind c acc in
            let acc = self#default_value d acc in
            acc)
            a acc
        | Rft_optional a ->
          (fun (a, b, c, d) acc ->
            let acc = self#field_type a acc in
            let acc = self#encoding_number b acc in
            let acc = self#payload_kind c acc in
            let acc = self#default_value d acc in
            acc)
            a acc
        | Rft_repeated a ->
          (fun (a, b, c, d, e) acc ->
            let acc = self#repeated_type a acc in
            let acc = self#field_type b acc in
            let acc = self#encoding_number c acc in
            let acc = self#payload_kind d acc in
            let acc = self#is_packed e acc in
            acc)
            a acc
        | Rft_associative a ->
          (fun (a, b, c, d) acc ->
            let acc = self#associative_type a acc in
            let acc = self#encoding_number b acc in
            let acc =
              (fun (a, b) acc ->
                let acc = self#basic_type a acc in
                let acc = self#payload_kind b acc in
                acc)
                c acc
            in
            let acc =
              (fun (a, b) acc ->
                let acc = self#field_type a acc in
                let acc = self#payload_kind b acc in
                acc)
                d acc
            in
            acc)
            a acc
        | Rft_variant a -> self#variant a acc

    method variant_constructor : variant_constructor -> 'acc -> 'acc =
      fun {
            vc_constructor;
            vc_field_type;
            vc_encoding_number;
            vc_payload_kind;
            vc_options;
          } acc ->
        let acc = self#string vc_constructor acc in
        let acc = self#variant_constructor_type vc_field_type acc in
        let acc = self#encoding_number vc_encoding_number acc in
        let acc = self#payload_kind vc_payload_kind acc in
        let acc = self#pb_option__set vc_options acc in
        acc

    method variant_constructor_type : variant_constructor_type -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Vct_nullary -> acc
        | Vct_non_nullary_constructor a -> self#field_type a acc

    method variant : variant -> 'acc -> 'acc =
      fun { v_name; v_constructors } acc ->
        let acc = self#string v_name acc in
        let acc = self#list self#variant_constructor v_constructors acc in
        acc

    method record_field : record_field -> 'acc -> 'acc =
      fun { rf_label; rf_field_type; rf_mutable; rf_options } acc ->
        let acc = self#string rf_label acc in
        let acc = self#record_field_type rf_field_type acc in
        let acc = self#bool rf_mutable acc in
        let acc = self#pb_option__set rf_options acc in
        acc

    method record : record -> 'acc -> 'acc =
      fun { r_name; r_fields } acc ->
        let acc = self#string r_name acc in
        let acc = self#list self#record_field r_fields acc in
        acc

    method const_variant_constructor : const_variant_constructor -> 'acc -> 'acc
        =
      fun { cvc_name; cvc_binary_value; cvc_string_value; cvc_options } acc ->
        let acc = self#string cvc_name acc in
        let acc = self#int cvc_binary_value acc in
        let acc = self#string cvc_string_value acc in
        let acc = self#pb_option__set cvc_options acc in
        acc

    method const_variant : const_variant -> 'acc -> 'acc =
      fun { cv_name; cv_constructors } acc ->
        let acc = self#string cv_name acc in
        let acc =
          self#list self#const_variant_constructor cv_constructors acc
        in
        acc

    method empty_record : empty_record -> 'acc -> 'acc =
      fun { er_name } acc -> self#string er_name acc

    method type_spec : type_spec -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Record a -> self#record a acc
        | Variant a -> self#variant a acc
        | Const_variant a -> self#const_variant a acc
        | Unit a -> self#empty_record a acc

    method type_ : type_ -> 'acc -> 'acc =
      fun { module_prefix; spec; type_level_ppx_extension; type_options } acc ->
        let acc = self#string module_prefix acc in
        let acc = self#type_spec spec acc in
        let acc = self#option self#string type_level_ppx_extension acc in
        let acc = self#pb_option__set type_options acc in
        acc

    method rpc_type : rpc_type -> 'acc -> 'acc =
      fun x acc ->
        match x with
        | Rpc_scalar a -> self#field_type a acc
        | Rpc_stream a -> self#field_type a acc

    method rpc : rpc -> 'acc -> 'acc =
      fun { rpc_name; rpc_req; rpc_res } acc ->
        let acc = self#string rpc_name acc in
        let acc = self#rpc_type rpc_req acc in
        let acc = self#rpc_type rpc_res acc in
        acc

    method service : service -> 'acc -> 'acc =
      fun { service_name; service_packages; service_body } acc ->
        let acc = self#string service_name acc in
        let acc = self#list self#string service_packages acc in
        let acc = self#list self#rpc service_body acc in
        acc

    method proto : proto -> 'acc -> 'acc =
      fun { proto_types; proto_services } acc ->
        let acc = self#list (self#list self#type_) proto_types acc in
        let acc = self#list self#service proto_services acc in
        acc
  end

class virtual ['acc] fold_map =
  object (self)
    method virtual bool : bool -> 'acc -> bool * 'acc
    method virtual int : int -> 'acc -> int * 'acc

    method virtual list
        : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc

    method virtual option
        : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc

    method virtual string : string -> 'acc -> string * 'acc

    method virtual pb_option__constant
        : Pb_option.constant -> 'acc -> Pb_option.constant * 'acc

    method virtual pb_option__set
        : Pb_option.set -> 'acc -> Pb_option.set * 'acc

    method payload_kind : payload_kind -> 'acc -> payload_kind * 'acc =
      fun x acc ->
        match x with
        | Pk_varint a ->
          let a, acc = self#bool a acc in
          Pk_varint a, acc
        | Pk_bits32 -> Pk_bits32, acc
        | Pk_bits64 -> Pk_bits64, acc
        | Pk_bytes -> Pk_bytes, acc

    method user_defined_type
        : user_defined_type -> 'acc -> user_defined_type * 'acc =
      fun { udt_module_prefix; udt_type_name; udt_type } acc ->
        let udt_module_prefix, acc =
          self#option self#string udt_module_prefix acc
        in
        let udt_type_name, acc = self#string udt_type_name acc in
        let udt_type, acc = (fun x acc -> x, acc) udt_type acc in
        { udt_module_prefix; udt_type_name; udt_type }, acc

    method basic_type : basic_type -> 'acc -> basic_type * 'acc =
      fun x acc -> x, acc

    method wrapper_type : wrapper_type -> 'acc -> wrapper_type * 'acc =
      fun { wt_type; wt_pk } acc ->
        let wt_type, acc = self#basic_type wt_type acc in
        let wt_pk, acc = self#payload_kind wt_pk acc in
        { wt_type; wt_pk }, acc

    method field_type : field_type -> 'acc -> field_type * 'acc =
      fun x acc ->
        match x with
        | Ft_unit -> Ft_unit, acc
        | Ft_basic_type a ->
          let a, acc = self#basic_type a acc in
          Ft_basic_type a, acc
        | Ft_user_defined_type a ->
          let a, acc = self#user_defined_type a acc in
          Ft_user_defined_type a, acc
        | Ft_wrapper_type a ->
          let a, acc = self#wrapper_type a acc in
          Ft_wrapper_type a, acc

    method default_value : default_value -> 'acc -> default_value * 'acc =
      self#option self#pb_option__constant

    method associative_type
        : associative_type -> 'acc -> associative_type * 'acc =
      fun x acc -> x, acc

    method repeated_type : repeated_type -> 'acc -> repeated_type * 'acc =
      fun x acc -> x, acc

    method encoding_number : encoding_number -> 'acc -> encoding_number * 'acc =
      self#int

    method is_packed : is_packed -> 'acc -> is_packed * 'acc = self#bool

    method record_field_type
        : record_field_type -> 'acc -> record_field_type * 'acc =
      fun x acc ->
        match x with
        | Rft_nolabel a ->
          let a, acc =
            (fun (a, b, c) acc ->
              let a, acc = self#field_type a acc in
              let b, acc = self#encoding_number b acc in
              let c, acc = self#payload_kind c acc in
              (a, b, c), acc)
              a acc
          in
          Rft_nolabel a, acc
        | Rft_required a ->
          let a, acc =
            (fun (a, b, c, d) acc ->
              let a, acc = self#field_type a acc in
              let b, acc = self#encoding_number b acc in
              let c, acc = self#payload_kind c acc in
              let d, acc = self#default_value d acc in
              (a, b, c, d), acc)
              a acc
          in
          Rft_required a, acc
        | Rft_optional a ->
          let a, acc =
            (fun (a, b, c, d) acc ->
              let a, acc = self#field_type a acc in
              let b, acc = self#encoding_number b acc in
              let c, acc = self#payload_kind c acc in
              let d, acc = self#default_value d acc in
              (a, b, c, d), acc)
              a acc
          in
          Rft_optional a, acc
        | Rft_repeated a ->
          let a, acc =
            (fun (a, b, c, d, e) acc ->
              let a, acc = self#repeated_type a acc in
              let b, acc = self#field_type b acc in
              let c, acc = self#encoding_number c acc in
              let d, acc = self#payload_kind d acc in
              let e, acc = self#is_packed e acc in
              (a, b, c, d, e), acc)
              a acc
          in
          Rft_repeated a, acc
        | Rft_associative a ->
          let a, acc =
            (fun (a, b, c, d) acc ->
              let a, acc = self#associative_type a acc in
              let b, acc = self#encoding_number b acc in
              let c, acc =
                (fun (a, b) acc ->
                  let a, acc = self#basic_type a acc in
                  let b, acc = self#payload_kind b acc in
                  (a, b), acc)
                  c acc
              in
              let d, acc =
                (fun (a, b) acc ->
                  let a, acc = self#field_type a acc in
                  let b, acc = self#payload_kind b acc in
                  (a, b), acc)
                  d acc
              in
              (a, b, c, d), acc)
              a acc
          in
          Rft_associative a, acc
        | Rft_variant a ->
          let a, acc = self#variant a acc in
          Rft_variant a, acc

    method variant_constructor
        : variant_constructor -> 'acc -> variant_constructor * 'acc =
      fun {
            vc_constructor;
            vc_field_type;
            vc_encoding_number;
            vc_payload_kind;
            vc_options;
          } acc ->
        let vc_constructor, acc = self#string vc_constructor acc in
        let vc_field_type, acc =
          self#variant_constructor_type vc_field_type acc
        in
        let vc_encoding_number, acc =
          self#encoding_number vc_encoding_number acc
        in
        let vc_payload_kind, acc = self#payload_kind vc_payload_kind acc in
        let vc_options, acc = self#pb_option__set vc_options acc in
        ( {
            vc_constructor;
            vc_field_type;
            vc_encoding_number;
            vc_payload_kind;
            vc_options;
          },
          acc )

    method variant_constructor_type
        : variant_constructor_type -> 'acc -> variant_constructor_type * 'acc =
      fun x acc ->
        match x with
        | Vct_nullary -> Vct_nullary, acc
        | Vct_non_nullary_constructor a ->
          let a, acc = self#field_type a acc in
          Vct_non_nullary_constructor a, acc

    method variant : variant -> 'acc -> variant * 'acc =
      fun { v_name; v_constructors } acc ->
        let v_name, acc = self#string v_name acc in
        let v_constructors, acc =
          self#list self#variant_constructor v_constructors acc
        in
        { v_name; v_constructors }, acc

    method record_field : record_field -> 'acc -> record_field * 'acc =
      fun { rf_label; rf_field_type; rf_mutable; rf_options } acc ->
        let rf_label, acc = self#string rf_label acc in
        let rf_field_type, acc = self#record_field_type rf_field_type acc in
        let rf_mutable, acc = self#bool rf_mutable acc in
        let rf_options, acc = self#pb_option__set rf_options acc in
        { rf_label; rf_field_type; rf_mutable; rf_options }, acc

    method record : record -> 'acc -> record * 'acc =
      fun { r_name; r_fields } acc ->
        let r_name, acc = self#string r_name acc in
        let r_fields, acc = self#list self#record_field r_fields acc in
        { r_name; r_fields }, acc

    method const_variant_constructor
        : const_variant_constructor -> 'acc -> const_variant_constructor * 'acc
        =
      fun { cvc_name; cvc_binary_value; cvc_string_value; cvc_options } acc ->
        let cvc_name, acc = self#string cvc_name acc in
        let cvc_binary_value, acc = self#int cvc_binary_value acc in
        let cvc_string_value, acc = self#string cvc_string_value acc in
        let cvc_options, acc = self#pb_option__set cvc_options acc in
        { cvc_name; cvc_binary_value; cvc_string_value; cvc_options }, acc

    method const_variant : const_variant -> 'acc -> const_variant * 'acc =
      fun { cv_name; cv_constructors } acc ->
        let cv_name, acc = self#string cv_name acc in
        let cv_constructors, acc =
          self#list self#const_variant_constructor cv_constructors acc
        in
        { cv_name; cv_constructors }, acc

    method empty_record : empty_record -> 'acc -> empty_record * 'acc =
      fun { er_name } acc ->
        let er_name, acc = self#string er_name acc in
        { er_name }, acc

    method type_spec : type_spec -> 'acc -> type_spec * 'acc =
      fun x acc ->
        match x with
        | Record a ->
          let a, acc = self#record a acc in
          Record a, acc
        | Variant a ->
          let a, acc = self#variant a acc in
          Variant a, acc
        | Const_variant a ->
          let a, acc = self#const_variant a acc in
          Const_variant a, acc
        | Unit a ->
          let a, acc = self#empty_record a acc in
          Unit a, acc

    method type_ : type_ -> 'acc -> type_ * 'acc =
      fun { module_prefix; spec; type_level_ppx_extension; type_options } acc ->
        let module_prefix, acc = self#string module_prefix acc in
        let spec, acc = self#type_spec spec acc in
        let type_level_ppx_extension, acc =
          self#option self#string type_level_ppx_extension acc
        in
        let type_options, acc = self#pb_option__set type_options acc in
        { module_prefix; spec; type_level_ppx_extension; type_options }, acc

    method rpc_type : rpc_type -> 'acc -> rpc_type * 'acc =
      fun x acc ->
        match x with
        | Rpc_scalar a ->
          let a, acc = self#field_type a acc in
          Rpc_scalar a, acc
        | Rpc_stream a ->
          let a, acc = self#field_type a acc in
          Rpc_stream a, acc

    method rpc : rpc -> 'acc -> rpc * 'acc =
      fun { rpc_name; rpc_req; rpc_res } acc ->
        let rpc_name, acc = self#string rpc_name acc in
        let rpc_req, acc = self#rpc_type rpc_req acc in
        let rpc_res, acc = self#rpc_type rpc_res acc in
        { rpc_name; rpc_req; rpc_res }, acc

    method service : service -> 'acc -> service * 'acc =
      fun { service_name; service_packages; service_body } acc ->
        let service_name, acc = self#string service_name acc in
        let service_packages, acc =
          self#list self#string service_packages acc
        in
        let service_body, acc = self#list self#rpc service_body acc in
        { service_name; service_packages; service_body }, acc

    method proto : proto -> 'acc -> proto * 'acc =
      fun { proto_types; proto_services } acc ->
        let proto_types, acc =
          self#list (self#list self#type_) proto_types acc
        in
        let proto_services, acc = self#list self#service proto_services acc in
        { proto_types; proto_services }, acc
  end

class virtual ['ctx] map_with_context =
  object (self)
    method virtual bool : 'ctx -> bool -> bool
    method virtual int : 'ctx -> int -> int
    method virtual list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list

    method virtual option
        : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option

    method virtual string : 'ctx -> string -> string

    method virtual pb_option__constant
        : 'ctx -> Pb_option.constant -> Pb_option.constant

    method virtual pb_option__set : 'ctx -> Pb_option.set -> Pb_option.set

    method payload_kind : 'ctx -> payload_kind -> payload_kind =
      fun ctx x ->
        match x with
        | Pk_varint a ->
          let a = self#bool ctx a in
          Pk_varint a
        | Pk_bits32 -> Pk_bits32
        | Pk_bits64 -> Pk_bits64
        | Pk_bytes -> Pk_bytes

    method user_defined_type : 'ctx -> user_defined_type -> user_defined_type =
      fun ctx { udt_module_prefix; udt_type_name; udt_type } ->
        let udt_module_prefix = self#option self#string ctx udt_module_prefix in
        let udt_type_name = self#string ctx udt_type_name in
        let udt_type = (fun _ctx x -> x) ctx udt_type in
        { udt_module_prefix; udt_type_name; udt_type }

    method basic_type : 'ctx -> basic_type -> basic_type = fun _ctx x -> x

    method wrapper_type : 'ctx -> wrapper_type -> wrapper_type =
      fun ctx { wt_type; wt_pk } ->
        let wt_type = self#basic_type ctx wt_type in
        let wt_pk = self#payload_kind ctx wt_pk in
        { wt_type; wt_pk }

    method field_type : 'ctx -> field_type -> field_type =
      fun ctx x ->
        match x with
        | Ft_unit -> Ft_unit
        | Ft_basic_type a ->
          let a = self#basic_type ctx a in
          Ft_basic_type a
        | Ft_user_defined_type a ->
          let a = self#user_defined_type ctx a in
          Ft_user_defined_type a
        | Ft_wrapper_type a ->
          let a = self#wrapper_type ctx a in
          Ft_wrapper_type a

    method default_value : 'ctx -> default_value -> default_value =
      self#option self#pb_option__constant

    method associative_type : 'ctx -> associative_type -> associative_type =
      fun _ctx x -> x

    method repeated_type : 'ctx -> repeated_type -> repeated_type =
      fun _ctx x -> x

    method encoding_number : 'ctx -> encoding_number -> encoding_number =
      self#int

    method is_packed : 'ctx -> is_packed -> is_packed = self#bool

    method record_field_type : 'ctx -> record_field_type -> record_field_type =
      fun ctx x ->
        match x with
        | Rft_nolabel a ->
          let a =
            (fun ctx (a, b, c) ->
              let a = self#field_type ctx a in
              let b = self#encoding_number ctx b in
              let c = self#payload_kind ctx c in
              a, b, c)
              ctx a
          in
          Rft_nolabel a
        | Rft_required a ->
          let a =
            (fun ctx (a, b, c, d) ->
              let a = self#field_type ctx a in
              let b = self#encoding_number ctx b in
              let c = self#payload_kind ctx c in
              let d = self#default_value ctx d in
              a, b, c, d)
              ctx a
          in
          Rft_required a
        | Rft_optional a ->
          let a =
            (fun ctx (a, b, c, d) ->
              let a = self#field_type ctx a in
              let b = self#encoding_number ctx b in
              let c = self#payload_kind ctx c in
              let d = self#default_value ctx d in
              a, b, c, d)
              ctx a
          in
          Rft_optional a
        | Rft_repeated a ->
          let a =
            (fun ctx (a, b, c, d, e) ->
              let a = self#repeated_type ctx a in
              let b = self#field_type ctx b in
              let c = self#encoding_number ctx c in
              let d = self#payload_kind ctx d in
              let e = self#is_packed ctx e in
              a, b, c, d, e)
              ctx a
          in
          Rft_repeated a
        | Rft_associative a ->
          let a =
            (fun ctx (a, b, c, d) ->
              let a = self#associative_type ctx a in
              let b = self#encoding_number ctx b in
              let c =
                (fun ctx (a, b) ->
                  let a = self#basic_type ctx a in
                  let b = self#payload_kind ctx b in
                  a, b)
                  ctx c
              in
              let d =
                (fun ctx (a, b) ->
                  let a = self#field_type ctx a in
                  let b = self#payload_kind ctx b in
                  a, b)
                  ctx d
              in
              a, b, c, d)
              ctx a
          in
          Rft_associative a
        | Rft_variant a ->
          let a = self#variant ctx a in
          Rft_variant a

    method variant_constructor
        : 'ctx -> variant_constructor -> variant_constructor =
      fun ctx
          {
            vc_constructor;
            vc_field_type;
            vc_encoding_number;
            vc_payload_kind;
            vc_options;
          } ->
        let vc_constructor = self#string ctx vc_constructor in
        let vc_field_type = self#variant_constructor_type ctx vc_field_type in
        let vc_encoding_number = self#encoding_number ctx vc_encoding_number in
        let vc_payload_kind = self#payload_kind ctx vc_payload_kind in
        let vc_options = self#pb_option__set ctx vc_options in
        {
          vc_constructor;
          vc_field_type;
          vc_encoding_number;
          vc_payload_kind;
          vc_options;
        }

    method variant_constructor_type
        : 'ctx -> variant_constructor_type -> variant_constructor_type =
      fun ctx x ->
        match x with
        | Vct_nullary -> Vct_nullary
        | Vct_non_nullary_constructor a ->
          let a = self#field_type ctx a in
          Vct_non_nullary_constructor a

    method variant : 'ctx -> variant -> variant =
      fun ctx { v_name; v_constructors } ->
        let v_name = self#string ctx v_name in
        let v_constructors =
          self#list self#variant_constructor ctx v_constructors
        in
        { v_name; v_constructors }

    method record_field : 'ctx -> record_field -> record_field =
      fun ctx { rf_label; rf_field_type; rf_mutable; rf_options } ->
        let rf_label = self#string ctx rf_label in
        let rf_field_type = self#record_field_type ctx rf_field_type in
        let rf_mutable = self#bool ctx rf_mutable in
        let rf_options = self#pb_option__set ctx rf_options in
        { rf_label; rf_field_type; rf_mutable; rf_options }

    method record : 'ctx -> record -> record =
      fun ctx { r_name; r_fields } ->
        let r_name = self#string ctx r_name in
        let r_fields = self#list self#record_field ctx r_fields in
        { r_name; r_fields }

    method const_variant_constructor
        : 'ctx -> const_variant_constructor -> const_variant_constructor =
      fun ctx { cvc_name; cvc_binary_value; cvc_string_value; cvc_options } ->
        let cvc_name = self#string ctx cvc_name in
        let cvc_binary_value = self#int ctx cvc_binary_value in
        let cvc_string_value = self#string ctx cvc_string_value in
        let cvc_options = self#pb_option__set ctx cvc_options in
        { cvc_name; cvc_binary_value; cvc_string_value; cvc_options }

    method const_variant : 'ctx -> const_variant -> const_variant =
      fun ctx { cv_name; cv_constructors } ->
        let cv_name = self#string ctx cv_name in
        let cv_constructors =
          self#list self#const_variant_constructor ctx cv_constructors
        in
        { cv_name; cv_constructors }

    method empty_record : 'ctx -> empty_record -> empty_record =
      fun ctx { er_name } ->
        let er_name = self#string ctx er_name in
        { er_name }

    method type_spec : 'ctx -> type_spec -> type_spec =
      fun ctx x ->
        match x with
        | Record a ->
          let a = self#record ctx a in
          Record a
        | Variant a ->
          let a = self#variant ctx a in
          Variant a
        | Const_variant a ->
          let a = self#const_variant ctx a in
          Const_variant a
        | Unit a ->
          let a = self#empty_record ctx a in
          Unit a

    method type_ : 'ctx -> type_ -> type_ =
      fun ctx { module_prefix; spec; type_level_ppx_extension; type_options } ->
        let module_prefix = self#string ctx module_prefix in
        let spec = self#type_spec ctx spec in
        let type_level_ppx_extension =
          self#option self#string ctx type_level_ppx_extension
        in
        let type_options = self#pb_option__set ctx type_options in
        { module_prefix; spec; type_level_ppx_extension; type_options }

    method rpc_type : 'ctx -> rpc_type -> rpc_type =
      fun ctx x ->
        match x with
        | Rpc_scalar a ->
          let a = self#field_type ctx a in
          Rpc_scalar a
        | Rpc_stream a ->
          let a = self#field_type ctx a in
          Rpc_stream a

    method rpc : 'ctx -> rpc -> rpc =
      fun ctx { rpc_name; rpc_req; rpc_res } ->
        let rpc_name = self#string ctx rpc_name in
        let rpc_req = self#rpc_type ctx rpc_req in
        let rpc_res = self#rpc_type ctx rpc_res in
        { rpc_name; rpc_req; rpc_res }

    method service : 'ctx -> service -> service =
      fun ctx { service_name; service_packages; service_body } ->
        let service_name = self#string ctx service_name in
        let service_packages = self#list self#string ctx service_packages in
        let service_body = self#list self#rpc ctx service_body in
        { service_name; service_packages; service_body }

    method proto : 'ctx -> proto -> proto =
      fun ctx { proto_types; proto_services } ->
        let proto_types = self#list (self#list self#type_) ctx proto_types in
        let proto_services = self#list self#service ctx proto_services in
        { proto_types; proto_services }
  end

[@@@end]
