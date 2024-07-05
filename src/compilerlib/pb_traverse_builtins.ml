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

module Ppxlib_traverse_builtins = struct
  (*
    BEGIN vendored Jane Street ppxlib builtins

    The MIT License

    Copyright (c) 2018 Jane Street Group, LLC opensource@janestreet.com
  *)
  module T = struct
    type 'a map = 'a -> 'a
    type 'a iter = 'a -> unit
    type ('a, 'acc) fold = 'a -> 'acc -> 'acc
    type ('a, 'acc) fold_map = 'a -> 'acc -> 'a * 'acc
    type ('ctx, 'a) map_with_context = 'ctx -> 'a -> 'a
    type ('a, 'res) lift = 'a -> 'res
    type ('ctx, 'a, 'res) lift_map_with_context = 'ctx -> 'a -> 'a * 'res
  end

  class map =
    let any x = x in
    object
      method int : int T.map = any
      method string : string T.map = any
      method bool : bool T.map = any
      method char : char T.map = any

      method option : 'a. 'a T.map -> 'a option T.map =
        fun f x ->
          match x with
          | None -> None
          | Some x -> Some (f x)

      method list : 'a. 'a T.map -> 'a list T.map = List.map
      method array : 'a. 'a T.map -> 'a array T.map = Array.map
    end

  class iter =
    let any = ignore in
    object
      method int : int T.iter = any
      method string : string T.iter = any
      method bool : bool T.iter = any
      method char : char T.iter = any

      method option : 'a. 'a T.iter -> 'a option T.iter =
        fun f x ->
          match x with
          | None -> ()
          | Some x -> f x

      method list : 'a. 'a T.iter -> 'a list T.iter = List.iter
      method array : 'a. 'a T.iter -> 'a array T.iter = Array.iter
    end

  class ['acc] fold =
    let any _ acc = acc in
    object
      method int : (int, 'acc) T.fold = any
      method string : (string, 'acc) T.fold = any
      method bool : (bool, 'acc) T.fold = any
      method char : (char, 'acc) T.fold = any

      method option : 'a. ('a, 'acc) T.fold -> ('a option, 'acc) T.fold =
        fun f x acc ->
          match x with
          | None -> acc
          | Some x -> f x acc

      method list : 'a. ('a, 'acc) T.fold -> ('a list, 'acc) T.fold =
        let rec loop f l acc =
          match l with
          | [] -> acc
          | x :: l -> loop f l (f x acc)
        in
        loop

      method array : 'a. ('a, 'acc) T.fold -> ('a array, 'acc) T.fold =
        fun f a acc ->
          let r = ref acc in
          for i = 0 to Array.length a - 1 do
            r := f (Array.unsafe_get a i) !r
          done;
          !r
    end

  class ['acc] fold_map =
    let any x acc = x, acc in
    object
      method int : (int, 'acc) T.fold_map = any
      method string : (string, 'acc) T.fold_map = any
      method bool : (bool, 'acc) T.fold_map = any
      method char : (char, 'acc) T.fold_map = any

      method option : 'a. ('a, 'acc) T.fold_map -> ('a option, 'acc) T.fold_map
          =
        fun f x acc ->
          match x with
          | None -> None, acc
          | Some x ->
            let x, acc = f x acc in
            Some x, acc

      method list : 'a. ('a, 'acc) T.fold_map -> ('a list, 'acc) T.fold_map =
        let rec loop f l acc =
          match l with
          | [] -> [], acc
          | x :: l ->
            let x, acc = f x acc in
            let l, acc = loop f l acc in
            x :: l, acc
        in
        loop

      method array : 'a. ('a, 'acc) T.fold_map -> ('a array, 'acc) T.fold_map =
        fun f a acc ->
          let len = Array.length a in
          if len = 0 then
            a, acc
          else (
            let x, acc = f (Array.unsafe_get a 0) acc in
            let a' = Array.make len x in
            let r = ref acc in
            for i = 1 to len - 1 do
              let x, acc = f (Array.unsafe_get a i) !r in
              Array.unsafe_set a' i x;
              r := acc
            done;
            a', !r
          )
    end

  class ['ctx] map_with_context =
    let any _ x = x in
    object
      method int : ('ctx, int) T.map_with_context = any
      method string : ('ctx, string) T.map_with_context = any
      method bool : ('ctx, bool) T.map_with_context = any
      method char : ('ctx, char) T.map_with_context = any

      method option
          : 'a.
            ('ctx, 'a) T.map_with_context ->
            ('ctx, 'a option) T.map_with_context =
        fun f ctx x ->
          match x with
          | None -> None
          | Some x -> Some (f ctx x)

      method list
          : 'a.
            ('ctx, 'a) T.map_with_context -> ('ctx, 'a list) T.map_with_context
          =
        fun f ctx l -> List.map (f ctx) l

      method array
          : 'a.
            ('ctx, 'a) T.map_with_context -> ('ctx, 'a array) T.map_with_context
          =
        fun f ctx a -> Array.map (f ctx) a
    end
  (*
    END vendored Jane Street ppxlib builtins
  *)
end

module T = Ppxlib_traverse_builtins.T

class map =
  let any x = x in
  object
    method pb_option__constant : Pb_option.constant T.map = any
    method pb_option__set : Pb_option.set T.map = any
  end

class ['acc] fold =
  let any _ acc = acc in
  object
    method pb_option__constant : (Pb_option.constant, 'acc) T.fold = any
    method pb_option__set : (Pb_option.set, 'acc) T.fold = any
  end

class ['acc] fold_map =
  let any x acc = x, acc in
  object
    method pb_option__constant : (Pb_option.constant, 'acc) T.fold_map = any
    method pb_option__set : (Pb_option.set, 'acc) T.fold_map = any
  end

class ['ctx] map_with_context =
  let any _ x = x in
  object
    method pb_option__constant : ('ctx, Pb_option.constant) T.map_with_context =
      any

    method pb_option__set : ('ctx, Pb_option.set) T.map_with_context = any
  end
