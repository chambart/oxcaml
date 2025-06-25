(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Always_expose
  | Never_expose
  | Default_expose

let [@ocamlformat "disable"] print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Always_expose -> fprintf ppf "Always_expose"
  | Never_expose -> fprintf ppf "Never_expose"
  | Default_expose -> fprintf ppf "Default_expose"

let equal t1 t2 =
  match t1, t2 with
  | Always_expose, Always_expose
  | Never_expose, Never_expose
  | Default_expose, Default_expose ->
    true
  | (Always_expose | Never_expose | Default_expose), _ -> false

let is_default t =
  match t with Default_expose -> true | Always_expose | Never_expose -> false

let from_lambda (attr : Lambda.expose_attribute) =
  match attr with
  | Always_expose -> Always_expose
  | Never_expose -> Never_expose
  | Default_expose -> Default_expose
