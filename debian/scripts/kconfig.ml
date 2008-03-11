#!/usr/bin/ocamlrun /usr/bin/ocaml
(*
 * kconfig.ml Copyright (C) 2006 Sven Luther <sl@powerlinux.fr>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License, version 2, as
 * published by * the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

(*
 * Syntax :
 *  kconfig.ml <action> [-bs sourcedir] <file> [<file>]
 *    <action> : single | check | create | diff
 *    <file> : -c <filename> | [-b basedir | -ba basedir]  -a arch [-s subarch] -f flavour
 *)

let usage_string =
  "Usage:\n\n" ^
  "  Check single config file : ./kconfig.ml single -c <filename>\n" ^
  "  Create config file       : ./kconfig.ml create [ -ba <dir> | -b <dir> ] -a <arch> [ -s <subarch> ] -f <flavour>\n" ^
  "  Check all config files   : ./kconfig.ml check [ -ba <dir> | -b <dir> ] [ -bs <dir> ]\n" ^
  "  Diff two config files    : ./kconfig.ml diff [ -ba <dir> | -b <dir> ] config config\n" ^
  "           where config is : (-c <filename> | -a <arch> [ -s <subarch> ] -f <flavour>)\n" ^
  "\nOptions:\n"

(* Command line argument parsing *)
exception Bad_action of string
exception Double_action of string
exception Bad_args of string

type action = Single | Check | Create | Diff | NoAction | Help
let action = ref NoAction
let set_action a =
  if !action != NoAction then raise (Double_action a) else
  match a with 
  | "check" ->  action := Check
  | "create" ->  action := Create
  | "diff" ->  action := Diff
  | "single" ->  action := Single
  | _ -> raise (Bad_action a)
let string_of_action = function
  | Single -> "single"
  | Check -> "check"
  | Create -> "create"
  | Diff ->  "diff"
  | NoAction ->  "NoAction"
  | Help ->  "Help"

let basedir = ref "debian/arch"
let sourcedir = ref "."
let set_basedir archindir dir =
  basedir := if archindir then Filename.dirname dir else dir

type kind = TConfig | TArch | TSubarch | TFlavour
type temp = (kind * string) list
let temp : temp ref = ref []
let set_temp k s = temp := (k,s) :: !temp 
let rec print_temp = function
  | [] -> ()
  | (TConfig,s)::t -> Printf.printf "<Config %s>\n" s; print_temp t
  | (TArch,s)::t -> Printf.printf "<Arch %s>\n" s; print_temp t
  | (TSubarch,s)::t -> Printf.printf "<Subarch %s>\n" s; print_temp t
  | (TFlavour,s)::t -> Printf.printf "<Flavour %s>\n" s; print_temp t
let string_of_temp_error a s l =
  let rec pte e = function
  | [] -> e
  | (TConfig,s)::t -> pte (e ^ (Printf.sprintf "-c %s " s)) t
  | (TArch,s)::t -> pte (e ^ (Printf.sprintf "-a %s " s)) t
  | (TSubarch,s)::t -> pte (e ^ (Printf.sprintf "-s %s " s)) t
  | (TFlavour,s)::t -> pte (e ^ (Printf.sprintf "-f %s " s)) t
  in
  (match a with Some a -> Printf.sprintf"-a %s " a | None -> "") ^
  (match s with Some s -> Printf.sprintf "-s %s " s | None -> "") ^
  (pte "" l)

type config = Config of string | Flavour of string * string | Subarch of string * string * string
let config_of_temp t = 
  let rec config_of_temp ta ts l = match ta, ts, l with
  | None, None, (TArch,a)::t -> config_of_temp (Some a) None t
  | Some a, None, (TSubarch,s)::t -> config_of_temp (Some a) (Some s) t
  | Some a, Some s, (TFlavour,f)::t -> (Subarch (a,s,f))::(config_of_temp None None t)
  | Some a, None, (TFlavour,f)::t -> (Flavour (a,f))::(config_of_temp None None t)
  | None, None, (TConfig,f)::t -> (Config f)::(config_of_temp None None t)
  | None, None, [] -> []
  | _, _, _ -> raise (Bad_args (string_of_temp_error ta ts l))
  in config_of_temp None None t 

let rec print_configs = function
  | [] -> ()
  | (Config f) :: t -> Printf.printf "Config file: %s\n" f; print_configs t
  | (Flavour (a,f)) :: t -> Printf.printf "Arch: %s, Flavour: %s\n" a f; print_configs t
  | (Subarch (a,s,f)) :: t -> Printf.printf "Arch: %s, Subarch: %s, Flavour: %s\n" a s f; print_configs t

let verbose = ref false

let spec = [
  "-b", Arg.String (set_basedir false), "specify basedir of the arch configurations [default: debian/arch]";
  "-ba", Arg.String (set_basedir true), "specify basedir of the arch configurations (after stripping arch dir) [default: debian/arch]";
  "-bs", Arg.Set_string sourcedir, "base source dir containing the patched debian linux source tree [default: .]";
  "-a", Arg.String (set_temp TArch), "specify config arch";
  "-s", Arg.String (set_temp TSubarch), "specify config subarch";
  "-f", Arg.String (set_temp TFlavour), "specify config flavour";
  "-c", Arg.String (set_temp TConfig), "specify config file name";
  "-v", Arg.Set verbose, "verbose output";
  "-h", Arg.Unit (function () -> action := Help), "Display this list of options";
]

let usage () = Arg.usage spec usage_string

(* Config file parsing *)
type options =
  | Config_Yes of string
  | Config_No of string
  | Config_Module of string
  | Config_Value of string * string
  | Config_Comment of string
  | Config_Empty

let print_option = function
  | Config_Yes s -> Printf.printf "CONFIG_%s=y\n" s
  | Config_No s -> Printf.printf "# CONFIG_%s is not set\n" s
  | Config_Module s -> Printf.printf "CONFIG_%s=m\n" s
  | Config_Value (s,v) -> Printf.printf "CONFIG_%s=%s\n" s v
  | Config_Comment s -> Printf.printf "#%s\n" s
  | Config_Empty -> Printf.printf "\n"
  
exception Comment
exception Error
  
let parse_config_line fd =
  let line = input_line fd in
  let len = String.length line in
  if len = 0 then Config_Empty else
  try
    if len <= 9 then raise Comment else 
    match line.[0], line.[1], line.[2], line.[3], line.[4], line.[5], line.[6], line.[7], line.[8] with 
    | '#', ' ', 'C', 'O', 'N', 'F', 'I', 'G', '_' ->
      begin
        try
          let space = String.index_from line 8 ' ' in
	  if String.sub line (space + 1) 10 = "is not set" then 
	    let o = String.sub line 9 (space - 9) in
	    Config_No o
	  else raise Comment
        with Not_found | Invalid_argument "String.sub" -> raise Comment
      end
    | '#', _, _, _, _, _, _, _, _ -> raise Comment
    | 'C', 'O', 'N', 'F', 'I', 'G', _, _, _ ->
      begin
        try
          let equal = String.index_from line 6 '=' in
	  let o = String.sub line 7 (equal - 7) in
	  let v = String.sub line (equal + 1) (len - equal - 1) in
	  match v with
	  | "y" -> Config_Yes o
	  | "m" -> Config_Module o
	  | _ -> Config_Value (o,v)
        with Not_found | Invalid_argument "String.sub" -> raise Comment
      end
    | _ -> raise Comment
  with Comment -> Config_Comment (String.sub line 1 (len - 1))

module C = Map.Make (String)

(* Map.add behavior ensures the latest entry is the one staying *)
let rec parse_config fd m =
  try 
    let line = parse_config_line fd in
    match line with
    | Config_Comment _ | Config_Empty -> parse_config fd m
    | Config_Yes s | Config_No s | Config_Module s | Config_Value (s,_) ->
      parse_config fd (C.add s line m)
  with End_of_file -> m

let print_config m = C.iter (function _ -> print_option) m

let parse_config_file name m force =
  try 
    let config = open_in name in
    let m = parse_config config m in
    close_in config;
    m
  with Sys_error s ->
    if force then raise (Sys_error s) else m

(* Diffing two config files *)
module Diff = Map.Make (String)
type diff = Add of options | Del of options | Change of options * options
let diff_configs a b =
  let diff1 n v (b,d) = try
    let v' = C.find n b in
    let b = C.remove n b in
    if v = v' then b, d else b, Diff.add n (Change (v,v')) d
  with Not_found -> b, Diff.add n (Add v) d
  in
  let diff2 n v d = Diff.add n (Del v) d in
  let b,d = C.fold diff1 a (b, Diff.empty) in
  C.fold diff2 b d
let print_diff d = 
  let print_diff n = function
    | Add v -> Printf.printf "+ "; print_option v
    | Del v -> Printf.printf "- "; print_option v
    | Change (v,v') -> Printf.printf "+ "; print_option v; Printf.printf "- "; print_option v'
  in
  Diff.iter print_diff d

(* Defines parsing *)
type define =
  | Defines_Base of string
  | Defines_Field of string * string
  | Defines_List of string
  | Defines_Comment of string
  | Defines_Error of string
  | Defines_Empty

let print_define = function
  | Defines_Base s -> Printf.printf "[%s]\n" s
  | Defines_Field (n, v) -> Printf.printf "%s:%s\n" n v
  | Defines_List s -> Printf.printf " %s\n" s
  | Defines_Comment s -> Printf.printf "#%s\n" s
  | Defines_Error s -> Printf.printf "*** ERROR *** %s\n" s
  | Defines_Empty -> Printf.printf "\n"

let parse_define_line fd =
  let line = input_line fd in
  let len = String.length line in
  if len = 0 then begin Defines_Empty end else
  try
    match line.[0] with
    | '#' -> Defines_Comment (String.sub line 1 (len - 1))
    | '[' -> begin
        try 
          let c = String.index_from line 1 ']' in
	  Defines_Base (String.sub line 1 (c - 1))
        with Not_found | Invalid_argument "String.sub" -> raise Error
      end
    | ' ' -> Defines_List (String.sub line 1 (len - 1))
    | _ ->  begin
        try 
          let c = String.index_from line 1 ':' in
	  Defines_Field (String.sub line 0 c, String.sub line (c + 1) (len - c - 1))
        with Not_found | Invalid_argument "String.sub" -> raise Error
      end
  with Error -> Defines_Error line

let rec parse_defines fd m l =
  try 
    let line = parse_define_line fd in
    match line with
    | Defines_Comment _ | Defines_Empty -> parse_defines fd (line::m) (l+1)
    | Defines_Error error ->
      Printf.eprintf "*** Error at line %d : %s\n" l error;
      parse_defines fd m (l+1)
    | Defines_Base _ | Defines_Field _ | Defines_List _ -> parse_defines fd (line::m) (l+1)
  with End_of_file -> List.rev m

let parse_defines_file name m force =
  try 
    let defines = open_in name in
    let m = parse_defines defines m 0 in
    close_in defines;
    m
  with Sys_error s ->
    if force then raise (Sys_error s) else m

let print_defines m = List.iter print_define m

(* Main functionality *)
let create_config = function 
  | Config c ->
    begin if !verbose then Printf.eprintf "Reading config file %s" c end;
    let config = open_in c in
    let m = parse_config config C.empty in
    let () = close_in config in
    m
  | Subarch (a,s,f) -> 
    if !verbose then 
      Printf.eprintf "Creating config file for arch %s, subarch %s, flavour %s (basedir is %s)\n" a s f !basedir;
    let m = parse_config_file (!basedir ^ "/config") C.empty false in
    let archdir = !basedir ^ "/" ^ a in
    let m = parse_config_file (archdir ^ "/config") m false in
    let archdir = archdir ^ "/" ^ s in
    let m = parse_config_file (archdir ^ "/config") m false in
    parse_config_file (archdir ^ "/config." ^ f) m true
  | Flavour (a,f) ->
    if !verbose then 
      Printf.eprintf "Creating config file for arch %s, flavour %s (basedir is %s)\n" a f !basedir;
    let m = parse_config_file (!basedir ^ "/config") C.empty false in
    let archdir = !basedir ^ "/" ^ a in
    let m = parse_config_file (archdir ^ "/config") m false in
    parse_config_file (archdir ^ "/config." ^ f) m true

let do_single configs =
  match configs with 
  | [] -> raise (Sys_error "Too few arguments for action single")
  | (Config c)::[] ->
    let m = create_config (Config c) in
    print_config m
  | _::[] -> raise (Sys_error "Only config file supported for action single")
  | _ -> raise (Sys_error "Too many arguments for action single")

let do_create configs = 
  match configs with 
  | [] -> raise (Sys_error "Too few arguments for action create")
  | (Config c)::[] -> raise (Sys_error "config file not supported for action create")
  | (Subarch (a,s,f))::[] ->
    let m = create_config (Subarch (a,s,f)) in 
    print_config m
  | (Flavour (a,f))::[] ->
    let m = create_config (Flavour (a,f)) in 
    print_config m
  | _ -> raise (Sys_error "Too many arguments for action create")

let do_check configs = 
  match configs with 
  | [] -> 
    begin if !verbose then Printf.eprintf "Checking config files in %s\n" !basedir end;
    let m = parse_defines_file (!basedir ^ "/defines") [] true in
    print_defines m
  | _ -> raise (Sys_error "Too many arguments for action check")

let do_diff configs =
  match configs with 
  | [] | _::[] -> raise (Sys_error "Too few arguments for action diff")
  | a::b::[] -> 
    let ma = create_config a in
    let mb = create_config b in
    let d = diff_configs ma mb in
    print_diff d
  | _ -> raise (Sys_error "Too many arguments for action check")

let print_error s = Printf.eprintf "Error:\n\n  %s\n\n" s; usage ()

let () = try 
    let () = Arg.parse spec set_action usage_string in
    let configs : config list = config_of_temp (List.rev !temp) in
    match !action with
    | Single -> do_single configs
    | Create -> do_create configs
    | Check -> do_check configs
    | Diff -> do_diff configs
    | NoAction -> print_error "no action provided"
    | Help -> usage ()
  with
    | Sys_error s -> print_error s
    | Bad_args s -> print_error (Printf.sprintf "while parsing args at \"%s\"" s)
    | Bad_action s -> print_error (Printf.sprintf "unrecognized action \"%s\"" s)
    | Double_action s ->print_error (Printf.sprintf "only one action allowed \"%s\"" s)

(* 
 * TODO items
 *
 *   full parsing of defines.
 *   parsing of kernel kbuild tree.
 *   parsing of kernel/module Makefiles.
 *   generation of module description files.
 *
 *   organisation of kernels in a per-version, per-arch/subarch/flavour way in the archive.
 *   (outside of the normal udeb/deb stable/testing/unstable/experimental archive).
 *   Linking into the normal distributions, out of this alternate archive.
 *   Implications for the archive tools : new upload queues.
 *)
