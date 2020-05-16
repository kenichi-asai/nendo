(* 使用法を表示する *)
(* usage : string -> unit *)
let usage name =
  print_endline ("Usage: " ^ name ^ " [[M|T|S|H|R] year [month [day]]]")

(* ３月までだった場合、年をずらす *)
(* nendoclassify : int -> int -> int *)
let nendoclassify year month =
  if month < 4 then year - 1 else year

(* 年度を表す文字列を返す *)
(* nendo : int -> int -> string *)
let nendo year month =
  if year < 1912 || year = 1912 && month < 4 then
    "明治 " ^ string_of_int (nendoclassify (year - 1867) month) ^ " 年度"
  else if year < 1926 || year = 1926 && month < 4 then
    "大正 " ^ string_of_int (nendoclassify (year - 1911) month) ^ " 年度"
  else if year < 1989 || year = 1989 && month < 4 then
    "昭和 " ^ string_of_int (nendoclassify (year - 1925) month) ^ " 年度"
  else if year < 2019 || year = 2019 && month < 4 then
    "平成 " ^ string_of_int (nendoclassify (year - 1988) month) ^ " 年度"
  else
    "令和 " ^ string_of_int (nendoclassify (year - 2018) month) ^ " 年度"

(* 元号を表す文字列を返す *)
(* gengou : in -> string *)
let gengou year =
  if year < 1912 then
    "明治 " ^ string_of_int (year - 1867) ^ " 年"
  else if year < 1926 then
    "大正 " ^ string_of_int (year - 1911) ^ " 年"
  else if year < 1989 then
    "昭和 " ^ string_of_int (year - 1925) ^ " 年"
  else if year < 2019 then
    "平成 " ^ string_of_int (year - 1988) ^ " 年"
  else
    "令和 " ^ string_of_int (year - 2018) ^ " 年"

(* 今日の日付を返す *)
(* today : unit -> int * int * int *)
let today () =
  let tm = Unix.localtime (Unix.time ()) in
  let open Unix in
  (tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday)

(* 出力する文字列を返す *)
(* month = 13 だったら月以降を省略する *)
(* day = 0 だったら日以降を省略する *)
(* date_string : int -> int -> int -> string *)
let date_string year month day =
  if year < 1868 || year = 1868 && month < 4
  then "明治 1 年（1868 年）4 月以降のみサポートしています"
  else
  let y = string_of_int year ^ " 年" in
  let m = if month = 13 then "" else string_of_int month ^ " 月" in
  let d = if day = 0 then "" else " " ^ string_of_int day ^ " 日" in
  let g = gengou year in
  let n = if month = 13 then "" else "（" ^ nendo year month ^ "）" in
  y ^ "（" ^ g ^ "）" ^ m ^ d ^ n

(* メインルーチン *)
(* main : int -> string list -> unit *)
let main offset args = try
  begin match args with
    [] ->
      let (year, month, day) = today () in
      print_endline (date_string year month day)
  | [year] ->
      let year = int_of_string year + offset in
      print_endline (date_string year 13 0)
  | [year; month] ->
      let year = int_of_string year + offset in
      let month = int_of_string month in
      print_endline (date_string year month 0)
  | [year; month; day] ->
      let year = int_of_string year + offset in
      let month = int_of_string month in
      let day = int_of_string day in
      print_endline (date_string year month day)
  | _ -> raise Not_found
  end
with _ -> usage Sys.argv.(0)

(* 引数を parse する *)
(* parse : string list -> int * string list *)
let parse args = match args with
    [] -> (0, args)
  | first :: rest ->
      match String.get first 0 with (* 引数の最初の文字 *)
        'm' | 'M' -> (1867, rest)
      | 't' | 'T' -> (1911, rest)
      | 's' | 'S' -> (1925, rest)
      | 'h' | 'H' -> (1988, rest)
      | 'r' | 'R' -> (2018, rest)
      | _ -> (0, args)

(* スタートアップ *)
let _ =
  let (offset, args) = parse (List.tl (Array.to_list Sys.argv)) in
  main offset args
