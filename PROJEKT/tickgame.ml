type player = X | O;;

type board = player option array array;;

let empty_board : board = Array.make_matrix 3 3 None;;

let print_player = function
  | X -> print_string "X"
  | O -> print_string "O"
;;

let print_board board =
  Array.iter
    (fun row ->
      Array.iter
        (function
          | Some p -> print_player p
          | None -> print_string "-"
        )
        row;
      print_newline ()
    )
    board
;;

let rec get_move () =
  print_string "Enter row and column (e.g. '1 2'): ";
  match read_line () with
  | exception End_of_file -> None
  | input ->
      match String.split_on_char ' ' input with
      | [row; col] ->
          let row = int_of_string_opt row and col = int_of_string_opt col in
          (match (row, col) with
          | (Some r, Some c) when r >= 1 && r <= 3 && c >= 1 && c <= 3 -> Some (r - 1, c - 1)
          | _ -> print_string "Bad coordinates. Please enter valid coordinates.\n";
                 get_move ()
          )
      | _ -> 
        print_string "Bad coordinates. Please enter valid coordinates.\n";
        get_move ()
;;

let get_winner board =
  let diags = [| [| board.(0).(0); board.(1).(1); board.(2).(2) |];
                  [| board.(0).(2); board.(1).(1); board.(2).(0) |] |]
  in
  let cols = Array.make_matrix 3 3 None in
  for i = 0 to 2 do
    for j = 0 to 2 do
      cols.(i).(j) <- board.(j).(i)
    done
  done;
  let lines = Array.append board (Array.append cols diags) in
  let is_win line =
    let first = line.(0) in
    Array.for_all ((=) first) line && first <> None
  in
  let winning_line = Array.find_opt is_win lines
  in
  match winning_line with
  | Some line -> line.(0)
  | None -> None
;;

let clear_terminal ()  =
  ignore(Sys.command "clear")
;;
    
let rec play_game board player =
  clear_terminal ();
  print_board board;
  match get_winner board with
  | Some p ->
      clear_terminal ();
      print_board board;
      print_player p;
      print_string " wins!\n";
      board
  | None ->
      match get_move () with
      | None -> board
      | Some (row, col) ->
          match board.(row).(col) with
          | Some _ ->
              clear_terminal ();
              print_board board;
              print_string "That space is already taken.\n";
              play_game board player
          | None ->
              board.(row).(col) <- Some player;
              let next_player = if player = X then O else X in
              clear_terminal();
              play_game board next_player
;;
    
let () =
  print_string "Welcome to Tic-Tac-Toe!\n";
  let final_board = play_game empty_board X in
  clear_terminal ();
  print_board final_board;
  match get_winner final_board with
    | Some p ->
        clear_terminal();
        print_board final_board;
        print_player p;
        print_string " wins!\n"
    | None -> print_string "It's a draw\n"
;;