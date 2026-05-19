let file = "~/.ocaml-tasks"
let version = "0.0.1"

let create_file () =
  Out_channel.with_open_text file (fun _ -> ())

let get_lines () =
  if not (Sys.file_exists file) then
    create_file ();
  In_channel.with_open_bin file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter ((<>) "")

let output_updated upd =
  Out_channel.with_open_bin file (fun oc ->
    List.iter (fun line ->
      Out_channel.output_string oc (String.trim line ^ "\n")
    ) upd
  )

type status = Active | Done

type task = { desc: string; status: status }

let output_updated_tasks tasks =
  tasks
  |> List.map (fun t -> Printf.sprintf "%s %s" (if t.status = Done then "[x]" else "[ ]") t.desc)
  |> output_updated
  
let parse_tasks () = 
  let lines = get_lines () in
  lines
  |> List.mapi (fun i line ->
      try
        match String.get line 0, String.get line 1, String.get line 2 with
          '[', (' ' | 'x' as status), ']' ->
            let desc = String.sub line 4 (String.length line - 4) in
            Ok (if status = 'x' then
              { desc; status = Done }
            else
              { desc; status = Active }) 
        | _ -> Error (Printf.sprintf "Parsing error: invalid format in line %i" (i + 1))
      with e ->
        Error (Printf.sprintf "Parsing error: line %i too short" (i + 1)))

let add_todo (tasks : task list) desc =
  let task = { status = Active ; desc } in
  tasks @ [task] |> output_updated_tasks;
  Printf.printf "Task '%s' was successfully created" task.desc 

let toggle_todo tasks id is_done =
  tasks
  |> (List.mapi (fun idx t ->
    if idx = id then
      { t with status = if is_done then Done else Active } 
    else
      t
  ))
  |> output_updated_tasks

let task_exists tasks id = 
  match List.nth_opt tasks id with
    Some l -> true
  | None -> false

let done_todo tasks id =
  match task_exists tasks id with
    true ->
      toggle_todo tasks id true;
      Printf.printf "Congrats! You finished the task %i" (id + 1)
  | false -> Printf.printf "Task %i not found" (id + 1)

let undone_todo tasks id =
  match task_exists tasks id with
    true ->
      toggle_todo tasks id false;
      Printf.printf "Task %i now active" (id + 1)
  | false -> Printf.printf "Task %i not found" (id + 1)

let remove_todo tasks id =
  match task_exists tasks id with
    true ->
      tasks
      |> List.filteri (fun i _ -> i <> id)
      |> output_updated_tasks;
      Printf.printf "The task %i removed" (id + 1)
  | false -> Printf.printf "Task %i not found" (id + 1)

let remove_filter tasks status =
  let filtered = tasks
    |> List.filter (fun t -> t.status <> status) in

  filtered
    |> output_updated_tasks;

  Printf.printf "%i %s tasks removed"
    (List.length tasks - List.length filtered)
    (match status with Active -> "Active" | Done -> "Done")

let list_todo tasks status =
  tasks
    |> List.mapi (fun i t -> (i + 1, t))
    |> List.filter (fun (_, t) -> match status with
          None -> true
        | Some s -> t.status = s)
    |> List.map
      (fun (id, t) -> Printf.sprintf "%i. %s %s\n"
          id (if t.status = Done then "[x]" else "[ ]") t.desc)
    |> List.iter print_string

let clear_todo () =
  Out_channel.with_open_gen [Open_wronly ; Open_creat ; Open_trunc] 0o666
    file (fun _ -> ())
 
let print_help () =
  print_endline ("ocaml-todo v" ^ version);
  print_endline "";
  print_endline "add <TASK>      Add task to list";
  print_endline "done <ID>       Complete the task by id";
  print_endline "undone <ID>     Set task Active again by id";
  print_endline "remove <ID>     Remove the task by id from list";
  print_endline "       --active Remove active tasks";
  print_endline "       --done   Remove done tasks";
  print_endline "list            Print list of tasks";
  print_endline "       --active Print only active tasks";
  print_endline "       --done   Print only done tasks";
  print_endline "clear           Clear all list";
  print_endline "";
  print_endline "--help    -h    Print available commands";
  print_endline "--version -v    Print version of program"

let parse_args tasks = function
  | [||] | [|"--help"|] | [|"-h"|] -> print_help
  | [|"--version"|] | [|"-v"|] -> fun () -> print_endline ("v" ^ version)
  | [|"add" ; task|] -> fun () -> add_todo tasks task
  | [|"done" ; id|] -> fun () -> done_todo tasks ((int_of_string id) - 1)
  | [|"undone" ; id|] -> fun () -> undone_todo tasks ((int_of_string id) - 1)
  | [|"remove" ; "--active"|] -> fun () -> remove_filter tasks Active
  | [|"remove" ; "--done"|] -> fun () -> remove_filter tasks Done
  | [|"remove" ; id|] -> fun () -> remove_todo tasks ((int_of_string id) - 1)
  | [|"list"|] -> fun () -> list_todo tasks None
  | [|"list" ; "--active"|] -> fun () -> list_todo tasks (Some Active) 
  | [|"list" ; "--done"|] -> fun () -> list_todo tasks (Some Done) 
  | [|"clear"|] -> clear_todo
  | _ -> fun () -> print_endline "Unknown command. Type --help"

let () =
  let tasks = parse_tasks ()
  |> List.map (fun task -> match task with
      Ok t -> t
    | Error e -> prerr_endline e; exit 0) in
  parse_args tasks (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) ()

