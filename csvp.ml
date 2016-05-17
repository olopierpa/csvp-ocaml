
type field = string;;
type record = field array;;
  
exception Bad_parameters;;  
exception Bad_syntax of string;;
  
let default_fields_separator = ',';;
let default_field_quoter = '"';;
let default_records_separator = '\n';;
  
let initial_field_buffer_length = 64;;
let initial_record_buffer_length = 16;;
  
type csv_parameters = {
    compressed : bool;
    quote_all : bool;
    field_quoter : char;
    fields_separator : char;
    records_separator : char;
  };;
  
let make_csv_parameters
      ?(compressed = true)
      ?(quote_all = false)
      ?(field_quoter = default_field_quoter)
      ?(fields_separator = default_fields_separator)
      ?(records_separator = default_records_separator)
      () =
  if field_quoter = fields_separator ||
       field_quoter = records_separator ||
         fields_separator = records_separator then
    raise Bad_parameters
  else
    { compressed; quote_all; fields_separator; field_quoter;
      records_separator;
    };;
  
(****************************************************************)
  
type csv_status_common = {
    parameters : csv_parameters;
    mutable record_buffer_length : int;
    mutable record_buffer : field array;
    mutable current_field_number : int;
    mutable current_record_number : int;
  };;
  
(****************************************************************)
(* The Writer Status *)
  
type csv_writer_status = {
    out_channel : out_channel;
    common : csv_status_common;
  };;
  
let make_csv_writer_status_from_channel parameters out_channel =
  { out_channel;
    common = {
        parameters;
        current_field_number = 0;
        record_buffer_length = initial_record_buffer_length;
        record_buffer = Array.make initial_record_buffer_length "";
        current_record_number = 0;
      }
  };;
  
let make_csv_writer_status_from_filename parameters filename =
  make_csv_writer_status_from_channel parameters (open_out_bin filename);;
  
let finished_with_this_csv_writer writer_status =
  close_out writer_status.out_channel;
  writer_status.common.record_buffer <- [||];;
  
(****************************************************************)
(* The Reader Status *)
  
type csv_reader_status = {
    in_channel : in_channel;
    common : csv_status_common;
    mutable field_buffer_length : int;
    mutable field_buffer : Bytes.t;
    mutable field_buffer_fill_pointer : int;
    mutable record_buffer_fill_pointer : int;
    mutable record_at_end : bool;
    mutable input_at_end : bool
  };;
  
let make_csv_reader_status_from_channel parameters in_channel =
  { in_channel;
    common = {
        parameters;
        record_buffer_length = initial_record_buffer_length;
        record_buffer = Array.make initial_record_buffer_length "";
        current_field_number = 0;
        current_record_number = 0;
      };
    field_buffer_length = initial_field_buffer_length;
    field_buffer = Bytes.create initial_field_buffer_length;
    field_buffer_fill_pointer = 0;
    record_buffer_fill_pointer = 0;
    record_at_end = false;
    input_at_end = false;
  };;
  
let make_csv_reader_status_from_filename parameters filename =
  make_csv_reader_status_from_channel parameters (open_in_bin filename);;
  
let no_bytes = Bytes.create 0;;
  
let finished_with_this_csv_reader reader_status =
  close_in reader_status.in_channel;
  reader_status.field_buffer <- no_bytes;
  reader_status.common.record_buffer <- [||];;
  
(****************************************************************)
(* Buffers Management *)
  
let grow_array array len default =
  let new_len = 2 * len + 1 in
  let new_array = Array.make new_len default in
  Array.blit array 0 new_array 0 len;
  (new_array, new_len);;
  
(*  
let grow_bytes bytes len =
  let new_len = 2 * len + 1 in
  let new_bytes = Bytes.create new_len in
  Bytes.blit bytes 0 new_bytes 0 len;
  (new_bytes, new_len);;
 *)
  
let grow_bytes bytes len =
  let additional_space = len + 1 in
  (Bytes.extend bytes 0 additional_space, len + additional_space);;
  
let grow_record_buffer common_status =
  let record_buffer', record_buffer_length' =
    grow_array common_status.record_buffer common_status.record_buffer_length "" in
  common_status.record_buffer <- record_buffer';
  common_status.record_buffer_length <- record_buffer_length';;
  
let grow_field_buffer reader_status =
  let field_buffer', field_buffer_length' =
    grow_bytes reader_status.field_buffer reader_status.field_buffer_length in
  reader_status.field_buffer <- field_buffer';
  reader_status.field_buffer_length <- field_buffer_length';;
  
(****************************************************************)
(* Default Fields in Compressed Records *)
  
let set_record_default common field =
  if common.current_field_number = common.record_buffer_length then begin
      grow_record_buffer common
    end;
  common.record_buffer.(common.current_field_number) <- field;;
  
let get_record_default { record_buffer;
                         record_buffer_length;
                         current_field_number } =
  if current_field_number >= record_buffer_length then ""
  else record_buffer.(current_field_number);;
  
(****************************************************************)
(* The Writer *)  
  
let needs_quoting parameters field =
  (* This function is called only when is known
   * that field <> buffered field... *)
  let len = String.length field in
  if len = 0 then begin
      (* ...hence when compressed, len = 0 => needs_quoting *)
      parameters.compressed
    end else if field.[0] = parameters.field_quoter then true
  else let rec loop i =
         if i = len then false
         else let c = field.[i] in
              c = parameters.fields_separator ||
                c = parameters.records_separator ||
                  loop (succ i)
       in loop 0;;
  
let write_quoted_field out_channel field_quoter field =
  output_char out_channel field_quoter;
  for i = 0 to String.length field - 1 do
    let c = field.[i] in
    output_char out_channel c;
    if c = field_quoter then begin
        output_char out_channel field_quoter
      end
  done;
  output_char out_channel field_quoter;;
  
let write_field writer_status field =
  let { out_channel; common } = writer_status in
  if common.current_field_number >= common.record_buffer_length then begin
      grow_record_buffer common
    end;
  let { current_field_number; record_buffer;
        record_buffer_length; parameters } = common in
  let { compressed; quote_all; fields_separator; field_quoter } = parameters in
  if current_field_number > 0 then begin
      output_char out_channel fields_separator
    end;
  if not compressed || not (field = record_buffer.(current_field_number)) then begin
      if quote_all || needs_quoting parameters field then begin
          write_quoted_field out_channel field_quoter field;
        end else begin
          output_string out_channel field
        end;
      record_buffer.(current_field_number) <- field
    end;
  common.current_field_number <- succ current_field_number;;
  
let new_record writer_status =
  let { out_channel; common } = writer_status in
  output_char out_channel common.parameters.records_separator;
  common.current_field_number <- 0;;
  
let write_field_list writer_status list =
  List.iter (fun field -> write_field writer_status field) list;
  new_record writer_status;;
  
let write_record_array writer_status array =
  for i = 0 to Array.length array - 1 do
    write_field writer_status array.(i)
  done;
  new_record writer_status;;
  
let write_record = write_record_array;;
  
(****************************************************************)
  
let artifex = "PETRVS·PAVLVS·NEPTVNENSIS·ME·FECIT·MMXVI";;
  
(****************************************************************)
(* The Reader *)
  
let reset_field_buffer reader_status =
  reader_status.field_buffer_fill_pointer <- 0;;
  
let field_accumulate_char reader_status char =
  if reader_status.field_buffer_fill_pointer = reader_status.field_buffer_length then begin
      grow_field_buffer reader_status
    end;
  Bytes.set reader_status.field_buffer reader_status.field_buffer_fill_pointer char;
  reader_status.field_buffer_fill_pointer <- succ reader_status.field_buffer_fill_pointer;;
  
let extract_field_buffer_content reader_status =
  Bytes.sub_string reader_status.field_buffer 0 reader_status.field_buffer_fill_pointer;;
  
let reset_record_buffer (reader_status : csv_reader_status) =
  reader_status.common.current_field_number <- 0;
  reader_status.record_buffer_fill_pointer <- 0;;
  
let record_accumulate_field reader_status field =
  let { common; record_buffer_fill_pointer } = reader_status in
  if record_buffer_fill_pointer = common.record_buffer_length then begin
      grow_record_buffer common
    end;
  common.record_buffer.(reader_status.record_buffer_fill_pointer) <- field;
  reader_status.record_buffer_fill_pointer <- succ reader_status.record_buffer_fill_pointer;;
  
let extract_record_buffer_content (reader_status : csv_reader_status) =
  Array.sub reader_status.common.record_buffer 0 reader_status.record_buffer_fill_pointer;;
  
let read_unquoted_field reader_status c0 =
  let { in_channel; common } = reader_status in
  let { records_separator; fields_separator } = common.parameters in
  let rec loop c0 =
    field_accumulate_char reader_status c0;
    match input_char in_channel with
    | exception End_of_file ->
       reader_status.record_at_end <- true;
       reader_status.input_at_end <- true;
       extract_field_buffer_content reader_status
    | c when c = records_separator ->
       reader_status.record_at_end <- true;
       extract_field_buffer_content reader_status
    | c when c = fields_separator ->
       extract_field_buffer_content reader_status
    | c -> loop c
  in loop c0;;
  
let read_quoted_field reader_status =
  (* The opening quote has already been consumed. *)
  let { in_channel; common } = reader_status in
  let { field_quoter; fields_separator; records_separator } = common.parameters in 
  let rec loop () =
    match input_char in_channel with
    | exception End_of_file ->
       raise (Bad_syntax (Printf.sprintf
                            "Data ended while inside a quoted field at record %d, field %d"
                            common.current_record_number
                            common.current_field_number))
    | c0 when c0 = field_quoter ->
       begin match input_char in_channel with
       | exception End_of_file ->
          reader_status.record_at_end <- true;
          reader_status.input_at_end <- true;
          extract_field_buffer_content reader_status
       | c1 when c1 = field_quoter ->
          field_accumulate_char reader_status c1;
          loop ()
       | c1 when c1 = fields_separator ->
          extract_field_buffer_content reader_status
       | c1 when c1 = records_separator ->
          reader_status.record_at_end <- true;
          extract_field_buffer_content reader_status
       | _ ->
          raise (Bad_syntax (Printf.sprintf
                               "Extraneous character after closing quote at record %d, field %d"
                               common.current_record_number
                               common.current_field_number))
       end
    | c0 -> field_accumulate_char reader_status c0;
            loop ()
  in loop ();;
  
let read_field reader_status =
  if reader_status.record_at_end then begin
      reader_status.record_at_end <- false;
      reset_record_buffer reader_status;
      reader_status.common.current_field_number <- 0;
      None
    end else begin
      let { in_channel; common } = reader_status in
      let { parameters } = common in
      reset_field_buffer reader_status;
      let field = match input_char in_channel with
        | exception End_of_file ->
           reader_status.record_at_end <- true;
           reader_status.input_at_end <- true;
           None
        | c0 when c0 = parameters.fields_separator ->
           if parameters.compressed then Some (get_record_default common)
           else Some ""
        | c0 when c0 = parameters.records_separator ->
           reader_status.record_at_end <- true;
           if parameters.compressed then Some (get_record_default common)
           else Some ""
        | c0 when c0 = parameters.field_quoter ->
           let field = read_quoted_field reader_status in
           set_record_default common field;
           Some field
        | c0 ->
           let field = read_unquoted_field reader_status c0 in
           set_record_default common field;
           Some field
      in if reader_status.input_at_end then begin
             finished_with_this_csv_reader reader_status;
           end else begin
             common.current_field_number <- succ common.current_field_number;
           end;
         field
    end;;
  
let input_at_end { input_at_end } = input_at_end;;
  
let read_record reader_status =
  let ({ common } : csv_reader_status) = reader_status in
  if reader_status.input_at_end then begin
      None
    end else begin
      reset_record_buffer reader_status;
      reader_status.record_at_end <- false;
      let record =
        let rec loop () =
          match read_field reader_status with
          | Some field ->
             record_accumulate_field reader_status field;
             if reader_status.record_at_end then
               Some (extract_record_buffer_content reader_status)
             else begin
                 loop ()
               end
          | None ->
             if reader_status.record_buffer_fill_pointer = 0 then None
             else Some (extract_record_buffer_content reader_status)
        in loop ()
      in common.current_record_number <- succ common.current_record_number;
         record
    end;;
