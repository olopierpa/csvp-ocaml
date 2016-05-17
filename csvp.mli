
type field = string;;
  
type record = field array;;
  
(* Raised on bad arguments to make_csv_parameters. *)
exception Bad_parameters;;
  
(* Raised on reading syntactically malformed fields. *)
exception Bad_syntax of string;;
  
(* csv_parameters objects define the data format. *)
type csv_parameters;;
  
(* ?compressed:true => 
 *   On writing, fields with the same content as in the previous row are
 *   written out as empty. On reading, empty fields are expanded as the
 *   content in the previous row, if there is one.
 * 
 * ?quote_all:true =>
 *   All fields are quoted, whatever the content of the field.
 * ?quote_all:false =>
 *   Quoting is used only when otherwise the field would not be readable back.
 * 
 * field_quoter, fields_separator, and records_separator, must be DIFFERENT
 * characters.
 *)
val make_csv_parameters :
  ?compressed:bool ->           (* Default: true *)
  ?quote_all:bool ->            (* Default: false *)
  ?field_quoter:char ->         (* Default: '"' *)
  ?fields_separator:char ->     (* Default: ',' *)
  ?records_separator:char ->    (* Default: '\n' *)
  unit -> csv_parameters;;
  
(* Strings passed in or returned from the library can be
 * retained by the library, and hence MUST NOT be mutated.
 * 
 * Records passed in or returned from the library are not
 * retained by the library and hence CAN be mutated.
 *)
  
(* The Writer *)
  
(* csv_writer_status objects contain the internal status of the writer.
 * WARNING: the status contains mutable parts and is not protected in 
 * any way against concurrent usage. Don't do this. *)
type csv_writer_status;;
  
val make_csv_writer_status_from_channel : csv_parameters -> out_channel -> csv_writer_status;;
  
(* Opens filename with open_out_bin. If control on the opening mode is needed, use ⤴. *)
val make_csv_writer_status_from_filename : csv_parameters -> string -> csv_writer_status;;
  
(* Field oriented *)  
val write_field : csv_writer_status -> field -> unit;;
val new_record : csv_writer_status -> unit;;
  
(* Record oriented*)
val write_record : csv_writer_status -> record -> unit;;
val write_field_list : csv_writer_status -> field list -> unit;; 
  
(* Finishing an already finished writer is allowed and has no effect.
   This function uses Pervasives.close_out and can raise the same
   exceptions as this function. *)
 val finished_with_this_csv_writer : csv_writer_status -> unit;;
  
(* The Reader *)
  
(* csv_reader_status objects contain the internal status of the reader.
 * WARNING: the status contains mutable parts and is not protected in 
 * any way against concurrent usage. Don't do this. *)
type csv_reader_status;;
  
val make_csv_reader_status_from_channel : csv_parameters -> in_channel -> csv_reader_status;;
  
(* Opens filename with open_in_bin. If control on the opening mode is needed, use ⤴. *)
val make_csv_reader_status_from_filename : csv_parameters -> string -> csv_reader_status;;
  
(* Field oriented *)  
val read_field : csv_reader_status -> field option;;   (* None => record at end && reader
                                                          has advanced to next record. *)
  
val input_at_end : csv_reader_status -> bool;;
  
(* Record oriented *)
val read_record : csv_reader_status -> record option;; (* None => input at end && next calls  
                                                          will continue to return None. *)
  
(* If a reader_status is read to the end, its associated channel is closed 
 * automatically and there's no need to finish the reader with this function.
 * Finishing an already finished reader is allowed and has no effect. *)
val finished_with_this_csv_reader : csv_reader_status -> unit;;
