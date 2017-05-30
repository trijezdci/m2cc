(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE SimpleFileIO;

(* Low Level File IO library *)


TYPE File = POINTER TO FileDescriptor;

TYPE FileDescriptor = RECORD
  handle : CStdIOFile;
  mode : Mode;
  status : Status
END;

TYPE CStdIOFile = ADDRESS;

TYPE CStdIOMode = ARRAY [0..1] OF CHAR;


(* from DEFINITION module

TYPE Mode = ( read, write, append );

TYPE Status = ( success, failure );
*)

(* TO DO : port to R10 using FFI pragma *)

(* C StdIO Library Interface -- legacy M2C syntax *)
%{
#include <stdio.h>

#define stdio_fopen(fptr, name, mode) \
  (fptr = (void *)fopen((char *)name, (char *)mode))

#define stdio_fclose(fptr, result) \
  (result = fclose((FILE *)fptr))

#define stdio_fread(fptr, bufptr, item_size, item_count, items_read) \
  (items_read = fread((void *)bufptr, item_size, item_count, (FILE *)fptr))

#define stdio_fwrite(fptr, bufptr, item_size, item_count, items_read) \
  (items_read = fwrite((void *)bufptr, item_size, item_count, (FILE *)fptr))

#define stdio_ftell(fptr, offset) \
  (offset = ftell((FILE *)fptr))

#define stdio_fseek(fptr, offset, result) \
  (result = fseek(FILE *)fptr, (long)offset)

#define stdio_ferror(fptr, result) \
  (result = ferror((FILE *)fptr))

#define stdio_feof(fptr, result) \
  (result = feof((FILE *)fptr))

%}

(* Operations common to all modes *)

PROCEDURE Open
  ( VAR f : File; filename : ARRAY OF CHAR; mode : Mode; VAR s : Status );
(* Opens file filename in mode. Passes file handle in f and status in s. *)

VAR
  fmode : CStdIOMode;
  fhandle : CStdIOFile;
  
BEGIN
  
  (* compose C file mode string *)
  CASE mode OF
    read : fmode[0] := "r";
  | write : fmode[0] := "w";
  | append : fmode[0] := "a"
  END; (* CASE *)
  fmode[1] := CHR(0);
  
  (* open file via C stdio fopen *)
  stdio_fopen(fhandle, fpath, fmode);
  
  IF fhandle = NIL THEN
    (* fopen failed *)
    s := failure;
    RETURN
  END;
  
  NEW(f);
  
  IF f = NIL THEN
    (* allocation failed *)
    s := failure;
    RETURN
  END;
  
  s := success;
  f^.handle := fhandle;
  f^.mode := mode;
  f^.status := s;

  RETURN  
END Open;


PROCEDURE GetStatus ( f : File; VAR s : Status );
(* Passes the status of the last operation on file f in s. *)

BEGIN
  
  IF f = NIL THEN
    s := invalidFileRef
  ELSE
    s := f^.status
  END;
  
  RETURN
END GetStatus;


PROCEDURE Close ( VAR f : File; s : Status );
(* Closes file associated with file handle f. Passes status in s. *)

VAR
  result : INTEGER;

BEGIN

  IF f = NIL THEN
    s := invalidFileRef
  
  ELSE
    stdio_fclose(f^.handle, result);
    
    IF result = 0 THEN
      s := success;
      DISPOSE(f)
    
    ELSE (* result # 0 *)
      (* TO DO : error handling *)
      
    END;
  
  RETURN
END Close;


(* Operations exclusive to mode read *)

PROCEDURE SetPos ( f : File; pos : LONGCARD );
(* Sets the reading position of file f to pos. *)

BEGIN
  (* TO DO *)
END SetPos;


PROCEDURE ReadOctets
  ( f : File; VAR buffer : ARRAY OF BYTE; VAR bytesRead : LONGCARD );
(* Reads contents starting at the current reading position of file f into
   buffer until either buffer is full or eof is reached. The number of octets
   actually read is passed in bytesRead. *)

BEGIN
  (* TO DO *)
END ReadOctets;


PROCEDURE ReadChars
  ( f : File; VAR buffer : ARRAY OF CHAR; VAR charsRead : LONGCARD );
(* Reads contents starting at the current reading position of file f into
   buffer until either the pen-ultimate index of buffer is written or eof
   is reached. The buffer is then terminated with ASCII NUL. The number of
   characters actually read is passed in charsRead. *)

BEGIN
  (* TO DO *)
END ReadOctets;


PROCEDURE eof ( f : File ) : BOOLEAN;
(* Returns TRUE if the end of file f has been reached, otherwise FALSE. *)

BEGIN
  (* TO DO *)
END eof;


(* Operations common to modes read and write *)

PROCEDURE GetPos ( f : File; VAR pos : LONGCARD );
(* Passes the current reading or writing position of file f in pos. *)

BEGIN
  (* TO DO *)
END GetPos;


(* Operations common to modes write and append *)

PROCEDURE WriteOctets
  ( f : File; buffer : ARRAY OF BYTE; VAR bytesWritten : LONGCARD );
(* Writes the contents of buffer at the current writing position to file f.
   The number of bytes actually written is passed in bytesWritten. *)

BEGIN
  (* TO DO *)
END WriteOctets;


PROCEDURE WriteChars
  ( f : File; buffer : ARRAY OF CHAR; VAR charsWritten : LONGCARD );
(* Writes the contents of buffer up to and excluding the first ASCII NUL
   character code at the current writing position to file f.
   The number of characters actually written is passed in charsWritten. *)

BEGIN
  (* TO DO *)
END WriteChars;


END SimpleFileIO.