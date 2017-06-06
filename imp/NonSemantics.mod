(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE NonSemantics;

(* Non-Semantic Symbol Parser for Modula-2 R10 Core Compiler *)

IMPORT ASCII, Capabilities, Source;


(* Operations *)


(* ---------------------------------------------------------------------------
 * procedure MatchLineComment ( source, diag )
 *  matches the input in source to line comment syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening exclamation point of a line comment.
 *
 * post-conditions:
 *  (1) if the comment is terminated by end-of-line:
 *       lookahead of s is the new-line character that closes the line comment
 *       whose opening exclamation point was the lookahead of s upon entry
 *       into the procedure, or
 *      if the comment is terminated by end-of-file:
 *       the last character in input s has been consumed.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 *  (2) maximum comment length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchLineComment ( source : Source; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  
BEGIN

  REPEAT
    source.GetChar(ch, next)
  UNTIL source.eof() OR (next = ASCII.NEWLINE);
  
END MatchLineComment;


(* ---------------------------------------------------------------------------
 * procedure MatchBlockComment ( source, diag )
 *  matches the input in source to block comment syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening parenthesis of a block comment.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      parenthesis that closes the block comment whose opening parenthesis
 *      was the lookahead of s upon entry into the procedure.
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum comment length exceeded
 *       TO DO
 *  (4) maximum nesting level exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchBlockComment ( source : Source; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  nestLevel : CARDINAL;
  
BEGIN
  
  nestLevel := 1;
  
  WHILE NOT source.eof() AND (nestLevel > 0) DO
    source.GetChar(ch, next);
    
    IF (ch = "*") AND (next = ")") THEN
      source.ConsumeChar();
      nestLevel--
    
    ELSIF (ch = "(") AND (next = "*") THEN
      source.ConsumeChar();
      nestLevel++
      
    END;
    
    source.ConsumeChar()
    
  END; (* WHILE *)
  
  (* TO DO : diagnostics *)

END MatchBlockComment;


(* ---------------------------------------------------------------------------
 * procedure SkipDisabledCode ( source, diag )
 *  matches the input in source to disabled code block syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening '?' of a disabled code block.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      '?' that closes the disabled code block whose opening '?'
 *      was the lookahead of s upon entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE SkipDisabledCode ( source : Source; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  delimiterFound : BOOLEAN;
BEGIN

  (* consume opening '?' and '<' *)
  next := source.consumeChar();
  next := source.consumeChar();
  
  delimiterFound := FALSE;
  
  WHILE NOT delimiterFound AND NOT source.eof() DO
    
    (* check for closing delimiter *)
    IF next = '>' AND source.la2Char() = '?' AND source.currentCol() = 1 THEN
      delimiterFound := TRUE;
      
      (* consume closing '>' and '?' *)
      next := source.consumeChar();
      next := source.consumeChar();
      
    ELSE (* not closing delimiter *)
      (* consume this character *)
      next := source.consumeChar()
      
      (* TO DO check for illegal chars, report diagnostics *)
      
    END (* IF *)
    
  END; (* WHILE *)
    
END SkipDisabledCode;


END NonSemantics.