(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE MatchLex;

(* Lexer Support Library for Modula-2 R10 Core Compiler *)

IMPORT ASCII, Capabilities, Source, Token;


(* Semantic Symbols *)

PROCEDURE Ident
  ( source : Source; token : Token; VAR diag : Diagnostic );
(* Matches the input in source to an identifier and consumes it.
 *
 * Ident :
 *   Letter LetterOrDigit* ( ( '_' | '$' ) LetterOrDigit+ )*
 *   ;
 *)

VAR
  next : CHAR;
  isIdentChar, lowlinePermitted, dollarPermitted : BOOLEAN;
  
BEGIN
  
  lowlinePermitted := Capabilities.lowlineIdentifiers();
  dollarPermitted := Capabilities.dollarIdentifiers();
    
  REPEAT
    next := source.consumeChar();
        
    isIdentChar :=
      ASCII.isAlphanum(next) OR
      (lowlinePermitted AND
       next = '_' AND ASCII.isAlphanum(source.la2Char())) OR
      (dollarPermitted AND
       next = '$' AND ASCII.isAlphanum(source.la2Char()));
      
  UNTIL NOT isIdentChar
      
END Ident;


PROCEDURE IdentOrResword
  ( source : Source; token : Token; VAR diag : Diagnostic );
(* Matches the input in source to an identifier or reserved word
   and consumes it. *)

VAR
  next : CHAR;
  allChars, upperChars : CARDINAL;
  isIdentChar, isUpperChar, lowlinePermitted, dollarPermitted : BOOLEAN;
  
BEGIN
  
  allChars := 0;
  upperChars := 0;
  lowlinePermitted := Capabilities.lowlineIdentifiers();
  dollarPermitted := Capabilities.dollarIdentifiers();
  
  next := source.lookaheadChar();
  isUpperChar := (next >= 'A' AND next <= 'Z');
  
  REPEAT
        
    next := source.consumeChar();
    allChars++;
    
    IF isUpperChar THEN
      upperChars++
    END;

    isUpperChar := (next >= 'A' AND next <= 'Z');
    
    isIdentChar :=
      isUpperChar OR
      (next >= 'a' AND next <= 'z') OR
      (next >= '0' AND next <= '9') OR
      (lowlinePermitted AND
       next = '_' AND ASCII.isAlphanum(source.la2Char())) OR
      (dollarPermitted AND
       next = '$' AND ASCII.isAlphanum(source.la2Char()));
      
  UNTIL NOT isIdentChar;
  
  IF allChars = upperChars THEN (* possibly reserved word found *)
    (* TO DO check for reserved word match *)
    
  ELSE (* not a reserved word *)
    token := Token.Identifier
  END
  
END IdentOrResword;


PROCEDURE NumericLiteral
  ( source : Source; token : Token; VAR diag : Diagnostic );
(* Matches the input in source to a numeric literal and consumes it. *)

VAR
  ch, next : CHAR;

BEGIN
  
  source.GetChar(ch, next);
  
  IF ch = '0' THEN
        
    CASE next OF
    | '.' : (* sole '0' or real number *)
      IF source.la2Char() # '.' THEN
        (* real number found *)
        next := matchRealNumberTail(source)        
      END (* IF *)
      
    | 'b' : (* base-2 integer *)
      next := matchBase2DigitSeq(source)
      
    | 'u' : (* character code *)
      next := matchBase16DigitSeq(source)
      
    | 'x' : (* base-16 integer *)
      next := matchBase16DigitSeq(source)
      
    END (* CASE *)
       
  ELSIF ch >= '1' AND ch <= '9' THEN
    (* decimal integer or real number *)
    next := matchDecimalNumberTail(source)    
  END (* IF *)
  
END NumericLiteral;


(* ---------------------------------------------------------------------------
 * procedure QuotedLiteral ( source, diag )
 *  matches the input in source to a quoted literal
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening single or double quote.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      single or double quote matching the opening quote that was the
 *      lookahead of s upon entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE QuotedLiteral
  ( source : Source; token : Token; VAR diag : Diagnostic );
(* Matches the input in source to a quoted literal and consumes it. *)

VAR
  next, delimiter : CHAR;

BEGIN
  
  (* consume string delimiter *)
  source.GetChar(delimiter, next);
  
  WHILE next # delimiter DO
    
    (* check for control characters *)
    IF ASCII.isControl(next) THEN
      
      IF next = ASCII.NEWLINE THEN
        
        (* error: new line in string literal *)
        
      ELSIF source.eof() THEN
        
        (* error: EOF in string literal *)
        
      ELSE (* any other control character *)
        
        (* error: illegal character in string literal *)
        
      END (* IF *)
    END (* IF *)
    
    (* check for escape sequence *)
    IF next = ASCII.BACKSLASH THEN
      
      next := source.consumeChar();
      
      IF next # 'n' AND # = 't' AND next # ASCII.BACKSLASH THEN
        
        (* error: invalid escape sequence *)
        
      END (* IF *)
    END (* IF *)
    
    next := source.consumeChar()
  END (* WHILE *)
  
  (* consume closing delimiter *)
  IF next = delimiter THEN
    next := source.consumeChar();
  END (* IF *)
  
END QuotedLiteral;


(* Non-Semantic Symbols *)

(* ---------------------------------------------------------------------------
 * procedure Pragma ( source, diag )
 *  matches the input in source to a pragma
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the '<' of the opening pragma delimiter.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing '>'
 *      that closes the pragma whose opening '<' was the lookahead of s upon
 *      entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Pragma ( source : Source; VAR diag : Diagnostic );

VAR
  next : CHAR;
  delimiterFound : BOOLEAN;
  
BEGIN
  
  delimiterFound := FALSE;
  
  (* consume opening '<' and '*' *)
  next := source.consumeChar();
  next := source.consumeChar();
  
  WHILE NOT delimiterFound DO
  
    IF next = '*' AND source.la2Char() = '>' THEN
      delimiterFound := TRUE;
      
      (* consume closing '*' and '>' *)
      next := source.consumeChar();
      next := source.consumeChar();
    ELSE (* not closing delimiter *)
    
      (* consume this character *)
      next := source.consumeChar()
      
      (* TO DO check for eof, illegal chars, report diagnostics *)
      
    END (* IF *)
  END (* WHILE *)
  
 END Pragma;


(* ---------------------------------------------------------------------------
 * procedure LineComment ( source, diag )
 *  matches the input in source to a line comment
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening exclamation point of a line comment.
 *
 * post-conditions:
 *  (1) if the comment is terminated by end-of-line:
 *       lookahead of s is the new-line character that closes the line comment
 *       whose opening delimiter was the lookahead of s upon entry
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
PROCEDURE LineComment ( source : Source; VAR diag : Diagnostic );

VAR
  next : CHAR;
  
BEGIN

  REPEAT
    next := source.consumeChar();
  UNTIL source.eof() OR (next = ASCII.NEWLINE);
  
END LineComment;


(* ---------------------------------------------------------------------------
 * procedure BlockComment ( source, diag )
 *  matches the input in source to a block comment
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
PROCEDURE BlockComment ( source : Source; VAR diag : Diagnostic );

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

END BlockComment;


(* Disabled Code Sections *)

(* ---------------------------------------------------------------------------
 * procedure DisabledCode ( source, diag )
 *  matches the input in source to a disabled code block
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
PROCEDURE DisabledCode ( source : Source; VAR diag : Diagnostic );

VAR
  next : CHAR;
  delimiterFound : BOOLEAN;
BEGIN

  delimiterFound := FALSE;
  
  (* consume opening '?' and '<' *)
  next := source.consumeChar();
  next := source.consumeChar();
    
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
    
  END (* WHILE *)
    
END DisabledCode;


(* Private Procedures *)

PROCEDURE matchDecimalNumberTail ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN

END matchDecimalNumberTail;


PROCEDURE matchRealNumberTail ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN

END matchRealNumberTail;


PROCEDURE matchBase2DigitSeq ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN

END matchBase2DigitSeq;


PROCEDURE matchBase16DigitSeq ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN

END matchBase16DigitSeq;


END MatchLex.