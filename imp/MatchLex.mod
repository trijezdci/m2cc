(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE MatchLex;

(* Lexer Support Library for Modula-2 R10 Core Compiler *)

IMPORT ASCII, Capabilities, Source, Token;


(* Semantic Symbols *)

(* ---------------------------------------------------------------------------
 * procedure Ident ( source, token )
 *  matches the input in s to an identifier
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Ident :
 *   Letter LetterOrDigit* ( ( '_' | '$' ) LetterOrDigit+ )*
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the identifier.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the identifier whose first character was the
 *      lookahead of s upon entry into the procedure.
 *  (2) token value identifier is passed back in token.
 *
 * error-conditions:
 *  (1) identifier consists entirely of non-alphanumeric characters
 *       TO DO
 *  (2) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Ident
  ( source : Source; token : Token; VAR diag : Diagnostic );

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


(* ---------------------------------------------------------------------------
 * procedure IdentOrResword ( source, token )
 *  matches the input in s to an identifier or reserved word
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Ident :
 *   Letter LetterOrDigit* ( ( '_' | '$' ) LetterOrDigit+ )*
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the identifier or RW.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the identifier or RW whose first character was the
 *      lookahead of s upon entry into the procedure.
 *  (2) if the input represents a reserved word or dual-use identifier,
 *       its token value is passed back in token.
 *      if the input represents any other identifier,
 *       token value identifier is passed back in token.
 *
 * error-conditions:
 *  (1) identifier consists entirely of non-alphanumeric characters
 *       TO DO
 *  (2) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE IdentOrResword
  ( source : Source; token : Token; VAR diag : Diagnostic );

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


(* ---------------------------------------------------------------------------
 * procedure NumericLiteral ( source, token )
 *  matches the input in s to a numeric literal
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * NumericLiteral :
 *   '0' ( RealNumberTail | NonDecimalNumberTail )? |
 *   ( '1' .. '9' ) DecimalNumberTail?
 *   ;
 *
 * NonDecimalNumberTail :
 *   'b' Base2DigitSeq | ( 'u' | 'x' ) Base16DigitSeq
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first digit of the literal.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *  (2) if the numeric literal represents a whole number,
 *       token value WholeNumber is passed back in token.
 *      if the numeric literal represents a character code,
 *       token value QuotedChar is passed back in token.
 *      if the numeric literal represents a real number,
 *       token value RealNumber is passed back in token.
 *
 * error-conditions:
 *  (1) missing digit after prefix
 *       TO DO
 *  (2) missing fractional part after decimal point
 *       TO DO
 *  (3) missing exponent part after exponent prefix
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE NumericLiteral
  ( source : Source; token : Token; VAR diag : Diagnostic );

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
 * procedure QuotedLiteral ( source, token )
 *  matches the input in s to a quoted literal
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * QuotedLiteral :
 *   SingleQuotedLiteral | DoubleQuotedLiteral
 *   ;
 *
 * SingleQuotedLiteral :
 *   "'" ( QuotableCharacter | '"' )* "'"
 *   ;
 *
 * DoubleQuotedLiteral :
 *   '"' ( QuotableCharacter | "'" )* '"'
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening quotation mark of the literal.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      quotation mark that closes the literal whose opening quotation mark
 *      was the lookahead of s upon entry into the procedure.
 *  (2) if the quoted literal represents the empty string or a single
 *      character, token value quotedChar is passed back in token.
 *      Otherwise, token value quotedString is passed back in token.
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) unescaped backslash encountered
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE QuotedLiteral
  ( source : Source; token : Token; VAR diag : Diagnostic );

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


(* ---------------------------------------------------------------------------
 * procedure ChevronText ( source, token )
 *  matches the input in s to chevron text
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * ChevronText :
 *   "<<" ( QuotableCharacter | "'" | '"' )* ">>"
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the opening chevron.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the closing chevron that closes the chevron text whose
 *      opening delimiter was the lookahead of s upon entry into the procedure.
 *  (2) token value chevronText is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MatchChevronText
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END ChevronText;


(* Non-Semantic Symbols *)

(* ---------------------------------------------------------------------------
 * procedure Pragma ( source, diag )
 *  matches the input in source to a pragma
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Pragma :
 *   "<*" ( QuotableCharacter | QuotedLiteral )* "*>"
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the opening pragma delimiter.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the closing delimiter that closes the pragma whose
 *      opening delimiter was the lookahead of s upon entry into the procedure.
 *  (2) token value pragma is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum length exceeded
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
 * EBNF
 *
 * LineComment :
 *   "!" CommentCharacter* EndOfLine
 *   ;
 *
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
 *  (2) token value lineComment is passed back in token
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
 * EBNF
 *
 * BlockComment :
 *   '(' '*' ( CommentCharacter | BlockComment | EndOfLine )* '*' ')'
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening parenthesis of a block comment.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      parenthesis that closes the block comment whose opening parenthesis
 *      was the lookahead of s upon entry into the procedure.
 *  (2) token value blockComment is passed back in token
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
 * EBNF
 *
 * DisabledCode :
 *   ( StartOfSourceFile | EndOfLine ) '?' '<'
 *   ( PrintableCharacter | Tabulator | EndOfLine )*
 *   EndOfLine '>' '?'
 *   ;
 *
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

(* ---------------------------------------------------------------------------
 * procedure matchDecimalNumberTail ( source, diag )
 *  matches the input in source to a decimal number tail
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * DecimalNumberTail :
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is a digit between 1 and 9 or a decimal point.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchDecimalNumberTail ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  (* TO DO *)
  
END matchDecimalNumberTail;


(* ---------------------------------------------------------------------------
 * procedure matchRealNumberTail ( source, diag )
 *  matches the input in source to a real number tail
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * RealNumberTail :
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is a decimal point.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose decimal point was the lookahead of s upon entry
 *      into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchRealNumberTail ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  (* TO DO *)
  
END matchRealNumberTail;


(* ---------------------------------------------------------------------------
 * procedure matchBase2DigitSeq ( source, diag )
 *  matches the input in source to a base-2 digit sequence
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Base2DigitSeq :
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is a base-2 digit.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchBase2DigitSeq ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  (* TO DO *)
  
END matchBase2DigitSeq;


(* ---------------------------------------------------------------------------
 * procedure matchBase16DigitSeq ( source, diag )
 *  matches the input in source to a base-16 digit sequence
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Base16DigitSeq :
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is a base-16 digit.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchBase16DigitSeq ( source : Source ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  (* TO DO *)
  
END matchBase16DigitSeq;


END MatchLex.