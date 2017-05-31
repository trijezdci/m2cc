(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE M2Lexer;

(* Lexer for Modula-2 R10 Bootstrap Compiler *)

IMPORT ASCII, M2Source, M2Token, M2Symbol;


(* Lexer Descriptor *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  source : M2Source;
  status : Status;
  nextSymbol : M2Symbol;
  errorCount,
  warnCount : CARDINAL;
END;


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure New ( lexer, filename, status )
 *  creates a new lexer instance, associated with filename
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE New ( VAR lexer : Lexer; filename : Filename; VAR s : Status );

VAR
  sourceStatus : M2Source.Status;

BEGIN
 
  (* lexer must not have been initialised *)
  IF lexer # NIL THEN
    status := Status.AlreadyInitialised;
    RETURN
  END;
  
  (* allocate a lexer instance *)
  NEW(lexer);
  IF lexer = NIL THEN
    status := Status.UnableToAllocate;
    RETURN
  END;
  
  (* initialise source *)
  M2Source.New(lexer^.source, filename, sourceStatus);
  IF sourceStatus # M2Source.Status.Success THEN
  (* TO DO: status *)
    RETURN
  END;
  
  (* initialise error count and status *)
  errorCount := 0;
  status := Status.Success;
  
  (* read the first symbol to be returned *)
  ConsumeSym(lexer);
  
  RETURN
END New;


(* ---------------------------------------------------------------------------
 * procedure GetSym ( lexer, symbol, lookaheadSymbol )
 *  passes and consumes current lookahead symbol, passes new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE GetSym ( lexer : Lexer; VAR sym, next : M2Symbol );

BEGIN
  
  (* nextSymbol holds current lookahead, pass it back in sym *)
  sym := lexer^.nextSymbol;
  
  (* consume the current and read the new lookahead symbol *)
  ConsumeSym(lexer);
  
  (* nextSymbol holds new lookahead, pass it back in next *)
  next := lexer^.nextSymbol;
  
  RETURN
END GetSym;


(* ---------------------------------------------------------------------------
 * procedure ConsumeSym ( lexer )
 *  consumes current lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE ConsumeSym ( lexer : Lexer );

VAR
  ch, next, la2 : CHAR;
  sym : M2Symbol;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN
  END;
  
  (* all decisions are based on lookahead *)
  next := M2Source.lookaheadChar(lexer^.source);
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT M2Source.eof(lexer^.source) AND
    ((next = ASCII.SPACE) OR (next = ASCII.TAB) OR (next = ASCII.NEWLINE)) DO
    M2Source.GetChar(source, ch, next)
  END; (* WHILE *)
  
  (* TO DO *)
  M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);

  (* check for end-of-file *)
  IF eof(lexer^.source) THEN
    sym.token := M2Token.EOF;
    sym.lexeme := 0
  
  (* check for reserved word or identifier *)
  ELSIF ASCII.isLetter(next) OR (next = "_") OR (next = "$") THEN
    M2Source.MarkLexeme(source, sym.line, sym.column);
    MatchResWordOrIdent(source, sym.token);
    M2Source.CopyLexeme(lexer^.source, lexer^.dict, sym.lexeme)

  (* check for numeric literal *)
  ELSIF (next >= "0") AND (next <= "9") THEN 
    M2Source.MarkLexeme(lexer^.source, sym.line, sym.column);
    MatchNumericLiteral(lexer^.source, sym.token);
    M2Source.CopyLexeme(lexer^.source, lexer^.dict, sym.lexeme)

  (* check for quoted literal *)
  ELSIF (next = ASCII.SINGLEQUOTE) OR (next = ASCII.DOUBLEQUOTE) THEN
    M2Source.MarkLexeme(lexer^.source, sym.line, sym.column);
    MatchQuotedLiteral(lexer^.source, sym.token);
    M2Source.CopyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
      
  (* check for any other symbol *)
  ELSE
    CASE next OF
    
    (* next symbol is line comment *)
    | "!" :
        M2Source.MarkLexeme(lexer^.source, sym.line, sym.column);
        MatchLineComment(lexer^.source, sym.token);
        M2Source.CopyLexeme(lexer^.source, dict, sym.lexeme)
    
    (* next symbol is "#" *)
    | "#" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.NotEqual;
        sym.lexeme := M2Token.lexemeForToken(M2Token.NotEqual)
    
    (* next symbol is "(" or block comment *)
    | "(" :
        IF M2Source.la2Char(lexer^.source) = "*" THEN (* found block comment *)
          M2Source.MarkLexeme(lexer^.source, sym.line, sym.column);
          MatchBlockComment(lexer^.source, sym.token);
          M2Source.CopyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
        
        ELSE (* found "(" *)
          M2Source.ConsumeChar(lexer^.source);
          M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
          sym.token := M2Token.LParen;
          sym.lexeme := M2Token.lexemeForToken(M2Token.LParen)
          
        END (* "(" and block comment *)
    
    (* next symbol is ")" *)
    | ")" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.value := M2Token.RParen;
        sym.lexeme := M2Token.lexemeForToken(M2Token.RParen)
    
    (* next symbol is "*" or "*." *)
    | "*" :
        M2Source.GetChar(lexer^.source, ch, next);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "." THEN (* found "*" *)
          sym.token := M2Token.Asterisk;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Asterisk)
        
        ELSE (* found "*." *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.AsterDot;
          sym.lexeme := M2Token.lexemeForToken(M2Token.AsterDot)
        
        END (* "*" or "*." *)
    
    (* next symbol is "+" or "++" *)
    | "+" :
        M2Source.GetChar(lexer^.source, ch, next);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "+" THEN (* found "+" *)
          sym.token := M2Token.Plus;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Plus)
        
        ELSE (* found "++" *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.PlusPlus;
          sym.lexeme := M2Token.lexemeForToken(M2Token.PlusPlus)
        
        END (* "+" and "++" *)
      
    (* next symbol is "," *)
    | "," :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.Comma;
        sym.lexeme := M2Token.lexemeForToken(M2Token.Comma)
    
    (* next symbol is "-", "--" or "->" *)
    | "-" :
        M2Source.GetChar(lexer^.source, ch, next);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next = "-" THEN (* found "--" *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.MinusMinus;
          sym.lexeme := M2Token.lexemeForToken(M2Token.MinusMinus)
        
        ELSIF next = ">" THEN (* found "->" *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.RArrow;
          sym.lexeme := M2Token.lexemeForToken(M2Token.RArrow)
        
        ELSE (* found "-" *)
          sym.token := M2Token.Minus;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Minus)
        
        END (* "-", "--" or "->" *)
    
    (* next symbol is "." or ".." *)
    | "." :
        M2Source.GetChar(lexer^.source, ch, next);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "." THEN
          sym.token := M2Token.Dot;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Dot)
        
        ELSE (* found ".." *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.DotDot;
          sym.lexeme := M2Token.lexemeForToken(M2Token.DotDot)
        
        END (* "." and ".." *)
      
    (* next symbol is "/" *)
    | "/" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.RealDiv;
        sym.lexeme := M2Token.lexemeForToken(M2Token.RealDiv)
    
    (* next symbol is ":", ":=" or "::" *)
    | ":" :
        M2Source.GetChar(lexer^.source, ch, next);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next = "=" THEN (* found ":=" *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.Assign;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Assign)
        
        ELSIF next = ":" THEN (* found "::" *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.TypeConv;
          sym.lexeme := M2Token.lexemeForToken(M2Token.TypeConv)
        
        ELSE (* found ":" *)
          sym.token := M2Token.Colon;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Colon)
        
        END (* ":", ":=" and "::" *)
    
    (* next symbol is ";" *)
    | ";" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.Semicolon;
        sym.lexeme := M2Token.lexemeForToken(M2Token.Semicolon)
    
    (* next symbol is "<", "<=", chevron text or pragma *)
    | "<" :
        la2 := la2Char(lexer^.source);
        
        IF la2 = ">" THEN (* found "<<" *)
          M2Source.MarkLexeme(lexer^.source, sym.line, sym.column);
          MatchChevronText(lexer^.source, sym.token);
          M2Source.CopyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
        
        ELSIF la2 = "*" THEN (* found "<*" *)
          M2Source.MarkLexeme(lexer^.source, sym.line, sym.column);
          MatchPragma(lexer^.source, sym.token);
          M2Source.CopyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
        
        ELSE (* "<" or "<=" *)
          M2Source.GetChar(lexer^.source, ch, next);
          M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
                  
          IF next = "=" THEN (* found "<=" *)
            M2Source.ConsumeChar(source);
            sym.token := M2Token.LessEq;
            sym.lexeme := M2Token.lexemeForToken(M2Token.LessEq)
            
          ELSE (* found "<" *)
            sym.token := M2Token.Less;
            sym.lexeme := M2Token.lexemeForToken(M2Token.Less)
          
          END (* "<" or "<=" *)
          
        END (* chevron text or pragma *)
    
    (* next symbol is "=" or "==" *)
    | "=" :
        M2Source.GetChar(lexer^.source, ch, next);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "=" THEN (* found "=" *)
          sym.token := M2Token.Equal;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Equal)
        
        ELSE (* found "==" *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.Identity;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is ">" and ">=" *)
    | ">" :
        M2Source.GetChar(lexer^.source, ch, next);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "=" THEN (* found ">=" *)
          M2Source.ConsumeChar(lexer^.source);
          sym.token := M2Token.GreaterEq;
          sym.lexeme := M2Token.lexemeForToken(M2Token.GreaterEq)
        
        ELSE (* found ">" *)
          sym.token := M2Token.Greater;
          sym.lexeme := M2Token.lexemeForToken(M2Token.Greater)
        
        END (* ">" or ">=" *)
    
    (* next symbol is "[" *)
    | "[" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.LBracket;
        sym.lexeme := M2Token.lexemeForToken(M2Token.LBracket)
    
    (* next symbol is backslash *)
    | BACKSLASH :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.SetDiff;
        sym.lexeme := M2Token.lexemeForToken(M2Token.SetDiff)
    
    (* next symbol is "]" *)
    | "]" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.RBracket;
        sym.lexeme := M2Token.lexemeForToken(M2Token.RBracket)
    
    (* next symbol is "^" *)
    | "^" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.Deref;
        sym.lexeme := M2Token.lexemeForToken(M2Token.Deref)
    
    (* next symbol is "{" *)
    | "{" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.LBrace;
        sym.lexeme := M2Token.lexemeForToken(M2Token.LBrace)
    
    (* next symbol is "|" *)
    | "|" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.VerticalBar;
        sym.lexeme := M2Token.lexemeForToken(M2Token.VerticalBar)
    
    (* next symbol is "}" *)
    | "}" :
        M2Source.ConsumeChar(lexer^.source);
        M2Source.GetLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := M2Token.RBrace;
        sym.lexeme := M2Token.lexemeForToken(M2Token.RBrace)
    
    (* next symbol is invalid *)
    ELSE
      M2Source.MarkLexeme(lexer^.source, sym.line, sym.column);
      M2Source.ConsumeChar(lexer^.source);
      sym.token := M2Token.Invalid;
      M2Source.CopyLexeme(lexer^.source, lexer^.dict, sym.lexeme);
      lexer^.errorCount++
      
    END; (* CASE *)
  
  END (* IF *);

  lexer^.nextSymbol := sym;
  
  RETURN
END ConsumeSym;


(* ---------------------------------------------------------------------------
 * procedure lookaheadSym ( lexer ) : M2Symbol
 *  returns current lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE lookaheadSym ( lexer : Lexer ) : M2Symbol; (* PURE *)

BEGIN
  
  RETURN lexer^.nextSymbol
  
END lookaheadSym;


(* ---------------------------------------------------------------------------
 * procedure GetStatus ( lexer, status )
 *  returns status of last operation
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE status ( lexer : Lexer ) : Status;

BEGIN

  IF lexer = NIL THEN
    RETURN Status.NotInitialised
  ELSE
    RETURN lexer^.status
  END

END status;


(* ---------------------------------------------------------------------------
 * procedure warnCount ( lexer ) : CARDINAL
 *  returns current lexical warning count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE warnCount ( lexer : Lexer ) : CARDINAL; (* PURE *)
 (* Returns the lexer's accumulated warning count. *)

BEGIN
  
  RETURN lexer^.warnCount
  
END warnCount;


(* ---------------------------------------------------------------------------
 * procedure errorCount ( lexer ) : CARDINAL
 *  returns current lexical error count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE errorCount ( lexer : Lexer ) : CARDINAL; (* PURE *)
 (* Returns the lexer's accumulated error count. *)

BEGIN
  
  RETURN lexer^.errorCount
  
END errorCount;


(* ---------------------------------------------------------------------------
 * procedure release ( lexer )
 *  releases lexer instance
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) lexer must not be NIL
 *
 * post-conditions:
 *  (1) lexer is deallocated
 *  (2) NIL is passed back in lexer
 *
 * error-conditions:
 *  (1) reference to lexer remains unmodified
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Release ( VAR lexer : Lexer );

VAR
  sourceStatus : M2Source.Status;
  
BEGIN

  (* lexer must not be NIL *)
  IF lexer = NIL THEN
    RETURN
  END;
  
  (* release source *)
  M2Source.Release(lexer^.source, sourceStatus);
  
  (* TO DO: check for and handle error(s) *)
  
  (* release lexer *)
  RELEASE(lexer);
  lexer := NIL;
  
  RETURN
END Release;


(* Private Operations *)

(* ---------------------------------------------------------------------------
 * procedure matchResWordOrIdent ( s, t, diag )
 *  matches the input in s to a reserved word or identifier
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the RW or identifier.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the RW or identifier whose first character was the
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
PROCEDURE MatchResWordOrIdent
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  allChars, upperChars, nonStdChars : CARDINAL;
  
BEGIN
  
  allChars := 0;
  upperChars := 0;
  nonStdChars := 0;
  
  REPEAT
    getChar(s, ch, next);
    allChars++;
    
    IF (ch >= "A") AND (ch <= "Z") THEN
      upperChars++
    END;
    
    IF (ch = "_") OR (ch = "$") THEN
      nonStdChars++
    END

  UNTIL eof(s) OR NOT isIdentChar(next);
  
  IF allChars = upperChars THEN (* possibly reserved word found *)
    (* TO DO : check for reserved word *)
    
  ELSIF allChars = nonStdChars THEN (* illegal identifier found *)
    M2LexDiag.New(diag, illegalIdent, 0, 0, "")
    
  END
  
END MatchResWordOrIdent;


(* ---------------------------------------------------------------------------
 * procedure matchNumericLiteral ( s, t, diag )
 *  matches the input in s to numeric literal syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first digit of the literal.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *  (2) if the numeric literal represents a whole number,
 *       token value wholeNumber is passed back in token.
 *      if the numeric literal represents a character code,
 *       token value quotedChar is passed back in token.
 *      if the numeric literal represents a real number,
 *       token value realNumber is passed back in token.
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
PROCEDURE MatchNumericLiteral
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

BEGIN
  
  M2Source.GetChar(s, ch, next);
  IF ch = "0" THEN
          
    CASE next OF
    | "'" : (* decimal number *)
    | "." : (* real number or range *)
    | "b" : (* base-2 whole number *)
    | "u" : (* base-16 character code *)
    | "x" : (* base-16 whole number *)
    
    ELSE (* single digit zero *)
      (* TO DO *)
    
    END; (* CASE *)
      
  ELSE
  
  END
  (* TO DO *)

END MatchNumericLiteral;


(* ---------------------------------------------------------------------------
 * procedure matchQuotedLiteral ( s, t, diag )
 *  matches the input in s to quoted literal syntax
 * ---------------------------------------------------------------------------
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
PROCEDURE MatchQuotedLiteral
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  delimiter, ch, next : CHAR;
  len : CARDINAL;

BEGIN

  (* TO DO : update, following change to M2Source *)
  
  len := 0;
  M2Source.GetChar(s, delimiter, next);
  
  WHILE NOT eof(s) AND (next # delimiter) AND isPrintable(next) DO
    
    IF next # ASCII.BACKSLASH THEN
      M2Source.GetChar(s, ch, next);
      len++
      
    ELSE (* backslash *)
      MatchEscapeSequence(s, success);
      
      IF NOT success THEN (* unescaped backslash found *)
        (* TO DO : handle error *)
        
      END
    END
  END; (* WHILE *)
  
  IF next = delimiter THEN
    M2Source.ConsumeChar(s);
    len++
    
  ELSE (* illegal character in string literal found *)
    (* TO DO : handle error *)
    
  END;
  
  IF len <= 1 THEN
    token := M2Token.QuotedChar
    
  ELSE (* len > 1 *)
    token := M2Token.QuotedString
    
  END
  
END MatchQuotedLiteral;


(* ---------------------------------------------------------------------------
 * procedure MatchLineComment ( s, t, diag )
 *  matches the input in s to line comment syntax
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
 *  (2) token value lineComment is passed back in token
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 *  (2) maximum comment length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchLineComment
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  
BEGIN

  REPEAT
    M2Source.GetChar(s, ch, next)
  UNTIL M2Source.eof(s) OR (next = ASCII.NEWLINE);
  
  token := M2Token.LineComment

END MatchLineComment;


(* ---------------------------------------------------------------------------
 * procedure MatchBlockComment ( s, t, diag )
 *  matches the input in s to block comment syntax
 * ---------------------------------------------------------------------------
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
PROCEDURE MatchBlockComment
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  nestLevel : CARDINAL;
  
BEGIN
  
  nestLevel := 1;
  
  WHILE NOT M2Source.eof(s) AND (nestLevel > 0) DO
    M2Source.GetChar(s, ch, next);
    
    IF (ch = "*") AND (next = ")") THEN
      M2Source.ConsumeChar(s);
      nestLevel--
    
    ELSIF (ch = "(") AND (next = "*") THEN
      M2Source.ConsumeChar(s);
      nestLevel++
      
    END;
    
    M2Source.ConsumeChar(s)
    
  END; (* WHILE *)
  
  (* TO DO *)

END MatchBlockComment;


(* ---------------------------------------------------------------------------
 * procedure MatchChevronText ( s, t, diag )
 *  matches the input in s to chevron text syntax
 * ---------------------------------------------------------------------------
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
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END MatchChevronText;


(* ---------------------------------------------------------------------------
 * procedure MatchPragma ( s, t, diag )
 *  matches the input in s to "<*" any legal characters "*>"
 * ---------------------------------------------------------------------------
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
PROCEDURE MatchPragma
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END MatchPragma;


END M2Lexer.