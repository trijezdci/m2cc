(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Lexer;

(* Lexer for Modula-2 R10 Core Compiler *)

IMPORT ASCII, Capabilities, Source, Token, Symbol;


(* Lexer Type *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  source     : Source;
  nextSymbol : Symbol;
  warnings,
  errors     : CARDINAL;
  lastStatus : Status
END; (* LexerDescriptor *)


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure New ( newLexer, filename, status )
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
PROCEDURE New ( VAR newLexer : Lexer; filename : Filename; VAR s : Status );

VAR
  source : Source;
  sourceStatus : Source.Status;

BEGIN
 
  (* lexer must not have been initialised *)
  IF newLexer # NIL THEN
    status := Status.AlreadyInitialised;
    RETURN
  END;
  
  (* allocate and initialise source *)
  Source.New(source, filename, sourceStatus);
  IF sourceStatus # Source.Status.Success THEN
    s := Status.UnableToAllocate;
    RETURN
  END;
  
  (* allocate a lexer instance *)
  NEW newLexer;
  IF newLexer = NIL THEN
    s := Status.UnableToAllocate;
    RELEASE source;
    RETURN
  END;
  
  (* initialise lexer *)
  newLexer^.source := source;
  newLexer^.warnings := 0;
  newLexer^.errors := 0;
  newLexer^.lastStatus := Status.Success;
  
  (* read the first symbol to be returned *)
  newLexer^.nextSymbol := newLexer.consumeSym();
  
  s := Status.Success;
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
PROCEDURE GetSym ( self : Lexer; VAR sym, next : Symbol );

BEGIN
  
  (* nextSymbol holds current lookahead, pass it back in sym *)
  sym := self^.nextSymbol;
  
  (* consume the current lookahead,
     read the new lookahead symbol, pass it back in next *)
  next := self.consumeSym();
  
  RETURN
END GetSym;


(* ---------------------------------------------------------------------------
 * procedure consumeSym ( lexer )
 *  consumes current lookahead symbol and returns new lookahead symbol
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
PROCEDURE consumeSym ( self : Lexer ) : Symbol;

VAR
  ch, next, la2 : CHAR;
  source : Source;
  sym : Symbol;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN
  END;
  
  source := self^.source;
  
  (* all decisions are based on lookahead *)
  next := source.lookaheadChar();
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT source.eof() AND
    (next = ASCII.SPACE OR next = ASCII.TAB OR next = ASCII.NEWLINE) DO
    source.GetChar(ch, next)
  END; (* WHILE *)
  
  (* get current position *)
  source.GetLineAndColumn(sym.line, sym.column);

  (* check for end-of-file *)
  IF source.eof() THEN
    sym.token := Token.EOF;
    sym.lexeme := 0
  
  (* check for reserved word or identifier *)
  ELSIF ASCII.isLetter(next) THEN
    source.MarkLexeme(sym.line, sym.column);
    MatchResWordOrIdent(source, sym.token);
    source.CopyLexeme(self^.dict, sym.lexeme)

  (* check for numeric literal *)
  ELSIF next >= "0" AND next <= "9" THEN 
    source.MarkLexeme(sym.line, sym.column);
    MatchNumericLiteral(source, sym.token);
    source.CopyLexeme(self^.dict, sym.lexeme)

  (* check for quoted literal *)
  ELSIF next = ASCII.SINGLEQUOTE OR next = ASCII.DOUBLEQUOTE THEN
    source.MarkLexeme(sym.line, sym.column);
    MatchQuotedLiteral(source, sym.token);
    source.CopyLexeme(self^.dict, sym.lexeme)
    
  (* check for optional OpenVMS identifier starting with "$" *)
  ELSIF next = "$" AND
        Capabilities.isEnabled(Capabilities.OpenVMSIdentifiers) THEN
    source.MarkLexeme(sym.line, sym.column);
    MatchOpenVMSIdent(source, sym.token);
    source.CopyLexeme(self^.dict, sym.lexeme)
    
  (* check for any other symbol *)
  ELSE
    CASE next OF
    
    (* next symbol is line comment *)
    | "!" :
        source.MarkLexeme(sym.line, sym.column);
        MatchLineComment(source, sym.token);
        source.CopyLexeme(self^.dict, sym.lexeme)
    
    (* next symbol is "#" *)
    | "#" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.NotEqual;
        sym.lexeme := Token.lexemeForToken(Token.NotEqual)
    
    (* next symbol is "&" *)
    | "&" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.Concat;
        sym.lexeme := Token.lexemeForToken(Token.Concat)
    
    (* next symbol is "(" or block comment *)
    | "(" :
        IF source.la2Char() = "*" THEN (* found block comment *)
          source.MarkLexeme(sym.line, sym.column);
          MatchBlockComment(source, sym.token);
          source.CopyLexeme(self^.dict, sym.lexeme)
        
        ELSE (* found "(" *)
          source.ConsumeChar();
          source.GetLineAndColumn(sym.line, sym.column);
          sym.token := Token.LParen;
          sym.lexeme := Token.lexemeForToken(Token.LParen)
          
        END (* "(" and block comment *)
    
    (* next symbol is ")" *)
    | ")" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.value := Token.RParen;
        sym.lexeme := Token.lexemeForToken(Token.RParen)
    
    (* next symbol is "*" or "**" *)
    | "*" :
        source.GetChar(ch, next);
        source.GetLineAndColumn(sym.line, sym.column);
        
        IF next # "*" THEN (* found sole "*" *)
          sym.token := Token.Asterisk;
          sym.lexeme := Token.lexemeForToken(Token.Asterisk)
        
        ELSE (* found "**" *)
          source.ConsumeChar();
          sym.token := Token.Power;
          sym.lexeme := Token.lexemeForToken(Token.Power)
        
        END (* "*" or "**" *)
    
    (* next symbol is "+" or "++" *)
    | "+" :
        source.GetChar(ch, next);
        source.GetLineAndColumn(sym.line, sym.column);
        
        IF next # "+" THEN (* found sole "+" *)
          sym.token := Token.Plus;
          sym.lexeme := Token.lexemeForToken(Token.Plus)
        
        ELSE (* found "++" *)
          source.ConsumeChar();
          sym.token := Token.PlusPlus;
          sym.lexeme := Token.lexemeForToken(Token.PlusPlus)
        
        END (* "+" and "++" *)
      
    (* next symbol is "," *)
    | "," :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.Comma;
        sym.lexeme := Token.lexemeForToken(Token.Comma)
    
    (* next symbol is "-", "--" or "->" *)
    | "-" :
        source.GetChar(ch, next);
        source.GetLineAndColumn(sym.line, sym.column);
        
        IF next = "-" THEN (* found "--" *)
          source.ConsumeChar();
          sym.token := Token.MinusMinus;
          sym.lexeme := Token.lexemeForToken(Token.MinusMinus)
        
        ELSIF next = ">" THEN (* found "->" *)
          source.ConsumeChar();
          sym.token := Token.OneWayDep;
          sym.lexeme := Token.lexemeForToken(Token.OneWayDep)
        
        ELSE (* found sole "-" *)
          sym.token := Token.Minus;
          sym.lexeme := Token.lexemeForToken(Token.Minus)
        
        END (* "-", "--" or "->" *)
    
    (* next symbol is ".", ".." or ".*" *)
    | "." :
        source.GetChar(ch, next);
        source.GetLineAndColumn(sym.line, sym.column);
        
        IF next = "." THEN (* found ".." *)
          source.ConsumeChar();
          sym.token := Token.DotDot;
          sym.lexeme := Token.lexemeForToken(Token.DotDot)
        
        ELSIF next = "*" THEN (* found ".*" *)
          source.ConsumeChar();
          sym.token := Token.DotStar;
          sym.lexeme := Token.lexemeForToken(Token.DotStar)
        
        ELSE (* found sole "." *)
          sym.token := Token.Dot;
          sym.lexeme := Token.lexemeForToken(Token.Dot)
        
        END (* ".", ".." and ".*" *)
      
    (* next symbol is "/" *)
    | "/" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.RealDiv;
        sym.lexeme := Token.lexemeForToken(Token.RealDiv)
    
    (* next symbol is ":", ":=" or "::" *)
    | ":" :
        source.GetChar(ch, next);
        source.GetLineAndColumn(sym.line, sym.column);
        
        IF next = "=" THEN (* found ":=" *)
          source.ConsumeChar();
          sym.token := Token.Assign;
          sym.lexeme := Token.lexemeForToken(Token.Assign)
        
        ELSIF next = ":" THEN (* found "::" *)
          source.ConsumeChar();
          sym.token := Token.TypeConv;
          sym.lexeme := Token.lexemeForToken(Token.TypeConv)
        
        ELSE (* found sole ":" *)
          sym.token := Token.Colon;
          sym.lexeme := Token.lexemeForToken(Token.Colon)
        
        END (* ":", ":=" and "::" *)
    
    (* next symbol is ";" *)
    | ";" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.Semicolon;
        sym.lexeme := Token.lexemeForToken(Token.Semicolon)
    
    (* next symbol is "<", "<=", "<>", chevron text or pragma *)
    | "<" :
        la2 := source.la2Char();
        
        IF la2 = "<" THEN (* found "<<" *)
          source.MarkLexeme(sym.line, sym.column);
          MatchChevronText(source, sym.token);
          source.CopyLexeme(self^.dict, sym.lexeme)
        
        ELSIF la2 = "*" THEN (* found "<*" *)
          source.MarkLexeme(sym.line, sym.column);
          MatchPragma(source, sym.token);
          source.CopyLexeme(self^.dict, sym.lexeme)
          
        ELSE (* "<", "<=" or "<> "*)
          source.GetChar(ch, next);
          source.GetLineAndColumn(sym.line, sym.column);
                  
          IF next = "=" THEN (* found "<=" *)
            source.ConsumeChar();
            sym.token := Token.LessEq;
            sym.lexeme := Token.lexemeForToken(Token.LessEq)
            
          ELSIF next = ">" THEN (* found "<>" *)
            sym.token := Token.MutualDep;
            sym.lexeme := Token.lexemeForToken(Token.MutualDep)
            
          ELSE (* found "<" *)
            sym.token := Token.Less;
            sym.lexeme := Token.lexemeForToken(Token.Less)
          
          END (* "<", "<=" or "<>" *)
          
        END (* chevron text or pragma *)
    
    (* next symbol is "=" or "==" *)
    | "=" :
        source.GetChar(ch, next);
        source.GetLineAndColumn(sym.line, sym.column);
        
        IF next # "=" THEN (* found "=" *)
          sym.token := Token.Equal;
          sym.lexeme := Token.lexemeForToken(Token.Equal)
        
        ELSE (* found "==" *)
          source.ConsumeChar();
          sym.token := Token.Identity;
          sym.lexeme := Token.lexemeForToken(Token.Identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is ">", ">=" or "><" *)
    | ">" :
        source.GetChar(ch, next);
        source.GetLineAndColumn(sym.line, sym.column);
        
        IF next = "=" THEN (* found ">=" *)
          source.ConsumeChar();
          sym.token := Token.GreaterEq;
          sym.lexeme := Token.lexemeForToken(Token.GreaterEq)
        
        ELSIF next = "<" THEN (* found "><" *)
          source.ConsumeChar();
          sym.token := Token.MutualExcl;
          sym.lexeme := Token.lexemeForToken(Token.MutualExcl)
          
        ELSE (* found sole ">" *)
          sym.token := Token.Greater;
          sym.lexeme := Token.lexemeForToken(Token.Greater)
        
        END (* ">", ">=" or "><" *)
    
    (* next symbol is "[" *)
    | "[" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.LBracket;
        sym.lexeme := Token.lexemeForToken(Token.LBracket)
    
    (* next symbol is backslash *)
    | ASCII.BACKSLASH :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.SetDiff;
        sym.lexeme := Token.lexemeForToken(Token.SetDiff)
    
    (* next symbol is "]" *)
    | "]" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.RBracket;
        sym.lexeme := Token.lexemeForToken(Token.RBracket)
    
    (* next symbol is "^" *)
    | "^" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.Deref;
        sym.lexeme := Token.lexemeForToken(Token.Deref)
    
    (* next symbol is "{" *)
    | "{" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.LBrace;
        sym.lexeme := Token.lexemeForToken(Token.LBrace)
    
    (* next symbol is "|" *)
    | "|" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.VerticalBar;
        sym.lexeme := Token.lexemeForToken(Token.VerticalBar)
    
    (* next symbol is "}" *)
    | "}" :
        source.ConsumeChar();
        source.GetLineAndColumn(sym.line, sym.column);
        sym.token := Token.RBrace;
        sym.lexeme := Token.lexemeForToken(Token.RBrace)
    
    (* next symbol is invalid *)
    ELSE
      source.MarkLexeme(sym.line, sym.column);
      source.ConsumeChar();
      sym.token := Token.Invalid;
      source.CopyLexeme(self^.dict, sym.lexeme);
      self^.errors++
      
    END; (* CASE *)
  
  END (* IF *);
  
  (* store symbol for use by lookaheadSym *)
  self^.nextSymbol := sym;
  
  RETURN
END consumeSym;


(* ---------------------------------------------------------------------------
 * procedure lookaheadSym ( lexer ) : Symbol
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
PROCEDURE lookaheadSym ( self : Lexer ) : Symbol;

BEGIN
  RETURN self^.nextSymbol
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
PROCEDURE status ( self : Lexer ) : Status;

BEGIN

  IF lexer = NIL THEN
    RETURN Status.NotInitialised
  ELSE
    RETURN self^.lastStatus
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
PROCEDURE warnCount ( self : Lexer ) : CARDINAL; (* PURE *)
 (* Returns the lexer's accumulated warning count. *)

BEGIN
  RETURN self^.warnings
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
PROCEDURE errorCount ( self : Lexer ) : CARDINAL; (* PURE *)
 (* Returns the lexer's accumulated error count. *)

BEGIN
  RETURN self^.errors
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
  
BEGIN

  (* lexer must not be NIL *)
  IF lexer = NIL THEN
    RETURN
  END;
  
  (* release source and lexer *)
  Source.Release(self^.source);
  RELEASE lexer
  
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
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  allChars, upperChars, nonStdChars : CARDINAL;
  
BEGIN
  
  allChars := 0;
  upperChars := 0;
  nonStdChars := 0;
  
  REPEAT
    source.GetChar(ch, next);
    allChars++;
    
    IF (ch >= "A") AND (ch <= "Z") THEN
      upperChars++
    END;
    
    IF (ch = "_") OR (ch = "$") THEN
      nonStdChars++
    END
    
  UNTIL source.eof() OR NOT ASCII.isIdentChar(next);
  
  IF allChars = upperChars THEN (* possibly reserved word found *)
    (* TO DO : check for reserved word *)
    
  ELSIF allChars = nonStdChars THEN (* illegal identifier found *)
    LexDiag.New(diag, illegalIdent, 0, 0, "")
    
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
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

BEGIN
  
  source.GetChar(ch, next);
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
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

VAR
  delimiter, ch, next : CHAR;
  len : CARDINAL;

BEGIN
  
  len := 0;
  source.GetChar(delimiter, next);
  
  WHILE NOT source.eof() AND
        next # delimiter AND
        ASCII.isPrintable(next) DO
    
    IF next # ASCII.BACKSLASH THEN
      source.GetChar(ch, next);
      len++
      
    ELSE (* backslash *)
      MatchEscapeSequence(source, success);
      
      IF NOT success THEN (* unescaped backslash found *)
        (* TO DO : handle error *)
        
      END
    END
  END; (* WHILE *)
  
  IF next = delimiter THEN
    source.ConsumeChar();
    len++
    
  ELSE (* illegal character in string literal found *)
    (* TO DO : handle error *)
    
  END;
  
  IF len <= 1 THEN
    token := Token.QuotedChar
    
  ELSE (* len > 1 *)
    token := Token.QuotedString
    
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
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  
BEGIN

  REPEAT
    source.GetChar(ch, next)
  UNTIL source.eof() OR (next = ASCII.NEWLINE);
  
  token := Token.LineComment

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
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

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
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

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
  ( source : Source; VAR token : Token; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END MatchPragma;


END Lexer.