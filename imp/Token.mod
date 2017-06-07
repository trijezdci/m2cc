(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Token;

(* Token Subranges *)

TYPE
  ResWords = [Alias..Yield] OF Token;
  ProcBindables = [Abs..Writef] OF Token;
  ConstBindables = [Tbase..Trefc] OF Token;
  Identifiers = [Abs..OtherIdent] OF Token;
  Numbers = [WholeNumber..RealNumber] OF Token;
  CharsAndStrings = [CharCode..QuotedString] OF Token;
  NonOpPunctuation = [Dot..Minus] OF Token;
  Operators = [Equal..TypeConv] OF Token;
  NonRWOperL1 = [Equal..Identity] OF Token;
  NonRWOperL2 = [Plus..SetDiff] OF Token;
  NonRWOperL3 = [Asterisk..RealDiv] OF Token;
  
  
(* Functions To Determine Token Classification *)

PROCEDURE isResWord ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a reserved word, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(ResWords) AND t <= TMAX(ResWords))
END isResWord;


PROCEDURE isIdentifier ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is an identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(Identifiers) AND t <= TMAX(Identifiers))
END isResWord;


PROCEDURE isConstBindableIdent ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a constant bindable identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(ConstBindables) AND t <= TMAX(ConstBindables))
END isResWord;


PROCEDURE isProcBindableIdent ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a procedure bindable identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(ProcBindables) AND t <= TMAX(ProcBindables))
END isResWord;


PROCEDURE isNumber ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a number literal, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(Numbers) AND t <= TMAX(Numbers))
END isNumber;


PROCEDURE isCharOrString ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a character or string, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(CharsAndStrings) AND t <= TMAX(CharsAndStrings))
END isResWord;


PROCEDURE isPunctuation ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a number literal, otherwise FALSE. *)
BEGIN
  RETURN
    (t >= TMIN(NonOpPunctuation) AND t <= TMAX(NonOpPunctuation)) OR
    (t = Token.Aster) OR (t = Token.Plus) OR (t = Token.Minus)
END isPunctuation;


PROCEDURE isOperL1 ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a level-1 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = Token.In) OR
    (t >= TMIN(OperatorsL1) AND t <= TMAX(OperatorsL1))
END isOperL1;


PROCEDURE isOperL2 ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a level-2 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = Token.Or) OR
    (t >= TMIN(NonRWOperL2) AND t <= TMAX(NonRWOperL2))
END isOperL2;


PROCEDURE isOperL3 ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a level-3 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = Token.And) OR (t = Token.Div) OR (t = Token.Mod) OR
    (t >= TMIN(NonRWOperL2) AND t <= TMAX(NonRWOperL2))
END isOperL3;


PROCEDURE isComment ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a comment, otherwise FALSE. *)
BEGIN
  RETURN (t = Token.Comment)
END isComment;


PROCEDURE isPragma ( t : Token ) : BOOLEAN;
 (* Returns TRUE if t is a pragma, otherwise FALSE. *)
BEGIN
  RETURN (t = Token.Pragma)
END isPragma;


END Token.