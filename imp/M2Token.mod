(*!m2r10*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE M2Token;

(* Token Subranges *)

TYPE
  ResWords = [Alias..Yield] OF M2Token;
  ProcBindables = [Abs..Writef] OF M2Token;
  ConstBindables = [Tbase..Trefc] OF M2Token;
  Identifiers = [Abs..OtherIdent] OF M2Token;
  Numbers = [WholeNumber..RealNumber] OF M2Token;
  CharsAndStrings = [CharCode..QuotedString] OF M2Token;
  NonOpPunctuation = [Dot..Minus] OF M2Token;
  Operators = [Equal..TypeConv] OF M2Token;
  NonRWOperL1 = [Equal..Identity] OF M2Token;
  NonRWOperL2 = [Plus..SetDiff] OF M2Token;
  NonRWOperL3 = [Asterisk..RealDiv] OF M2Token;
  
  
(* Functions To Determine Token Classification *)

PROCEDURE isResWord ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a reserved word, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(ResWords) AND (t <= TMAX(ResWords)
END isResWord;


PROCEDURE isIdentifier ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is an identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(Identifiers) AND (t <= TMAX(Identifiers)
END isResWord;


PROCEDURE isConstBindableIdent ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a constant bindable identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(ConstBindables) AND (t <= TMAX(ConstBindables)
END isResWord;


PROCEDURE isProcBindableIdent ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a procedure bindable identifier, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(ProcBindables) AND (t <= TMAX(ProcBindables)
END isResWord;


PROCEDURE isNumber ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a number literal, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(Numbers) AND (t <= TMAX(Numbers)
END isNumber;


PROCEDURE isCharOrString ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a character or string, otherwise FALSE. *)
BEGIN
  RETURN (t >= TMIN(CharsAndStrings) AND (t <= TMAX(CharsAndStrings)
END isResWord;


PROCEDURE isPunctuation ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a number literal, otherwise FALSE. *)
BEGIN
  RETURN
    (t >= TMIN(NonOpPunctuation) AND (t <= TMAX(NonOpPunctuation) OR
    (t = M2Token.Aster) OR (t = M2Token.Plus) OR (t = M2Token.Minus)
END isPunctuation;


PROCEDURE isOperL1 ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a level-1 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = M2Token.In) OR
    ((t >= TMIN(OperatorsL1) AND (t <= TMAX(OperatorsL1)) OR
END isOperL1;


PROCEDURE isOperL2 ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a level-2 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = M2Token.Or) OR
    ((t >= TMIN(NonRWOperL2) AND (t <= TMAX(NonRWOperL2))
END isOperL2;


PROCEDURE isOperL3 ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a level-3 operator, otherwise FALSE. *)
BEGIN
  RETURN
    (t = M2Token.And) OR (t = M2Token.Div) OR (t = M2Token.Mod) OR
    ((t >= TMIN(NonRWOperL2) AND (t <= TMAX(NonRWOperL2))
END isOperL3;


PROCEDURE isComment ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a comment, otherwise FALSE. *)
BEGIN
  RETURN (t = M2Token.Comment)
END isComment;


PROCEDURE isPragma ( t : M2Token ) : BOOLEAN;
 (* Returns TRUE if t is a pragma, otherwise FALSE. *)
BEGIN
  RETURN (t = M2Token.Pragma)
END isPragma;


END M2Token.