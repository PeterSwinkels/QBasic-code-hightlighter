DEFINT A-Z

CONST COMMENT = 0
CONST DEFAULT = 1
CONST DELIMITER = 2
CONST KEYWORD = 3
CONST LABEL = 4
CONST LINENUMBER = 5
CONST OPERATOR = 6
CONST STRINGV = 7

CONST CONTROLFLOW$ = " ELSE ERL GOSUB GOTO RESUME RETURN RESTORE RUN THEN "
CONST DELIMITERS$ = "(),;"
CONST OPERATORS$ = "*+-/<=>\"
CONST FALSE = 0
CONST TRUE = -1

DECLARE FUNCTION IsControlFlow (Buffer AS STRING)
DECLARE FUNCTION IsIdentifier (Buffer AS STRING)
DECLARE FUNCTION IsKeyword (Code AS STRING)
DECLARE FUNCTION IsNumber (Buffer AS STRING)
DECLARE SUB DisplayIdentifiers (Buffer AS STRING, EndOfStatement AS INTEGER)
DECLARE SUB DisplayLabels (Buffer AS STRING)
DECLARE SUB Initialize ()
DECLARE SUB Main ()
DECLARE SUB Parse (LineV AS STRING)
DECLARE SUB SubParse (Buffer AS STRING, TypeV AS INTEGER)

DIM SHARED Colors(0 TO 7) AS INTEGER

Initialize
CALL Main
END

Keywords:
DATA "ABSOLUTE"
DATA "ABS"
DATA "ABSOLUTE"
DATA "ACCESS"
DATA "ALIAS"
DATA "AND"
DATA "ANY"
DATA "APPEND"
DATA "AS"
DATA "ASC"
DATA "ATN"
DATA "ATN"
DATA "BASE"
DATA "BEEP"
DATA "BINARY"
DATA "BLOAD"
DATA "BSAVE"
DATA "BYVAL"
DATA "CALL"
DATA "CALLS"
DATA "CASE"
DATA "CDBL"
DATA "CDECL"
DATA "CHAIN"
DATA "CHDIR"
DATA "CHR$"
DATA "CINT"
DATA "CIRCLE"
DATA "CLEAR"
DATA "CLNG"
DATA "CLOSE"
DATA "CLS"
DATA "COLOR"
DATA "COM"
DATA "COMMAND$"
DATA "COMMON"
DATA "CONST"
DATA "COS"
DATA "CSNG"
DATA "CSRLIN"
DATA "CVD"
DATA "CVDMBF"
DATA "CVSMBF"
DATA "CVI"
DATA "CVL"
DATA "CVS"
DATA "DATA"
DATA "DATE$"
DATA "DECLARE"
DATA "DEF"
DATA "DEFDBL"
DATA "DEFINT"
DATA "DEFLNG"
DATA "DEFSNG"
DATA "DEFSTR"
DATA "DIM"
DATA "DO"
DATA "DOUBLE"
DATA "DRAW"
DATA "ELSE"
DATA "ELSEIF"
DATA "END"
DATA "ENDIF"
DATA "ENVIRON"
DATA "ENVIRON$"
DATA "EOF"
DATA "EQV"
DATA "ERASE"
DATA "ERDEV"
DATA "ERDEV$"
DATA "ERL"
DATA "ERR"
DATA "ERROR"
DATA "EXIT"
DATA "EXP"
DATA "FIELD"
DATA "FILEATTR"
DATA "FILES"
DATA "FIX"
DATA "FN"
DATA "FOR"
DATA "FRE"
DATA "FREEFILE"
DATA "FUNCTION"
DATA "GET"
DATA "GOSUB"
DATA "GOTO"
DATA "HEX$"
DATA "IF"
DATA "IMP"
DATA "INKEY$"
DATA "INP"
DATA "INPUT"
DATA "INPUT$"
DATA "INSTR"
DATA "INT"
DATA "INTEGER"
DATA "IOCTL"
DATA "IOCTL$"
DATA "IS"
DATA "KEY"
DATA "KILL"
DATA "LBOUND"
DATA "LCASE$"
DATA "LEFT$"
DATA "LEN"
DATA "LET"
DATA "LINE"
DATA "LIST"
DATA "LOC"
DATA "LOCAL"
DATA "LOCATE"
DATA "LOCK"
DATA "LOF"
DATA "LOG"
DATA "LONG"
DATA "LOOP"
DATA "LPOS"
DATA "LPRINT"
DATA "LSET"
DATA "LTRIM$"
DATA "MID$"
DATA "MKD$"
DATA "MKDIR"
DATA "MKDMBF$"
DATA "MKI$"
DATA "MKL$"
DATA "MKS$"
DATA "MKSMBF$"
DATA "MOD"
DATA "NAME"
DATA "NEXT"
DATA "NOT"
DATA "OCT"
DATA "OFF"
DATA "ON"
DATA "OPEN"
DATA "OPTION"
DATA "OR"
DATA "OUT"
DATA "OUTPUT"
DATA "PAINT"
DATA "PALETTE"
DATA "PCOPY"
DATA "PEEK"
DATA "PEN"
DATA "PLAY"
DATA "PMAP"
DATA "POINT"
DATA "POKE"
DATA "POS"
DATA "PRESET"
DATA "PRINT"
DATA "PSET"
DATA "PUT"
DATA "RANDOM"
DATA "RANDOMIZE"
DATA "READ"
DATA "REDIM"
DATA "REM"
DATA "RESET"
DATA "RESTORE"
DATA "RESUME"
DATA "RETURN"
DATA "RIGHT$"
DATA "RMDIR"
DATA "RND"
DATA "RSET"
DATA "RTRIM$"
DATA "RUN"
DATA "SADD"
DATA "SCREEN"
DATA "SEEK"
DATA "SEG"
DATA "SELECT"
DATA "SETMEM"
DATA "SGN"
DATA "SHARED"
DATA "SHELL"
DATA "SIGNAL"
DATA "SIN"
DATA "SINGLE"
DATA "SLEEP"
DATA "SNG"
DATA "SOUND"
DATA "SPACE$"
DATA "SPC"
DATA "SQR"
DATA "STATIC"
DATA "STEP"
DATA "STICK"
DATA "STOP"
DATA "STR$"
DATA "STRIG"
DATA "STRING"
DATA "STRING$"
DATA "SUB"
DATA "SWAP"
DATA "SYSTEM"
DATA "TAB"
DATA "TAN"
DATA "THEN"
DATA "TIME$"
DATA "TIMER"
DATA "TO"
DATA "TROFF"
DATA "TRON"
DATA "TYPE"
DATA "UBOUND"
DATA "UCASE$"
DATA "UEVENT"
DATA "UNLOCK"
DATA "UNTIL"
DATA "USING"
DATA "VAL"
DATA "VARPTR"
DATA "VARPTR$"
DATA "VARSEG"
DATA "VIEW"
DATA "WAIT"
DATA "WEND"
DATA "WHILE"
DATA "WIDTH"
DATA "WINDOW"
DATA "WRITE"
DATA "XOR"
DATA "$DYNAMIC"
DATA "$INCLUDE"
DATA "$STATIC"
DATA ""

SUB DisplayIdentifiers (Buffer AS STRING, EndOfStatement AS INTEGER)
DIM Identifier AS STRING
DIM NextPosition AS INTEGER
DIM Position AS INTEGER

 EndOfStatement = INSTR(Buffer, ":")
 IF EndOfStatement = 0 THEN EndOfStatement = INSTR(Buffer, "'")
 IF EndOfStatement = 0 THEN EndOfStatement = LEN(Buffer) + 1

 Position = 1
 DO
  NextPosition = INSTR(Position + 1, Buffer, ",")
  IF NextPosition >= EndOfStatement THEN NextPosition = EndOfStatement
  IF NextPosition = 0 THEN NextPosition = EndOfStatement
  
  Identifier = MID$(Buffer, Position, NextPosition - Position)
 
  IF IsNumber(Identifier) THEN
   COLOR Colors(LABEL)
  ELSE
   COLOR Colors(LINENUMBER)
  END IF
  
  PRINT Identifier;

  IF NextPosition >= EndOfStatement THEN
   EXIT DO
  ELSE
   COLOR Colors(DELIMITER)
   PRINT ",";
  END IF

  Position = NextPosition
 LOOP
END SUB

SUB Initialize

 Colors(COMMENT) = 14
 Colors(DEFAULT) = 9
 Colors(DELIMITER) = 13
 Colors(KEYWORD) = 10
 Colors(LABEL) = 15
 Colors(LINENUMBER) = 15
 Colors(OPERATOR) = 11
 Colors(STRINGV) = 12

 SCREEN 0
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS
END SUB

FUNCTION IsControlFlow (Buffer AS STRING)

 IsControlFlow = (INSTR(CONTROLFLOW$, " " + UCASE$(LTRIM$(RTRIM$(Buffer))) + " ") > 0)
END FUNCTION

FUNCTION IsIdentifier (Buffer AS STRING)
DIM Position AS INTEGER
DIM Result AS INTEGER

 Result = TRUE
 FOR Position = 1 TO LEN(Buffer)
  SELECT CASE MID$(Buffer, Position, 1)
   CASE "0" TO "9", "A" TO "Z", "a" TO "z"
   CASE ELSE
    Result = FALSE
    EXIT FOR
  END SELECT
 NEXT Position

 IsIdentifier = Result
END FUNCTION

FUNCTION IsKeyword (Code AS STRING)
DIM Found AS INTEGER
DIM Item AS STRING
DIM Query AS STRING

 Found = FALSE
 Query = UCASE$(LTRIM$(RTRIM$(Code)))
 RESTORE Keywords
 DO
  READ Item
  Found = (Item = Query)
 LOOP UNTIL Found OR Item = ""

 IsKeyword = Found
END FUNCTION

FUNCTION IsNumber (Buffer AS STRING)
 IsNumber = (LTRIM$(STR$(VAL(Buffer))) = Buffer)
END FUNCTION

SUB Main
DIM FileH AS INTEGER
DIM FileName AS STRING
DIM LineV AS STRING

 FileName = COMMAND$
 IF FileName = "" THEN
  PRINT "Syntax Highlighter for QBasic v1.01 - by: Peter Swinkels, ***2022***"
  PRINT
  PRINT "Usage: Hghlghtr.exe FILENAME"
 ELSE
  FileH = FREEFILE
  OPEN FileName FOR INPUT LOCK READ WRITE AS FileH
   DO UNTIL EOF(FileH) OR INKEY$ = CHR$(27)
    LINE INPUT #FileH, LineV
    Parse LineV
   LOOP
  CLOSE FileH
 END IF
END SUB

SUB Parse (LineV AS STRING)
DIM Buffer AS STRING
DIM Character AS STRING
DIM EndOfStatement AS INTEGER
DIM FirstCheck AS INTEGER
DIM Indentation AS INTEGER
DIM InIdentifiers AS INTEGER
DIM InString AS INTEGER
DIM Position AS INTEGER

 IF NOT LTRIM$(RTRIM$(LineV)) = "" THEN
  Buffer = ""
  Indentation = LEN(LineV) - LEN(LTRIM$(LineV))
  FirstCheck = INSTR(Indentation + 1, LineV, " ")
  IF FirstCheck = 0 THEN FirstCheck = LEN(LineV) + 1
  InString = FALSE
  Position = 1

  IF FirstCheck > 0 THEN
   IF IsNumber(LEFT$(LineV, FirstCheck - 1)) THEN
    COLOR Colors(LINENUMBER)
    PRINT LEFT$(LineV, FirstCheck);
    Position = FirstCheck + 1
   ELSEIF MID$(LineV, FirstCheck - 1, 1) = ":" THEN
    IF IsIdentifier(LEFT$(LineV, FirstCheck - 2)) THEN
     COLOR Colors(LABEL)
     PRINT LEFT$(LineV, FirstCheck);
     Position = FirstCheck + 1
    END IF
   END IF
  END IF
  
  DO UNTIL Position > LEN(LineV)
   Character = MID$(LineV, Position, 1)
  
   IF InString THEN
    IF Character = CHR$(34) THEN
     Buffer = Buffer + Character
     SubParse Buffer, STRINGV
     InString = FALSE
    ELSE
     Buffer = Buffer + Character
    END IF
   ELSE
    IF InIdentifiers THEN
     SubParse Buffer, DEFAULT
     DisplayIdentifiers MID$(LineV, Position), EndOfStatement
     Position = Position + (EndOfStatement - 2)
     InIdentifiers = FALSE
    ELSE
     SELECT CASE Character
      CASE " ", ":", "(", ")"
       IF IsKeyword(Buffer) THEN
        IF UCASE$(LTRIM$(RTRIM$(Buffer))) = "REM" THEN
         SubParse Buffer, KEYWORD
         Buffer = MID$(LineV, Position)
         SubParse Buffer, COMMENT
         EXIT DO
        ELSE
         InIdentifiers = IsControlFlow(Buffer)
         SubParse Buffer, KEYWORD
        END IF
       ELSE
        SubParse Buffer, DEFAULT
       END IF
      CASE CHR$(34)
       SubParse Buffer, DEFAULT
       InString = TRUE
      CASE "'"
       SubParse Buffer, DEFAULT
       Buffer = MID$(LineV, Position)
       SubParse Buffer, COMMENT
       EXIT DO
     END SELECT

     Buffer = Buffer + Character
    END IF
   END IF

   Position = Position + 1
  LOOP

  IF NOT Buffer = "" THEN
   IF IsKeyword(Buffer) THEN
    IF UCASE$(LTRIM$(RTRIM$(Buffer))) = "REM" THEN
     SubParse Buffer, KEYWORD
     Buffer = MID$(LineV, Position)
     SubParse Buffer, COMMENT
    ELSE
     InIdentifiers = IsControlFlow(Buffer)
     SubParse Buffer, KEYWORD
    END IF
   ELSE
    SubParse Buffer, DEFAULT
   END IF
  END IF
 END IF

 PRINT
END SUB

SUB SubParse (Buffer AS STRING, TypeV AS INTEGER)
DIM Character AS STRING
DIM Position AS INTEGER

 SELECT CASE TypeV
  CASE COMMENT, STRINGV
   COLOR Colors(TypeV)
   PRINT Buffer;
  CASE ELSE
   FOR Position = 1 TO LEN(Buffer)
    Character = MID$(Buffer, Position, 1)
    IF INSTR(DELIMITERS$, Character) > 0 THEN
     COLOR Colors(DELIMITER)
    ELSEIF INSTR(OPERATORS$, Character) > 0 THEN
     COLOR Colors(OPERATOR)
    ELSE
     COLOR Colors(TypeV)
    END IF
    PRINT Character;
   NEXT Position
 END SELECT

 Buffer = ""
END SUB

