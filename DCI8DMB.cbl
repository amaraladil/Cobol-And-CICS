       01  MAP1I.
           02  FILLER PIC X(12).
           02  SCHNAMEL    COMP  PIC  S9(4).
           02  SCHNAMEF    PICTURE X.
           02  FILLER REDEFINES SCHNAMEF.
             03 SCHNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SCHNAMEI  PIC X(15).
           02  LINED OCCURS 10 TIMES.
             03  LINEL    COMP  PIC  S9(4).
             03  LINEF    PICTURE X.
             03  FILLER   PICTURE X(2).
             03  LINEI  PIC X(79).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(60).
       01  MAP1O REDEFINES MAP1I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  SCHNAMEC    PICTURE X.
           02  SCHNAMEH    PICTURE X.
           02  SCHNAMEO  PIC X(15).
           02  DFHMS1 OCCURS 10 TIMES.
             03  FILLER PICTURE X(2).
             03  LINEA    PICTURE X.
             03  LINEC    PICTURE X.
             03  LINEH    PICTURE X.
             03  LINEO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(60).