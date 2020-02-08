DEFINT A-Z
DECLARE SUB LoadConfiguration ()
DECLARE SUB mousespeed (speed%)
DECLARE SUB MouseArea (x1%, x2%, y1%, y2%)
DECLARE FUNCTION Mouseinterr% (num%, ax%, bx%, cx%, dx%)
DECLARE SUB mousepointer (OnOrOff$)
DECLARE SUB mouseset (x%, y%)
DECLARE SUB mousewait (xpos%, ypos%, brique%)
DECLARE SUB dessinetableau ()
DECLARE SUB graphisme ()
DECLARE SUB dessineinter ()
DECLARE SUB restoretableau ()

DIM Mouse%(0 TO 45)
DIM SHARED TABLEAU%(15, 19)
DIM SHARED tableau2$(15, 19)
DIM SHARED violet&(125)
DIM SHARED marron&(125)
DIM SHARED GRIS&(125)
DIM SHARED GRISFONCE&(125)
DIM SHARED bleuplus&(125)
DIM SHARED vertplus&(125)
DIM SHARED bleucielplus&(125)
DIM SHARED rougeplus&(125)
DIM SHARED violetplus&(125)
DIM SHARED jaune&(125)
DIM SHARED blanc&(125)
DIM SHARED noir&(125)


COMMON SHARED mouseb%, mousex%, mousey%
'COMMON SHARED xpos%, ypos%, brique%
brique% = 5

'Initialisation
SCREEN 12

'Creation des sprites
graphisme

'Dessine l'interface
WINDOW (1, 1)-(640, 480)
PALETTE 1, 15
'LINE (10, 50)-(630, 470), 15, B
debut:
ON ERROR GOTO gestionerreur
dessinetableau
dessineinter
restoretableau
'initialise la souris
LoadConfiguration
MouseArea 0, 638, 0, 479
mouseset 320, 240
mousepointer "On"

GOSUB eteindrevoyant

KEY 17, CHR$(&H0) + CHR$(&H1)
    ON KEY(17) GOSUB fin
KEY 18, CHR$(&H0) + CHR$(59)
    ON KEY(18) GOSUB save
KEY 19, CHR$(&H0) + CHR$(60)
    ON KEY(19) GOSUB load
KEY 20, CHR$(&H0) + CHR$(61)
    ON KEY(20) GOSUB quit
KEY 21, CHR$(&H0) + CHR$(62)
    ON KEY(21) GOSUB retour

KEY(17) ON
KEY(18) ON
KEY(19) ON
KEY(20) ON
KEY(21) ON

DO
CALL mousewait(xpos%, ypos%, brique%)
LOOP
END




eteindrevoyant:
DEF SEG = 0
KeyFlags = PEEK(1047)
POKE 1047, &H0
DEF SEG
RETURN


load:
LOCATE 1, 1
PRINT "LISTE DES NIVEAUX"
FILES "c:\popanoye\niveaux\*.niv"
COLOR 14
PRINT "NE TAPE RIEN SI TU NE VEUT RIEN CHARGER"
INPUT "QUELLE FICHIER CHOISI TU ?: ", fichier$
IF LEN(fichier$) = 0 GOTO debut
fichier$ = "c:\popanoye\niveaux\" + fichier$ + ".niv"
ON ERROR GOTO loaderreur
OPEN fichier$ FOR INPUT AS #1
  FOR b% = 1 TO 15
  FOR A% = 1 TO 19
  INPUT #1, c%
  TABLEAU%(b%, A%) = c%
  NEXT
  NEXT
CLOSE #1
mouseset 5, 100
GOTO debut
RETURN

fin:
END

quit:
OPEN "c:\popanoye\niveaux\a.niv" FOR OUTPUT AS #1
  FOR b% = 1 TO 15
  FOR A% = 1 TO 19
  PRINT #1, TABLEAU%(b%, A%)
  NEXT
  NEXT
CLOSE #1
END

retour:
OPEN "c:\popanoye\niveaux\a.niv" FOR OUTPUT AS #1
  FOR b% = 1 TO 15
  FOR A% = 1 TO 19
  PRINT #1, TABLEAU%(b%, A%)
  NEXT
  NEXT
CLOSE #1
RESET
CHAIN "c:\popanoye\popanoye.bas"
RETURN


save:
COLOR 12
LOCATE 1, 1
INPUT "Nom du fichier (8 caractere maxi):", fichier$
IF LEN(fichier$) > 8 OR LEN(fichier$) = 0 THEN GOTO save
fichier$ = "c:\popanoye\niveaux\" + fichier$ + ".niv"
OPEN fichier$ FOR OUTPUT AS #1
  FOR b% = 1 TO 15
  FOR A% = 1 TO 19
  PRINT #1, TABLEAU%(b%, A%)
  NEXT
  NEXT
CLOSE #1

dessineinter
dessinetableau

GOTO debut
RETURN

loaderreur:
SELECT CASE ERR
CASE 53
CLS
COLOR 12
PRINT "ERREUR : "; ERR
COLOR 15
PRINT "CE FICHIER N'EXISTE PAS"
CASE ELSE
END SELECT
END

gestionerreur:
CLS
COLOR 12
PRINT "ERREUR : "; ERR
COLOR 15
PRINT "UNE ERREUR IMPREVU EST SURVENU"
PRINT "LE TABLEAU ACTUEL A ETE SAUVEGARDE DANS a.niv"
OPEN "c:\popanoye\niveaux\a.niv" FOR OUTPUT AS #1
  FOR b% = 1 TO 15
  FOR A% = 1 TO 19
  PRINT #1, TABLEAU%(b%, A%)
  NEXT
  NEXT
CLOSE #1
END

DATA.Mouse:
DATA 55,8b,ec,56,57,8b,76,0c,8b,04,8b,76,0a,8b,1c,8b,76,08,8b,0c,8b,76,06,8b
DATA 14,cd,21,8b,76,0c,89,04,8b,76,0a,89,1c,8b,76,08,89,0c,8b,76,06,89,14,5f
DATA 5e,5d,ca,08,00

SUB dessineinter
LINE (1, 1)-(640, 480), 1, BF
'grille du niveau
FOR b = 0 TO 13
FOR A = 1 TO 20
LINE (A * 31 - 5, 479)-(A * 31 - 5, 78), 15
LINE (26, 449 - (((b - 1) * 31)))-(614, 449 - (((b - 1) * 31))), 15
NEXT
NEXT

COLOR 13
LOCATE 1, 1
PRINT "R"
LINE (1, 480)-(10, 465), 13, B
PAINT (1, 480), 1, 13

LINE (1, 70)-(640, 1), 12, B
LOCATE 27, 50
PRINT "F1->SAUVEGARDER   F2->CHARGER "
LOCATE 28, 50
PRINT "F3->QUITTER       F4->RETOUR  "


FOR A = 0 TO 10
'PUT (a * 30 + 5, 29), violet&, PSET
SELECT CASE A + 5
CASE 5: PUT (A * 30 + 5, 29), violet&, PSET
CASE 6: PUT (A * 30 + 5, 29), marron&, PSET
CASE 7: PUT (A * 30 + 5, 29), GRIS&, PSET
CASE 8: PUT (A * 30 + 5, 29), GRISFONCE&, PSET
CASE 9: PUT (A * 30 + 5, 29), bleuplus&, PSET
CASE 10: PUT (A * 30 + 5, 29), vertplus&, PSET
CASE 11: PUT (A * 30 + 5, 29), bleucielplus&, PSET
CASE 12: PUT (A * 30 + 5, 29), rougeplus&, PSET
CASE 13: PUT (A * 30 + 5, 29), violetplus&, PSET
CASE 14: PUT (A * 30 + 5, 29), jaune&, PSET
CASE 15: PUT (A * 30 + 5, 29), blanc&, PSET
CASE ELSE
END SELECT
NEXT


END SUB

SUB dessinetableau
FOR b = 1 TO 13
FOR A = 1 TO 19
SELECT CASE TABLEAU%(b, A)
CASE 5: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), violet&, PSET
CASE 6: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), marron&, PSET
CASE 7: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), GRIS&, PSET
CASE 8: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), GRISFONCE&, PSET
CASE 9: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), bleuplus&, PSET
CASE 10: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), vertplus&, PSET
CASE 11: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), bleucielplus&, PSET
CASE 12: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), rougeplus&, PSET
CASE 13: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), violetplus&, PSET
CASE 14: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), jaune&, PSET
CASE 15: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), blanc&, PSET
CASE ELSE
END SELECT
NEXT
NEXT
END SUB

SUB graphisme
'CREATION DES MATRICES POUR LES BRIQUES
WINDOW (1, 1)-(320, 240)
LINE (100, 115)-(115, 100), 1, BF
'MISE EN MEMOIRE DES BRIQUES
FOR couleur% = 5 TO 16
LINE (100, 100)-(115, 115), 0, B
PAINT (101, 101), couleur, 0
LINE (102, 102)-(113, 113), 0, B
LINE (100, 100)-(102, 102), 0
LINE (100, 115)-(102, 113), 0
LINE (115, 100)-(113, 102), 0
LINE (115, 115)-(113, 113), 0
IF couleur% = 5 THEN GET (100, 115)-(115, 100), violet&
IF couleur% = 6 THEN GET (100, 115)-(115, 100), marron&
IF couleur% = 7 THEN GET (100, 115)-(115, 100), GRIS&
IF couleur% = 8 THEN GET (100, 115)-(115, 100), GRISFONCE&
IF couleur% = 9 THEN GET (100, 115)-(115, 100), bleuplus&
IF couleur% = 10 THEN GET (100, 115)-(115, 100), vertplus&
IF couleur% = 11 THEN GET (100, 115)-(115, 100), bleucielplus&
IF couleur% = 12 THEN GET (100, 115)-(115, 100), rougeplus&
IF couleur% = 13 THEN GET (100, 115)-(115, 100), violetplus&
IF couleur% = 14 THEN GET (100, 115)-(115, 100), jaune&
IF couleur% = 15 THEN GET (100, 115)-(115, 100), blanc&
PALETTE 1, 15
LINE (100, 115)-(115, 100), 1, BF
NEXT
PALETTE 1, 15
LINE (95, 115)-(120, 95), 1, BF
GET (100, 115)-(115, 100), noir&
CLS

END SUB

SUB LoadConfiguration
SHARED Mouse%()
RESTORE DATA.Mouse
DEF SEG = VARSEG(Mouse%(0))
FOR i% = 0 TO 52
 READ byte$
 POKE VARPTR(Mouse%(0)) + i%, VAL("&H" + byte$)
NEXT
MouseArea 0, 638, 0, 479
mousespeed 20
mouseset 200, 100
mousepointer "On"
END SUB

SUB MouseArea (x1%, x2%, y1%, y2%)
R% = Mouseinterr%(&H33, 7, 0, x1%, x2%)
R% = Mouseinterr%(&H33, 8, bx%, y1%, y2%)
END SUB

FUNCTION Mouseinterr% (num%, ax%, bx%, cx%, dx%)

SHARED Mouse%()
 IF Mouse%(0) = 0 THEN
  PRINT "Mouse error, program stopped"
  SYSTEM
 END IF
DEF SEG = VARSEG(Mouse%(0))
POKE VARPTR(Mouse%(0)) + 26, num%
CALL ABSOLUTE(ax%, bx%, cx%, dx%, VARPTR(Mouse%(0)))
Mouseinterr% = ax%
END FUNCTION

SUB mousepointer (OnOrOff$)
SELECT CASE OnOrOff$
  CASE "On": R% = Mouseinterr%(&H33, 1, bx%, cx%, dx%)
  CASE "Off": R% = Mouseinterr%(&H33, 2, bx%, cx%, dx%)
END SELECT
END SUB

SUB mouseset (x%, y%)
R% = Mouseinterr%(&H33, 4, bx%, x%, y%)
END SUB

SUB mousespeed (speed%)
R% = Mouseinterr%(&H33, 15, bx%, speed%, speed% * 2)
END SUB

SUB mousewait (xpos%, ypos%, brique%)
oldxpos% = xpos%
oldypos% = ypos%
R% = Mouseinterr%(&H33, 3, mouseb%, mousex%, mousey%)
xpos% = INT((mousex% + 5) / 31)
ypos% = INT((mousey%) / 31) + 1


IF mouseb% = 2 AND xpos% >= 1 AND xpos% <= 19 AND ypos% >= 1 AND ypos% <= 13 THEN TABLEAU%(ypos%, xpos%) = 0: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), noir&, PSET

IF mouseb% = 1 THEN
SELECT CASE mousey%
CASE IS < 412
IF mousex% < 10 AND mousey% < 15 THEN restoretableau
IF xpos% >= 1 AND xpos% <= 19 AND ypos% >= 1 AND ypos% <= 13 THEN
SELECT CASE brique%
CASE 5: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), violet&, PSET: TABLEAU%(ypos%, xpos%) = 5
CASE 6: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), marron&, PSET: TABLEAU%(ypos%, xpos%) = 6
CASE 7: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), GRIS&, PSET: TABLEAU%(ypos%, xpos%) = 7
CASE 8: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), GRISFONCE&, PSET: TABLEAU%(ypos%, xpos%) = 8
CASE 9: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), bleuplus&, PSET: TABLEAU%(ypos%, xpos%) = 9
CASE 10: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), vertplus&, PSET: TABLEAU%(ypos%, xpos%) = 10
CASE 11: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), bleucielplus&, PSET: TABLEAU%(ypos%, xpos%) = 11
CASE 12: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), rougeplus&, PSET: TABLEAU%(ypos%, xpos%) = 12
CASE 13: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), violetplus&, PSET: TABLEAU%(ypos%, xpos%) = 13
CASE 14: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), jaune&, PSET: TABLEAU%(ypos%, xpos%) = 14
CASE 15: PUT (xpos% * 31 - 5, (16 - ypos%) * 31 - 16), blanc&, PSET: TABLEAU%(ypos%, xpos%) = 15
CASE ELSE
END SELECT
END IF

CASE 421 TO 452
SELECT CASE mousex%
CASE 3 TO 33: brique% = 5
CASE 34 TO 62: brique% = 6
CASE 64 TO 92: brique% = 7
CASE 94 TO 122: brique% = 8
CASE 124 TO 152: brique% = 9
CASE 154 TO 182: brique% = 10
CASE 184 TO 212: brique% = 11
CASE 214 TO 242: brique% = 12
CASE 244 TO 272: brique% = 13
CASE 274 TO 302: brique% = 14
CASE 304 TO 332: brique% = 15
CASE ELSE
END SELECT
CASE ELSE
END SELECT

END IF






END SUB

SUB restoretableau
LINE (26, 78)-(640, 480), 1, BF
FOR b = 0 TO 13
FOR A = 1 TO 20
LINE (A * 31 - 5, 479)-(A * 31 - 5, 78), 15
LINE (26, 449 - (((b - 1) * 31)))-(614, 449 - (((b - 1) * 31))), 15
NEXT
NEXT
FOR b = 1 TO 13
FOR A = 1 TO 19
SELECT CASE TABLEAU%(b, A)
CASE 5: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), violet&, PSET
CASE 6: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), marron&, PSET
CASE 7: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), GRIS&, PSET
CASE 8: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), GRISFONCE&, PSET
CASE 9: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), bleuplus&, PSET
CASE 10: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), vertplus&, PSET
CASE 11: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), bleucielplus&, PSET
CASE 12: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), rougeplus&, PSET
CASE 13: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), violetplus&, PSET
CASE 14: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), jaune&, PSET
CASE 15: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), blanc&, PSET
CASE ELSE
END SELECT
NEXT
NEXT
END SUB

