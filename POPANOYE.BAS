



'*****************************************************************************
'***************************** POPANOYE v1.0 *********************************
'*****************************************************************************
'*****************************************************************************
'*********************  Programmer par COMBES BERTRAND  **********************
'*********************  Graphisme par COMBES BERTRAND   **********************
'*****************************************************************************
'-----------------------------------------------------------------------------
'-----------------------------APPUYER SUR F5------------------------------------
'-----------------------------------------------------------------------------





DEFINT A-Z
DECLARE SUB restorebrique (xobjet!, yobjet!)
DECLARE SUB lancerballe (xbarre%, ybarre%, testbarre%, xballe!, yballe!, testballe%, bouge%, divers%, mousex%, mouseb%, n!, commande%, mode, m!)
DECLARE SUB generique ()
DECLARE SUB effacebrique (xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
DECLARE SUB choc (xballe!, yballe!, xbarre%, ybarre%, testbarre%, n!, m!, bouge%, xpositionmatricielleballe%, ypositionmatricielleballe%, score, life%, briquerestante%, divers%, xobjet!, yobjet!, divers3%, testballe%, divers2%, vitesse%, directory$, A$)
DECLARE SUB choc2 (xballe!, yballe!, xbarre%, ybarre%, testbarre%, n!, m!, bouge%, xpositionmatricielleballe%, ypositionmatricielleballe%, score, life%, briquerestante%, divers%, xobjet!, yobjet!, divers3%, testballe%, divers2%, vitesse%, directory$, A$, lancer%)
DECLARE SUB center (ROW!, text$)
DECLARE SUB defiler (aze!, azr!)
DECLARE SUB dessinebarre (xbarre%, ybarre%, testbarre%, divers%)
DECLARE SUB affichescore (score, divers%, divers2%)
DECLARE SUB affichevie (life%, divers%)
DECLARE SUB choper (testbarre%, divers%, divers2%, divers3%, xbarre%, testballe%, life%, commande%, mousex%)
DECLARE SUB dessineballe (xballe!, yballe!, testballe%)
DECLARE SUB dessinebrique ()
DECLARE SUB savescore (score, directory$, A$, interface)
DECLARE SUB decompose (score, A$)
DECLARE SUB affichehighscore (hnom1$, hsco1$, hnom2$, hsco2$, hnom3$, hsco3$, interface)
      
'C'est pour la souris
DECLARE SUB LoadConfiguration ()
DECLARE SUB MouseArea (x1%, x2%, y1%, y2%)
DECLARE SUB mousepointer (OnOrOff$)
DECLARE SUB mousespeed (speed%)
DECLARE SUB mouseset (x%, y%)
DECLARE SUB mousewait (xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%, xballe!, m!, lancer%)
DECLARE FUNCTION Mouseinterr% (num%, ax%, bx%, cx%, dx%)
DECLARE SUB mousewait2 (xbarre%, mousex%, mouseb%, curpos)
DECLARE SUB mousewait3 (xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%)
DECLARE SUB MouseArea2 (testbarre%)

'C'est pour la carte son
DECLARE SUB playwav (sonwav$)
DECLARE FUNCTION ResetDSP% ()
DECLARE SUB MasterVolume (Right%, Left%, Getvol%)
DECLARE SUB WriteDSP (byte%)
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
DECLARE SUB GetBLASTER (DMA%, BasePort%, IRQ%)
DECLARE FUNCTION DSPversion! ()
DECLARE SUB SetReg (Reg%, Value%)

CONST BaseAddr = &H220
CONST RegAddr = BaseAddr + 8, DataAddr = BaseAddr + 9
COMMON SHARED BasePort%, LenPort%, Channel%
'les repertoires
COMMON SHARED directory$, directory2$, directory3$
COMMON SHARED interface
'les sons
COMMON SHARED sonb5$, sonb6$, sonb78$, sonb9$, sonb10$, sonb11$
COMMON SHARED sonb12$, sonb13$, sonb14$, sonb15$, sonsouris$, sonhello$
COMMON SHARED sonmur$, sonwin$, sonlancer$, sonrecup$, sonambience$, sonbyby$
COMMON SHARED sonoui$, sonnon$, sonperdu$, sonchoper$, sonlift$

ON ERROR GOTO 100

GetBLASTER Channel%, BasePort%, IRQ%



PRINT
IF ResetDSP% THEN
   PRINT
ELSE
   PRINT
END IF

SpeakerState 1

MasterVolume Right%, Left%, -1
PRINT
MasterVolume 13, 13, 0

DIM SHARED WavBuffer(1 TO 1) AS STRING * 32767

'les repertoires
directory$ = "c:\popanoye\save\popscore"   'repertoire des high scores
directory2$ = "c:\popanoye\save\popcon"    'fichier setup
directory3$ = "c:\popanoye\son\"           'fichier wav

'les sons approprier
sonb5$ = "son1.wav"
sonb6$ = "crash.wav"
sonb78$ = "bomb.wav"
sonb9$ = "son2.wav"
sonb10$ = "son4.wav"
sonb11$ = "beep.wav"
sonb12$ = "crash.wav"
sonb13$ = "son5.wav"
sonb14$ = "son6.wav"
sonb15$ = "son7.wav"
sonmur$ = "mur2.wav"
sonwin$ = "wewin.wav"
sonlancer$ = "go!.wav"
sonrecup$ = "bomb.wav"
sonsouris$ = "click.wav"
sonambience$ = "pluie.wav"
sonhello$ = "hello.wav"
sonbyby$ = "byby.wav"
sonhello$ = "hello.wav"
sonoui$ = "yes.wav"
sonnon$ = "no.wav"
sonperdu$ = "boo.wav"
sonchoper$ = "nuclear.wav"
sonlift$ = "ricochet.wav"



DIM Mouse%(0 TO 45)

SCREEN 12

'*****************************************************************************
'************   REGLE LA VARIABLE VITESSE POUR AJUSTER          **************
'************   LA VITESSE DU JEU EN FCT DE TON PC              **************
'************   PLUS ELLE EST PROCHE DE ZERO PLUS CA VA VITE    **************
'************   NE MET PAS DE CHIFFRE NEGATIF OU CA PLANTE      **************
'*****************************************************************************

'permet a tout moment de pouvoir quitter le jeu
KEY 17, CHR$(&H0) + CHR$(&H1)
    ON KEY(17) GOSUB fin

KEY(17) ON



ON TIMER(1) GOSUB vit
RANDOMIZE TIMER
TIMER ON
aqs# = 0
DO
aqs# = aqs# + .1
LOOP

vitesse:
vitessedep% = INT(aqs# * 2)
vitesse% = INT(aqs# * 2)
IF vitesse% > 1001 THEN vitesse% = vitesse% - 1000: vitessedep% = vitesse%
vitesseordi% = INT(aqs# * 2)

GOSUB eteindrevoyant
      
ON ERROR GOTO 200
LoadConfiguration
mousepointer "Off"
DIM c$(30)

ON ERROR GOTO 300

inter2:
'Lecture du fichier
OPEN directory2$ FOR INPUT AS #2
     INPUT #2, var1    'Vitesse
     INPUT #2, var2    'Niveaux
     INPUT #2, var3    'Mode
     INPUT #2, var4    'Sensibilite souris
CLOSE #2


ON ERROR GOTO 400
vitesse% = var1
vitessedep% = var1
IF kaka = 1 THEN kaka = 0: GOTO inte

inter:
'*****************************************************************************
GOSUB eteindrevoyant
life% = 0
score = 0
interface = 1
charger% = 0
KEY 17, CHR$(&H0) + CHR$(&H1)
    ON KEY(17) GOSUB fin
KEY 18, CHR$(&H0) + CHR$(&H19)
    ON KEY(18) GOSUB pause



KEY(17) ON
KEY(18) ON



SCREEN 12
WINDOW (1, 1)-(640, 480)

CLS

LINE (120, 46)-(530, 430), 14, B
LINE (110, 36)-(540, 440), 14, B
PAINT (115, 45), 14, 14

LINE (325, 315)-(522, 80), 14, B

COLOR 15
LOCATE 12, 49
PRINT "PARAMETRES"

LOCATE 15, 43
PRINT "VITESSE ORDI :"; vitesseordi%
LOCATE 16, 43
PRINT "VITESSE CHOISIT:"; vitessedep%
LOCATE 18, 43
PRINT "DIFFICULTE : "
LOCATE 18, 56
SELECT CASE var2
CASE 5: PRINT " HARD"
CASE 10: PRINT " NORMAL"
CASE 15: PRINT " EASY"
CASE ELSE
END SELECT

LOCATE 20, 43
PRINT "MODE"
LOCATE 20, 48
SELECT CASE var3
CASE 0: PRINT "POP CORN"
CASE 1: PRINT "ARKANOID"
CASE ELSE
END SELECT

LOCATE 22, 43
PRINT "SENSIBILITE  SOURIS :"


'i$(1) = "…Õª  …Õª  …Õª       …ÕÕ  …Õª  …Õª  …Õπ"
'i$(2) = "ÃÕº  ∫ ∫  ÃÕº       ∫    ∫ ∫  ÃÀº  ∫ ∫"
'i$(3) = "     »Õº            »ÕÕ  »Õº   »Õ     "

i$(1) = "…Õª  …Õª  …Õª  …Õª  …Õπ  …Õª  À À  …Õª      "
i$(2) = "ÃÕº  ∫ ∫  ÃÕº  ÃÕπ  ∫ ∫  ∫ ∫  »Àº  Ãπ      "
i$(3) = "     »Õº                 »Õº       »Õº     "


LINE (130, 350)-(520, 420), 14, B

FOR t% = 1 TO 36 STEP 5
COLOR INT(14 * RND) + 1
FOR x% = 1 TO 3
LOCATE x% + 4, 21 + t%: PRINT MID$(A$(x%), t%, 3)
NEXT x%, t%

COLOR 14
LOCATE 8, 60
PRINT "V1.0"

COLOR 12
center 10, "   MENU PRINCIPAL"

COLOR 14
LOCATE 12, 23
PRINT "NOUVELLE PARTIE"

LOCATE 14, 23
PRINT "CONFIGURATION"

LOCATE 16, 23
PRINT "VIEW HIGH SCORES"

LOCATE 18, 23
PRINT "EDITEUR DE NIVEAUX"

LOCATE 20, 23
PRINT "MODE DE JEU"

LOCATE 22, 23
PRINT "CHARGER NIVEAUX"

LOCATE 24, 23
PRINT "QUITTER"

COLOR 12
LOCATE 27, 17
PRINT "Email: BER1981@AOL.COM"

LOCATE 27, 48
PRINT "By COMBES BERTRAND"

'encadre les menus
'FOR a = 0 TO 170 STEP 32
'LINE (170, 288 - a)-(322, 307 - a), 13, B
'NEXT

TIMER OFF
ON TIMER(1) GOSUB ambience
TIMER ON
playwav sonhello$

inte:
'Creer le truc souris
LINE (376, 101)-(474, 114), 0, BF

LINE (478, 107)-(487, 107), 12
LINE (478, 108)-(487, 108), 12
LINE (482, 103)-(482, 112), 12
LINE (483, 103)-(483, 112), 12
LINE (363, 107)-(373, 107), 12
LINE (363, 108)-(373, 108), 12

LINE (375, 100)-(360, 115), 9, B
LINE (475, 100)-(490, 115), 9, B

LINE (375, 100)-(475, 115), 9, B
LINE (475 - var4, 100)-(375, 115), 9, BF


ycurseur = 292



'LoadConfiguration
mousepointer "On"
MouseArea 108, 530, 40, 430
mousepointer "On"
mousespeed var4
curpos = -1
DO
CALL mousewait2(xbarre%, mousex%, mouseb%, curpos)
FOR t% = 1 TO 36 STEP 5
COLOR INT(14 * RND) + 1
CALL mousewait2(xbarre%, mousex%, mouseb%, curpos)
FOR x% = 1 TO 3
LOCATE x% + 4, 21 + t%: PRINT MID$(i$(x%), t%, 3)
CALL mousewait2(xbarre%, mousex%, mouseb%, curpos)
NEXT x%
CALL mousewait2(xbarre%, mousex%, mouseb%, curpos)
NEXT t%
LOOP WHILE mouseb% = 0

mousepointer "Off"

GOSUB entrer

depart:
TIMER OFF
mousepointer "Off"

'*******************************************************************************
interface = 0


life% = var2
mode = var3

'********TOUCHE DU JEU*******
'PAVE NUMERIQUE
'+ ou - quand la balle est lance:CHANGE DE NIVEAU
'Echap: SORTIR DU JEU

'******************************************************************************
'********FAIT UN TOUR A LA FIN DU PROGRAMME POUR FAIRE TES NIVEAUX*************
'*****************************************************************************


ON TIMER(7) GOSUB redessinecran
TIMER OFF

DIM SHARED z$(20)

'DESSIN NUMEROS
COLOR 2
DIM SHARED num0&(100)
DIM SHARED num1&(100)
DIM SHARED num2&(100)
DIM SHARED num3&(100)
DIM SHARED num4&(100)
DIM SHARED num5&(100)
DIM SHARED num6&(100)
DIM SHARED num7&(100)
DIM SHARED num8&(100)
DIM SHARED num9&(100)

LOCATE 5, 30
PRINT "  PATIENTEZ !!!"

'ACCELERE LA REPONSE DU CLAVIER
DEF SEG = &H40
z% = PEEK(&H6C)
WHILE z% = PEEK(&H6C): WEND     'Ceci est un programme que vous devez
OUT &H60, &HF3                  'implanter dans les votre,il ameliore
z% = PEEK(&H6C)                 'nettement la maniabilite.Il reduit
WHILE z% = PEEK(&H6C): WEND     'le temps de reponse entre 2 touches.
OUT &H60, &H0
DEF SEG



WINDOW (1, 1)-(640, 480)
PALETTE 1, 15

   

DIM SHARED objet&(100)
COLOR 12
LOCATE 10, 10
PRINT CHR$(2)
GET (73, 334)-(80, 325), objet&






COLOR 2


LOCATE 20, 20
PRINT "Lives:"
DIM SHARED life&(200)
DIM SHARED score&(200)
DIM SHARED pause&(200)
LINE (149, 160)-(201, 180), 2, B
PAINT (199, 162), 1, 2
GET (150, 163)-(200, 176), life&

CLS

LOCATE 20, 20
PRINT "Score:"
LINE (149, 160)-(201, 180), 2, B
PAINT (199, 162), 1, 2
GET (150, 163)-(200, 176), score&

CLS

LOCATE 20, 20
PRINT "PAUSE!"
LINE (149, 160)-(201, 180), 2, B
PAINT (199, 162), 1, 2
GET (150, 163)-(200, 176), pause&


CLS
LOCATE 10, 9
PRINT 0
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num0&

CLS
LOCATE 10, 9
PRINT 1
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num1&


CLS
LOCATE 10, 9
PRINT 2
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num2&

CLS
LOCATE 10, 9
PRINT 3
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num3&

CLS
LOCATE 10, 9
PRINT 4
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num4&


CLS
LOCATE 10, 9
PRINT 5
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num5&

CLS
LOCATE 10, 9
PRINT 6
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num6&

CLS
LOCATE 10, 9
PRINT 7
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num7&

CLS
LOCATE 10, 9
PRINT 8
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num8&


CLS
LOCATE 10, 9
PRINT 9
LINE (65, 340)-(84, 315), 2, B
PAINT (67, 320), 1, 2
GET (70, 337)-(82, 322), num9&

CLS

WINDOW (1, 1)-(320, 240)
DIM SHARED TABLEAU%(16, 20)


'DECLARATION DES VARIABLES




xballe! = 100
yballe! = 100
n! = 1
m! = 2

niveau% = 0
testballe% = 1
      

xbarre% = 100
ybarre% = 20
testbarre% = 1

score = 0
life% = var2

divers% = 0

GOSUB eteindrevoyant

DIM SHARED test&(200)
LINE (95, 115)-(120, 95), 1, BF
CIRCLE (107, 107), 6, 14
GET (100, 115)-(115, 100), test&

'CREATION DES MATRICES POUR LES BRIQUES
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
'DESSIN DES DIFFERENTES BARRE

PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF
LOCATE 5, 30
PRINT "  PATIENTEZ !!!"

WINDOW (1, 1)-(400, 240)
'BARRE NORMALE
DIM SHARED barre&(200)
CIRCLE (100, 100), 30, 0
LINE (50, 120)-(180, 150), 1, BF
LINE (118, 119)-(82, 119), 0
LINE (77, 114)-(125, 114), 0
PAINT (100, 116), 9, 0
GET (75, 114)-(125, 119), barre&


PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

LOCATE 5, 30
PRINT "  PATIENTEZ !!!"


'BARRE COLLE
DIM SHARED barrecolle&(300)
CIRCLE (100, 100), 30, 0
LINE (50, 120)-(180, 150), 1, BF
LINE (118, 119)-(82, 119), 0
LINE (77, 114)-(125, 114), 0
PAINT (100, 116), 10, 0
GET (75, 114)-(125, 119), barrecolle&


PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

LOCATE 5, 30
PRINT "  PATIENTEZ !!!"


'BARRE TIR
DIM SHARED barretir&(300)
CIRCLE (100, 100), 30, 0
LINE (50, 120)-(180, 150), 1, BF
LINE (118, 119)-(82, 119), 0
LINE (77, 114)-(125, 114), 0
PAINT (100, 116), 12, 0
GET (75, 114)-(125, 119), barretir&


WINDOW (1, 1)-(320, 240)
PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

LOCATE 5, 30
PRINT "  PATIENTEZ !!!"


'GRANDE BARRE
DIM SHARED grandebarre&(300)
CIRCLE (100, 100), 30, 0
LINE (50, 120)-(180, 150), 1, BF
LINE (123, 119)-(76, 119), 0
LINE (73, 114)-(127, 114), 0
PAINT (100, 116), 9, 0
GET (73, 114)-(127, 119), grandebarre&
'LINE (73, 114)-(127, 119), 14, B
'SLEEP


PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

LOCATE 5, 30
PRINT "  PATIENTEZ !!!"



'GRANDE BARRE COLLE
DIM SHARED grandebarrecolle&(300)
CIRCLE (100, 100), 30, 0
LINE (50, 120)-(180, 150), 1, BF
LINE (123, 119)-(76, 119), 0
LINE (73, 114)-(127, 114), 0
PAINT (100, 116), 10, 0
GET (73, 114)-(127, 119), grandebarrecolle&

PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

'GRANDE BARRE COLLE
DIM SHARED grandebarretir&(300)
CIRCLE (100, 100), 30, 0
LINE (50, 120)-(180, 150), 1, BF
LINE (123, 119)-(76, 119), 0
LINE (73, 114)-(127, 114), 0
PAINT (100, 116), 12, 0
GET (73, 114)-(127, 119), grandebarretir&


PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF


WINDOW (1, 1)-(640, 480)

PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

DIM SHARED balle&(100)
DIM SHARED balleff&(100)
GET (96, 96)-(104, 104), balleff&

CIRCLE (100, 100), 4, 0
PAINT (100, 100), 8, 0
LINE (97, 99)-(97, 101), 7
LINE (98, 98)-(98, 102), 7
LINE (99, 101)-(99, 103), 7
LINE (100, 103)-(100, 102), 7

GET (96, 96)-(104, 104), balle&

PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

DIM SHARED balleP&(100)
CIRCLE (100, 100), 4, 0
PAINT (100, 100), 15, 0
GET (96, 96)-(104, 104), balleP&



PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

DIM SHARED balleN&(100)
CIRCLE (100, 100), 4, 0
PAINT (100, 100), 12, 0
GET (96, 96)-(104, 104), balleN&


jeu:
PALETTE 1, 15
LINE (1, 1)-(640, 480), 1, BF

dessinecran:
WINDOW (1, 1)-(640, 480)

niveau% = niveau% + 1

GOSUB initialiseniveau

RANDOMIZE TIMER

CALL dessinebrique



testbarre% = 1
testballe% = 1
divers% = 0
divers2% = 0
divers3% = 0

programmeprincipal:
ON ERROR GOTO 500

KEY 17, CHR$(&H0) + CHR$(&H1)
    ON KEY(17) GOSUB fin
KEY(17) ON
   
KEY 18, CHR$(&H0) + CHR$(&H19)
    ON KEY(18) GOSUB pause
KEY(18) ON


LINE (1, 1)-(640, 40), 1, BF
n! = 1
m! = 2
divers% = 0

CALL affichevie(life%, divers%)
CALL affichescore(score, divers%, divers2%)
GOSUB affichetexte
CALL lancerballe(xbarre%, ybarre%, testbarre%, xballe!, yballe!, testballe%, bouge%, divers%, mousex%, mouseb%, n!, commande%, mode, m!)


'PROGRAMME PRINCIPAL
DO
FOR azertyu% = 0 TO vitesse% - bidule%
NEXT
bidule% = 0
SELECT CASE mode
CASE 0
CALL mousewait(xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%, xballe!, m!, lancer%)
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
CALL choc(xballe!, yballe!, xbarre%, ybarre%, testbarre%, n!, m!, bouge%, xpositionmatricielleballe%, ypositionmatricielleballe%, score, life%, briquerestante%, divers%, xobjet!, yobjet!, divers3%, testballe%, divers2%, vitesse%, directory$, A$)
CASE 1
CALL mousewait3(xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%)
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
CALL choc2(xballe!, yballe!, xbarre%, ybarre%, testbarre%, n!, m!, bouge%, xpositionmatricielleballe%, ypositionmatricielleballe%, score, life%, briquerestante%, divers%, xobjet!, yobjet!, divers3%, testballe%, divers2%, vitesse%, directory$, A$, lancer%)
CASE ELSE
END SELECT
CALL dessineballe(xballe!, yballe!, testballe%)
IF divers% = 5 THEN CALL restorebrique(xobjet!, yobjet! + 10)
SELECT CASE divers%
CASE 1: GOTO programmeprincipal
CASE 2: GOTO dessinecran
CASE 3
ON TIMER(7) GOSUB redessinecran
TIMER ON
divers% = 0
CASE 4
divers% = 0
CASE 5
bidule% = 1000
yobjet! = yobjet! - 3
PUT (xobjet!, yobjet!), objet&, PSET
SELECT CASE yobjet!
CASE IS < 10: divers% = 0: yobjet! = 0: LINE (xobjet!, yobjet! + 7)-(xobjet! + 7, yobjet + 18), 1, BF
CASE IS < 30
SELECT CASE testbarre%
CASE IS < 4
IF xobjet! + 4 >= xbarre% AND xobjet! <= xbarre% + 76 THEN CALL choper(testbarre%, divers%, divers2%, divers3%, xbarre%, testballe%, life%, commande%, mousex%): divers% = 0: yobjet! = 0: LINE (xbarre%, 17)-(xbarre% + 80, 50), 1, BF: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
CASE IS > 3
IF xobjet! + 4 >= xbarre% AND xobjet! <= xbarre% + 106 THEN CALL choper(testbarre%, divers%, divers2%, divers3%, xbarre%, testballe%, life%, commande%, mousex%): divers% = 0: yobjet! = 0: LINE (xbarre%, 17)-(xbarre% + 110, 50), 1, BF: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
CASE ELSE
END SELECT
CASE ELSE
END SELECT
CASE ELSE
END SELECT
IF yballe! < 33 THEN
SELECT CASE testbarre%
CASE 2
IF xballe! >= xbarre% + 7 AND xballe! <= xbarre% + 93 THEN GOTO programmeprincipal
CASE 5
IF xballe! >= xbarre% + 7 AND xballe! <= xbarre% + 124 THEN GOTO programmeprincipal
CASE ELSE
END SELECT
END IF
IF life% = 0 THEN GOTO fin
LOOP
   

nivsup:
GOTO dessinecran




pause:
      PUT (300, 4), pause&, PSET
      gh% = xbarre%
      DO
      SELECT CASE INKEY$
      CASE "+": GOTO dessinecran
      CASE "-": IF niveau% <> 1 THEN niveau% = niveau% - 2: GOTO dessinecran
      CASE ELSE
      END SELECT
      R% = Mouseinterr%(&H33, 3, mouseb%, mousex%, mousey%)
      LOOP WHILE mouseb% = 0
      mouseset gh%, 10
qsdf:
      LINE (300, 1)-(350, 15), 1, BF
      RETURN

affichetexte:
       PUT (2, 4), score&, PSET
       PUT (555, 4), life&, PSET
RETURN



fin:

IF interface = 0 THEN CALL savescore(score, directory$, A$, interface)
IF interface = 1 THEN mousepointer "Off": CALL generique
GOTO inter2

redessinecran:
WINDOW (1, 1)-(640, 480)

CALL dessinebrique


CALL affichevie(life%, divers%)
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
CALL affichescore(score, divers%, divers2%)
GOSUB affichetexte



TIMER OFF

RETURN



'INITIALISATION NIVEAUX
initialiseniveau:
IF niveau% <> 1 THEN playwav sonwin$
briquerestante% = 0
vitesse% = vitessedep%

SELECT CASE niveau%
CASE 1: RESTORE level1
CASE 2: RESTORE level2
CASE 3: RESTORE level3
CASE 4: RESTORE level4
CASE 5: RESTORE level5
CASE 6: RESTORE level6
CASE 7: RESTORE level7
CASE 8: RESTORE level8
CASE 9: RESTORE level9
CASE 10: RESTORE level10
CASE 11
CALL savescore(score, directory$, A$, interface)
CALL generique
CASE ELSE
END SELECT
CLS
IF charger% = 1 THEN GOSUB load: GOTO 102

FOR b = 1 TO 16
FOR A = 1 TO 19
101
READ TABLEAU%(b, A)
IF TABLEAU%(b, A) > 4 AND TABLEAU%(b, A) < 7 THEN briquerestante% = briquerestante% + 1
IF TABLEAU%(b, A) > 8 THEN briquerestante% = briquerestante% + 1
NEXT
NEXT
102
'LOCATE 1, 1
'PRINT "NOMBRE DE BRIQUE:"
'LOCATE 1, 18
'PRINT briquerestante%
RETURN


'GESTION DES ERREUR
100
CLS
COLOR 12
PRINT "ERROR:"; ERR
COLOR 15
PRINT "DESOLE MAIS IL Y A UN PROBLEME AVEC TA CARTE SON, POUR UN BON"
PRINT "FONCTIONNEMENT IL TE FAUT UNE CARTE SON SOUND BLASTER OU COMPATIBLE."
PRINT "TU PEUT TOUJOURS LANCER POPNOSON.BAS ,C'EST LE MEME MAIS"
PRINT "SANS LA GESTION DE LA CARTE SON."
END


200
CLS
COLOR 12
PRINT "ERREUR"; ERR
COLOR 15
PRINT "IL Y A UN PROBLEME AVEC TA SOURIS"
PRINT "ESSAI DE CHANGER DE GESTIONNAIRE SOURIS."
END



'Gestion de l'erreur
'Si le fichier de sauvegarde (directory$) n'existe pas alors il est creer
600
SELECT CASE ERR
CASE 53
Hnom01$ = "Trop Nul"
Hnom02$ = "Trou de bal"
Hnom03$ = "Face d'anus"
hscore1$ = "0000"
hscore2$ = "0000"
hscore3$ = "0000"
OPEN directory$ FOR OUTPUT AS #2
  PRINT #2, Hnom01$
  PRINT #2, hscore1$
  PRINT #2, Hnom02$
  PRINT #2, hscore2$
  PRINT #2, Hnom03$
  PRINT #2, hscore3$
CLOSE #2
RESUME
CASE ELSE
CLS
COLOR 12
PRINT "ERREUR:"; ERR
COLOR 15
END
END SELECT


400
CLS
COLOR 12
PRINT "ERREUR:"; ERR
COLOR 15
PRINT "NE TAQUINE LE REGLAGE DE LA SOURIS QUE SI NECESSAIRE"
PRINT "IL EST SOUVENT LA CAUSE DE L'ERREUR."
END


500
mouseset 100, 10
CALL lancerballe(xbarre%, ybarre%, testbarre%, xballe!, yballe!, testballe%, bouge%, divers%, mousex%, mouseb%, n!, commande%, mode, m!)
RESUME
COLOR 12
PRINT "ERREUR:"; ERR
COLOR 15
END


'ETEINDRE (Num Lock,Caps Lock,Scroll Lock)

eteindrevoyant:
DEF SEG = 0
KeyFlags = PEEK(1047)
POKE 1047, &H0
DEF SEG
RETURN



300
CLS
c$(1) = "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
c$(2) = "≥                  BIENVENUE                      ≥"
c$(3) = "√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
c$(4) = "≥ FELICITATION VOUS VENEZ D'ACQUERIR POPANOYE     ≥"
c$(5) = "≥ UN EXCELENT CASSE BRIQUE AUX MULTIPLES OPTIONS. ≥"
c$(6) = "≥ CE CASSE BRIQUE EST ENTIEREMENT PROGRAMMER      ≥"
c$(7) = "≥ EN QBASIC V1.1.                                 ≥"
c$(8) = "≥ IL COMPORTE UN GESTIONNAIRE DE SOURIS AINSI     ≥"
c$(9) = "≥ QU'UN GESTIONNAIRE SOUND BLASTER.               ≥"
c$(10) = "≥ CE JEU EST LE TROISIEME QUE JE FABRIQUE MON     ≥"
c$(11) = "≥ 1ER ETANT EGALEMENT UN CASSE BRIQUE NOMMEE      ≥"
c$(12) = "≥ BANANOID ET UN LABYRINTHE 3D NOMMEE LABY3D      ≥"
c$(13) = "≥                                                 ≥"
c$(14) = "≥ POPANOYE UN JEU DE COMBES BERTRAND.             ≥"
c$(15) = "≥ JE REMERCIE JULIEN PERROT AUTEUR DES            ≥"
c$(16) = "≥ GRAPHISMES DE LABY3D AINSI QUE L'AIDE           ≥"
c$(17) = "≥ QU'IL A PU M'APPORTER                           ≥"
c$(18) = "≥                                                 ≥"
c$(19) = "≥ Ou nous contacter:                              ≥"
c$(20) = "≥ email: BER1981@AOL.COM                          ≥"
c$(21) = "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"

FOR A = 1 TO 21
center 4 + A, c$(A)
NEXT

SLEEP


'creation de fichier de sauvegarde
OPEN directory2$ FOR OUTPUT AS #2
     PRINT #2, vitesse%
     PRINT #2, 10
     PRINT #2, 0
     PRINT #2, 17         'Sensibilite
CLOSE #2
RESUME
END


vit:
TIMER OFF
GOTO vitesse
END


'*********************************************************************************

entrer:

SELECT CASE curpos
CASE -1: GOTO inte
CASE 0: playwav sonoui$: GOTO depart
CASE 1
playwav sonsouris$
COLOR 14
LOCATE 16, 43
INPUT "VITESSE CHOISIT: ", var1
LOCATE 18, 43
INPUT "DIFFICULTE (1,2,3) : ", var2
var2 = var2 * 5
LOCATE 18, 56
SELECT CASE var2
CASE 5: PRINT " HARD"
CASE 10: PRINT " NORMAL"
CASE 15: PRINT " EASY"
CASE ELSE
END SELECT
GOTO enregistre
CASE 2
TIMER OFF
playwav sonsouris$
CLS
CALL savescore(score, directory$, A$, interface)
TIMER ON
GOTO inter
CASE 3: CHAIN "c:\popanoye\editeur.bas": TIMER OFF: playwav sonnon$: COLOR 12: LOCATE 23, 43: PRINT "DESOLE C'EST POUR": LOCATE 24, 43: PRINT "VERSION 1.2 SEULEMENT": SLEEP 2: TIMER ON: GOTO inter
CASE 4
playwav sonsouris$
LOCATE 20, 48
SELECT CASE var3
CASE 1: PRINT "POP CORN": var3 = 0: mode = 0: GOTO enregistre
CASE 0: PRINT "ARKANOID": var3 = 1: mode = 1: GOTO enregistre
CASE ELSE
END SELECT

CASE 5: playwav sonsouris$: CALL generique
CASE 6
playwav sonsouris$
kaka = 1
IF var4 < 99 THEN var4 = var4 + 1
mousespeed var4
GOTO enregistre
CASE 7:
playwav sonsouris$
kaka = 1
IF var4 > 1 THEN var4 = var4 - 1
mousespeed var4
GOTO enregistre
CASE 8     'charger niveaux
charger% = 1
GOTO depart
'GOTO load
CASE ELSE
END SELECT

END
GOTO inter
RETURN


enregistre:

OPEN directory2$ FOR OUTPUT AS #2
     PRINT #2, var1
     PRINT #2, var2
     PRINT #2, var3
     PRINT #2, var4
     CLOSE #2
GOTO inter2

ambience:
playwav sonambience$
RETURN

load:
CLS
LOCATE 1, 1
PRINT "LISTE DES NIVEAUX"
FILES "c:\popanoye\niveaux\*.niv"
COLOR 14
KEY(17) OFF
KEY(18) OFF
PRINT "NE TAPE RIEN SI TU NE VEUT RIEN CHARGER"
INPUT "QUELLE FICHIER CHOISI TU ?: ", fichier$
IF LEN(fichier$) = 0 THEN END
fichier$ = "c:\popanoye\niveaux\" + fichier$ + ".niv"
ON ERROR GOTO loaderreur
OPEN fichier$ FOR INPUT AS #1
  FOR b% = 1 TO 15
  FOR A% = 1 TO 19
  INPUT #1, c%
  TABLEAU%(b%, A%) = c%
  IF TABLEAU%(b, A) > 4 AND TABLEAU%(b, A) < 7 THEN briquerestante% = briquerestante% + 1
  IF TABLEAU%(b, A) > 8 THEN briquerestante% = briquerestante% + 1
  NEXT
  NEXT
CLOSE #1
GOSUB eteindrevoyant
KEY(17) ON
KEY(18) ON
ON ERROR GOTO 500

RETURN

loaderreur:
CLS
COLOR 12
center 15, "OH! CE FICHIER N'EXISTE PAS"
END

'******************************************************************************

'-------------------------------------------------------------------------------
'-------------------------------------NIVEAUX----------------------------------
'----------------------------------------------------------------------------------

'IL EST TRES FACILE
'D'EN FAIRE SOIT MEME
'VOICI LES CODE DES BRIQUES ET LEUR ACTION
' ⁄ƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
' ≥  COULEUR   ≥ CODE ≥                CARACTERISTIQUE                   ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥  violet    ≥   5  ≥ RIEN                                             ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥  marron    ≥   6  ≥ DEVIENNE VIOLETE                                 ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥ gris clair ≥   7  ≥ INCASSABLE                                       ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥ gris fonce ≥   8  ≥ INCASSABLE                                       ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥   bleu     ≥   9  ≥ RIEN                                             ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥   vert     ≥  10  ≥ RIEN                                             ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥ bleu ciel  ≥  11  ≥ UN OBJET TOMBE DU CIEL                           ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥   rouge    ≥  12  ≥ DEVIENNE MARRON                                  ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥  violet+   ≥  13  ≥ INVERSE LES TOUCHE DE DIRECTION                  ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥   jaune    ≥  14  ≥ RENDE INVISIBLE TOUTES LES BRIQUES PENDANT 15s   ≥
' √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
' ≥   blanc    ≥  15  ≥ LA BALLE CHOISIT SA DIRECTION ALEATOIREMENT      ≥
' ¿ƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ

'NE CHANGE PAS LA TAILLE DES MATRICE 16*19
'IL TE SUFFIT DE REMPLIR LES MATRICE PAR L'ACTION VOULU (cf TBLEAU)

'CONSEIL : LAISSE LES 5 DERNIERES LIGNES DE LA MATRICE VIDE
'ESSAILLE TU NE RISQUE RIEN MAIS LE RENDU EST NUL

'***********************************NIVEAUX 1er********************************

level1:
DATA 5,0,0,0,5,5,5,0,5,0,5,0,5,5,5,0,5,0,0
DATA 5,0,0,0,5,0,0,0,5,0,5,0,5,0,0,0,5,0,0
DATA 5,0,0,0,5,5,0,0,5,0,5,0,5,5,0,0,5,0,0
DATA 5,0,0,0,5,0,0,0,5,0,5,0,5,0,0,0,5,0,0
DATA 5,5,5,0,5,5,5,0,0,5,0,0,5,5,5,0,5,5,5
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,11,11,0,0,0,0,0,0,0,0,0,0
DATA 0,5,0,0,0,5,0,11,11,0,0,0,0,5,0,0,0,0,0
DATA 0,5,5,0,0,5,0,0,0,0,0,0,5,5,0,0,0,0,0
DATA 0,5,0,5,0,5,0,0,0,0,0,5,0,5,0,0,0,0,0
DATA 0,5,0,0,5,5,0,0,0,0,0,0,0,5,0,0,0,0,0
DATA 0,5,0,0,0,5,0,0,0,0,0,0,5,5,5,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level2:
DATA 0,0,0,0,0,0,0,0,11,11,11,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,5,5,5,5,5,0,0,6,0,0,0,0,6,0,0,0
DATA 0,0,0,5,15,15,15,5,11,11,6,0,0,0,6,0,0,0,0
DATA 0,0,0,5,15,14,15,5,11,11,6,0,0,6,0,0,0,0,0
DATA 0,0,0,5,15,15,15,5,11,11,6,0,6,0,0,0,0,0,0
DATA 0,0,0,5,11,12,11,13,11,11,6,11,0,0,0,0,0,0,0
DATA 0,0,0,5,15,15,15,5,11,11,6,0,6,0,0,0,0,0,0
DATA 0,0,0,5,15,15,15,5,11,11,6,0,0,6,0,0,0,0,0
DATA 0,0,0,5,15,15,15,5,11,11,6,0,0,0,6,0,0,0,0
DATA 0,0,0,5,5,5,5,5,11,11,6,0,0,0,0,6,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level3:
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,10,0,0,0,10,0,9,12,12,12,9,0,11,0,0,0,11
DATA 0,0,10,9,0,9,10,0,9,0,0,0,9,0,11,11,0,0,11
DATA 0,0,10,0,9,0,10,0,9,0,0,0,9,0,11,0,11,0,11
DATA 0,0,10,0,0,0,10,0,9,0,0,0,9,0,11,0,11,0,11
DATA 0,0,10,0,0,0,10,0,9,11,11,11,9,0,11,0,11,0,11
DATA 0,0,10,0,0,0,10,0,9,0,0,0,9,0,11,0,11,0,11
DATA 0,0,10,0,0,0,10,0,9,0,0,0,9,0,11,0,0,11,11
DATA 0,0,10,0,0,0,10,0,9,0,0,0,9,0,11,0,0,0,11
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level4:
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,5,5,5,0,0,0,0,0,6,0,0,0,0,0,6,0,0
DATA 0,0,5,0,0,5,0,0,0,0,0,6,0,0,0,6,0,0,0
DATA 0,0,5,0,0,5,0,0,0,0,0,0,6,0,6,0,0,0,0
DATA 0,0,13,5,5,0,0,0,0,0,0,0,0,6,0,0,0,0,0
DATA 0,0,5,0,0,5,0,0,0,0,0,0,6,0,0,0,0,0,0
DATA 0,0,5,0,0,5,0,0,0,0,0,6,0,0,0,0,0,0,0
DATA 0,0,5,0,0,5,0,0,0,0,6,0,0,0,0,0,0,0,0
DATA 0,0,5,5,5,5,0,0,0,6,0,0,0,0,0,0,0,0,0
DATA 15,15,15,15,15,15,15,15,0,0,15,15,15,15,15,15,15,15,15
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level5:
DATA ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 10,10,0,10,10,10,0,10,0,10,0,10,10,10,0,10,10,10,0
DATA 10,0,0,10,11,10,0,10,10,10,0,10,0,10,0,10,0,0,0
DATA 10,0,0,10,11,10,0,10,14,10,0,10,10,0,0,10,10,10,0
DATA 10,0,0,10,11,10,0,10,14,10,0,10,0,10,0,10,0,0,0
DATA 10,10,0,10,10,10,0,10,14,10,0,10,10,10,0,10,10,10,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,9,11,9,0,9,0,9,0,0,10,10,10,0,0
DATA 0,0,5,0,0,9,11,9,0,9,9,9,0,0,10,13,14,0,0
DATA 0,9,9,9,0,9,11,9,0,9,9,9,0,0,10,10,10,0,0
DATA 0,0,9,0,0,9,11,9,0,9,9,9,0,0,14,14,10,0,0
DATA 0,0,0,0,0,9,9,9,0,9,0,9,0,0,10,10,10,0,0
DATA 8,8,8,8,8,12,12,12,12,12,12,12,12,12,8,8,8,8,8
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level6:
DATA 5,0,10,0,0,10,0,0,10,0,0,10,0,0,10,0,0,0,5
DATA 11,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,11
DATA 11,11,5,0,10,0,0,10,0,0,10,0,0,10,0,0,5,11,11
DATA 11,11,11,5,0,0,0,0,0,0,0,0,0,0,0,5,11,11,11
DATA 11,11,11,11,5,0,10,0,10,0,10,0,10,0,5,11,11,11,11
DATA 11,11,11,11,11,7,7,7,7,14,8,8,8,8,11,11,11,11,11
DATA 11,11,11,11,5,15,15,15,15,15,15,15,15,15,5,11,11,11,11
DATA 11,11,11,5,15,15,15,15,15,15,15,15,15,15,15,5,11,11,11
DATA 11,11,5,15,15,15,15,15,15,15,15,15,15,15,15,15,5,11,11
DATA 11,5,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,5,11
DATA 5,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,5
DATA 12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level7:

DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,6,0,6,0,12,12,0,9,9,9,0,11,0,0,0,11,0,0
DATA 0,6,0,6,0,12,0,0,9,0,9,0,0,11,0,11,0,0,0
DATA 0,6,0,6,0,12,12,0,9,9,9,0,0,0,11,0,0,0,0
DATA 0,6,0,6,0,12,0,0,9,9,0,0,0,11,0,0,0,0,0
DATA 0,0,6,0,0,12,12,0,9,0,9,0,11,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,13,5,5,5,0,10,10,10,0,15,15,15,0,14,7,0,0,0
DATA 0,5,0,0,0,0,10,0,10,0,15,0,15,0,7,0,7,0,0
DATA 0,5,0,5,5,0,10,0,10,0,15,0,15,0,7,0,7,0,0
DATA 0,5,0,0,5,0,10,0,10,0,15,0,15,0,7,0,7,0,0
DATA 0,5,5,5,5,0,10,10,10,0,15,15,15,0,7,7,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level8:
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 11,12,11,12,11,12,12,12,11,12,12,12,11,12,12,12,11,11,11
DATA 11,12,12,12,11,12,11,12,11,12,11,12,11,12,11,11,11,11,11
DATA 11,12,11,12,11,12,11,12,11,12,12,12,11,12,12,11,11,11,11
DATA 11,12,11,12,11,12,11,12,11,12,12,11,11,12,11,11,11,11,11
DATA 11,12,11,12,11,12,12,12,11,12,11,12,11,12,12,12,11,11,11
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 11,11,11,12,12,12,11,12,11,11,11,12,11,11,11,11,11,11,11
DATA 11,11,11,12,11,12,11,12,11,11,11,12,11,11,11,11,11,11,11
DATA 11,11,11,12,12,12,11,12,11,11,11,12,11,11,11,11,11,11,11
DATA 11,11,11,12,11,12,11,12,11,11,11,12,11,11,11,11,11,11,11
DATA 11,11,11,12,11,12,11,12,12,12,11,12,12,12,11,11,11,11,11
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level9:
DATA 6,0,5,0,9,0,10,0,6,0,5,0,9,0,10,0,6,0,5
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,5,0,9,0,10,0,6,0,5,0,9,0,10,0,6,0,5,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 11,0,12,0,12,0,11,0,12,0,12,0,11,0,12,0,12,0,11
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,13,0,5,0,5,0,5,0,13,0,5,0,5,0,5,0,13,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 14,0,12,0,12,0,12,0,14,0,12,0,12,0,12,0,12,0,14
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,5,0,6,0,9,0,10,0,5,0,6,0,9,0,10,0,5,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 12,0,12,0,12,0,12,0,12,0,12,0,12,0,12,0,12,0,12
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

level10:
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,11,11,11,0,11,0,11,0,0,11,11,11,0,0,0,0,0,0
DATA 0,0,11,0,0,11,0,11,0,0,11,0,0,0,0,0,0,0,0
DATA 0,0,11,0,0,11,11,11,0,0,11,11,0,0,0,0,0,0,0
DATA 0,0,11,0,0,11,0,11,0,0,11,0,0,0,0,0,0,0,0
DATA 0,0,11,0,0,11,0,11,0,0,11,11,11,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,11,11,11,0,11,0,0,0,11,0,11,11,0,0,0,0
DATA 0,0,0,11,0,0,0,11,11,0,0,11,0,11,0,11,0,0,0
DATA 0,0,0,11,11,0,0,11,0,11,0,11,0,11,0,11,0,0,0
DATA 0,0,0,11,0,0,0,11,0,11,11,11,0,11,0,11,0,0,0
DATA 0,0,0,11,11,11,0,11,0,0,0,11,0,11,11,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

DATA.Mouse:
DATA 55,8b,ec,56,57,8b,76,0c,8b,04,8b,76,0a,8b,1c,8b,76,08,8b,0c,8b,76,06,8b
DATA 14,cd,21,8b,76,0c,89,04,8b,76,0a,89,1c,8b,76,08,89,0c,8b,76,06,89,14,5f
DATA 5e,5d,ca,08,00

SUB affichehighscore (hnom1$, hsco1$, hnom2$, hsco2$, hnom3$, hsco3$, interface)

z$(1) = "À À  ÕÀÕ  …ÕÕ  À À       …ÕÕ  …ÕÕ  …Õª  …Õª  …ÕÕ  …ÕÕ"
z$(2) = "ÃÕπ   ∫   ∫…ª  ÃÕπ       »Õª  ∫    ∫ ∫  ÃÀº  ÃÕ   »Õª"
z$(3) = "     Õ Õ  »Õº            ÕÕº  »ÕÕ  »Õº   »Õ  »ÕÕ  ÕÕº"


z$(4) = "⁄ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
z$(5) = "≥ PLACE  ≥     DES BOSS       ≥  ET LEUR SCORE  ≥"
z$(6) = "√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
z$(7) = "≥  1er   ≥                    ≥                 ≥"
z$(8) = "√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
z$(9) = " ≥  2eme  ≥                    ≥                 ≥"
z$(10) = "√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
z$(11) = "≥  3eme  ≥                    ≥                 ≥"
z$(12) = "¿ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"


'⁄ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
'≥ PLACE  ≥     DES BOSS       ≥  ET LEUR SCORE  ≥
'√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
'≥  1er   ≥                    ≥                 ≥
'√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
'≥  2eme  ≥                    ≥                 ≥
'√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
'≥  3eme  ≥                    ≥                 ≥
'√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
'≥  4eme  ≥                    ≥                 ≥
'√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
'≥  5eme  ≥                    ≥                 ≥
'¿ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ




RANDOMIZE TIMER

FOR t% = 1 TO 53 STEP 5
COLOR INT(14 * RND) + 1
FOR x% = 1 TO 3
LOCATE x%, 12 + t%: PRINT MID$(z$(x%), t%, 3)
NEXT x%, t%

FOR t = 1 TO 9
COLOR 9
center 7 + t, z$(3 + t)
NEXT

LOCATE 11, 27 + (18 - LEN(hnom1$)) / 2
PRINT UCASE$(hnom1$)
LOCATE 13, 27 + (18 - LEN(hnom2$)) / 2
PRINT UCASE$(hnom2$)
LOCATE 15, 27 + (18 - LEN(hnom3$)) / 2
PRINT UCASE$(hnom3$)

LOCATE 11, 53
PRINT hsco1$
LOCATE 13, 53
PRINT hsco2$
LOCATE 15, 53
PRINT hsco3$
IF interface = 1 THEN EXIT SUB

END SUB

SUB affichescore (score, divers%, divers2%)
milier% = INT(score / 1000)
centaine% = INT((score - milier% * 1000) / 100)
dizaine% = INT((score - centaine% * 100 - milier% * 1000) / 10)
unite% = INT(score - centaine% * 100 - milier% * 1000 - dizaine% * 10)
SELECT CASE milier%
CASE 0: PUT (65, 4), num0&, PSET
CASE 1: PUT (65, 4), num1&, PSET
CASE 2: PUT (65, 4), num2&, PSET
CASE 3: PUT (65, 4), num3&, PSET
CASE 4: PUT (65, 4), num4&, PSET
CASE 5: PUT (65, 4), num5&, PSET
CASE 6: PUT (65, 4), num6&, PSET
CASE 7: PUT (65, 4), num7&, PSET
CASE 8: PUT (65, 4), num8&, PSET
CASE 9: PUT (65, 4), num9&, PSET
CASE ELSE
END SELECT
SELECT CASE centaine%
CASE 0: PUT (75, 4), num0&, PSET
CASE 1: PUT (75, 4), num1&, PSET
CASE 2: PUT (75, 4), num2&, PSET
CASE 3: PUT (75, 4), num3&, PSET
CASE 4: PUT (75, 4), num4&, PSET
CASE 5: PUT (75, 4), num5&, PSET
CASE 6: PUT (75, 4), num6&, PSET
CASE 7: PUT (75, 4), num7&, PSET
CASE 8: PUT (75, 4), num8&, PSET
CASE 9: PUT (75, 4), num9&, PSET
CASE ELSE
END SELECT
SELECT CASE dizaine%
CASE 0: PUT (85, 4), num0&, PSET
CASE 1: PUT (85, 4), num1&, PSET
CASE 2: PUT (85, 4), num2&, PSET
CASE 3: PUT (85, 4), num3&, PSET
CASE 4: PUT (85, 4), num4&, PSET
CASE 5: PUT (85, 4), num5&, PSET
CASE 6: PUT (85, 4), num6&, PSET
CASE 7: PUT (85, 4), num7&, PSET
CASE 8: PUT (85, 4), num8&, PSET
CASE 9: PUT (85, 4), num9&, PSET
CASE ELSE
END SELECT
SELECT CASE unite%
CASE 0: PUT (95, 4), num0&, PSET
CASE 1: PUT (95, 4), num1&, PSET
CASE 2: PUT (95, 4), num2&, PSET
CASE 3: PUT (95, 4), num3&, PSET
CASE 4: PUT (95, 4), num4&, PSET
CASE 5: PUT (95, 4), num5&, PSET
CASE 6: PUT (95, 4), num6&, PSET
CASE 7: PUT (95, 4), num7&, PSET
CASE 8: PUT (95, 4), num8&, PSET
CASE 9: PUT (95, 4), num9&, PSET
CASE ELSE
END SELECT
END SUB

SUB affichevie (life%, divers%)

Dix = INT(life% / 10)
unit = life% - Dix * 10

SELECT CASE Dix
CASE 0: PUT (617, 4), num0&, PSET
CASE 1: PUT (617, 4), num1&, PSET
CASE 2: PUT (617, 4), num2&, PSET
CASE 3: PUT (617, 4), num3&, PSET
CASE 4: PUT (617, 4), num4&, PSET
CASE 5: PUT (617, 4), num5&, PSET
CASE 6: PUT (617, 4), num6&, PSET
CASE 7: PUT (617, 4), num7&, PSET
CASE 8: PUT (617, 4), num8&, PSET
CASE 9: PUT (617, 4), num9&, PSET
CASE ELSE
END SELECT

SELECT CASE unit
CASE 0: PUT (627, 4), num0&, PSET
CASE 1: PUT (627, 4), num1&, PSET
CASE 2: PUT (627, 4), num2&, PSET
CASE 3: PUT (627, 4), num3&, PSET
CASE 4: PUT (627, 4), num4&, PSET
CASE 5: PUT (627, 4), num5&, PSET
CASE 6: PUT (627, 4), num6&, PSET
CASE 7: PUT (627, 4), num7&, PSET
CASE 8: PUT (627, 4), num8&, PSET
CASE 9: PUT (627, 4), num9&, PSET
CASE ELSE
END SELECT

END SUB

SUB center (ROW!, text$)
LOCATE ROW!, 40 - LEN(text$) / 2
PRINT text$;
END SUB

SUB choc (xballe!, yballe!, xbarre%, ybarre%, testbarre%, n!, m!, bouge%, xpositionmatricielleballe%, ypositionmatricielleballe%, score, life%, briquerestante%, divers%, xobjet!, yobjet!, divers3%, testballe%, divers2%, vitesse%, directory$, A$)


IF divers3% = 1 THEN
c = INT(80 * RND) + 1
IF c = 7 THEN n! = -n!
END IF

IF xballe! > 625 THEN n! = -n!: playwav sonmur$: LINE (xballe!, yballe!)-(xballe! + 8, yballe! + 8), 1, BF: xballe! = 625
IF xballe! < 5 THEN n! = -n!: playwav sonmur$: LINE (xballe!, yballe!)-(xballe! + 8, yballe! + 8), 1, BF: xballe! = 5

SELECT CASE yballe!
CASE IS > 463
m! = -m!
playwav sonmur$
CASE IS < 15
life% = life% - 1
playwav sonperdu$
IF life% = 0 THEN
EXIT SUB
END IF

testbarre% = 1
testballe% = 1
divers3% = 0
divers2% = 0
divers% = 1
n! = 1
m! = 2

GOTO 10

CASE IS < 32
SELECT CASE testbarre%
CASE IS < 4
IF xballe! + 4 >= xbarre% AND xballe! <= xbarre% + 6 AND n! > 0 THEN n! = -n!: LINE (1, 18)-(640, 50), 1, BF: playwav sonrecup$
IF xballe! >= xbarre% + 66 AND xballe! <= xbarre% + 76 THEN IF n! < 0 THEN n! = -n!: LINE (1, 18)-(640, 50), 1, BF: playwav sonrecup$
IF xballe! + 4 >= xbarre% AND xballe! <= xbarre% + 76 THEN m! = -m!: playwav sonrecup$: yballe! = 32: : LINE (1, 18)-(640, 50), 1, BF: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): IF testbarre% = 3 OR testbarre% = 6 THEN c = INT(2 * RND) + 1: IF c = 1 THEN n! = -n!: SOUND 2000, .3
IF vitesse% > 26 THEN vitesse% = vitesse% - 25

CASE IS > 3
IF xballe! + 4 >= xbarre% AND xballe! <= xbarre% + 6 AND n! > 0 THEN n! = -n!: LINE (1, 18)-(640, 50), 1, BF: playwav sonrecup$
IF xballe! >= xbarre% + 96 AND xballe! <= xbarre% + 106 AND n! < 0 THEN n! = -n!: LINE (1, 18)-(640, 50), 1, BF: playwav sonrecup$
IF xballe! + 4 >= xbarre% AND xballe! <= xbarre% + 106 THEN m! = -m!: playwav sonrecup$: yballe! = 32: : LINE (1, 18)-(640, 50), 1, BF: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): IF testbarre% = 3 OR testbarre% = 6 THEN c = INT(2 * RND) + 1: IF c = 1 THEN n! = -n!: SOUND 2000, .3
IF vitesse% > 26 THEN vitesse% = vitesse% - 25

CASE ELSE
END SELECT

CASE ELSE
END SELECT

IF TABLEAU%(15 - (INT((yballe! - 2) / 31)), INT((xballe! + 11) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 11) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
m! = -m!
IF testballe% = 2 THEN m! = -m!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2) / 31)), INT((xballe! + 11) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2) / 31)), INT((xballe! + 11) / 31)) = 8 THEN m! = -m!
END IF

IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 17) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 17) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2 - 6) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
n! = -n!
IF testballe% = 2 THEN n! = -n!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 17) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 17) / 31)) = 8 THEN n! = -n!
END IF

IF TABLEAU%(15 - (INT((yballe! - 2 - 12) / 31)), INT((xballe! + 11) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 11) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2 - 12) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
m! = -m!
IF testballe% = 2 THEN m! = -m!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2 - 12) / 31)), INT((xballe! + 11) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2 - 12) / 31)), INT((xballe! + 11) / 31)) = 8 THEN m! = -m!
END IF

IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 5) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 5) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2 - 6) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
n! = -n!
IF testballe% = 2 THEN n! = -n!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 5) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 5) / 31)) = 8 THEN n! = -n!
END IF

bouge% = 0
LINE (xballe!, yballe!)-(xballe! + 8, yballe! + 8), 1, BF
xballe! = xballe! + n!
yballe! = yballe! + m!
10
END SUB

SUB choc2 (xballe!, yballe!, xbarre%, ybarre%, testbarre%, n!, m!, bouge%, xpositionmatricielleballe%, ypositionmatricielleballe%, score, life%, briquerestante%, divers%, xobjet!, yobjet!, divers3%, testballe%, divers2%, vitesse%, directory$, A$, lancer%)

IF divers3% = 1 THEN
c = INT(80 * RND) + 1
IF c = 7 THEN n! = -n!
END IF

IF xballe! > 625 THEN n! = -n!: playwav sonmur$: LINE (xballe!, yballe!)-(xballe! + 8, yballe! + 8), 1, BF: xballe! = 625
IF xballe! < 5 THEN n! = -n!: playwav sonmur$: LINE (xballe!, yballe!)-(xballe! + 8, yballe! + 8), 1, BF: xballe! = 5

SELECT CASE yballe!
CASE IS > 463
m! = -m!: playwav sonmur$
CASE IS < 15
life% = life% - 1
playwav sonperdu$
IF life% = 0 THEN
CALL savescore(score, directory$, A$, interface)
CALL generique
END IF

testbarre% = 1
testballe% = 1
divers3% = 0
divers2% = 0
divers% = 1
n! = 1
m! = 2

GOTO 11

CASE IS < 32
SELECT CASE testbarre%
CASE IS < 4
IF xballe! + 4 >= xbarre% AND xballe! + 4 <= xbarre% + 80 THEN
m! = -m!
playwav sonrecup$
yballe! = 32
LINE (1, 18)-(640, 50), 1, BF
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
IF testbarre% = 3 THEN c = INT(2 * RND) + 1
IF c = 1 THEN n! = -n!
                                                                              
n! = -((xbarre% + 40) - (xballe! + 4)) / 10
IF n! > 3 THEN n! = 3
IF n! < -3 THEN n! = -3


IF vitesse% > 26 THEN vitesse% = vitesse% - 25
END IF

CASE IS > 3

IF xballe! + 4 >= xbarre% AND xballe! + 4 <= xbarre% + 110 THEN
m! = -m!
playwav sonrecup$
yballe! = 32
LINE (1, 18)-(640, 50), 1, BF
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
IF testbarre% = 3 THEN c = INT(2 * RND) + 1
IF c = 1 THEN n! = -n!
                                                                              
n! = -((xbarre% + 55) - (xballe! + 4)) / 15
IF n! > 3 THEN n! = 3
IF n! < -3 THEN n! = -3

IF vitesse% > 26 THEN vitesse% = vitesse% - 25
END IF
CASE ELSE
END SELECT
CASE ELSE
END SELECT

IF TABLEAU%(15 - (INT((yballe! - 2) / 31)), INT((xballe! + 6 + 5) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 6 + 5) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
m! = -m!
IF testballe% = 2 THEN m! = -m!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2) / 31)), INT((xballe! + 6 + 5) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2) / 31)), INT((xballe! + 6 + 5) / 31)) = 8 THEN m! = -m!
END IF

IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 12 + 5) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 12 + 5) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2 - 6) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
n! = -n!
IF testballe% = 2 THEN n! = -n!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 12 + 5) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 12 + 5) / 31)) = 8 THEN n! = -n!
END IF

IF TABLEAU%(15 - (INT((yballe! - 2 - 12) / 31)), INT((xballe! + 6 + 5) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 6 + 5) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2 - 12) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
m! = -m!
IF testballe% = 2 THEN m! = -m!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2 - 12) / 31)), INT((xballe! + 6 + 5) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2 - 12) / 31)), INT((xballe! + 6 + 5) / 31)) = 8 THEN m! = -m!
END IF

IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 5) / 31)) > 4 THEN
xpositionmatricielleballe% = INT((xballe! + 5) / 31)
ypositionmatricielleballe% = 16 - (INT((yballe! - 2 - 6) / 31))
CALL effacebrique(xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)
n! = -n!
IF testballe% = 2 THEN n! = -n!
IF testballe% = 2 THEN IF TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 5) / 31)) = 7 OR TABLEAU%(15 - (INT((yballe! - 2 - 6) / 31)), INT((xballe! + 5) / 31)) = 8 THEN n! = -n!
END IF

LINE (xballe!, yballe!)-(xballe! + 8, yballe! + 8), 1, BF
xballe! = xballe! + n!
yballe! = yballe! + m!
bouge% = 0
11
END SUB

SUB choper (testbarre%, divers%, divers2%, divers3%, xbarre%, testballe%, life%, commande%, mousex%)

c = INT(14 * RND) + 1

SELECT CASE c
CASE 1: testbarre% = 1: LINE (1, 20)-(640, 30), 1, BF: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): MouseArea2 (testbarre%)
CASE 2: testbarre% = 2: LINE (1, 20)-(640, 30), 1, BF: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): MouseArea2 (testbarre%)
CASE 3: testbarre% = 3: LINE (1, 20)-(640, 30), 1, BF: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): MouseArea2 (testbarre%)
CASE 4: testbarre% = 4: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): MouseArea2 (testbarre%)
CASE 5: testbarre% = 5: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): MouseArea2 (testbarre%)
CASE 6: testbarre% = 6: CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%): MouseArea2 (testbarre%)
CASE 7: divers3% = 0: testballe% = 1
CASE 8: divers3% = 1: testballe% = 3
CASE 9: divers3% = 2: testballe% = 2
CASE 10: life% = life% + 1: CALL affichevie(life%, divers%)
CASE 11
LINE (1, 480)-(640, 1), 1, BF
PUT (2, 4), score&, PSET
PUT (555, 4), life&, PSET
CALL affichescore(score, divers%, divers2%)
CALL affichevie(life%, divers%)
divers2% = 3
TIMER ON
CASE 12
IF commande% = 1 THEN GOTO 123
 commande% = 1

SELECT CASE testbarre%
CASE IS < 4: mouseset 531 - xbarre%, 1
CASE IS > 3: mouseset 493 - xbarre%, 1
CASE ELSE
END SELECT

CALL mousewait(xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%, xballe!, m!, lancer%)
LINE (1, 20)-(640, 50), 1, BF
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
123

CASE 13

WINDOW SCREEN (1, 1)-(640, 480)
LINE (1, 1)-(640, 480), 1, BF
CALL dessinebrique
CALL affichevie(life%, divers%)
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
CALL affichescore(score, divers%, divers2%)
PUT (2, 4), score&, PSET
PUT (555, 4), life&, PSET
TIMER ON

CASE 14
IF commande% = 0 THEN GOTO 1234
commande% = 0

CALL mousewait(xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%, xballe!, m!, lancer%)

SELECT CASE testbarre%
CASE IS < 4: mouseset 531 - xbarre%, 1
CASE IS > 3: mouseset 493 - xbarre%, 1
CASE ELSE
END SELECT

LINE (1, 20)-(640, 50), 1, BF
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
1234

CASE ELSE
END SELECT


playwav sonchoper$

END SUB

SUB decompose (score, A$)

A$ = ""

milier = INT(score / 1000)
centaine = INT((score - (milier * 1000)) / 100)
dizaine = INT((score - ((centaine * 100) + (milier * 1000))) / 10)
unite = INT(score - ((centaine * 100) + (milier * 1000) + (dizaine * 10)))

SELECT CASE milier
CASE 0: A$ = A$ + "0"
CASE 1: A$ = A$ + "1"
CASE 2: A$ = A$ + "2"
CASE 3: A$ = A$ + "3"
CASE 4: A$ = A$ + "4"
CASE 5: A$ = A$ + "5"
CASE 6: A$ = A$ + "6"
CASE 7: A$ = A$ + "7"
CASE 8: A$ = A$ + "8"
CASE 9: A$ = A$ + "9"
CASE ELSE
END SELECT

SELECT CASE centaine
CASE 0: A$ = A$ + "0"
CASE 1: A$ = A$ + "1"
CASE 2: A$ = A$ + "2"
CASE 3: A$ = A$ + "3"
CASE 4: A$ = A$ + "4"
CASE 5: A$ = A$ + "5"
CASE 6: A$ = A$ + "6"
CASE 7: A$ = A$ + "7"
CASE 8: A$ = A$ + "8"
CASE 9: A$ = A$ + "9"
CASE ELSE
END SELECT

SELECT CASE dizaine
CASE 0: A$ = A$ + "0"
CASE 1: A$ = A$ + "1"
CASE 2: A$ = A$ + "2"
CASE 3: A$ = A$ + "3"
CASE 4: A$ = A$ + "4"
CASE 5: A$ = A$ + "5"
CASE 6: A$ = A$ + "6"
CASE 7: A$ = A$ + "7"
CASE 8: A$ = A$ + "8"
CASE 9: A$ = A$ + "9"
CASE ELSE
END SELECT

SELECT CASE unite
CASE 0: A$ = A$ + "0"
CASE 1: A$ = A$ + "1"
CASE 2: A$ = A$ + "2"
CASE 3: A$ = A$ + "3"
CASE 4: A$ = A$ + "4"
CASE 5: A$ = A$ + "5"
CASE 6: A$ = A$ + "6"
CASE 7: A$ = A$ + "7"
CASE 8: A$ = A$ + "8"
CASE 9: A$ = A$ + "9"
CASE ELSE
END SELECT


END SUB

SUB defiler (aze!, azr!)
FOR aze = 1 TO 2
FOR azr = 1 TO 400
NEXT
PRINT " "
NEXT
END SUB

SUB dessineballe (xballe!, yballe!, testballe%)
SELECT CASE testballe%
CASE 1: PUT (xballe!, yballe!), balle&, PSET
CASE 2: PUT (xballe!, yballe!), balleP&, PSET
CASE 3: PUT (xballe!, yballe!), balleN&, PSET
CASE ELSE
END SELECT
END SUB

SUB dessinebarre (xbarre%, ybarre%, testbarre%, divers%)

SELECT CASE testbarre%
CASE 1: PUT (xbarre%, 20), barre&, PSET
CASE 2: PUT (xbarre%, 20), barrecolle&, PSET
CASE 3: PUT (xbarre%, 20), barretir&, PSET
CASE 4: PUT (xbarre%, 20), grandebarre&, PSET
CASE 5: PUT (xbarre%, 20), grandebarrecolle&, PSET
CASE 6: PUT (xbarre%, 20), grandebarretir&, PSET
CASE ELSE
END SELECT

END SUB

SUB dessinebrique

LINE (1, 1)-(640, 480), 1, BF
FOR b = 1 TO 15
FOR A = 1 TO 19
couleur = TABLEAU%(b, A)
IF couleur = 5 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), violet&, PSET
IF couleur = 6 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), marron&, PSET
IF couleur = 7 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), GRIS&, PSET
IF couleur = 8 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), GRISFONCE&, PSET
IF couleur = 9 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), bleuplus&, PSET
IF couleur = 10 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), vertplus&, PSET
IF couleur = 11 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), bleucielplus&, PSET
IF couleur = 12 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), rougeplus&, PSET
IF couleur = 13 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), violetplus&, PSET
IF couleur = 14 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), jaune&, PSET
IF couleur = 15 THEN PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), blanc&, PSET
NEXT
NEXT

END SUB

SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
Length& = Length& - 1
Page% = 0
MemLoc& = Segment& * 16 + Offset&
SELECT CASE Channel%
    CASE 0
       PgPort% = &H87
       AddPort% = &H0
       LenPort% = &H1
       ModeReg% = &H48
    CASE 1
       PgPort% = &H83
       AddPort% = &H2
       LenPort% = &H3
       ModeReg% = &H49
    CASE 2
       PgPort% = &H81
       AddPort% = &H4
       LenPort% = &H5
       ModeReg% = &H4A
    CASE 3
       PgPort% = &H82
       AddPort% = &H6
       LenPort% = &H7
       ModeReg% = &H4B
    CASE ELSE
       PRINT "DMA channels 0-3 only are supported."
       EXIT SUB
END SELECT

OUT &HA, &H4 + Channel%
OUT &HC, &H0
OUT &HB, ModeReg%
OUT AddPort%, MemLoc& AND &HFF
OUT AddPort%, (MemLoc& AND &HFFFF&) \ &H100
IF (MemLoc& AND 65536) THEN Page% = Page% + 1
IF (MemLoc& AND 131072) THEN Page% = Page% + 2
IF (MemLoc& AND 262144) THEN Page% = Page% + 4
IF (MemLoc& AND 524288) THEN Page% = Page% + 8
OUT PgPort%, Page%
OUT LenPort%, Length& AND &HFF
OUT LenPort%, (Length& AND &HFFFF&) \ &H100
OUT &HA, Channel%

IF Freq& < 23000 THEN
   TimeConst% = 256 - 1000000 \ Freq&
   WriteDSP &H40
   WriteDSP TimeConst%
   WriteDSP &H14
   WriteDSP (Length& AND &HFF)
   WriteDSP ((Length& AND &HFFFF&) \ &H100)
ELSE
   IF DSPversion! >= 3 THEN
      TimeConst% = ((65536 - 256000000 \ Freq&) AND &HFFFF&) \ &H100
      WriteDSP &H40
      WriteDSP TimeConst%
      WriteDSP (Length& AND &HFF)
      WriteDSP ((Length& AND &HFFFF&) \ &H100)
      WriteDSP &H91
   ELSE
      PRINT "You need a Sound Blaster with a DSP v3.x+ to play at high speed."
      EXIT SUB
   END IF
END IF

END SUB

FUNCTION DSPversion!

WriteDSP &HE1
Temp% = ReadDSP%
Temp2% = ReadDSP%
DSPversion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))

END FUNCTION

SUB effacebrique (xpositionmatricielleballe%, ypositionmatricielleballe%, score, briquerestante%, m!, n!, life%, divers%, xobjet!, yobjet!, testballe%, divers2%)

SELECT CASE TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%)

CASE 5
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
playwav sonb5$
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 0
score = score + 1
CALL affichescore(score, divers%, divers2%)
briquerestante% = briquerestante% - 1

CASE 9
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
playwav sonb9$
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 0
score = score + 1
CALL affichescore(score, divers%, divers2%)
briquerestante% = briquerestante% - 1

CASE 10
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
playwav sonb10$
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 0
score = score + 1
CALL affichescore(score, divers%, divers2%)
briquerestante% = briquerestante% - 1



CASE 12
IF divers2% = 3 THEN
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
ELSE
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), marron&, PSET
END IF

playwav sonb12$
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 6
 score = score + 1
CALL affichescore(score, divers%, divers2%)


CASE 11
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
playwav sonb11$
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 0
score = score + 1
CALL affichescore(score, divers%, divers2%)
briquerestante% = briquerestante% - 1

IF yobjet! = 0 THEN
xobjet! = xpositionmatricielleballe% * 31 + 5
yobjet! = 511 - ypositionmatricielleballe% * 31
END IF

divers% = 5

CASE 6

IF divers2% = 3 THEN
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
ELSE
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), violet&, PSET
END IF

playwav sonb6$
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 5
score = score + 1
CALL affichescore(score, divers%, divers2%)

CASE 7 TO 8
IF TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 7 THEN PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), GRIS&, PSET
IF TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 8 THEN PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), GRISFONCE&, PSET
playwav sonb78$

CASE 13
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
playwav sonb12$
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 0
score = score + 1
CALL affichescore(score, divers%, divers2%)
briquerestante% = briquerestante% - 1
divers% = 4

CASE 14
playwav sonb14$
PUT (xpositionmatricielleballe% * 31 - 5, 449 - ypositionmatricielleballe% * 31 + 62), noir&, PSET
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 0
score = score + 1
CALL affichescore(score, divers%, divers2%)
briquerestante% = briquerestante% - 1
LINE (1, 480)-(640, 1), 1, BF
PUT (2, 4), score&, PSET
PUT (555, 4), life&, PSET
CALL affichescore(score, divers%, divers2%)
CALL affichevie(life%, divers%)
divers% = 3

CASE 15
playwav sonb15$
PUT (xpositionmatricielleballe% * 31 - 5, 511 - ypositionmatricielleballe% * 31), noir&, PSET
TABLEAU%(ypositionmatricielleballe% - 1, xpositionmatricielleballe%) = 0
score = score + 1
CALL affichescore(score, divers%, divers2%)
briquerestante% = briquerestante% - 1
IF testballe% <> 2 THEN
RANDOMIZE TIMER
c = INT(2 * RND) + 1
IF c = 2 THEN n! = -n!
c = INT(2 * RND) + 1
IF c = 2 THEN m! = -m!
END IF
CASE ELSE
END SELECT
IF briquerestante% = 0 THEN divers% = 2
END SUB

SUB generique

TIMER OFF



CLS
FOR aze = 1 TO 45
PRINT " "
NEXT

COLOR 12
center 29, "PROGRAMMEUR :"
CALL defiler(aze!, azr!)

COLOR 15
center 29, "COMBES BERTRAND"
CALL defiler(aze!, azr!)

COLOR 12
center 29, "GRAPHIC DESIGNER :"
CALL defiler(aze!, azr!)

COLOR 15
center 29, "COMBES BERTRAND"
CALL defiler(aze!, azr!)

COLOR 12
center 29, "SOUNDS :"
CALL defiler(aze!, azr!)

COLOR 15
center 29, "COMBES BERTRAND"
FOR azt = 1 TO 2
CALL defiler(aze!, azr!)
NEXT
COLOR 13
center 29, "THE END !"


FOR azt = 1 TO 4
CALL defiler(aze!, azr!)
NEXT

playwav sonbyby$
SLEEP

END

END SUB

SUB GetBLASTER (DMA%, BasePort%, IRQ%)
IF LEN(ENVIRON$("BLASTER")) = 0 THEN PRINT "BLASTER environment variable not set.": EXIT SUB
FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
   SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
      CASE "I"
        IRQ% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
      CASE "D"
        DMA% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
   END SELECT
NEXT
END SUB

SUB lancerballe (xbarre%, ybarre%, testbarre%, xballe!, yballe!, testballe%, bouge%, divers%, mousex%, mouseb%, n!, commande%, mode, m!)


commande% = 0
CALL MouseArea2(testbarre%)
yballe! = 34
lancer% = 1

DO
IF bouge% > 0 THEN LINE (xbarre%, 33)-(xbarre% + 90, 45), 1, BF: bouge% = 0
IF testbarre% < 4 THEN xballe! = xbarre% + 35
IF testbarre% > 3 THEN xballe! = xbarre% + 50
CALL dessineballe(xballe!, yballe! - 3, testballe%)
CALL mousewait(xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%, xballe!, m!, lancer%)
CALL dessinebarre(xbarre%, ybarre%, testbarre%, divers%)
LOOP WHILE mouseb% = 0


m! = 2
n! = 1
lancer% = 0
IF mouseb% = 1 THEN n! = -ABS(n!) ELSE n! = ABS(n!)
IF testbarre% = 2 OR testbarre% = 5 THEN yballe! = 35 ELSE playwav sonlancer$
LINE (xballe!, yballe! - 3)-(xballe! + 8, yballe! + 5), 1, BF

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

SUB MasterVolume (Right%, Left%, Getvol%)
OUT BasePort% + 4, &H22
IF Getvol% THEN
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB MouseArea (x1%, x2%, y1%, y2%)
R% = Mouseinterr%(&H33, 7, 0, x1%, x2%)
R% = Mouseinterr%(&H33, 8, bx%, y1%, y2%)
END SUB

SUB MouseArea2 (testbarre%)

SELECT CASE testbarre%
CASE IS < 4: x1% = 1: x2% = 560
CASE ELSE
x1% = 1: x2% = 530
END SELECT

R% = Mouseinterr%(&H33, 7, 0, x1%, x2%)
R% = Mouseinterr%(&H33, 8, bx%, 1, 480)
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

SUB mousewait (xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%, xballe!, m!, lancer%)
SHARED mousey%
oldmx% = mousex%
R% = Mouseinterr%(&H33, 3, mouseb%, mousex%, mousey%)
IF oldmx% <> mousex% THEN
dif% = oldmx% - mousex%
incn! = 0
SELECT CASE ABS(dif%)
CASE 8 TO 15: incn! = .1
CASE 16 TO 25: incn! = .2
CASE 26 TO 35: incn! = .4
CASE 36 TO 55: incn! = .7
CASE IS > 55: incn! = 1
CASE ELSE
incn! = 0
END SELECT

SELECT CASE commande%
CASE 0: IF dif% >= 0 THEN bouge% = 2 ELSE bouge% = 1
CASE ELSE
IF dif% >= 0 THEN bouge% = 1 ELSE bouge% = 2
END SELECT

SELECT CASE testbarre%
CASE IS < 4: LINE (xbarre%, 17)-(xbarre% + 80, 50), 1, BF
CASE ELSE
LINE (xbarre%, 17)-(xbarre% + 110, 50), 1, BF
END SELECT

IF lancer% = 0 THEN
SELECT CASE yballe!
CASE IS < 35
SELECT CASE testbarre%

CASE IS < 4
IF xballe! + 4 >= xbarre% AND xballe! <= xbarre% + 76 AND incn! <> 0 THEN
IF bouge% = 1 THEN IF n > 0 THEN n! = n! - incn! ELSE n! = n! + incn!: m! = -m!: yballe! = 35: playwav sonlift$
IF bouge% = 2 THEN IF n > 0 THEN n! = n! + incn! ELSE n! = n! - incn!: m! = -m!: yballe! = 35: playwav sonlift$
IF n! > 3 THEN n! = 3
IF n! < -3 THEN n! = -3
IF vitesse% > 26 THEN vitesse% = vitesse% - 25
END IF

CASE IS > 3
IF xballe! + 4 >= xbarre% AND xballe! <= xbarre% + 106 AND incn! <> 0 THEN
IF bouge% = 1 THEN IF n > 0 THEN n! = n! - incn! ELSE n! = n! + incn!: m! = -m!: yballe! = 35: playwav sonlift$
IF bouge% = 2 THEN IF n > 0 THEN n! = n! + incn! ELSE n! = n! - incn!: m! = -m!: yballe! = 35: playwav sonlift$
IF vitesse% > 26 THEN vitesse% = vitesse% - 25
END IF
CASE ELSE
END SELECT
CASE ELSE
END SELECT
END IF
END IF

IF commande% = 1 THEN
SELECT CASE testbarre%
CASE IS < 4: xbarre% = 561 - mousex%
CASE IS > 3: xbarre% = 531 - mousex%
CASE ELSE
END SELECT
ELSE xbarre% = mousex%
END IF
END SUB

SUB mousewait2 (xbarre%, mousex%, mouseb%, curpos)
SHARED mousey%
oldmy% = mousey%
oldmx% = mousex%
R% = Mouseinterr%(&H33, 3, mouseb%, mousex%, mousey%)
IF oldmy% = mousey% AND oldmx% = mousex% THEN GOTO 12345
COLOR 14
LOCATE 12, 23: PRINT "NOUVELLE PARTIE"
LOCATE 14, 23: PRINT "CONFIGURATION"
LOCATE 16, 23: PRINT "VIEW HIGH SCORES"
LOCATE 18, 23: PRINT "EDITEUR DE NIVEAUX"
LOCATE 20, 23: PRINT "MODE DE JEU"
LOCATE 22, 23: PRINT "CHARGER NIVEAUX"
LOCATE 24, 23: PRINT "QUITTER"
COLOR 12
IF mousex% > 170 AND mousex% < 322 THEN
curpos = -1
SELECT CASE 479 - mousey%
CASE 288 TO 307: LOCATE 12, 23: PRINT "NOUVELLE PARTIE": curpos = 0
CASE 256 TO 275: LOCATE 14, 23: PRINT "CONFIGURATION": curpos = 1
CASE 224 TO 243: LOCATE 16, 23: PRINT "VIEW HIGH SCORES": curpos = 2
CASE 192 TO 211: LOCATE 18, 23: PRINT "EDITEUR DE NIVEAUX": curpos = 3
CASE 160 TO 179: LOCATE 20, 23: PRINT "MODE DE JEU": curpos = 4
CASE 128 TO 147: LOCATE 22, 23: PRINT "CHARGER NIVEAUX": curpos = 8
CASE 96 TO 115: LOCATE 24, 23: PRINT "QUITTER": curpos = 5
CASE ELSE
END SELECT
ELSE curpos = -1
END IF
12345
IF mousey% < 380 AND mousey% > 365 THEN
SELECT CASE mousex%
CASE 360 TO 375: curpos = 6
CASE 475 TO 490: curpos = 7
CASE ELSE
END SELECT
END IF
END SUB

SUB mousewait3 (xbarre%, mousex%, mouseb%, testbarre%, n!, yballe!, commande%)
SHARED mousey%

oldmx% = mousex%

R% = Mouseinterr%(&H33, 3, mouseb%, mousex%, mousey%)

IF oldmx% <> mousex% THEN

SELECT CASE testbarre%
CASE IS < 4: LINE (xbarre%, 17)-(xbarre% + 80, 50), 1, BF
CASE ELSE
LINE (xbarre%, 17)-(xbarre% + 110, 50), 1, BF
END SELECT

END IF

IF commande% = 1 THEN
SELECT CASE testbarre%
CASE IS < 4: xbarre% = 561 - mousex%
CASE IS > 3: xbarre% = 531 - mousex%
CASE ELSE
END SELECT
ELSE xbarre% = mousex%
END IF

END SUB

SUB playwav (sonwav$)
filename$ = directory3$ + sonwav$
OPEN filename$ FOR BINARY AS #1
GET #1, 44, WavBuffer(1)
Length& = LOF(1) - 44
IF Length& > 32767 THEN Length& = 32767
Freq& = 22000
DMAPlay VARSEG(WavBuffer(1)), VARPTR(WavBuffer(1)), Length&, Freq&
FOR i = 0 TO 224
  SetReg i, 0
NEXT i
SetReg &H20, &H1
SetReg &H23, &H1
SetReg &H40, &H1F
SetReg &H43, &H0
SetReg &H60, &HE4
SetReg &H63, &HE4
SetReg &H80, &H9D
SetReg &H83, &H9D
SetReg &H21, &H1
SetReg &H24, &H1
SetReg &H41, &H1F
SetReg &H44, &H0
SetReg &H61, &HE4
SetReg &H64, &HE4
SetReg &H81, &H9D
SetReg &H85, &H9D
CLOSE #1
END SUB

FUNCTION ResetDSP%
OUT BasePort% + 6, 1
FOR Count% = 1 TO 4
   junk% = INP(BasePort% + 6)
NEXT
OUT BasePort% + 6, 0
IF INP(BasePort% + 14) AND &H80 = &H80 AND INP(BasePort% + 10) = &HAA THEN
   ResetDSP% = -1
ELSE
   ResetDSP% = 0
END IF
END FUNCTION

SUB restorebrique (xobjet!, yobjet!)
b = 15 - (INT((yobjet! - 2 - 12) / 31))
A = INT((xobjet! + 11) / 31)
couleur = TABLEAU%(b, A)
SELECT CASE couleur
CASE 0: PUT ((A * 31) - 5, 449 - (((b - 1) * 31))), noir&, PSET
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

END SUB

SUB savescore (score, directory$, A$, interface)
ON ERROR GOTO 600
OPEN directory$ FOR INPUT AS #2
     INPUT #2, hnom1$
     INPUT #2, hsco1$
     INPUT #2, hnom2$
     INPUT #2, hsco2$
     INPUT #2, hnom3$
     INPUT #2, hsco3$
CLOSE #2
CALL decompose(score, A$)
place = 0

IF A$ > hsco1$ THEN
CLS
PRINT "FELICITATION!!! TU EST 1 er"
INPUT "Ton nom: ", nom$
place = 1
GOTO apres
END IF

IF A$ > hsco2$ THEN
CLS
PRINT "BRAVO!!! TU EST 2 nd"
INPUT "Ton nom: ", nom$
place = 2
GOTO apres
END IF

IF A$ > hsco3$ THEN
CLS
PRINT "PAS MAL!!! TU EST 3 eme"
INPUT "Ton nom: ", nom$
place = 3
GOTO apres
END IF

apres:

SELECT CASE place
CASE 0
nomdec1$ = hnom1$: scodec1$ = hsco1$
nomdec2$ = hnom2$: scodec2$ = hsco2$
nomdec3$ = hnom3$: scodec3$ = hsco3$


CASE 1
nomdec1$ = nom$: scodec1$ = A$
nomdec2$ = hnom1$: scodec2$ = hsco1$
nomdec3$ = hnom2$: scodec3$ = hsco2$
CASE 2
nomdec1$ = hnom1$: scodec1$ = hsco1$
nomdec2$ = nom$: scodec2$ = A$
nomdec3$ = hnom2$: scodec3$ = hsco2$

CASE 3
nomdec1$ = hnom1$: scodec1$ = hsco1$
nomdec2$ = hnom2$: scodec2$ = hsco2$
nomdec3$ = nom$: scodec3$ = A$
CASE ELSE
END SELECT

OPEN directory$ FOR OUTPUT AS #2
  PRINT #2, nomdec1$: PRINT #2, scodec1$
  PRINT #2, nomdec2$: PRINT #2, scodec2$
  PRINT #2, nomdec3$: PRINT #2, scodec3$
CLOSE #2

OPEN directory$ FOR INPUT AS #2
     INPUT #2, hnom1$
     INPUT #2, hsco1$
     INPUT #2, hnom2$
     INPUT #2, hsco2$
     INPUT #2, hnom3$
     INPUT #2, hsco3$
CLOSE #2

CLS
CALL affichehighscore(hnom1$, hsco1$, hnom2$, hsco2$, hnom3$, hsco3$, interface)
SLEEP
IF interface = 1 THEN EXIT SUB
EXIT SUB
CALL generique
END

END SUB

SUB SetReg (Reg, Value)
  OUT RegAddr, Reg
  OUT DataAddr, Value

END SUB

SUB SpeakerState (OnOff%)
IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
END SUB

SUB WriteDSP (byte%)
DO
LOOP WHILE INP(BasePort% + 12) AND &H80
OUT BasePort% + 12, byte%
END SUB

