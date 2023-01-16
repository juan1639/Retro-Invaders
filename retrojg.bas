'-----------------------------------------------------------------
'------                                                     ------
'------             R E T R O - A L I E N S                 ------
'------                                                     ------
'------             Programado por: Juan Eguia              ------
'------                                                     ------
'-----------------------------------------------------------------
'-----------------------------------------------------------------
'------                                                     ------
'------                 V A R I A B L E S                   ------
'------                                                     ------
'-----------------------------------------------------------------
CONST pi = 3.14159
CONST coemp = -57.2957795131

CONST pantancho = 320
CONST pantalto = 320
CONST locateancho = 40
CONST locatealto = 20
CONST cadenciadisparo = 9
CONST numeromarcianos = 24
CONST asteduraexplo = 100
CONST duracioninvisible = 100
CONST pedazosesparcidos = 40
'CONST numerodisparos = 7
CONST disparosovni = 3
CONST axptosextra = 5000
CONST timeup = 32000
CONST timevel = 10

CONST blanco = _RGB32(211, 211, 211)
CONST gris = _RGB32(105, 122, 122)
CONST grisaste = _RGB32(150, 150, 150)
CONST negro = _RGB32(0, 0, 0)
CONST grisaceo = _RGB32(128, 133, 144)
CONST azulado = _RGB32(0, 0, 25)
CONST amarillo = _RGB32(194, 128, 0)
CONST verde = _RGB32(0, 205, 28)
CONST azulc = _RGB32(6, 205, 222)
CONST rojo = _RGB32(255, 55, 0)
CONST mconfg = _RGB32(0, 100, 100)

TYPE nave
    pic AS LONG
    x AS INTEGER
    y AS INTEGER
    vecx AS SINGLE
    vel AS INTEGER
    ancho AS INTEGER
    alto AS INTEGER
    aceleracion AS SINGLE
    deceleracion AS SINGLE
    velmax AS INTEGER
    left AS INTEGER
    right AS INTEGER
    abducida AS INTEGER
    explosion AS INTEGER
    duraexplo AS INTEGER
    invisible AS INTEGER
END TYPE

TYPE enemigo
    pic AS LONG
    tipo AS INTEGER
    x AS INTEGER
    y AS INTEGER
    posx AS INTEGER
    posy AS INTEGER
    vecx AS SINGLE
    vecy AS SINGLE
    ancho AS INTEGER
    alto AS INTEGER
    vel AS INTEGER
    anima AS INTEGER
    aviso AS INTEGER
    radian AS SINGLE
    radiocos AS SINGLE
    radiosen AS SINGLE
    centrox AS SINGLE
    centroy AS SINGLE
    grados AS INTEGER
    parab AS INTEGER
    puntos AS INTEGER
    explosion AS INTEGER
    duraexplo AS INTEGER
    activo AS INTEGER
END TYPE

TYPE ovni
    x AS INTEGER
    y AS SINGLE
    vecx AS INTEGER
    vecy AS SINGLE
    ancho AS INTEGER
    alto AS INTEGER
    anima AS SINGLE
    anivec AS SINGLE
    limitesx AS INTEGER
    limitealto AS INTEGER
    limitebajo AS INTEGER
    explosion AS INTEGER
    duraexplo AS INTEGER
    xexplo AS INTEGER
    yexplo AS SINGLE
    puntos AS INTEGER
END TYPE

TYPE disp
    pic AS LONG
    x AS INTEGER
    y AS INTEGER
    vecy AS SINGLE
    ancho AS INTEGER
    alto AS INTEGER
    vel AS INTEGER
    activo AS INTEGER
END TYPE

TYPE ovdisp
    x AS SINGLE
    y AS SINGLE
    vecx AS SINGLE
    vecy AS SINGLE
    ancho AS INTEGER
    alto AS INTEGER
    radio AS INTEGER
    qangulo AS SINGLE
    vel AS INTEGER
    activo AS INTEGER
END TYPE

TYPE abducc
    x AS INTEGER
    y AS INTEGER
    vecy AS INTEGER
    ancho AS INTEGER
    alto AS INTEGER
    tope AS INTEGER
    cual AS INTEGER
    anima AS INTEGER
    tres AS INTEGER
    duracion AS INTEGER
    activo AS INTEGER
END TYPE

TYPE explonave
    x AS SINGLE
    y AS SINGLE
    vecx AS SINGLE
    vecy AS SINGLE
    ancho AS _BYTE
END TYPE

TYPE fondo
    x AS INTEGER
    y AS INTEGER
    vecy AS INTEGER
    brillo AS INTEGER
    ancho AS INTEGER
END TYPE

TYPE scroll
    pic AS LONG
    x AS INTEGER
    y AS SINGLE
    y2 AS SINGLE
    vel AS SINGLE
END TYPE

TYPE mostrar
    x AS INTEGER
    y AS INTEGER
    activo AS INTEGER
END TYPE

TYPE ptomira
    x AS INTEGER
    y AS INTEGER
END TYPE

DIM nave AS nave
DIM enemigo(24) AS enemigo
DIM ovni AS ovni
DIM disp(7) AS disp
DIM ovdisp(3) AS ovdisp
DIM abducc AS abducc
DIM explonave(40) AS explonave
DIM exploene(40) AS explonave
DIM mostrar(5) AS mostrar
DIM fondo(500) AS fondo
DIM scroll AS scroll
DIM ptomira AS ptomira

DIM a AS INTEGER
DIM b AS INTEGER
DIM c AS INTEGER
DIM fotogramas AS INTEGER
DIM ciclos AS LONG
DIM cadencia AS INTEGER
DIM comenzar AS INTEGER
DIM masciclos AS INTEGER
DIM primstar AS _BIT
DIM puntos AS LONG
DIM nivel AS INTEGER
DIM level AS INTEGER
DIM superado AS _BIT
DIM objetivo AS INTEGER
DIM pausasuperado AS INTEGER
DIM cuantos AS INTEGER
DIM cualaste AS INTEGER
DIM atacantes AS INTEGER
DIM numerodisparos AS INTEGER
DIM astexplosion AS INTEGER
DIM tiempo AS LONG
DIM vidas(4) AS INTEGER
DIM valores AS INTEGER
DIM fshoot AS INTEGER

DIM presentacion AS LONG
DIM creditos AS LONG
DIM losniveles AS LONG
DIM gameover AS LONG
DIM nivelsuperado AS LONG
DIM lavidaextra AS LONG
DIM elranking AS LONG
DIM enhorabuena AS LONG

DIM record(10) AS LONG
DIM rnivel(10) AS INTEGER

DIM nombre(10) AS STRING
DIM pulsar AS STRING
DIM latecla(6) AS LONG

DIM sonidolaser AS LONG
DIM sonidocaer AS LONG
DIM sonidogo AS LONG
DIM sonidogameover AS LONG
DIM sonidoexplosion AS LONG
DIM sonidonavexplota AS LONG
DIM sonidosuperado AS LONG
DIM sonidoimpactomini AS LONG
DIM sonidolevelup AS LONG
DIM sonidoextralive AS LONG
DIM sonidovibracion AS LONG
DIM sonidoataque AS LONG
DIM sonidodescendente2 AS LONG
DIM sonidoclick AS LONG

'---------------------------------------------------------------
'--------                                               --------
'--------            INICIALIZACION GENERAL             --------
'--------                                               --------
'---------------------------------------------------------------
SCREEN _NEWIMAGE(pantancho, pantalto, 32)

_FULLSCREEN
_MOUSESHOW
RANDOMIZE TIMER

CLS , azulado
COLOR amarillo, azulado
LOCATE locatealto / 2, locateancho / 2 - 4: PRINT " Cargando... "

updatesgenerales
updatesgraficos

'---------------------------------------------------------------
'---     Sonidos descargados de: sonidosmp3gratis.com        ---
'---------------------------------------------------------------
sonidoexplosion = _SNDOPEN("misc188.mp3")
sonidolaser = _SNDOPEN("disparolaser1.mp3")
sonidogameover = _SNDOPEN("gameoveretro.mp3")
sonidogo = _SNDOPEN("gameover.mp3")
sonidonavexplota = _SNDOPEN("navexplota.mp3")
sonidosuperado = _SNDOPEN("sonidoarcade1.mp3")
sonidocaer = _SNDOPEN("arcadecaer.mp3")
sonidoimpactomini = _SNDOPEN("impactomini.mp3")
sonidolevelup = _SNDOPEN("levelup.mp3")
sonidoextralive = _SNDOPEN("extralive.mp3")
sonidoclick = _SNDOPEN("click.mp3")
sonidovibracion = _SNDOPEN("vibracion.mp3")
sonidoataque = _SNDOPEN("caeagudo.mp3")
sonidodescendente2 = _SNDOPEN("descendente2.mp3")

'-------------------------------------------------------------
'----------------                                   ----------
'----------------     P R E S E N T A C I O N       ----------
'----------------                                   ----------
'-------------------------------------------------------------
_SNDPLAYCOPY sonidocaer
CLS , azulado
_PRINTMODE _KEEPBACKGROUND

valores = 1
comenzar = 0
cadencia = 0
ciclos = 0
masciclos = 0
primstar = 0

FOR a = 1 TO 500
    iniciafondo a
NEXT a

primstar = -1

DO
    _LIMIT fotogramas + 30
    PCOPY _DISPLAY, 1

    elfondo
    'elscroll

    pulsar = UCASE$(INKEY$)

    IF pulsar = CHR$(13) THEN soniquete 100, 700: comenzar = 1
    IF pulsar = CHR$(9) THEN BEEP: valores = valores + 1

    IF _KEYDOWN(27) THEN
        salir
        salirsoni
        BEEP
        SYSTEM
    END IF

    WHILE _MOUSEINPUT
        ptomira.x = _MOUSEX
        ptomira.y = _MOUSEY

        IF _MOUSEBUTTON(1) OR _MOUSEBUTTON(2) THEN
            IF comenzar < 1 THEN
                IF ptomira.x > 35 AND ptomira.x < 230 AND ptomira.y > 200 AND ptomira.y < 220 THEN
                    _MOUSEHIDE
                    soniquete 100, 700
                    comenzar = 10
                END IF
            END IF
        END IF
    WEND

    IF masciclos < 711 THEN
        _PUTIMAGE (pantancho / 2 - INT(cadencia / 2.69), pantalto / 3 - INT(cadencia / 6))-STEP(INT(cadencia / 1.38), INT(cadencia * 2 / 6)), presentacion
    END IF
    _PUTIMAGE (pantancho / 2 - INT(cadencia / 4), INT(pantalto / 1.02) - INT(cadencia / 10))-STEP(INT(cadencia / 2), INT(cadencia * 2 / 28)), creditos

    IF ciclos > 519 AND ciclos < 531 THEN
        _PUTIMAGE (pantancho + 270 - INT(masciclos / 2), 180), enemigo(1).pic, , (109, 37)-STEP(14, 14)
        _PUTIMAGE (pantancho + 290 - INT(masciclos / 2), 180), enemigo(1).pic, , (109, 55)-STEP(14, 14)
        _PUTIMAGE (pantancho + 310 - INT(masciclos / 2), 180), enemigo(1).pic, , (109, 73)-STEP(14, 14)
        _PUTIMAGE (pantancho + 330 - INT(masciclos / 2), 180), enemigo(1).pic, , (109, 91)-STEP(14, 14)
        _PUTIMAGE (pantancho + 350 - INT(masciclos / 2), 180), enemigo(1).pic, , (109, 145)-STEP(14, 14)
    END IF

    IF ciclos > 499 AND ciclos < 520 THEN
        COLOR amarillo, azulado
        LOCATE 14, 5: PRINT " Click aqui para jugar... "
        LOCATE 16, 5: PRINT " ENTER menu de configuracion ... "
        _PUTIMAGE (pantancho + 270 - INT(masciclos / 2), 180), enemigo(1).pic, , (127, 37)-STEP(14, 14)
        _PUTIMAGE (pantancho + 290 - INT(masciclos / 2), 180), enemigo(1).pic, , (127, 55)-STEP(14, 14)
        _PUTIMAGE (pantancho + 310 - INT(masciclos / 2), 180), enemigo(1).pic, , (127, 73)-STEP(14, 14)
        _PUTIMAGE (pantancho + 330 - INT(masciclos / 2), 180), enemigo(1).pic, , (127, 91)-STEP(14, 14)
        _PUTIMAGE (pantancho + 350 - INT(masciclos / 2), 180), enemigo(1).pic, , (127, 145)-STEP(14, 14)
    END IF

    IF masciclos > 710 THEN poneranking
    IF valores / 2 = INT(valores / 2) THEN mostrartips

    IF comenzar = 1 THEN menuconfiguracion
    IF comenzar = 11 THEN menucontroles
    IF comenzar = 111 THEN redefinir

    IF cadencia = 290 THEN _SNDPLAYCOPY sonidosuperado
    IF cadencia < 500 THEN cadencia = cadencia + 1

    ciclos = ciclos + 1
    masciclos = masciclos + 1
    IF ciclos = 530 THEN ciclos = 500
    IF masciclos >= 1200 THEN masciclos = 0

    _DISPLAY
    PCOPY 1, _DISPLAY

LOOP UNTIL comenzar = 10

SLEEP 1
_SNDPLAYCOPY sonidosuperado
_MOUSEHIDE
_PRINTMODE _KEEPBACKGROUND

'-------------------------------------------------------------
'---------                                           ---------
'---------    I N I C I O   D E   N I V E L   x      ---------
'---------                                           ---------
'-------------------------------------------------------------
DO
    DO
        updatesnivelx

        '-----------------------------------------------------------
        '--------                                           --------
        '--------     B U C L E   P R I N C I P A L         --------
        '--------                                           --------
        '-----------------------------------------------------------

        DO

            _LIMIT fotogramas
            PCOPY _DISPLAY, 1

            elfondo
            'elscroll

            IF _KEYDOWN(latecla(1)) OR _KEYDOWN(32) THEN
                IF nave.abducida = 0 THEN
                    IF nave.explosion = 0 THEN
                        IF cadencia = 0 THEN
                            cadencia = cadenciadisparo
                            iniciadisparo
                        END IF
                    END IF
                END IF
            END IF

            IF _KEYDOWN(100306) THEN INPUT fotogramas
            IF _KEYDOWN(8) THEN SLEEP 99
            IF _KEYDOWN(9) THEN BEEP: valores = valores + 1

            IF _KEYDOWN(27) THEN
                salir
                salirsoni
                BEEP
                SYSTEM
            END IF

            WHILE _MOUSEINPUT
                ptomira.x = _MOUSEX
                ptomira.y = _MOUSEY
            WEND

            IF pausasuperado = 0 THEN

                mostrarcosas
                IF nave.explosion = 0 THEN muevenave
                eldisparo
                elovni
                IF ciclos < 300 THEN ampliaenemigos
                IF ciclos > 300 THEN muevenemigos
                labduccion
                elenedisparo
                laovniexplosion
                laastexplosion enemigo(), exploene()
                laexplosion

                IF puntos >= axptosextra AND vidas(2) = 0 THEN
                    vidas(1) = vidas(1) + 1
                    vidas(2) = 1
                    vidas(4) = 200
                    _SNDPLAYCOPY sonidoextralive
                END IF
                IF puntos >= axptosextra * 4 AND vidas(3) = 0 THEN
                    vidas(1) = vidas(1) + 1
                    vidas(3) = 1
                    vidas(4) = 200
                    _SNDPLAYCOPY sonidoextralive
                END IF

                IF objetivo >= numeromarcianos THEN superado = -1

                ciclos = ciclos + 1
                tiempo = tiempo - timevel

                IF tiempo = 0 THEN nave.explosion = nave.duraexplo
                IF cadencia > 0 THEN cadencia = cadencia - 1
                IF ciclos = 32000 THEN ciclos = 1

            END IF

            mostrarmarcadores
            IF valores / 2 = INT(valores / 2) THEN mostrartips

            IF pausasuperado > 0 THEN
                _PUTIMAGE (pantancho / 2 - 120, pantalto / 2 - 50)-STEP(240, 50), nivelsuperado
                _PUTIMAGE (nave.x, nave.y), nave.pic, , (109, 1)-STEP(14, 14)
                pausasuperado = pausasuperado - 1
            END IF

            _DISPLAY
            PCOPY 1, _DISPLAY

        LOOP UNTIL vidas(1) < 0 OR superado

        '-----------------------------------------------------------
        '-----------------                          ----------------
        '-----------------     G A M E   O V E R    ----------------
        '-----------------                          ----------------
        '-----------------------------------------------------------

        cadencia = 0
        ciclos = 0
        masciclos = 0

        IF superado THEN
            nivel = nivel + 1
            level = level + 1
            IF level >= 5 THEN level = 1
            fotogramas = fotogramas + 2
            pausasuperado = 250
            ciclos = 0
            masciclos = 0
            cadencia = 0
            superado = 0
            objetivo = 0
            astexplosion = 0
            _SNDPLAYCOPY sonidolevelup
        END IF

    LOOP UNTIL vidas(1) < 0

    comenzar = 20
    _KEYCLEAR
    _MOUSESHOW
    _SNDPLAYCOPY sonidogameover

    DO
        _LIMIT 50
        PCOPY _DISPLAY, 1

        elfondo
        mostrarmarcadores
        IF valores / 2 = INT(valores / 2) THEN mostrartips

        IF _KEYDOWN(27) THEN
            salir
            salirsoni
            BEEP
            SYSTEM
        END IF

        WHILE _MOUSEINPUT

            ptomira.x = _MOUSEX
            ptomira.y = _MOUSEY

            IF ptomira.x < 160 AND ptomira.y > 200 AND ptomira.y < 220 THEN
                IF _MOUSEBUTTON(1) OR _MOUSEBUTTON(2) THEN
                    soniquete 100, 800
                    comenzar = 30
                END IF
            END IF

        WEND

        _PUTIMAGE (pantancho / 2 - 140, pantalto / 2 + 50)-STEP(280, 100), gameover

        poneranking

        IF ciclos = 0 THEN batirecords

        COLOR azulc
        LOCATE 14, 27: PRINT " Esc. salir "
        IF cadencia < 11 THEN LOCATE 14, 1: PRINT " Click aqui jugar... "

        IF ciclos = 90 THEN _SNDPLAYCOPY sonidogo

        ciclos = ciclos + 1
        cadencia = cadencia + 1

        IF cadencia = 20 THEN cadencia = 0
        IF ciclos = 32000 THEN ciclos = 0

        _DISPLAY
        PCOPY 1, _DISPLAY

    LOOP UNTIL comenzar >= 30

    fotogramas = fotogramas - (nivel - 1) * 2
    nivel = 1
    level = 1
    superado = 0
    pausasuperado = 0
    objetivo = 0
    atacantes = 2
    cuantos = 0
    numerodisparos = 2
    tiempo = timeup
    puntos = 0
    vidas(1) = 3
    vidas(2) = 0
    vidas(3) = 0
    vidas(4) = 0
    astexplosion = 0
    cualaste = 0
    _SNDPLAYCOPY sonidosuperado
    _MOUSEHIDE

LOOP UNTIL comenzar >= 40

BEEP
SYSTEM

'-----------------------------------------------------------
'-----------------                         -----------------
'-----------------   S U B R U T I N A S   -----------------
'-----------------                         -----------------
'-----------------------------------------------------------
SUB muevenave

    DIM c AS INTEGER

    SHARED nave AS nave
    SHARED latecla() AS LONG

    IF _KEYDOWN(19712) OR _KEYDOWN(latecla(3)) THEN
        nave.vecx = nave.vecx + nave.aceleracion
    ELSEIF _KEYDOWN(19200) OR _KEYDOWN(latecla(2)) THEN
        nave.vecx = nave.vecx - nave.aceleracion
    END IF

    IF nave.vecx < -nave.velmax THEN nave.vecx = -nave.velmax
    IF nave.vecx > nave.velmax THEN nave.vecx = nave.velmax

    IF nave.vecx > 0 THEN nave.vecx = nave.vecx - nave.deceleracion
    IF nave.vecx < 0 THEN nave.vecx = nave.vecx + nave.deceleracion

    IF nave.abducida = 0 THEN nave.x = nave.x + nave.vecx * nave.vel
    lanaveabducida nave

    IF nave.x >= pantancho - nave.ancho THEN nave.x = pantancho - nave.ancho
    IF nave.x <= 0 THEN nave.x = 0

    IF nave.invisible > 0 THEN
        c = INT(RND * 255)
        CIRCLE (nave.x + INT(nave.ancho / 2), nave.y + INT(nave.alto / 2)), 12, _RGB32(250, c, 0), , , 1
        PAINT (nave.x + INT(nave.ancho / 2), nave.y + INT(nave.alto / 2)), _RGB32(250, c, 0)
        nave.invisible = nave.invisible - 1
    END IF
    _PUTIMAGE (nave.x, nave.y), nave.pic, , (109, 1)-STEP(14, 14)

END SUB

SUB lanaveabducida (nave AS nave)

    SHARED enemigo() AS enemigo
    SHARED abducc AS abducc

    IF nave.abducida > 0 THEN
        nave.y = nave.y - 5
        IF nave.x < enemigo(abducc.cual).x THEN
            nave.x = nave.x + 1
        ELSE
            nave.x = nave.x - 1
        END IF
    END IF

    IF nave.y <= 1 THEN nave.explosion = nave.duraexplo

END SUB

SUB iniciadisparo

    DIM b AS INTEGER

    SHARED disp() AS disp
    SHARED nave AS nave
    SHARED sonidolaser AS LONG
    SHARED valores AS INTEGER
    SHARED numerodisparos AS INTEGER

    b = 1
    DO
        IF disp(b).activo = 0 THEN
            disp(b).activo = 1
            disp(b).x = INT(nave.x) + INT(nave.ancho / 2)
            disp(b).y = INT(nave.y) - nave.alto

            IF valores < 3 OR valores > 4 THEN
                _SNDPLAYCOPY sonidolaser
            ELSE
                soniquete 1200, 1800
            END IF

            EXIT SUB
        END IF
        b = b + 1
    LOOP UNTIL b = numerodisparos + 1

END SUB

SUB eldisparo

    DIM b AS INTEGER
    DIM c AS INTEGER

    SHARED disp() AS disp
    SHARED numerodisparos AS INTEGER

    FOR b = 1 TO numerodisparos
        IF disp(b).activo > 0 THEN
            disp(b).y = disp(b).y + disp(b).vecy * disp(b).vel
            c = INT(RND * 125) + 130
            LINE (disp(b).x, disp(b).y)-STEP(0, 14), _RGB32(150, c, 6)
            'LINE (disp(b).x - 1, disp(b).y + 3)-STEP(2, y + 9), _RGB32(150, c, 6), BF

            IF disp(b).y + disp(b).vecy <= 0 THEN disp(b).activo = 0
        END IF
    NEXT b

END SUB

SUB ampliaenemigos

    DIM a AS INTEGER
    DIM amplix AS INTEGER
    DIM ampliy AS INTEGER
    DIM ti AS INTEGER

    SHARED enemigo() AS enemigo
    SHARED ciclos AS LONG

    ti = enemigo(1).tipo

    FOR a = 1 TO numeromarcianos
        amplix = enemigo(a).x + enemigo(a).ancho / 2 - INT(ciclos / 40)
        ampliy = enemigo(a).y + enemigo(a).alto / 2 - INT(ciclos / 40)

        _PUTIMAGE (amplix, ampliy)-STEP(INT(ciclos / 20), INT(ciclos / 20)), enemigo(1).pic, , (109, ti)-STEP(14, 14)
    NEXT a

END SUB

SUB muevenemigos

    DIM a AS INTEGER
    DIM b AS INTEGER
    DIM c AS INTEGER
    DIM ti AS INTEGER
    DIM bordes AS INTEGER

    SHARED enemigo() AS enemigo
    SHARED nave AS nave
    SHARED disp() AS disp
    SHARED mostrar() AS mostrar
    SHARED abducc AS abducc
    SHARED cuantos AS INTEGER
    SHARED atacantes AS INTEGER
    SHARED numerodisparos AS INTEGER
    SHARED cualaste AS INTEGER
    SHARED astexplosion AS INTEGER
    SHARED objetivo AS INTEGER
    SHARED nivel AS INTEGER
    SHARED level AS INTEGER
    SHARED puntos AS LONG

    ti = enemigo(1).tipo

    FOR a = 1 TO numeromarcianos

        IF enemigo(a).activo = 1 THEN

            c = INT(RND * 100)
            IF c < 2 AND cuantos < atacantes THEN iniciataque a

            enemigo(a).x = enemigo(a).x + enemigo(a).vecx * enemigo(a).vel
            enemigo(a).y = enemigo(a).y + enemigo(a).vecy * enemigo(a).vel
            enemigo(a).anima = enemigo(a).anima + 1

            IF enemigo(a).anima >= 20 THEN enemigo(a).anima = 0: enemigo(a).vecx = -enemigo(a).vecx

            IF enemigo(a).anima < 10 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y), enemigo(1).pic, , (109, ti)-STEP(14, 14)
            ELSE
                _PUTIMAGE (enemigo(a).x, enemigo(a).y), enemigo(1).pic, , (127, ti)-STEP(14, 14)
            END IF

        END IF

        IF enemigo(a).activo = 2 THEN

            enemigo(a).x = enemigo(a).x + enemigo(a).vecx * enemigo(a).vel
            enemigo(a).y = enemigo(a).y + enemigo(a).vecy * enemigo(a).vel
            enemigo(a).anima = enemigo(a).anima + 1
            enemigo(a).aviso = enemigo(a).aviso + 1

            IF enemigo(a).anima >= 20 THEN
                enemigo(a).anima = 0
                enemigo(a).vecx = -enemigo(a).vecx
            END IF

            IF enemigo(a).aviso < 12 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (109, ti)-STEP(14, 14)
            ELSEIF enemigo(a).aviso < 24 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (127, ti)-STEP(14, 14)
            ELSEIF enemigo(a).aviso < 36 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (109, ti)-STEP(14, 14)
            ELSEIF enemigo(a).aviso < 48 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (127, ti)-STEP(14, 14)
            ELSEIF enemigo(a).aviso < 60 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (109, ti)-STEP(14, 14)
            ELSE
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (127, ti)-STEP(14, 14)
            END IF

            'SOUND (enemigo(a).aviso + 400) * 3, 0.2
            IF enemigo(a).aviso >= 72 THEN enemigo(a).activo = 3

        END IF

        IF enemigo(a).activo = 3 THEN

            IF enemigo(a).y < 250 + nivel THEN
                IF INT(RND * 100) < 1 + nivel THEN iniciaenedisparo a
            END IF

            IF enemigo(a).grados = 90 THEN enemigo(a).activo = 4

            enemigo(a).radian = enemigo(a).grados * pi / 180
            enemigo(a).x = INT(enemigo(a).centrox + enemigo(a).radiocos * COS(enemigo(a).radian))
            enemigo(a).y = INT(enemigo(a).centroy + enemigo(a).radiosen * SIN(enemigo(a).radian))

            enemigo(a).anima = enemigo(a).anima + 1
            enemigo(a).grados = enemigo(a).grados + enemigo(a).parab

            IF enemigo(a).parab = 2 AND enemigo(a).grados = 360 THEN enemigo(a).grados = 0

            IF enemigo(a).anima >= 20 THEN
                enemigo(a).anima = 0
                enemigo(a).vecx = -enemigo(a).vecx
            END IF

            IF enemigo(a).anima < 10 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (109, ti)-STEP(14, 14)
            ELSE
                _PUTIMAGE (enemigo(a).x, enemigo(a).y)-STEP(-enemigo(a).parab * 7 + 1, 15), enemigo(1).pic, , (127, ti)-STEP(14, 14)
            END IF

        END IF

        IF enemigo(a).activo = 4 THEN

            IF enemigo(a).y < 250 + nivel THEN
                IF INT(RND * 100) < 2 + nivel THEN iniciaenedisparo a
            END IF

            enemigo(a).vecy = 1
            bordes = INT(enemigo(a).x + enemigo(a).vecx * (enemigo(a).vel + INT(RND * 3)))

            IF bordes <= pantancho - enemigo(a).ancho AND bordes >= 0 THEN
                enemigo(a).x = enemigo(a).x + enemigo(a).vecx * (enemigo(a).vel + INT(RND * 3))
            END IF

            enemigo(a).y = enemigo(a).y + enemigo(a).vecy * enemigo(a).vel
            enemigo(a).anima = enemigo(a).anima + 1

            IF enemigo(a).anima >= 20 THEN enemigo(a).anima = 0: enemigo(a).vecx = -enemigo(a).vecx
            IF enemigo(a).y >= pantalto + 20 THEN enemigo(a).y = -20

            IF enemigo(a).anima < 10 THEN
                _PUTIMAGE (enemigo(a).x, enemigo(a).y), enemigo(1).pic, , (109, ti)-STEP(14, 14)
            ELSE
                _PUTIMAGE (enemigo(a).x, enemigo(a).y), enemigo(1).pic, , (127, ti)-STEP(14, 14)
            END IF

            IF abducc.activo = 0 THEN
                IF enemigo(a).y < 240 AND enemigo(a).y > 100 THEN
                    c = INT(RND * 1000)
                    IF c > 20 AND c < 22 + nivel THEN
                        enemigo(a).activo = 5
                        iniciabduccion a
                    END IF
                END IF
            END IF

        END IF

        IF enemigo(a).activo = 5 THEN

            _PUTIMAGE (enemigo(a).x, enemigo(a).y), enemigo(1).pic, , (109, ti)-STEP(14, 14)

        END IF

        IF enemigo(a).activo > 0 THEN

            FOR b = 1 TO numerodisparos
                IF disp(b).activo > 0 THEN

                    IF colisiondisparo(a, b, enemigo(), disp()) THEN

                        IF enemigo(a).activo > 1 THEN
                            cuantos = cuantos - 1

                            IF level > 2 THEN puntos = puntos + 800 ELSE puntos = puntos + 400

                            mostrar(1).activo = 200
                            mostrar(1).x = enemigo(a).x
                            mostrar(1).y = enemigo(a).y + enemigo(a).alto
                            soniquete 200, 500
                        END IF

                        IF enemigo(a).activo = 1 THEN
                            puntos = puntos + enemigo(a).puntos + INT(RND * 8) * 10
                        END IF

                        astexplosion = asteduraexplo
                        cualaste = a
                        enemigo(a).activo = 0
                        objetivo = objetivo + 1
                    END IF
                END IF
            NEXT b

            IF colisionave(a, enemigo(), nave) AND nave.explosion = 0 AND nave.invisible = 0 THEN
                nave.explosion = nave.duraexplo

                IF enemigo(a).activo > 1 THEN
                    cuantos = cuantos - 1

                    IF level > 2 THEN puntos = puntos + 800 ELSE puntos = puntos + 400

                    mostrar(1).activo = 200
                    mostrar(1).x = enemigo(a).x
                    mostrar(1).y = enemigo(a).y + enemigo(a).alto
                    soniquete 200, 500
                END IF

                astexplosion = asteduraexplo
                cualaste = a
                objetivo = objetivo + 1
                enemigo(a).activo = 0
            END IF

        END IF

    NEXT a

END SUB

SUB iniciataque (b AS INTEGER)

    DIM a AS INTEGER

    SHARED enemigo() AS enemigo
    SHARED nave AS nave
    SHARED cuantos AS INTEGER
    SHARED sonidoataque AS LONG

    a = INT(RND * 2)
    IF a = 0 THEN
        enemigo(b).parab = 2
    ELSE
        enemigo(b).parab = -2
    END IF

    cuantos = cuantos + 1
    enemigo(b).activo = 2
    enemigo(b).aviso = 1
    enemigo(b).grados = 270
    enemigo(b).centrox = enemigo(b).x
    enemigo(b).centroy = ABS(enemigo(b).y - nave.y) / 2 + enemigo(b).y
    enemigo(b).radiocos = 25 + INT(RND * 20)
    enemigo(b).radiosen = ABS(enemigo(b).y - nave.y) / 2
    _SNDPLAYCOPY sonidoataque

END SUB

SUB iniciaenedisparo (b AS INTEGER)

    DIM a AS INTEGER

    SHARED ovdisp() AS ovdisp
    SHARED enemigo() AS enemigo
    SHARED nave AS nave

    a = 1
    DO
        IF ovdisp(a).activo = 0 THEN
            ovdisp(a).qangulo = calcangulo(b, enemigo(), nave)
            ovdisp(a).x = enemigo(b).x
            ovdisp(a).y = enemigo(b).y + enemigo(b).alto
            ovdisp(a).vecx = SIN(ovdisp(a).qangulo * pi / 180)
            ovdisp(a).vecy = -COS(ovdisp(a).qangulo * pi / 180)
            ovdisp(a).activo = 1
            soniquete 1300, 1700
            EXIT SUB
        END IF
        a = a + 1
    LOOP UNTIL a >= disparosovni + 1

END SUB

SUB elenedisparo

    DIM a AS INTEGER
    DIM c AS INTEGER

    SHARED ovdisp() AS ovdisp
    SHARED nave AS nave

    FOR a = 1 TO disparosovni
        IF ovdisp(a).activo > 0 THEN
            ovdisp(a).x = ovdisp(a).x + ovdisp(a).vecx * ovdisp(a).vel
            ovdisp(a).y = ovdisp(a).y + ovdisp(a).vecy * ovdisp(a).vel
            c = INT(RND * 255) + 1
            CIRCLE (ovdisp(a).x, ovdisp(a).y), ovdisp(a).radio, _RGB32(255, c, 0)
            PAINT (ovdisp(a).x, ovdisp(a).y), _RGB32(255, c, 0), _RGB32(255, c, 0)

            IF colisionavedisparo(a) AND nave.explosion = 0 AND nave.invisible = 0 THEN
                nave.explosion = nave.duraexplo
                ovdisp(a).activo = 0
            END IF

            IF ovdisp(a).x + ovdisp(a).vecx * 3 > pantancho + 9 OR ovdisp(a).x + ovdisp(a).vecx * 3 < -9 THEN ovdisp(a).activo = 0
            IF ovdisp(a).y + ovdisp(a).vecy * 3 > pantalto + 9 OR ovdisp(a).y + ovdisp(a).vecy * 3 < -9 THEN ovdisp(a).activo = 0

        END IF
    NEXT a

END SUB

SUB elovni

    DIM a AS INTEGER

    SHARED ovni AS ovni
    'SHARED ovdisp() AS ovdisp
    SHARED nave AS nave
    SHARED disp() AS disp
    SHARED mostrar() AS mostrar
    SHARED ciclos AS LONG
    SHARED sonidovibracion AS LONG
    SHARED numerodisparos AS INTEGER

    ovni.x = ovni.x + ovni.vecx
    ovni.y = ovni.y + ovni.vecy
    ovni.anima = ovni.anima + ovni.anivec

    IF ovni.x + ovni.vecx >= ovni.limitesx OR ovni.x + ovni.vecx <= -ovni.limitesx THEN ovni.vecx = -ovni.vecx
    IF ovni.y + ovni.vecy >= ovni.limitealto OR ovni.y + ovni.vecy <= ovni.limitebajo THEN ovni.vecy = -ovni.vecy

    IF ovni.anima < 3 THEN
        _PUTIMAGE (ovni.x, ovni.y), nave.pic, , (109, 145)-STEP(14, 14)
    ELSE
        _PUTIMAGE (ovni.x, ovni.y), nave.pic, , (127, 145)-STEP(14, 14)
    END IF

    FOR a = 1 TO numerodisparos
        IF disp(a).activo > 0 THEN
            IF colisionovnidisparo(a, ovni) THEN
                ovni.explosion = ovni.duraexplo
                mostrar(2).activo = 200
                mostrar(2).x = ovni.x
                mostrar(2).y = ovni.y + ovni.alto
            END IF
        END IF
    NEXT a

    'IF ovni.x = tresdisparos OR ovni.x = tresdisparos + 200 OR ovni.x = tresdisparos + 400 THEN iniciaovnidisp ovni, ovdisp(), nave
    IF ovni.x > 0 AND ovni.x < pantancho AND ciclos / 50 = INT(ciclos / 50) THEN _SNDPLAYCOPY sonidovibracion
    IF ovni.anima > 6 OR ovni.anima < 0 THEN ovni.anivec = -ovni.anivec

END SUB

SUB iniciabduccion (b AS INTEGER)

    SHARED enemigo() AS enemigo
    SHARED abducc AS abducc
    SHARED sonidodescendente2 AS LONG

    abducc.x = enemigo(b).x - 17
    abducc.y = enemigo(b).y + enemigo(b).alto
    abducc.vecy = 1
    abducc.alto = 1
    abducc.cual = b
    abducc.anima = 0
    abducc.activo = 1
    _SNDPLAYCOPY sonidodescendente2

END SUB

SUB labduccion

    SHARED abducc AS abducc
    SHARED enemigo() AS enemigo
    SHARED nave AS nave

    IF abducc.activo > 0 THEN

        abducc.alto = abducc.alto + abducc.vecy
        abducc.anima = abducc.anima + 1
        abducc.tres = abducc.tres + 1

        IF abducc.tres < 5 THEN
            _PUTIMAGE (abducc.x, abducc.y)-STEP(abducc.ancho, abducc.alto), enemigo(1).pic, , (289, 37)-STEP(47, 78)
        ELSEIF abducc.tres < 10 THEN
            _PUTIMAGE (abducc.x, abducc.y)-STEP(abducc.ancho, abducc.alto), enemigo(1).pic, , (339, 37)-STEP(47, 78)
        ELSE
            _PUTIMAGE (abducc.x, abducc.y)-STEP(abducc.ancho, abducc.alto), enemigo(1).pic, , (389, 37)-STEP(47, 78)
        END IF

        IF colisionaveabducc AND nave.explosion = 0 AND nave.invisible = 0 THEN nave.abducida = 1

        IF enemigo(abducc.cual).activo = 0 THEN abducc.activo = 0

        IF abducc.alto + abducc.y + abducc.vecy >= pantalto THEN abducc.vecy = 0

        IF abducc.anima >= abducc.duracion THEN
            abducc.activo = 0
            IF enemigo(abducc.cual).activo > 0 THEN enemigo(abducc.cual).activo = 4
        END IF

        IF abducc.tres >= 15 THEN abducc.tres = 0
    END IF

END SUB

FUNCTION calcangulo (b AS INTEGER, enemigo() AS enemigo, nave AS nave)

    DIM c AS INTEGER

    IF enemigo(b).y = nave.y THEN
        IF enemigo(b).x = nave.x THEN
            EXIT FUNCTION
        END IF
        IF enemigo(b).x > nave.x THEN
            calcangulo = 90
        ELSE
            calcangulo = 270
        END IF
        EXIT FUNCTION
    END IF

    IF enemigo(b).x = nave.x THEN
        IF enemigo(b).y > nave.y THEN
            calcangulo = 180
        END IF
        EXIT FUNCTION
    END IF

    c = ATN((enemigo(b).x - nave.x) / (enemigo(b).y - nave.y)) * coemp

    IF enemigo(b).y < nave.y THEN
        IF enemigo(b).x > nave.x THEN
            calcangulo = c + 180
        ELSE
            calcangulo = c + 180
        END IF
    ELSE
        calcangulo = c
    END IF

END FUNCTION

FUNCTION colisiondisparo (a AS INTEGER, b AS INTEGER, ene() AS enemigo, di() AS disp)

    colisiondisparo = 0
    IF ene(a).x + ene(a).ancho >= di(b).x THEN
        IF ene(a).x <= di(b).x + di(b).ancho THEN
            IF ene(a).y + ene(a).alto >= di(b).y THEN
                IF ene(a).y <= di(b).y + di(b).alto THEN
                    colisiondisparo = -1
                    di(b).activo = 0
                END IF
            END IF
        END IF
    END IF

END FUNCTION

FUNCTION colisionave (a AS INTEGER, ene() AS enemigo, nave AS nave)

    colisionave = 0
    IF ene(a).x + ene(a).ancho >= nave.x THEN
        IF ene(a).x <= nave.x + nave.ancho THEN
            IF ene(a).y + ene(a).alto >= nave.y THEN
                IF ene(a).y <= nave.y + nave.alto THEN
                    colisionave = -1
                END IF
            END IF
        END IF
    END IF

END FUNCTION

FUNCTION colisionavedisparo (b AS INTEGER)

    SHARED ovdisp() AS ovdisp
    SHARED nave AS nave

    colisionavedisparo = 0
    IF ovdisp(b).x - ovdisp(b).radio + ovdisp(b).ancho >= nave.x THEN
        IF ovdisp(b).x - ovdisp(b).radio <= nave.x + nave.ancho THEN
            IF ovdisp(b).y - ovdisp(b).radio + ovdisp(b).alto >= nave.y THEN
                IF ovdisp(b).y - ovdisp(b).radio <= nave.y + nave.alto THEN
                    colisionavedisparo = -1
                END IF
            END IF
        END IF
    END IF

END FUNCTION

FUNCTION colisionaveabducc

    SHARED abducc AS abducc
    SHARED nave AS nave

    colisionaveabducc = 0
    IF abducc.x + abducc.ancho - 9 >= nave.x THEN
        IF abducc.x <= nave.x + nave.ancho - 9 THEN
            IF abducc.y + abducc.alto >= nave.y THEN
                IF abducc.y <= nave.y + nave.alto THEN
                    colisionaveabducc = -1
                END IF
            END IF
        END IF
    END IF

END FUNCTION

FUNCTION colisionovnidisparo (b AS INTEGER, ovni AS ovni)

    SHARED disp() AS disp

    colisionovnidisparo = 0
    IF ovni.x + ovni.ancho >= disp(b).x THEN
        IF ovni.x <= disp(b).x + disp(b).ancho THEN
            IF ovni.y + ovni.alto >= disp(b).y THEN
                IF ovni.y <= disp(b).y + disp(b).alto THEN
                    colisionovnidisparo = -1
                    disp(b).activo = 0
                END IF
            END IF
        END IF
    END IF

END FUNCTION

SUB laexplosion

    DIM a AS _BYTE

    SHARED nave AS nave
    SHARED explonave() AS explonave
    SHARED sonidonavexplota AS LONG
    SHARED numerodisparos AS INTEGER
    SHARED vidas() AS INTEGER
    SHARED tiempo AS LONG

    IF nave.explosion = nave.duraexplo THEN
        _SNDPLAYCOPY sonidonavexplota
        FOR a = 1 TO pedazosesparcidos
            explonave(a).x = nave.x + INT(RND * 9) - 4
            explonave(a).y = nave.y + INT(RND * 9) - 4
            explonave(a).vecx = INT((RND * 90) - 40) / 30
            explonave(a).vecy = INT((RND * 90) - 40) / 30
            explonave(a).ancho = INT(RND * 2)
        NEXT a
    END IF

    IF nave.explosion > 0 THEN
        FOR a = 1 TO pedazosesparcidos
            explonave(a).x = explonave(a).x + explonave(a).vecx
            explonave(a).y = explonave(a).y + explonave(a).vecy
            IF INT(RND * 2) = 0 THEN
                LINE (explonave(a).x, explonave(a).y)-STEP(explonave(a).ancho, explonave(a).ancho), _RGB32(255, INT(RND * 200) + 55, 0)
            ELSE
                LINE (explonave(a).x, explonave(a).y)-STEP(explonave(a).ancho, explonave(a).ancho), _RGB32(255, INT(RND * 200) + 55, 0), BF
            END IF

            IF tiempo <= 0 THEN
                COLOR rojo, azulado
                LOCATE locatealto - 8, locateancho / 2 - 4: PRINT " TIME UP "
            END IF
        NEXT a
        nave.explosion = nave.explosion - 1

        IF nave.explosion = 1 THEN
            nave.y = 300
            nave.abducida = 0
            nave.invisible = duracioninvisible
            numerodisparos = 2
            vidas(1) = vidas(1) - 1
            IF tiempo <= 0 THEN tiempo = timeup
        END IF

    END IF

END SUB

SUB laastexplosion (astero() AS enemigo, expast() AS explonave)

    DIM a AS INTEGER

    SHARED sonidoexplosion AS LONG
    SHARED cualaste AS INTEGER
    SHARED astexplosion AS INTEGER

    IF astexplosion = asteduraexplo THEN
        CIRCLE (astero(cualaste).x + 7, astero(cualaste).y + 7), 7, _RGB32(255, 150, 0)
        PAINT (astero(cualaste).x + 7, astero(cualaste).y + 7), _RGB32(255, 150, 0), _RGB32(255, 150, 0)
        _SNDPLAYCOPY sonidoexplosion
        FOR a = 1 TO pedazosesparcidos
            expast(a).x = astero(cualaste).x + INT(RND * 9) - 4
            expast(a).y = astero(cualaste).y + INT(RND * 9) - 4
            expast(a).vecx = INT((RND * 90) - 40) / 40
            expast(a).vecy = INT((RND * 90) - 40) / 40
            expast(a).ancho = INT(RND * 2)
        NEXT a
    END IF

    IF astexplosion > 0 THEN
        FOR a = 1 TO pedazosesparcidos
            expast(a).x = expast(a).x + expast(a).vecx
            expast(a).y = expast(a).y + expast(a).vecy
            LINE (expast(a).x, expast(a).y)-STEP(expast(a).ancho, expast(a).ancho), _RGB32(255, INT(RND * 200) + 55, 0), BF
        NEXT a
        astexplosion = astexplosion - 1
    END IF

END SUB

SUB laovniexplosion

    DIM a AS INTEGER

    SHARED ovni AS ovni
    SHARED explonave() AS explonave
    SHARED puntos AS LONG
    SHARED numerodisparos AS INTEGER
    SHARED fshoot AS INTEGER

    IF ovni.explosion = ovni.duraexplo THEN
        _SNDPLAYCOPY sonidoexplosion
        FOR a = 1 TO pedazosesparcidos
            explonave(a).x = ovni.x + INT(RND * 9) - 4
            explonave(a).y = ovni.y + INT(RND * 9) - 4
            explonave(a).vecx = INT((RND * 90) - 40) / 10
            explonave(a).vecy = INT((RND * 90) - 40) / 10
            explonave(a).ancho = INT(RND * 2) + 1
        NEXT a

        'quin.x = ov.x
        'quin.y = ov.y + 60
        'quin.activo = 100
        IF numerodisparos = 2 THEN fshoot = asteduraexplo + 50
        numerodisparos = 6

        IF INT(RND * 2) = 0 THEN ovni.x = ovni.limitesx ELSE ovni.x = -ovni.limitesx
        puntos = puntos + ovni.puntos
    END IF

    IF ovni.explosion > 0 THEN
        FOR a = 1 TO pedazosesparcidos
            explonave(a).x = explonave(a).x + explonave(a).vecx
            explonave(a).y = explonave(a).y + explonave(a).vecy
            IF INT(RND * 2) = 0 THEN
                LINE (explonave(a).x, explonave(a).y)-STEP(explonave(a).ancho, explonave(a).ancho), _RGB32(255, INT(RND * 200) + 55, 0)
            ELSE
                LINE (explonave(a).x, explonave(a).y)-STEP(explonave(a).ancho, explonave(a).ancho), _RGB32(255, INT(RND * 200) + 55, 0), BF
            END IF
        NEXT a
        ovni.explosion = ovni.explosion - 1

    END IF

END SUB

SUB mostrarcosas

    SHARED enemigo() AS enemigo
    SHARED mostrar() AS mostrar
    SHARED lavidaextra AS LONG
    SHARED vidas() AS INTEGER
    SHARED losniveles AS LONG
    SHARED nivel AS INTEGER
    SHARED level AS INTEGER
    SHARED ciclos AS LONG
    SHARED fshoot AS INTEGER

    IF mostrar(1).activo > 0 THEN
        IF level > 2 THEN
            _PUTIMAGE (mostrar(1).x, mostrar(1).y), enemigo(1).pic, , (37, 109)-STEP(29, 14)
        ELSE
            _PUTIMAGE (mostrar(1).x, mostrar(1).y), enemigo(1).pic, , (1, 109)-STEP(29, 14)
        END IF
        mostrar(1).activo = mostrar(1).activo - 1
    END IF

    IF vidas(4) > 0 THEN
        _PUTIMAGE (pantancho / 2 - 120, pantalto / 2 - 25)-STEP(240, 50), lavidaextra
        vidas(4) = vidas(4) - 1
    END IF

    IF ciclos < 200 THEN
        _PUTIMAGE (pantancho / 2 - 150, pantalto / 2 + 25)-STEP(240, 50), losniveles, , (0, 0)-(340, 100)
        IF nivel < 10 THEN _PUTIMAGE (pantancho / 2 + 110, pantalto / 2 + 25)-STEP(50, 50), losniveles, , (270 + nivel * 70, 0)-STEP(69, 100)
    END IF

    IF fshoot > 0 THEN
        fshoot = fshoot - 1
        COLOR rojo, azulado
        LOCATE locatealto - 5, locateancho / 2 - 7: PRINT " DISPARO RAPIDO! "
        '_PUTIMAGE (pantancho / 2 - 100, pantalto / 2 + 50)-STEP(200, 60), fastshoot
    END IF

    IF mostrar(2).activo > 0 THEN
        _PUTIMAGE (mostrar(2).x, mostrar(2).y), enemigo(1).pic, , (73, 109)-STEP(29, 14)
        mostrar(2).activo = mostrar(2).activo - 1
    END IF

END SUB

SUB iniciafondo (b)

    SHARED fondo() AS fondo
    SHARED primstar AS _BIT

    fondo(b).x = INT(RND * 320)

    IF primstar THEN
        fondo(b).y = INT(RND * 9) - 8
    ELSE
        fondo(b).y = INT(RND * 320)
    END IF

    fondo(b).vecy = 1
    'fondo(b).brillo = INT(RND * 200) + 55
    'fondo(b).ancho = 0

END SUB

SUB elfondo

    DIM a AS INTEGER

    SHARED fondo() AS fondo
    SHARED pausasuperado AS INTEGER

    FOR a = 1 TO 500
        IF pausasuperado = 0 THEN
            fondo(a).y = fondo(a).y + fondo(a).vecy
        ELSE
            fondo(a).y = fondo(a).y + fondo(a).vecy * 10
        END IF
        'PSET (fo(a).x, fo(a).y), _RGB32(fo(a).brillo, fo(a).brillo, fo(a).brillo)
        PSET (fondo(a).x, fondo(a).y), _RGB32(INT(RND * 255), INT(RND * 255), INT(RND * 255))

        IF fondo(a).y > pantalto THEN iniciafondo a
    NEXT a
END SUB

SUB elscroll

    SHARED scroll AS scroll

    scroll.y = scroll.y + scroll.vel
    scroll.y2 = scroll.y2 + scroll.vel

    IF INT(scroll.y) >= 320 THEN scroll.y = -320
    IF INT(scroll.y2) >= 320 THEN scroll.y2 = -320

    _PUTIMAGE (scroll.x, scroll.y)-STEP(321, 320), scroll.pic
    _PUTIMAGE (scroll.x, scroll.y2)-STEP(321, 320), scroll.pic

END SUB

SUB poneranking

    DIM a AS INTEGER

    SHARED elranking AS LONG
    SHARED nombre() AS STRING
    SHARED rnivel() AS INTEGER
    SHARED record() AS LONG

    _PUTIMAGE (pantancho / 2 - 150, 0), elranking
    COLOR verde
    'LOCATE 24, locateancho / 2 - 6: PRINT " R A N K I N G "

    FOR a = 1 TO 9
        'LOCATE a + 26, locateancho / 2 - 22: PRINT "                                         "
        LOCATE a + 4, 6: PRINT LTRIM$(STR$(a)); ". "; nombre(a)

        LOCATE a + 4, locateancho / 2 + 6: PRINT USING "##"; rnivel(a)
        LOCATE a + 4, locateancho / 2 + 9: PRINT USING "#######"; record(a)
    NEXT a

END SUB

SUB batirecords

    DIM a AS INTEGER
    DIM b AS INTEGER
    DIM batir AS INTEGER
    DIM jugador AS STRING
    DIM nc(10) AS STRING
    DIM copia(10) AS LONG
    DIM copn(10) AS INTEGER

    SHARED record() AS LONG
    SHARED rnivel() AS INTEGER
    SHARED nombre() AS STRING
    SHARED puntos AS LONG
    SHARED nivel AS INTEGER
    SHARED sonidolevelup AS LONG
    SHARED sonidogo AS LONG
    SHARED enhorabuena AS LONG

    COLOR _RGB32(255, INT(RND * 255), 0)
    FOR a = 1 TO 9

        IF puntos >= record(a) AND batir = 0 THEN
            batir = 1
            _SNDPLAYCOPY sonidolevelup
            cuadro 20, 15, 280, 100
            _PUTIMAGE (pantancho / 2 - 100, 10)-STEP(200, 50), enhorabuena
            LOCATE 5, 4: PRINT " Has establecido un nuevo RECORD! "
            LOCATE 7, 4: INPUT " Introduce tu nombre"; jugador
            _SNDPLAYCOPY sonidogameover

            IF a < 9 THEN
                FOR b = a + 1 TO 9
                    copia(b) = record(b - 1): nc(b) = nombre(b - 1): copn(b) = rnivel(b - 1)
                NEXT b
            END IF

            copia(a) = puntos: nc(a) = MID$(jugador, 1, 15): copn(a) = nivel
            'record(a) = crono: nombre(a) = MID$(jugador, 1, 15)
        END IF

        IF batir = 0 THEN copia(a) = record(a): nc(a) = nombre(a): copn(a) = rnivel(a)

    NEXT a

    IF batir = 1 THEN
        OPEN "retroggrecord.dat" FOR OUTPUT AS #1
        FOR a = 1 TO 9
            WRITE #1, copia(a)
            WRITE #1, copn(a)
            WRITE #1, nc(a)
        NEXT a
        CLOSE #1

        OPEN "retroggrecord.dat" FOR INPUT AS #1
        FOR a = 1 TO 9
            INPUT #1, record(a)
            INPUT #1, rnivel(a)
            INPUT #1, nombre(a)
        NEXT a
        CLOSE #1
    END IF

END SUB

SUB soniquete (uno AS INTEGER, dos AS INTEGER)

    DIM a AS INTEGER
    FOR a = uno TO dos STEP 50: SOUND a, 0.2: NEXT a
END SUB

SUB cuadro (x, y, ancho, alto)
    LINE (x - 5, y - 5)-STEP(ancho + 10, alto + 10), grisaceo, BF
    LINE (x, y)-STEP(ancho, alto), azulado, BF
END SUB

SUB mostrartips

    SHARED ciclos AS LONG
    SHARED comenzar AS INTEGER
    SHARED cadencia AS INTEGER
    SHARED fotogramas AS INTEGER
    SHARED atacantes AS INTEGER
    SHARED ptomira AS ptomira
    SHARED ovdisp() AS ovdisp
    SHARED latecla() AS LONG
    SHARED tiempo AS LONG

    LOCATE 3, 1: PRINT " ciclos, comenzar, cadencia"; ciclos; comenzar; cadencia
    LOCATE 5, 1: PRINT " Fps"; fotogramas; " Raton x,y"; ptomira.x; ptomira.y
    LOCATE 9, 1: PRINT "atacantes"; atacantes; "enedispvel"; ovdisp(1).vel
    LOCATE 11, 1: PRINT "Teclas"; latecla(1); latecla(2); latecla(3); latecla(4); "Tecla(6)"; latecla(6)
    LOCATE 7, 1: PRINT ovdisp(1).activo; ovdisp(2).activo; ovdisp(3).activo
    LOCATE 13, 1: PRINT "time"; INT(tiempo)

END SUB

SUB mostrarmarcadores

    SHARED puntos AS LONG
    SHARED nivel AS INTEGER
    SHARED vidas() AS INTEGER
    SHARED nave AS nave
    SHARED tiempo AS LONG
    SHARED record() AS LONG

    _PUTIMAGE (282, 0)-(293, 12), nave.pic, , (109, 1)-STEP(14, 14)
    _PUTIMAGE (230, 0), nave.pic, , (1, 127)-STEP(7, 15)

    COLOR verde, azulado
    LOCATE 1, 1: PRINT USING "#######"; puntos
    COLOR amarillo, azulado
    LOCATE 1, INT(locateancho / 2 - 11): PRINT " Record:";
    COLOR verde, azulado
    PRINT USING "#######"; record(1);
    LOCATE 1, locateancho / 2 + 11: PRINT USING "##"; nivel

    IF vidas(1) < 0 THEN
        LOCATE 1, INT(locateancho - 2): PRINT "0"
    ELSE
        LOCATE 1, INT(locateancho - 2): PRINT LTRIM$(STR$(vidas(1)))
    END IF

    'LINE (205, 0)-STEP(INT(tiempo / 900), 15), _RGB32(233, 0, 0), BF
    'LOCATE 1, locateancho / 2 + 6: PRINT " TIME "

END SUB

SUB menuconfiguracion

    DIM pulsar AS STRING

    SHARED comenzar AS INTEGER

    DO
        LINE (0, 0)-(pantancho, pantalto), azulado, BF

        COLOR rojo, azulado
        LOCATE 3, locateancho / 2 - 10: PRINT " MENU CONFIGURACION "
        COLOR azulc, azulado
        LOCATE 6, 10: PRINT " 1. Redefinir controles "
        LOCATE 8, 10: PRINT " 2. Ajustes generales "
        LOCATE 16, 10: PRINT " 0. Volver"

        pulsar = UCASE$(INKEY$)

        IF pulsar = "0" THEN comenzar = 0: soniquete 100, 700
        IF pulsar = "1" THEN comenzar = 11: soniquete 100, 700
        IF pulsar = "2" THEN comenzar = 0: soniquete 100, 700

        _DISPLAY

    LOOP UNTIL pulsar = "0" OR pulsar = "2" OR pulsar = "1"

END SUB

SUB menucontroles

    DIM pulsar AS STRING

    SHARED comenzar AS INTEGER
    SHARED latecla() AS LONG
    SHARED ciclos AS LONG

    DO
        LINE (0, 0)-(pantancho, pantalto), azulado, BF

        COLOR rojo, azulado
        LOCATE 3, locateancho / 2 - 5: PRINT " CONTROLES "

        IF latecla(6) = 0 THEN
            COLOR verde, azulado
            LOCATE 6, 5: PRINT " Disparo .... Barra espaciadora "
            LOCATE 8, 5: PRINT " Izquierda .. Flecha Izquierda "
            LOCATE 10, 5: PRINT " Derecha .... Flecha Derecha "
        ELSE
            COLOR verde, azulado
            LOCATE 6, 5: PRINT " Disparo .... "; CHR$(latecla(1))
            LOCATE 8, 5: PRINT " Izquierda .. "; CHR$(latecla(2))
            LOCATE 10, 5: PRINT " Derecha .... "; CHR$(latecla(3))
        END IF

        IF ciclos > 499 AND ciclos < 520 THEN
            COLOR azulc, azulado
            LOCATE 14, 5: PRINT " ENTER redefinir controles "
            LOCATE 16, 5: PRINT " 0. Volver "
        END IF

        pulsar = UCASE$(INKEY$)

        IF pulsar = CHR$(13) THEN comenzar = 111: soniquete 500, 1100
        IF pulsar = "0" THEN comenzar = 1: soniquete 100, 700

        ciclos = ciclos + 1
        IF ciclos = 530 THEN ciclos = 500

        _DISPLAY

    LOOP UNTIL pulsar = CHR$(13) OR pulsar = "0"

END SUB

SUB redefinir

    DIM a AS INTEGER

    SHARED latecla() AS LONG
    SHARED comenzar AS INTEGER

    _KEYCLEAR

    LINE (0, 0)-(pantancho, pantalto), azulado, BF

    FOR a = 1 TO 3
        latecla(a) = 0
        DO
            latecla(a) = _KEYHIT
            COLOR amarillo, azulado

            IF (latecla(a) < 65 OR latecla(a) > 122) AND latecla(a) <> 32 AND latecla(a) > 0 AND latecla(a) <> 13 THEN
                COLOR rojo, azulado
                LOCATE 15, locateancho / 2 - 11: PRINT " Tecla NO configurable "
                LOCATE 16, locateancho / 2 - 14: PRINT " (Solo [a-z], espacio, ENTER)"
                soniquete 100, 300
                soniquete 200, 200
            END IF

            COLOR amarillo, azulado
            IF a = 1 THEN
                LOCATE 6, 1: PRINT " Pulse una tecla para el Disparo... ";
                IF (latecla(a) > 64 AND latecla(a) < 123) OR latecla(a) = 32 THEN PRINT CHR$(latecla(a))
            END IF
            IF a = 2 THEN
                LOCATE 8, 1: PRINT " Pulse una tecla para moverse "
                LOCATE 9, 1: PRINT " a la izquierda... ";
                IF (latecla(a) > 64 AND latecla(a) < 123) OR latecla(a) = 32 THEN PRINT CHR$(latecla(a))
            END IF
            IF a = 3 THEN
                LOCATE 11, 1: PRINT " Pulse tecla para moverse "
                LOCATE 12, 1: PRINT " a la derecha... ";
                IF (latecla(a) > 64 AND latecla(a) < 123) OR latecla(a) = 32 THEN PRINT CHR$(latecla(a))
            END IF

            _DISPLAY

        LOOP UNTIL (latecla(a) > 64 AND latecla(a) < 123) OR latecla(a) = 32
        soniquete 200, 700
        soniquete 100, 600
    NEXT a

    comenzar = 11
    latecla(6) = 1

END SUB

SUB updatesgenerales

    DIM a AS INTEGER

    SHARED scroll AS scroll
    SHARED fotogramas AS INTEGER
    SHARED nivel AS INTEGER
    SHARED level AS INTEGER
    SHARED superado AS _BIT
    SHARED pausasuperado AS INTEGER
    SHARED objetivo AS INTEGER
    SHARED cualaste AS INTEGER
    SHARED atacantes AS INTEGER
    SHARED numerodisparos AS INTEGER
    SHARED tiempo AS LONG
    SHARED puntos AS LONG
    SHARED vidas() AS INTEGER
    SHARED latecla() AS LONG
    SHARED valores AS INTEGER
    SHARED comenzar AS INTEGER
    SHARED cadencia AS INTEGER
    SHARED ciclos AS LONG
    SHARED masciclos AS INTEGER
    SHARED record() AS LONG
    SHARED rnivel() AS INTEGER
    SHARED nombre() AS STRING

    fotogramas = 50
    nivel = 1
    level = 1
    superado = 0
    pausasuperado = 0
    objetivo = 0
    cualaste = 0
    atacantes = 2
    numerodisparos = 2
    tiempo = timeup
    puntos = 0
    vidas(1) = 3
    vidas(2) = 0
    vidas(3) = 0
    vidas(4) = 0
    latecla(1) = 120
    latecla(2) = 122
    latecla(3) = 109
    latecla(4) = 110
    latecla(6) = 0

    valores = 1
    comenzar = 0
    cadencia = 0
    ciclos = 0
    masciclos = 0

    scroll.x = 0
    scroll.y = 0
    scroll.y2 = scroll.y - 320
    scroll.vel = 0.5

    OPEN "retroggrecord.dat" FOR INPUT AS #1
    FOR a = 1 TO 9
        INPUT #1, record(a)
        INPUT #1, rnivel(a)
        INPUT #1, nombre(a)
    NEXT a
    CLOSE #1

END SUB

SUB updatesgraficos

    SHARED nave AS nave
    SHARED enemigo() AS enemigo
    SHARED scroll AS scroll
    SHARED presentacion AS LONG
    SHARED lavidaextra AS LONG
    SHARED elranking AS LONG
    SHARED enhorabuena AS LONG
    SHARED creditos AS LONG
    SHARED menupantalla AS LONG
    SHARED losniveles AS LONG
    SHARED gameover AS LONG
    SHARED nivelsuperado AS LONG

    nave.pic = _LOADIMAGE("picsjg.png")
    enemigo(1).pic = _LOADIMAGE("picsjg.png")
    scroll.pic = _LOADIMAGE("scrollgalaga.png")

    presentacion = _LOADIMAGE("presenretrogg.png")
    lavidaextra = _LOADIMAGE("astevidaextra.png")
    elranking = _LOADIMAGE("asteranking.png")
    enhorabuena = _LOADIMAGE("astenhorabuena.png")
    creditos = _LOADIMAGE("asteroidescreditos.png")
    menupantalla = _LOADIMAGE("astmenu.jpg")
    losniveles = _LOADIMAGE("asteniveles.png")
    gameover = _LOADIMAGE("astegameover.png")
    nivelsuperado = _LOADIMAGE("astesuperado.png")

END SUB

SUB updatesnivelx

    DIM a AS INTEGER

    SHARED comenzar AS INTEGER
    SHARED cadencia AS INTEGER
    SHARED ciclos AS LONG
    SHARED tiempo AS LONG
    SHARED nivel AS INTEGER
    SHARED level AS INTEGER
    SHARED atacantes AS INTEGER
    SHARED numerodisparos AS INTEGER

    SHARED nave AS nave
    SHARED enemigo() AS enemigo
    SHARED ovni AS ovni
    SHARED explonave() AS explonave
    SHARED disp() AS disp
    SHARED ovdisp() AS ovdisp
    SHARED abducc AS abducc

    comenzar = 10
    cadencia = 0
    ciclos = 0
    cuantos = 0
    atacantes = 2 + INT((nivel - 1) / 4)
    tiempo = timeup

    nave.x = pantancho / 2
    nave.y = 300
    nave.ancho = 15
    nave.alto = 15
    nave.vecx = 0
    nave.vel = 2
    nave.aceleracion = 0.3
    nave.deceleracion = 0.1
    nave.velmax = 7
    nave.left = 0
    nave.right = 0
    nave.abducida = 0
    nave.explosion = 0
    nave.duraexplo = 200
    nave.invisible = 0

    OPEN "losmarcianos.dat" FOR INPUT AS #1

    FOR a = 1 TO numeromarcianos

        INPUT #1, posx
        INPUT #1, posy

        enemigo(a).tipo = 19 + level * 18
        enemigo(a).x = posx
        enemigo(a).y = posy
        enemigo(a).ancho = 15
        enemigo(a).alto = 15
        enemigo(a).vecx = 1
        enemigo(a).vecy = 0
        enemigo(a).vel = 1
        enemigo(a).anima = 10
        enemigo(a).aviso = 0
        enemigo(a).radian = 0
        enemigo(a).radiocos = 100
        enemigo(a).radiosen = 100
        enemigo(a).grados = 0
        enemigo(a).centrox = 0
        enemigo(a).centroy = 0
        enemigo(a).explosion = 0
        enemigo(a).duraexplo = 100
        enemigo(a).activo = 1
        enemigo(a).puntos = 100

        IF a < pedazosesparcidos + 1 THEN
            explonave(a).x = 0
            explonave(a).y = 0
            explonave(a).vecx = 0
            explonave(a).vecy = 0
            explonave(a).ancho = 0
        END IF

        IF a < 7 THEN
            disp(a).x = 0
            disp(a).y = 0
            disp(a).ancho = 1
            disp(a).alto = 15
            disp(a).vecy = -5
            disp(a).vel = 1
            disp(a).activo = 0
        END IF

        IF a < disparosovni + 1 THEN
            ovdisp(a).x = 0
            ovdisp(a).y = 0
            ovdisp(a).vecx = 0
            ovdisp(a).vecy = 0
            ovdisp(a).ancho = 5
            ovdisp(a).alto = 5
            ovdisp(a).radio = 2
            ovdisp(a).qangulo = 0
            ovdisp(a).vel = 3 + INT(nivel / 5)
            ovdisp(a).activo = 0
        END IF
    NEXT a

    CLOSE #1

    ovni.limitesx = 2000
    ovni.limitealto = 230
    ovni.limitebajo = 20
    ovni.x = -ovni.limitesx
    ovni.y = INT(RND * 200) + 20
    ovni.vecx = 2
    ovni.vecy = INT(RND * 50) / 50
    ovni.ancho = 15
    ovni.alto = 15
    ovni.anima = 0
    ovni.anivec = 0.1
    ovni.explosion = 0
    ovni.duraexplo = 200
    ovni.xexplo = 0
    ovni.yexplo = 0
    ovni.puntos = 1000

    abducc.x = 0
    abducc.y = 0
    abducc.vecy = 1
    abducc.ancho = 48
    abducc.alto = 1
    abducc.cual = 1
    abducc.tope = 79
    abducc.anima = 0
    abducc.tres = 0
    abducc.duracion = 299
    abducc.activo = 0

END SUB

SUB salir

    DIM a AS INTEGER

    SHARED enemigo() AS enemigo
    SHARED nave AS nave
    SHARED scroll AS scroll

    SHARED creditos AS LONG
    SHARED enhorabuena AS LONG
    SHARED elranking AS LONG
    SHARED lavidaextra AS LONG
    SHARED nivelsuperado AS LONG
    SHARED gameover AS LONG
    SHARED losniveles AS LONG

    BEEP
    _FREEIMAGE nave.pic
    _FREEIMAGE enemigo(1).pic

    _FREEIMAGE creditos
    _FREEIMAGE enhorabuena
    _FREEIMAGE elranking
    _FREEIMAGE lavidaextra
    _FREEIMAGE nivelsuperado
    _FREEIMAGE gameover
    _FREEIMAGE losniveles
    _FREEIMAGE scroll.pic

END SUB

SUB salirsoni

    SHARED sonidoexplosion
    SHARED sonidolaser
    SHARED sonidogameover
    SHARED sonidogo
    SHARED sonidonavexplota
    SHARED sonidosuperado
    SHARED sonidocaer
    SHARED sonidoimpactomini
    SHARED sonidolevelup
    SHARED sonidoextralive
    SHARED sonidoclick
    SHARED sonidovibracion
    SHARED sonidoataque
    SHARED sonidodescendente2

    _SNDCLOSE sonidoexplosion
    _SNDCLOSE sonidolaser
    _SNDCLOSE sonidogameover
    _SNDCLOSE sonidogo
    _SNDCLOSE sonidonavexplota
    _SNDCLOSE sonidosuperado
    _SNDCLOSE sonidocaer
    _SNDCLOSE sonidoimpactomini
    _SNDCLOSE sonidolevelup
    _SNDCLOSE sonidoextralive
    _SNDCLOSE sonidoclick
    _SNDCLOSE sonidovibracion
    _SNDCLOSE sonidoataque
    _SNDCLOSE sonidodescendente2

END SUB










