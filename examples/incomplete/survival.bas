5 DIM T$(65):DIM M(42,8):DIM O(17)
10 PRINT CHR$(26):PRINT"Welcome to the game of SURVIVAL"
15 PRINT:PRINT"You have had an accident in space, escaped in a lifeboat capsule"
20 PRINT"and crash landed on the moon near an abandoned base. Your food and"
25 PRINT"water were destroyed in the crash but your space suit contains a power"
30 PRINT"unit and oxygen unit which will keep you alive for a while.":PRINT
35 PRINT"To explore enter single word directions such as NORTH, WEST and UP"
40 PRINT"or single letter directions such as N, S, E, W, U and D (up and down)."
45 PRINT"You will encounter various items and situations during your travels."
50 PRINT"To communicate, enter commands (verbs), followed by object names,"
55 PRINT"if applicable.   The following commands are valid -"
60 PRINT:PRINT"LOOK FIND GET DROP INVENTORY QUIT TRANSPORT"
65 PRINT"USE ON OFF DIG READ FIX BLASTOFF"
70 PRINT:PRINT"as well as a few others you may eventually figure out for yourself!"
75 PRINT:PRINT"Once you have survived, the object then is to achieve the"
80 PRINT"optimum survival time.  Good luck!!!!"
85 PRINT:PRINT"Enter <CR> for beginner or '1'  for expert":INPUT"which";CX:IF CX>1 THEN 85
90 PRINT CHR$(26)
95 PRINT:PRINT"Your crash landing has shaken things up a bit."
100 PRINT"Please wait while I sort it all out.":PRINT
105 FOR I=0 TO 65:READ T$(I):PRINT".":T$=T$(I):GOSUB 125:T$(I)=EC$:GOSUB 120:NEXT I
110 PRINT" Almost there. I'll try to hurry!"
115 FOR I=1 TO 42:FOR J=1 TO 8:READ M(I,J):NEXT J:PRINT".":NEXT I:GOTO 130
120 IF I<>37 THEN RETURN ELSE PRINT" What a mess! Half done!":RETURN
125 EC$="":FOR II=1 TO LEN(T$):CD=ASC(MID$(T$,II,1))-4:EC$=EC$+CHR$(CD):NEXT II:RETURN
130 P=1:C=2:T1=0:V=0:F0=1:O(1)=21:O(2)=19:O(3)=99:O(4)=6:O(5)=32:O(6)=0:
    O(7)=38:O(8)=36:O(9)=0:O(10)=35:O(11)=99:O(12)=33:O(13)=34:O(14)=37:
    O(17)=20:M(14,4)=0:Z=0:PRINT".";
135 IF CX=0 THEN T2=275:P1=320:P2=75:PRINT".";:GOTO 145
140 T2=185:P1=230:P2=50:PRINT".";
145 M(14,8)=M(14,7)+1:M(2,8)=M(2,7)+1:PRINT". OK! Ready!"
150 F0=1:F1=0:F2=0:F3=0:F4=0:F5=0:F6=0:F7=0:F8=0:F9=0:OF=0:
    BG=0:P3=1:T0=120
155 PRINT:PRINT"Elapsed Time: "; T1; "Minutes":T1=T1+5
160 IF F4=1 THEN T0=T0-5
165 O(15)=0:O(16)=0:MF=0
170 IF F1=2 THEN F1=0
175 IF F2=2 THEN 1320
180 IF F3=2 THEN F3=0
185 IF F4=2 THEN F4=0
190 IF T2=0 THEN 1240
195 IF O(11)=99 THEN PRINT"Power Unit: "; P1; "Units"
200 IF O(14)=99 THEN PRINT"Power Pack: "; P2; "Units"
205 IF P=38 THEN 215
210 IF P>21 AND F9=0 THEN 245
215 IF O(11)<>99 THEN 230
220 IF P1=0 OR P3=0 THEN 1230
225 P1=P1-5:GOTO 245
230 IF O(14)<>99 THEN 1230
235 IF P2=0 OR P4=0 THEN 1230
240 P2=P2-5
245 IF CX=1 THEN 270
250 IF T1>600 THEN 1260
255 IF T1>500 THEN 1425
260 IF T1=365 THEN PRINT"You've been here 6 hours! Getting thirsty?"
265 GOTO 280
270 IF T1>500 THEN 1260
275 IF T1>400 THEN 1425
280 IF T1>175 THEN 1410
285 IF F0=1 THEN T2=T2-5
290 IF T2<0 THEN T2=0
295 IF F0=0 THEN 305
300 IF T2>0 THEN 340
305 IF F9=0 THEN 315
310 IF P>21 THEN 1240
315 IF P<18 THEN 1240
320 IF P<>38 THEN 340
325 IF O(3)<>99 OR F0<>1 OR T2=0 THEN 1240
330 IF F9=1 OR R<>29 THEN 340
335 F9=1:PRINT"You have just blown the air seal in the space station."
340 IF O(3)=99 THEN PRINT"Oxygen Remaining: "; T2; "Minutes"
345 IF P<18 OR P>21 THEN 360
350 IF O(4)=P AND F4=1 AND T0<>0 THEN 360
355 IF O(4)<>99 OR F4=0 OR T0=0 THEN 420
360 PRINT"Present Location Status:  You are"
365 FOR I=M(P,7)TO M(P,8):PRINT T$(I):NEXT I
370 PRINT CHR$(27);")";"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
375 PRINT CHR$(27);"("
380 IF P<>2 THEN 395
385 IF O(4)<>99 THEN 395 ELSE O(4)=100:C=C-1
390 PRINT"You dropped your illuminator!":PRINT"You can't retrieve it!"
395 FOR I=1 TO 14
400 IF O(I)<>P THEN 415
405 GOSUB 1660
410 PRINT"There is ";B$; " here."
415 NEXT I:GOTO 895
420 INPUT B$:I=0:OF=0
425 IF LEN(B$)<>1 THEN 545
430 IF B$="N"OR B$="n"OR B$="NORTH"OR B$="north"THEN I=1
435 IF B$="S"OR B$="s"OR B$="SOUTH"OR B$="south"THEN I=2
440 IF B$="E"OR B$="e"OR B$="EAST"OR B$="east"THEN I=3
445 IF B$="W"OR B$="w"OR B$="WEST"OR B$="west"THEN I=4
450 IF B$="U"OR B$="u"OR B$="UP"OR B$="up"THEN I=5
455 IF B$="D"OR B$="d"OR B$="DOWN"OR B$="down"THEN I=6
460 IF B$="Q"OR B$="q"OR B$="QUIT"OR B$="quit"THEN 1275
465 IF I=0 THEN 660
470 IF P=37 AND O(5)=35 AND I=2 THEN Q=35:GOTO 525
475 IF P=35 AND O(5)=35 AND I=1 THEN Q=37:GOTO 525
480 IF P=14 AND R=2 AND I=5 THEN Q=2:GOTO 525
485 IF M(P,I)>21 AND BG=1 THEN 540
490 IF M(P,I)=0 THEN 540
495 IF M(P,I)=99 THEN 1250
500 Q=M(P,I)
505 IF P=12 THEN 1290
510 IF P=13 AND M(P,I)=22 THEN 1335
515 IF P=22 THEN 1355
520 IF P=29 THEN 1380
525 R=P:P=Q:IF P=35 THEN V=V+1
530 IF P<>23 THEN 155
535 IF O(4)<>99 OR F4=0 THEN 1250 ELSE 155
540 PRINT"You cannot go in that direction!":GOTO 420
545 C$=LEFT$(B$,3)
550 IF C$="LOO"OR C$="loo"THEN 670
555 IF C$="FIN"OR C$="fin"THEN 730
560 IF C$="GET"OR C$="get"THEN 960
565 IF C$="USE"OR C$="use"THEN 690
570 IF C$="DRO"OR C$="dro"THEN 1130
575 IF C$="OFF"OR C$="off"THEN OF=1:GOTO 750
580 IF C$="ON "OR C$="on "THEN OF=0:GOTO 750
585 IF C$="INV"OR C$="inv"THEN 1205
590 IF C$="QUI"OR C$="qui"THEN 1275
595 IF C$="TRA"OR C$="tra"THEN 845
600 IF C$="DIG"OR C$="dig"THEN 885
605 IF C$="TEL"OR C$="tel"THEN 715
610 IF C$="FUE"OR C$="fue"THEN 1520
615 IF C$="REA"OR C$="rea"THEN 1605
620 IF C$="FIX"OR C$="fix"THEN 1430
625 IF C$="BLA"OR C$="bla"THEN 1545
630 IF C$="UP"OR C$="up"THEN I=5:GOTO 480
635 IF C$="DOW"OR C$="dow"THEN I=6:GOTO 490
640 IF C$="NOR"OR C$="nor"THEN I=1:GOTO 490
645 IF C$="SOU"OR C$="sou"THEN I=2:GOTO 490
650 IF C$="EAS"OR C$="eas"THEN I=3:GOTO 490
655 IF C$="WES"OR C$="wes"THEN I=4:GOTO 490
660 PRINT"INVALID COMMAND!":GOTO 420
665 PRINT"I CANNOT PROCESS YOUR REQUEST!":GOTO 420
670 IF P<18 OR P>21 THEN 360
675 IF O(4)=P AND F4=1 AND T0<>0 THEN 360
680 IF O(4)<>99 OR F4=0 OR T0=0 THEN PRINT"Can't see in the dark!":GOTO 420
685 GOTO 360
690 GOSUB 1735:IF I=0 THEN 975
695 IF I<0 THEN 980
700 IF O(I)<>99 THEN 1180
705 ON I GOTO 1345,1310,775,1370,840,1470,1440,845,1520,840,785,
    1400,1125,785,840,1125,840
710 GOTO 975
715 PRINT"Enter Code Number"
720 INPUT TP:P=(TP-P)/100:IF P<1 OR P>42 THEN PRINT"Lost in LIMBO":GOTO 1270
725 GOTO 155
730 IF P=14 AND R=2 THEN PRINT"A square metal object":MF=1:O(15)=14
735 IF P=30 THEN PRINT"A metal cylinder":MF=1:O(16)=30
740 IF MF=0 THEN PRINT"Can't find anything!"
745 GOTO 420
750 GOSUB 1735
755 IF I=17 THEN 800
760 IF O(I)<>99 THEN 1180
765 ON I GOTO 840,840,775,820,840,840,840,830,840,840,785,840,840,
    790,840,840
770 GOTO 975
775 IF OF=1 THEN F0=0ELSE F0=1
780 PRINT"OK!":GOTO 420
785 IF OF=1 THEN P3=0ELSE P3=1
790 IF OF=1 THEN P4=0ELSE P4=1
795 PRINT"OK!":GOTO 420
800 IF P<>21 THEN 1510
805 IF O(4)<>99 OR F4=0 OR T0=0 THEN 1495
810 IF OF=1 THEN F8=0ELSE F8=1
815 PRINT"OK!":GOTO 420
820 IF OF=1 THEN F4=0ELSE F4=1
825 PRINT"OK!":GOTO 420
830 IF OF=1 THEN F3=0ELSE F3=1
835 PRINT"OK!":GOTO 420
840 PRINT"It can't be used that way":GOTO 420
845 IF P<>36 AND P<>35 THEN 665
850 IF O(8)<>P AND O(8)<>99 THEN 665
855 IF F3=0 THEN 665
860 IF O(8)=35 THEN O(8)=36:P=36:GOTO 880
865 IF O(8)=36 THEN O(8)=35:V=V+1:P=35
870 IF O(8)=99 AND P=35 THEN P=36:GOTO 880
875 IF O(8)=99 AND P=36 THEN P=35:V=V+1
880 PRINT"Beaming in process.":FOR I=1 TO 450:NEXT I:GOTO 155
885 IF P<>10 THEN 665
890 O(9)=10:Z=Z+1:GOTO 380
895 IF O(5)=37 THEN O(5)=35
900 IF O(5)=29 THEN O(5)=37
905 IF O(5)=28 THEN O(5)=29
910 IF O(5)=42 THEN O(5)=28
915 IF O(5)=41 THEN O(5)=42
920 IF O(5)=27 THEN O(5)=41
925 IF O(5)=25 THEN O(5)=27
930 IF O(5)=35 THEN M(37,2)=35
935 IF O(5)<>32 THEN 420
940 IF P<>32 THEN 420
945 O(5)=25:IF O(13)=99 THEN 420
950 PRINT"The robot fails to recognize you."
955 PRINT"It fires its weapon at you!!":GOTO 1270
960 GOSUB 1735
965 IF I>0 THEN 985
970 IF I<0 THEN 980
975 PRINT "I don't recognize "; RIGHT$(B$,LEN(B$)-J);".":GOTO 420
980 PRINT"What item?":GOTO 420
985 IF O(I)<>P THEN 1075
990 IF CX=1 AND C>2 THEN 1080
995 IF CX=1 THEN 1005
1000 IF C>3 THEN 1080
1005 IF I=5 THEN 1085
1010 IF I=9 THEN 1090
1015 IF I=10 THEN 1100
1020 IF I=11 THEN 1110
1025 IF I=14 THEN 1115
1030 IF P=14 AND I=15 AND MF=1 THEN I=6:GOTO 1060
1035 IF I<>16 THEN 1055
1040 IF P<>30 OR MF=0 THEN 1075
1045 IF T2<156 THEN T2=T2+30
1050 GOTO 1065
1055 IF I=17 THEN GOTO 1085
1060 C=C+1:O(I)=99
1065 PRINT"O.K."
1070 GOTO 420
1075 PRINT"I don't see any "; RIGHT$(B$,LEN(B$)-J);" here!":GOTO 420
1080 PRINT"You can't carry any more!":GOTO 420
1085 PRINT"You can't carry it!":GOTO 420
1090 IF Z<2 THEN 1060
1095 PRINT"The two crystals reacted with your power supply!":GOTO 1245
1100 PRINT"You can't get the message,"
1105 PRINT"it's on the terminal screen.":GOTO 420
1110 IF O(14)=99 THEN 1120ELSE 1060
1115 IF O(11)=99 THEN 1120ELSE 1060
1120 PRINT"You can't have more than one power supply, sorry.":GOTO 420
1125 PRINT"How?":GOTO 420
1130 GOSUB 1735
1135 IF I>0 THEN 1150
1140 IF I<0 THEN 980
1145 GOTO 975
1150 IF O(I)<>99 THEN 1180
1155 IF I=9 THEN Z=Z-1
1160 C=C-1:O(I)=P
1165 IF I=11 THEN 1185
1170 IF I=14 THEN 1185
1175 GOTO 1065
1180 PRINT"You don't have "; RIGHT$(B$,LEN(B$)-J);"!":GOTO 420
1185 IF P<22 THEN 1230
1190 IF P=38 THEN 1230
1195 IF F9=1 THEN 1230
1200 GOTO 1065
1205 FOR I=1 TO 14
1210 IF O(I)<>99 THEN 1225
1215 GOSUB 1660
1220 PRINT"You have "; B$; "."
1225 NEXT I:GOTO 420
1230 PRINT"You have no power!"
1235 PRINT"You have frozen to death!":GOTO 1270
1240 PRINT"Oxygen required here.  None available.":GOTO 1270
1245 PRINT"A nuclear detonation has just occured.":GOTO 1270
1250 PRINT"You have fallen to your death!":GOTO 1270
1255 PRINT"You have been zapped by the laser.":GOTO 1270
1260 PRINT"The moon base has just been destroyed by a large asteroid!":BG=1
1265 IF P<22 THEN 285
1270 PRINT"You have failed to survive."
1275 INPUT"Do you wish to try again";D$
1280 IF D$="Y"OR D$="y"OR D$="YES"OR D$="yes"THEN PRINT CHR$(27);CHR$(69):GOTO 130
1285 END
1290 IF M(P,I)<>13 THEN 525
1295 IF F2<>0 THEN 525
1300 PRINT"There is a metor shower!"
1305 PRINT"Your space suit has developed a leak!":F2=2:GOTO 420
1310 IF F2<>2 THEN 840
1315 PRINT"Your suit is now sealed.":F2=1:O(2)=0:C=C-1:GOTO 525
1320 IF F0=1 THEN T2=T2-20
1325 IF T2<0 THEN T2=5
1330 GOTO 180
1335 IF BG=1 THEN PRINT"The door is blocked by rubble!":GOTO 420
1340 IF F1=0 THEN PRINT"The shed is locked!":F1=2:GOTO 420
1345 IF F1=2 THEN PRINT"You are in the shed air lock":F1=0:GOTO 525
1350 IF F1=3 THEN PRINT:PRINT"Authority Confirmed!":PRINT:GOTO 1580
1355 IF M(P,I)<>23 THEN 525
1360 IF F4>0 THEN 525
1365 PRINT"It is dangerous to proceed in the dark!":F4=2:GOTO 420
1370 IF F4<>2 THEN F4=1:GOTO 420
1375 PRINT"The shaft is now illuminated.":F4=1:GOTO 525
1380 IF M(P,I)<>37 THEN 525
1385 IF F3=1 AND O(12)=37 THEN 525
1390 PRINT"There is a laser beam here."
1395 PRINT"Passage is not possible when beam is present.":F3=2:GOTO 420
1400 IF F3<>2 THEN 1125
1405 PRINT"The beam is deflected.":F3=1:C=C-1:O(12)=37:GOTO 525
1410 IF F5=1 THEN 285
1415 IF O(6)=0 THEN O(6)=14
1420 M(2,8)=M(2,7):M(14,8)=M(14,7):M(14,4)=2:F5=1:GOTO 285
1425 IF F6=0 THEN 1245ELSE 285
1430 GOSUB 1735:IF I=7 THEN 1470
1435 IF I<>17 THEN 1515
1440 IF O(7)<>99 THEN 1500
1445 IF O(4)<>99 AND O(4)<>I THEN 1495
1450 IF F4=0 OR T0=0 THEN 1495
1455 IF P<>20 THEN 1510
1460 IF F7<>1 THEN F6=1
1465 PRINT"The engine is now repaired.":F7=2:O(7)=0:C=C-1:GOTO 420
1470 IF O(6)<>99 THEN 1500
1475 IF O(7)<>99 THEN 1505
1480 IF P<18 OR P>21 THEN 1490
1485 IF O(4)<>99 OR F4=0 OR T0=0 THEN 1495
1490 PRINT"Motor is now regulated.":F7=1:O(6)=0:C=C-1:GOTO 420
1495 PRINT"You can't see to do it!":GOTO 420
1500 PRINT"You have nothing to do it with!":GOTO 420
1505 PRINT"There is nothing to do it to!":GOTO 420
1510 PRINT"You can't do it from here!":GOTO 420
1515 PRINT"You can't fix THAT!":GOTO 420
1520 IF P<>19 THEN 1510
1525 IF O(9)<>99 THEN 1500
1530 IF O(4)<>99 OR F4=0 OR T0=0 THEN 1495
1535 O(9)=98:C=C-1
1540 PRINT"Fuel is now loaded.":GOTO 420
1545 IF P<>21 THEN 1510
1550 IF O(9)<>98 THEN 1600
1555 IF F7=2 THEN 1565
1560 PRINT"Repairs not yet completed.":GOTO 420
1565 IF F8=0 THEN 665
1570 PRINT:PRINT"Spacecraft repairs completed.":PRINT
1575 PRINT"Insert `Command Authority' in slot!":F1=3:GOTO 420
1580 IF F6=1 THEN PRINT"Your ships engine has malfunctioned!":GOTO 1245
1585 PRINT"Congratulations, you have just blasted"
1590 PRINT"off and are now on your way to earth."
1595 PRINT"Your escape time:"; T1; "Minutes.":GOTO 1275
1600 PRINT"Your space craft has no fuel!":GOTO 420
1605 IF P<>35 THEN 1510
1610 IF V>6 THEN V=0
1615 ON V GOTO 1620,1625,1630,1635,1640,1645
1620 PRINT T$(48):PRINT T$(49):GOTO 420
1625 PRINT T$(50):GOTO 420
1630 PRINT T$(51):GOTO 420
1635 PRINT T$(0):GOTO 420
1640 PRINT T$(23):GOTO 420
1645 IF O(5)<>35 THEN PRINT"The message is garbled!":GOTO 420
1650 PRINT T$(21)
1655 PRINT T$(22):GOTO 420
1660 IF I=1 THEN B$=T$(52)
1665 IF I=2 THEN B$=T$(53)
1670 IF I=3 THEN B$=T$(54)
1675 IF I=4 THEN B$=T$(55)
1680 IF I=5 THEN B$=T$(56)
1685 IF I=6 THEN B$=T$(57)
1690 IF I=7 THEN B$=T$(58)
1695 IF I=8 THEN B$=T$(59)
1700 IF I=9 THEN B$=T$(60)
1705 IF I=10 THEN B$=T$(61)
1710 IF I=11 THEN B$=T$(62)
1715 IF I=12 THEN B$=T$(63)
1720 IF I=13 THEN B$=T$(64)
1725 IF I=14 THEN B$=T$(65)
1730 RETURN
1735 FOR J=1 TO LEN(B$):IF MID$(B$,J,1)=" "THEN 1745
1740 NEXT J:I=-1:RETURN
1745 C$=MID$(B$,J+1,3):I=0
1750 IF C$="KEY"OR C$="key"THEN I=1
1755 IF C$="SEA"OR C$="sea"THEN I=2
1760 IF C$="OXY"OR C$="oxy"THEN I=3
1765 IF C$="MOD"OR C$="mod"THEN I=3
1770 IF C$="ILL"OR C$="ill"THEN I=4
1775 IF C$="ROB"OR C$="rob"THEN I=5
1780 IF C$="REG"OR C$="reg"THEN I=6
1785 IF C$="MOT"OR C$="mot"THEN I=7
1790 IF C$="TRA"OR C$="tra"THEN I=8
1795 IF C$="DIL"OR C$="dil"THEN I=9
1800 IF C$="CRY"OR C$="cry"THEN I=9
1805 IF C$="MES"OR C$="mes"THEN I=10
1810 IF C$="UNI"OR C$="uni"THEN I=11
1815 IF C$="MIR"OR C$="mir"THEN I=12
1820 IF C$="BAD"OR C$="bad"THEN I=13
1825 IF C$="PAC"OR C$="pac"THEN I=14
1830 IF C$="OBJ"OR C$="obj"THEN I=15
1835 IF C$="CYL"OR C$="cyl"THEN I=16
1840 IF C$="ENG"OR C$="eng"THEN I=17
1845 RETURN
1850 DATA"I|xve$s|}kir$wxsvih$mr$wxexmsr"
1855 DATA"ex$Qevi$Wivirmxexmw2"
1860 DATA"sr$vmq$sj$Tswmhsrmyw2"
1865 DATA"Xsxep$hevoriww$xs$xli$Iewx2"
1870 DATA"X{m|x$He{iw$erh$Tpmrmyw2"
1875 DATA"ex$teww$mr$Leiqyw$Qsyrxemrw"
1880 DATA"ex$gvexiv$Qermpyw2"
1885 DATA"ex$Qevi$Zetsvyq2"
1890 DATA"ex$Qx2$Iyhs|yw2"
1895 DATA"mrwmhi$Evmwxsxipiw2"
1900 DATA"mr$glerkmrk$evie2"
1905 DATA"mr$Pegyw$Wsqrmsvyq2"
1910 DATA"ex$Pegyw$Qsvxmw2"
1915 DATA"xli$wyvjegi$mw$ziv}$wsjx$livi2"
1920 DATA"mr$Qevi$Mqfvmyq2"
1925 DATA"ex$xli$fewi$sj$Tpexs$gvexiv2$$E"
1930 DATA"wlmr}$sfnigx$mw$wiir${iwx2"
1935 DATA"fijsvi$e$qixep$wlih2$$E$wmkr$viehw"
1940 DATA"Zirxmpexmsr$wlejx$'62"
1945 DATA"iewx$sj$Qevi$Wivirmxexmw2"
1950 DATA"xlivi$mw$xsxep$hevoriww2"
1955 DATA"$$$$$$$$$$$$$$$$XIPITSVX$MRWXVYGXMSRW"
1960 DATA"$$$$We}$8$ryqfivw>$5wx$6$1$xlivi0$pewx$6$1$livi"
1965 DATA"]sy$gsyph$wezi$xmqi$f}$xipitsvxmrk$mj$}sy$ori{$ls{%"
1970 DATA"mr$xli$emv$psgo$gleqfiv$sj$xli$wlmt"
1975 DATA"mr$xli$ejx$wlejx$gevks$erh$jyip$wxsveki$vssq2"
1980 DATA"mr$xli$irkmri$vssq$sj$xli$wtegigvejx2"
1985 DATA"mr$xli$wlmt+w$gsrxvsp$vssq2"
1990 DATA"mr$er$emv$psgo$gleqfiv2"
1995 DATA"mrwmhi$e$zirxppexsv$wlih2$$E$pehhiv"
2000 DATA"piehw$hs{r$mrxs$e$pevki$qixep$wlejx2"
2005 DATA"mr$e$zirxmppexsv$tewweki2"
2010 DATA"ex$e$zirxmppexsv$stirmrk2$$Xlvsykl$xli"
2015 DATA"stirmrk$e$pmx$tewweki{e}$ger$fi$wiir2"
2020 DATA"mr$e$pmklxih$wtegi$wxexmsr$gsvvmhsv2"
2025 DATA"mr$xli$wtegi$wxexmsr$mrjmvqev}2"
2030 DATA"mr$xli$vigviexmsr$vssq$erh$pmfvev}2"
2035 DATA"mr$xli$qiww$lepp2$$Eferhsrih$jssh$xve}w"
2040 DATA"evi$wxmpp$sr$xli$xefpiw2"
2045 DATA"mr$xli$wxsveki$vssq$erh$wyttp}$evie2"
2050 DATA"mr$xli$wpiitmrk$uyevxivw2"
2055 DATA"mr$er$ipizexsv$ex$wyfwyvjegi$pizip2"
2060 DATA"mr$er$ipizexsv$ex$wyvjegi$pizip2"
2065 DATA"mr$xli$wxexmsr$gsrxvsp$girxiv2"
2070 DATA"mr$xli$xverwtsvxiv$vssq2"
2075 DATA"mr$xli$wtegi$wxexmsr$pefsvexsv}2"
2080 DATA"mr$xli$lerkiv$evie2$$Xli$peyrgl$evie"
2085 DATA"mw$nywx$wsyxl$sj$livi2"
2090 DATA"Rygpiev$vikypexsv$psgexih$wsqi{livi$iewx"
2095 DATA"sj$wtegi$wxexmsr$sr$qssr+w$wyvjegi"
2100 DATA"Psgep$jyip$wsyvgi1$Hmpmxlmyq$gv}wxep"
2105 DATA"Hmpmxlmyq$jsyrh$mr$xli$kvsyrh"
2110 DATA"er$ipigxvsrmg$oi}"
2115 DATA"wieperx"
2120 DATA"er$s|}kir$qshypi"
2125 DATA"er$mppyqmrexsv"
2130 DATA"e$vsfsx"
2135 DATA"e$rygpiev$qsxsv$vikypexsv"
2140 DATA"e$ywih$rygpiev$qsxsv"
2145 DATA"e$xverwtsvxiv$yrmx"
2150 DATA"e$hmpmxlmyq$gv}wxep"
2155 DATA"e$gsqtyxiv$qiwweki"
2160 DATA"e$ts{iv$yrmx"
2165 DATA"e$qmvvsv"
2170 DATA"e$gshih$fehki"
2175 DATA"e$ts{iv$tego"
2180 DATA 07,04,02,15,00,00,01,01
2185 DATA 09,03,14,01,00,00,02,03
2190 DATA 02,05,14,04,00,00,04,04
2195 DATA 01,00,03,00,00,06,05,05
2200 DATA 03,00,14,06,00,00,06,06
2205 DATA 00,00,05,00,04,00,07,07
2210 DATA 08,01,09,11,00,00,08,08
2215 DATA 00,07,10,00,00,00,09,09
2220 DATA 10,02,14,07,00,00,11,11
2225 DATA 00,09,14,08,00,00,12,13
2230 DATA 12,15,07,16,17,16,14,14
2235 DATA 00,11,00,13,00,00,15,16
2240 DATA 00,16,12,22,00,00,17,18
2245 DATA 99,99,99,99,00,00,19,20
2250 DATA 11,18,01,00,00,17,14,14
2255 DATA 16,17,11,16,11,16,14,14
2260 DATA 16,17,00,17,15,16,14,14
2265 DATA 15,19,00,00,00,00,24,24
2270 DATA 18,00,00,20,00,00,25,25
2275 DATA 00,00,19,00,21,00,26,26
2280 DATA 00,00,00,00,00,20,27,27
2285 DATA 00,00,13,00,00,23,29,30
2290 DATA 24,00,00,00,22,00,31,31
2295 DATA 25,23,00,00,00,25,32,33
2300 DATA 27,26,33,32,24,00,34,34
2305 DATA 25,00,30,31,00,00,34,34
2310 DATA 00,25,41,34,00,00,34,34
2315 DATA 00,29,42,36,00,00,34,34
2320 DATA 28,38,40,37,00,00,34,34
2325 DATA 00,00,00,26,00,00,35,35
2330 DATA 00,00,26,00,00,00,36,36
2335 DATA 00,00,25,00,00,00,37,38
2340 DATA 00,00,00,25,00,00,40,40
2345 DATA 00,00,27,00,00,00,39,39
2350 DATA 00,00,00,00,00,24,43,43
2355 DATA 00,00,28,00,00,00,44,44
2360 DATA 00,00,29,00,00,00,45,45
2365 DATA 29,00,39,00,00,00,46,47
2370 DATA 40,00,00,38,00,16,28,28
2375 DATA 00,39,00,29,00,00,10,10
2380 DATA 00,00,00,27,42,00,41,41
2385 DATA 00,00,00,28,00,41,42,42
00,00,38,00,16,28,28
2375 DATA 00,39,00,29,00,00,10,10
2380 DATA 00,00,00,27,42,00,41,41
23