//*
//*********************************************************************
//*   THIS IS THE BMS MAP DEFINITION
//*   WHICH IS USED TO ASSEMBLE PHYSICAL AND SYMBOLIC MAPS
//*********************************************************************
//*
//*   TO CUSTOMIZE THIS SCRIPT YOU WILL NEED TO CHANGE THE FOLLOWING:
//*
//*
//*       DSCTLIB - SET THIS TO THE NAME OF THE DATASET WHICH CONTAINS
//*                 YOUR CICS/COBOL APPLICATION CODE.
//*
//*       MAPNAME - SET THIS TO THE MAP SET NAME PROVIDED BY YOUR
//*                 INSTRUCTOR (SHOULD BE DURXXX# WHERE XXX ARE THE LAST
//*                 THREE CHARACTERS IN YOUR USER ID AND # IS THE NUMBER
//*                 OF THE MAP)
//*
//*       BMSLIB  - SET THIS TO THE NAME OF THE DATASET WHICH CONTAINS
//*                 YOUR BMS CODE TO BE INTERPRETED
//*
//*********************************************************************
//*
//KC03I8DA JOB 3U391710000034,MSGCLASS=Q,MSGLEVEL=(1,1)
//*
//CICSPROC JCLLIB ORDER=(TSOECCC.CICSTS12.PROCLIB)
//MAPSET EXEC DFHMAPS,OUTC='*',RMODE=24,
//*
//  BMSLIB='KC03I8D.DEMO.CICS.BMS',
//  MAPNAME='DCI8DMC',
//  DSCTLIB='KC03I8D.DEMO.CICS.COBOL',
//*
//*** DO NOT CHANGE MAPLIB OR INDEX!!!
//  MAPLIB='TSOECCC.CICSTS12.STUDENT.LOADLIB',
//  INDEX='DFH310.CICS'
//COPY.SYSUT1 DD DSN=&BMSLIB(&MAPNAME),DISP=SHR
//ASMDSECT.SYSPUNCH DD DSN=&DSCTLIB(&MAPNAME)