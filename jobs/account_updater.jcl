//ACCUPDT  JOB CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//* Task 2: Account Updater
//* Student: [YOUR NAME]
//* 
//* This job processes validated transactions:
//* - Reads validated transaction file
//* - Reads account master file
//* - Updates account balances
//* - Reports processing statistics
//*
//STEP1    EXEC PGM=ACCOUNT-UPDATE
//ACCOUNTS DD   DSN=ACCOUNTS.MASTER,DISP=SHR
//TRANSIN  DD   DSN=TRANSACTIONS.VALIDATED,DISP=SHR
//SYSOUT   DD   SYSOUT=*