//BATCHVAL JOB CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//* Task 1: Transaction Batch Validator
//* Student: [YOUR NAME]
//* 
//* This job validates banking transactions:
//* - Reads transaction input file
//* - Validates transaction types
//* - Writes valid transactions to output
//* - Reports validation statistics
//*
//STEP1    EXEC PGM=BATCH-VALIDATOR
//TRANSIN  DD   DSN=TRANSACTIONS.INPUT,DISP=SHR
//TRANSOUT DD   DSN=TRANSACTIONS.VALIDATED,DISP=(NEW,CATLG,DELETE)
//SYSOUT   DD   SYSOUT=*