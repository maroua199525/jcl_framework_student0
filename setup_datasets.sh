#!/bin/bash
#########################################################################
# Dataset Setup Script - Pre-allocate all datasets for JCL exercises
# This ensures all students have identical datasets and consistent results
#########################################################################

echo "🚀 JCL Framework - Dataset Setup"
echo "================================="
echo "Setting up all required datasets for consistent student experience..."
echo ""

# Initialize dataset manager
./dataset_manager.sh init

echo "📁 Allocating Banking Transaction Datasets..."
# Core banking datasets
./dataset_manager.sh allocate TRANSACTIONS.INPUT PS 2048 100
./dataset_manager.sh allocate TRANSACTIONS.VALIDATED PS 2048 100  
./dataset_manager.sh allocate TRANSACTIONS.BACKUP PS 2048 100

echo "📁 Allocating Account Management Datasets..."
./dataset_manager.sh allocate ACCOUNTS.MASTER PS 1024 50
./dataset_manager.sh allocate ACCOUNTS.UPDATED PS 1024 50
./dataset_manager.sh allocate ACCOUNTS.BACKUP PS 1024 50

echo "📁 Allocating Customer Datasets..."
./dataset_manager.sh allocate CUSTOMERS.MASTER PS 1024 50
./dataset_manager.sh allocate CUSTOMERS.REPORT PS 2048 100
./dataset_manager.sh allocate CUSTOMERS.BACKUP PS 1024 50

echo "📁 Allocating Student Exercise Datasets..."
./dataset_manager.sh allocate STUDENT.INPUT.DATA PS 1024 50
./dataset_manager.sh allocate STUDENT.OUTPUT.DATA PS 1024 50
./dataset_manager.sh allocate BATCH.INPUT.DATA PS 2048 100
./dataset_manager.sh allocate BATCH.OUTPUT.DATA PS 2048 100

echo "📁 Allocating Audit and Security Datasets..."
./dataset_manager.sh allocate AUDIT.LOG PS 4096 200
./dataset_manager.sh allocate SECURITY.REPORT PS 2048 100
./dataset_manager.sh allocate ERROR.LOG PS 1024 50

echo "📁 Allocating GDG (Generation Data Groups)..."
./dataset_manager.sh allocate DAILY.REPORTS.GDG PS 2048 100
./dataset_manager.sh allocate MONTHLY.BACKUP.GDG PS 4096 500

echo "📁 Copying Sample Data to Datasets..."
# Copy sample data to ensure consistent starting point
if [ -f "data/transactions.txt" ]; then
    cp data/transactions.txt datasets/transactions_input.dat
    echo "✓ Sample transactions copied to TRANSACTIONS.INPUT"
fi

if [ -f "data/accounts.txt" ]; then
    cp data/accounts.txt datasets/accounts_master.dat
    echo "✓ Sample accounts copied to ACCOUNTS.MASTER"
fi

if [ -f "data/customers.txt" ]; then
    cp data/customers.txt datasets/customers_master.dat
    echo "✓ Sample customers copied to CUSTOMERS.MASTER"
fi

echo ""
echo "📋 Dataset Allocation Summary:"
echo "=============================="
./dataset_manager.sh list

echo ""
echo "✅ Dataset setup complete!"
echo "📚 All students now have identical datasets for consistent results"
echo ""
echo "🎯 Ready for JCL exercises:"
echo "  • Task 1: Transaction Validator (TRANSACTIONS.INPUT → TRANSACTIONS.VALIDATED)"
echo "  • Task 2: Account Updater (ACCOUNTS.MASTER + TRANSACTIONS.VALIDATED)"
echo "  • Task 3: Customer Reporter (CUSTOMERS.MASTER → CUSTOMERS.REPORT)"
echo ""
echo "💡 Students can now run jobs without manual dataset allocation!"