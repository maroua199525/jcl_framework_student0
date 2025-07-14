#!/bin/bash
#########################################################################
# Environment Validation Script - Check if all datasets are properly set up
# Ensures consistent environment before students start exercises
#########################################################################

echo "🔍 JCL Framework - Environment Validation"
echo "=========================================="
echo "Checking if all required datasets are properly allocated..."
echo ""

# Check if dataset manager is working
if ! ./dataset_manager.sh list >/dev/null 2>&1; then
    echo "❌ Dataset manager not working. Run './setup_datasets.sh' first"
    exit 1
fi

# Required datasets for exercises
REQUIRED_DATASETS=(
    "TRANSACTIONS.INPUT"
    "TRANSACTIONS.VALIDATED"
    "ACCOUNTS.MASTER"
    "CUSTOMERS.MASTER"
    "STUDENT.INPUT.DATA"
    "STUDENT.OUTPUT.DATA"
)

echo "📋 Checking required datasets..."
MISSING_COUNT=0

for dataset in "${REQUIRED_DATASETS[@]}"; do
    if ./dataset_manager.sh list | grep -q "$dataset"; then
        echo "✅ $dataset - OK"
    else
        echo "❌ $dataset - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "📁 Checking data files..."
DATA_FILES=(
    "datasets/transactions_input.dat"
    "datasets/accounts_master.dat"
    "datasets/customers_master.dat"
)

for file in "${DATA_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ $file - OK ($(wc -l < "$file") lines)"
    else
        echo "❌ $file - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "🔧 Checking JCL programs..."
PROGRAMS=(
    "programs/batch_validator.cbl"
    "programs/account_updater.cbl"
    "programs/customer_reporter.cbl"
)

for program in "${PROGRAMS[@]}"; do
    if [ -f "$program" ]; then
        echo "✅ $program - OK"
    else
        echo "❌ $program - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "📄 Checking JCL jobs..."
JOBS=(
    "jobs/batch_validator.jcl"
    "jobs/account_updater.jcl"
    "jobs/customer_reporter.jcl"
)

for job in "${JOBS[@]}"; do
    if [ -f "$job" ]; then
        echo "✅ $job - OK"
    else
        echo "❌ $job - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "📊 Validation Summary:"
echo "====================="
if [ $MISSING_COUNT -eq 0 ]; then
    echo "🎉 Environment is READY!"
    echo "✅ All datasets allocated"
    echo "✅ All data files present"
    echo "✅ All programs available"
    echo "✅ All JCL jobs ready"
    echo ""
    echo "🚀 Students can now start exercises!"
    echo ""
    echo "Quick test commands:"
    echo "  ./scheduler.sh submit jobs/batch_validator.jcl"
    echo "  ./scheduler.sh process"
    echo "  cat /tmp/jcl_sim/sysout/BATCHVAL_STEP1.log"
else
    echo "⚠️  Environment has $MISSING_COUNT missing components"
    echo "🔧 Run './setup_datasets.sh' to fix missing datasets"
    echo "📚 Check documentation for missing programs/jobs"
    exit 1
fi