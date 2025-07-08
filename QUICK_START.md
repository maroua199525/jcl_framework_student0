# 🚀 Quick Start Guide

## Prerequisites
- Linux/Unix environment (Ubuntu, CentOS, macOS, WSL)
- Bash shell (version 4.0+)
- Basic command-line knowledge

## Installation

### 1. Clone the Repository
```bash
git clone <your-repo-url>
cd jcl-framework-student
```

### 2. Make Scripts Executable
```bash
chmod +x *.sh
```

### 3. Initialize the Framework
```bash
# Initialize all components
./scheduler.sh init
./dataset_manager.sh init

# Verify setup
./demo.sh
```

## Your First JCL Job

### Step 1: Examine the Example
```bash
# Look at the sample JCL job
cat jobs/hello_world.jcl

# Look at the COBOL program
cat programs/hello_world.cbl
```

### Step 2: Submit the Job
```bash
# Submit your first job
./scheduler.sh submit jobs/hello_world.jcl

# Check job status
./scheduler.sh status

# View execution history
./scheduler.sh history
```

### Step 3: Understand the Output
```bash
# View the job log
ls -la history/

# Check the latest log file
cat history/*HELLO*.log

# IMPORTANT: View the detailed execution log
cat /tmp/jcl_sim/sysout/HELLO_STEP1.log
```

**What you'll see in the execution log:**
```
Executing COBOL program: hello_world.cbl
Found COBOL program at: programs/hello_world.cbl
Compiling hello_world.cbl...
Compilation successful. Executing...
Hello from COBOL!
```

**This shows:**
- ✅ Real COBOL compilation (using GnuCOBOL compiler)
- ✅ Actual program execution (not simulation)
- ✅ Enterprise logging patterns (SYSOUT capture)
- ✅ Step-by-step job tracking

## Understanding the Framework

### JCL Syntax Basics
```jcl
//JOBNAME  JOB CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEPNAME EXEC PGM=PROGRAM-NAME
//DDNAME   DD   DSN=DATASET.NAME,DISP=SHR
//SYSOUT   DD   SYSOUT=*
```

### Key Commands
```bash
# Job Management
./scheduler.sh submit <jcl-file>    # Submit a job
./scheduler.sh status               # Check running jobs
./scheduler.sh history             # View job history
./scheduler.sh clean               # Clean old jobs

# Dataset Management
./dataset_manager.sh allocate <dsn> <type> <size> <records>
./dataset_manager.sh list          # List all datasets
./dataset_manager.sh delete <dsn>  # Delete a dataset
./dataset_manager.sh copy <src> <dest>  # Copy dataset

# Framework Operations
./demo.sh                          # Run demonstration
./jcl_parser.sh <jcl-file>        # Parse JCL directly
```

## Working with Datasets

### Create a Dataset
```bash
# Allocate a new dataset
./dataset_manager.sh allocate MY.TEST.DATA PS 1024 100

# List datasets to verify
./dataset_manager.sh list
```

### Add Data to Dataset
```bash
# Create sample data
echo "Sample record 1" > datasets/my_test_data.dat
echo "Sample record 2" >> datasets/my_test_data.dat
echo "Sample record 3" >> datasets/my_test_data.dat
```

## Next Steps

### 1. Complete Exercise 1
```bash
# Read the first exercise
cat student_exercises/exercise_01_basic_file_processing.md

# Follow the step-by-step instructions
```

### 2. Explore Advanced Features
- Job dependencies and conditional execution
- Error handling and recovery
- Multi-step workflows
- VSAM and GDG simulation

### 3. Practice with Real Scenarios
- Banking transaction processing
- Report generation
- Data validation and transformation
- Security auditing

## Troubleshooting

### Common Issues

**Permission Denied:**
```bash
chmod +x *.sh
```

**Job Fails to Execute:**
```bash
# Check the log file for details
cat history/*<JOBNAME>*.log

# Verify dataset exists
./dataset_manager.sh list
```

**COBOL Program Not Found:**
```bash
# Framework will simulate COBOL execution
# Check programs/ directory for .cbl files
ls -la programs/
```

### Getting Help
- Each error message includes helpful guidance
- Check log files in `history/` directory
- Review exercise instructions carefully
- Run `./demo.sh` to see working examples

## File Locations

| Component | Location | Purpose |
|-----------|----------|---------|
| JCL Jobs | `jobs/` | Job control language files |
| COBOL Programs | `programs/` | COBOL source code |
| Datasets | `datasets/` | Data files and catalogs |
| Exercises | `student_exercises/` | Practice assignments |
| Logs | `history/` | Job execution logs |
| Queue | `queue/` | Pending jobs |

---

**You're ready to go!** Start with `./demo.sh` and then dive into the exercises. Happy learning! 🎓