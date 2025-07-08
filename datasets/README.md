# 🚀 Open Source JCL Alternative - Student Edition

## Overview
This is an **educational framework** that simulates IBM mainframe JCL (Job Control Language) using open-source tools. Perfect for learning enterprise batch processing concepts without expensive mainframe access.

## 🎯 What You'll Learn
- Enterprise batch processing concepts
- Job Control Language (JCL) syntax and structure
- COBOL program integration
- Dataset management and file processing
- Job scheduling and dependencies
- Error handling and recovery

## ⚡ Quick Start

### 1. Clone and Setup
```bash
git clone <your-repo-url>
cd jcl-framework-student
chmod +x *.sh
```

### 2. Run the Demo
```bash
./demo.sh
```

### 3. Try Your First Job
```bash
# Submit the hello world job
./scheduler.sh submit jobs/hello_world.jcl

# Check status
./scheduler.sh status

# View results
./scheduler.sh history
```

## 📁 Project Structure
```
├── README.md                 # This file
├── QUICK_START.md            # Detailed setup guide
├── demo.sh                   # Interactive demonstration
├── jcl_parser.sh            # Core JCL parser (IBM → Bash)
├── scheduler.sh             # Job scheduler (simulates TWS/OPC)
├── dataset_manager.sh       # Dataset management (simulates VSAM/SMS)
├── jobs/
│   └── hello_world.jcl      # Example JCL job
├── programs/
│   └── hello_world.cbl      # Example COBOL program
├── student_exercises/       # All practice exercises
├── data/                    # Sample input data
└── datasets/               # Working datasets
```

## 🛠️ Core Components

### JCL Parser (`jcl_parser.sh`)
Converts IBM JCL syntax to executable bash commands:
```jcl
//MYJOB    JOB CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=HELLO-COBOL
//SYSOUT   DD   SYSOUT=*
```

### Job Scheduler (`scheduler.sh`)
Manages job submission and execution:
```bash
./scheduler.sh submit jobs/hello_world.jcl
./scheduler.sh status
./scheduler.sh history
```

### Dataset Manager (`dataset_manager.sh`)
Handles file allocation and management:
```bash
./dataset_manager.sh allocate MY.DATA.SET PS 1024 100
./dataset_manager.sh list
./dataset_manager.sh delete MY.DATA.SET
```

## 📚 Learning Path

1. **Start Here:** Read this README and run `./demo.sh`
2. **Setup:** Follow `QUICK_START.md` for detailed setup
3. **Practice:** Work through exercises in `student_exercises/`
4. **Advanced:** Explore enterprise concepts and real-world scenarios

## 🎓 Student Exercises

| Exercise | Topic | Difficulty |
|----------|-------|------------|
| Exercise 1 | Basic File Processing | Beginner |
| Exercise 2 | Banking Workflow | Intermediate |
| Exercise 3 | Security Audit | Intermediate |
| Exercise 4 | Monthly Reporting | Advanced |

## 💡 Key Features

- **Authentic JCL Syntax** - Learn real IBM JCL commands
- **Real COBOL Execution** - Compiles and runs actual COBOL programs (not just simulation)
- **Enterprise Logging** - SYSOUT capture and detailed execution logs
- **Enterprise Patterns** - Job dependencies, error handling, audit trails
- **Cost-Effective** - 90% cost reduction vs. mainframe training
- **Portable** - Runs on any Linux/Unix system

## 🔍 **Understanding Execution Logs**

When you run a job, the framework creates detailed logs showing exactly what happens:

```bash
# After running: ./scheduler.sh submit jobs/hello_world.jcl
# View the execution details:
cat /tmp/jcl_sim/sysout/HELLO_STEP1.log
```

**Example output:**
```
Executing COBOL program: hello_world.cbl
Found COBOL program at: programs/hello_world.cbl
Compiling hello_world.cbl...
Compilation successful. Executing...
Hello from COBOL!
```

This demonstrates **real enterprise batch processing** - your COBOL programs are actually compiled and executed, just like in IBM mainframes!

## 🔧 Requirements

- Linux/Unix environment (Ubuntu, CentOS, macOS)
- Bash shell
- Basic command-line knowledge
- Optional: GnuCOBOL compiler for real COBOL execution

## 🚀 Getting Help

- Run `./demo.sh` to see examples in action
- Check `QUICK_START.md` for detailed tutorials
- Read error messages carefully - they're designed to help you learn
- Each exercise includes step-by-step instructions

## 🎯 Success Metrics

After completing this course, you'll be able to:
- ✅ Write and execute JCL jobs
- ✅ Create COBOL programs for batch processing
- ✅ Manage datasets and file processing workflows
- ✅ Handle job dependencies and error conditions
- ✅ Understand enterprise batch processing concepts

---

**Ready to start?** Run `./demo.sh` and begin your journey into enterprise batch processing! 🚀