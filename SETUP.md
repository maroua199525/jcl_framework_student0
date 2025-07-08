# 🛠️ Setup Instructions

## One-Time Setup

### 1. Clone and Initialize
```bash
git clone <your-repo-url>
cd jcl-framework-student
chmod +x *.sh
./scheduler.sh init
./dataset_manager.sh init
```

### 2. Verify Installation
```bash
./demo.sh
```

### 3. Start Learning
```bash
# View the main example
cat jobs/hello_world.jcl
cat programs/hello_world.cbl

# Submit your first job
./scheduler.sh submit jobs/hello_world.jcl

# Check results
./scheduler.sh status
```

## What's Included

- **Core Framework:** JCL parser, scheduler, dataset manager
- **Example Job:** `jobs/hello_world.jcl`
- **Example Program:** `programs/hello_world.cbl`
- **4 Progressive Exercises:** From basic to advanced
- **Reference Materials:** Sample programs and JCL jobs

## Next Steps

1. Read `README.md` for overview
2. Follow `QUICK_START.md` for detailed guidance
3. Start with `student_exercises/exercise_01_basic_file_processing.md`

---
**You're ready to learn enterprise batch processing!** 🚀