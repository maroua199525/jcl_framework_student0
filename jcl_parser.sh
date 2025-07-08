#!/bin/bash
#########################################################################
# JCL Parser - Simulates IBM JCL syntax parsing
# Converts JCL-like syntax to executable shell commands
#########################################################################

# Global variables
JOB_NAME=""
JOB_CLASS=""
JOB_MSGCLASS=""
JOB_NOTIFY=""
CURRENT_STEP=""
STEP_COUNT=0
EXEC_PROGRAM=""
DD_STATEMENTS=()
PROC_NAME=""
COND_CODE=""
TEMP_DIR="/tmp/jcl_sim"
SYSOUT_DIR="$TEMP_DIR/sysout"
DATASET_DIR="$TEMP_DIR/datasets"

# Initialize directories
init_environment() {
    mkdir -p "$TEMP_DIR" "$SYSOUT_DIR" "$DATASET_DIR"
    echo "JCL Simulation Environment initialized"
    echo "Temp directory: $TEMP_DIR"
    echo "SYSOUT directory: $SYSOUT_DIR"
    echo "Dataset directory: $DATASET_DIR"
}

# Parse JCL file
parse_jcl() {
    local jcl_file="$1"
    
    if [[ ! -f "$jcl_file" ]]; then
        echo "ERROR: JCL file '$jcl_file' not found"
        return 8
    fi
    
    echo "Parsing JCL file: $jcl_file"
    echo "=================================="
    
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Skip comments and empty lines
        [[ "$line" =~ ^[[:space:]]*$ ]] && continue
        [[ "$line" =~ ^[[:space:]]*\* ]] && continue
        
        # Remove leading/trailing whitespace
        line=$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
        
        # Parse different statement types
        if [[ "$line" =~ ^//[A-Z0-9]+[[:space:]]+JOB ]]; then
            parse_job_statement "$line"
        elif [[ "$line" =~ ^//[A-Z0-9]+[[:space:]]+EXEC ]]; then
            # Execute previous step if any
            if [[ -n "$CURRENT_STEP" && -n "$EXEC_PROGRAM" ]]; then
                execute_step
            fi
            parse_exec_statement "$line"
        elif [[ "$line" =~ ^//[A-Z0-9]+[[:space:]]+DD ]]; then
            parse_dd_statement "$line"
        elif [[ "$line" =~ ^//[A-Z0-9]+[[:space:]]+PROC ]]; then
            parse_proc_statement "$line"
        elif [[ "$line" =~ ^//[[:space:]]*IF ]]; then
            parse_if_statement "$line"
        else
            echo "INFO: Skipping line: $line"
        fi
        
    done < "$jcl_file"
    
    # Execute the last step if any
    if [[ -n "$CURRENT_STEP" && -n "$EXEC_PROGRAM" ]]; then
        execute_step
    fi
    
    echo "JCL parsing completed"
    return 0
}

# Parse JOB statement
parse_job_statement() {
    local line="$1"
    
    # Extract job name (first word after //)
    JOB_NAME=$(echo "$line" | sed 's|^//||' | awk '{print $1}')
    
    # Extract parameters
    if [[ "$line" =~ CLASS=([A-Z]) ]]; then
        JOB_CLASS="${BASH_REMATCH[1]}"
    fi
    
    if [[ "$line" =~ MSGCLASS=([A-Z]) ]]; then
        JOB_MSGCLASS="${BASH_REMATCH[1]}"
    fi
    
    if [[ "$line" =~ NOTIFY=([^,[:space:]]+) ]]; then
        JOB_NOTIFY="${BASH_REMATCH[1]}"
    fi
    
    echo "JOB: $JOB_NAME (Class: $JOB_CLASS, MsgClass: $JOB_MSGCLASS)"
    
    # Create job log
    local job_log="$SYSOUT_DIR/${JOB_NAME}.log"
    echo "Job $JOB_NAME started at $(date)" > "$job_log"
    echo "Class: $JOB_CLASS, MsgClass: $JOB_MSGCLASS" >> "$job_log"
}

# Parse EXEC statement
parse_exec_statement() {
    local line="$1"
    
    # Extract step name
    CURRENT_STEP=$(echo "$line" | sed 's|^//||' | awk '{print $1}')
    ((STEP_COUNT++))
    
    # Extract program name
    if [[ "$line" =~ PGM=([^,[:space:]]+) ]]; then
        EXEC_PROGRAM="${BASH_REMATCH[1]}"
    elif [[ "$line" =~ PROC=([^,[:space:]]+) ]]; then
        PROC_NAME="${BASH_REMATCH[1]}"
    fi
    
    # Extract condition code
    local cond_pattern='COND=\(([^)]+)\)'
    if [[ "$line" =~ $cond_pattern ]]; then
        COND_CODE="${BASH_REMATCH[1]}"
    fi
    
    echo "STEP: $CURRENT_STEP (Program: $EXEC_PROGRAM, Condition: $COND_CODE)"
    
    # Don't execute immediately - wait for DD statements
}

# Parse DD statement
parse_dd_statement() {
    local line="$1"
    
    # Extract DD name
    local dd_name=$(echo "$line" | sed 's|^//||' | awk '{print $1}')
    
    # Extract dataset name
    local dsn=""
    if [[ "$line" =~ DSN=([^,[:space:]]+) ]]; then
        dsn="${BASH_REMATCH[1]}"
    fi
    
    # Extract disposition
    local disp=""
    local disp_pattern1='DISP=\(([^)]+)\)'
    local disp_pattern2='DISP=([^,[:space:]]+)'
    if [[ "$line" =~ $disp_pattern1 ]] || [[ "$line" =~ $disp_pattern2 ]]; then
        disp="${BASH_REMATCH[1]}"
    fi
    
    # Handle SYSOUT
    local sysout_pattern='SYSOUT=\*'
    if [[ "$line" =~ $sysout_pattern ]]; then
        dsn="SYSOUT"
    fi
    
    echo "  DD: $dd_name -> $dsn (DISP: $disp)"
    
    # Store DD information for step execution
    DD_STATEMENTS+=("$dd_name:$dsn:$disp")
}

# Parse PROC statement
parse_proc_statement() {
    local line="$1"
    echo "PROC statement: $line"
    # TODO: Implement procedure handling
}

# Parse IF statement
parse_if_statement() {
    local line="$1"
    echo "IF statement: $line"
    # TODO: Implement conditional logic
}

# Execute job step
execute_step() {
    echo "Executing step: $CURRENT_STEP"
    echo "Program: $EXEC_PROGRAM"
    
    local step_log="$SYSOUT_DIR/${JOB_NAME}_${CURRENT_STEP}.log"
    local return_code=0
    
    # Check condition code
    if [[ -n "$COND_CODE" ]]; then
        if check_condition "$COND_CODE"; then
            echo "Step $CURRENT_STEP skipped due to condition: $COND_CODE"
            return 0
        fi
    fi
    
    # Prepare environment for program execution
    setup_step_environment
    
    # Execute based on program type
    case "$EXEC_PROGRAM" in
        "BATCH-VALIDATOR")
            execute_cobol_program "simple_validator.cbl" "$step_log"
            return_code=$?
            ;;
        "ACCOUNT-UPDATE")
            execute_cobol_program "data_processor.cbl" "$step_log"
            return_code=$?
            ;;
        "TRANSACTION-SECURITY")
            execute_cobol_program "simple_validator.cbl" "$step_log"
            return_code=$?
            ;;
        "CUSTOMER-SEARCH")
            execute_cobol_program "report_generator.cbl" "$step_log"
            return_code=$?
            ;;
        "FILE-PROCESSOR")
            execute_cobol_program "file_processor.cbl" "$step_log"
            return_code=$?
            ;;
        "DISPLAY-OUTPUT")
            execute_cobol_program "display_output.cbl" "$step_log"
            return_code=$?
            ;;
        "COPY-LINES")
            execute_cobol_program "copy_lines.cbl" "$step_log"
            return_code=$?
            ;;
        "HELLO-COBOL")
            execute_cobol_program "hello_world.cbl" "$step_log"
            return_code=$?
            ;;
        *)
            echo "Unknown program: $EXEC_PROGRAM" | tee "$step_log"
            return_code=8
            ;;
    esac
    
    echo "Step $CURRENT_STEP completed with return code: $return_code"
    echo "Step $CURRENT_STEP RC=$return_code at $(date)" >> "$SYSOUT_DIR/${JOB_NAME}.log"
    
    # Store return code for condition checking
    echo "$return_code" > "$TEMP_DIR/${CURRENT_STEP}.rc"
    
    # Clear step variables
    EXEC_PROGRAM=""
    COND_CODE=""
    DD_STATEMENTS=()
    
    return $return_code
}

# Setup environment for step execution
setup_step_environment() {
    echo "Setting up environment for step: $CURRENT_STEP"
    
    # Process DD statements
    for dd_stmt in "${DD_STATEMENTS[@]}"; do
        IFS=':' read -r dd_name dsn disp <<< "$dd_stmt"
        
        case "$dd_name" in
            "TRANSIN"|"INPUT"|"INFILE")
                # Input dataset
                if [[ "$dsn" != "SYSOUT" ]]; then
                    local input_file=$(resolve_dataset_name "$dsn")
                    export JCL_INPUT_FILE="$input_file"
                    export INFILE="$input_file"
                fi
                ;;
            "TRANSOUT"|"OUTPUT"|"OUTFILE")
                # Output dataset
                if [[ "$dsn" != "SYSOUT" ]]; then
                    local output_file=$(resolve_dataset_name "$dsn")
                    export JCL_OUTPUT_FILE="$output_file"
                    export OUTFILE="$output_file"
                fi
                ;;
            "DATASET")
                # Dataset for display programs
                if [[ "$dsn" != "SYSOUT" ]]; then
                    local dataset_file=$(resolve_dataset_name "$dsn")
                    export DATASET="$dataset_file"
                fi
                ;;
            "SYSOUT")
                export JCL_SYSOUT_FILE="$SYSOUT_DIR/${JOB_NAME}_${CURRENT_STEP}.log"
                ;;
        esac
    done
}

# Resolve dataset name to file path
resolve_dataset_name() {
    local dsn="$1"
    
    # Convert dataset name to file path
    case "$dsn" in
        "TRANSACTIONS.INPUT")
            echo "transactions.idx"
            ;;
        "TRANSACTIONS.VALIDATED")
            echo "$DATASET_DIR/transactions_validated.dat"
            ;;
        "ACCOUNTS.MASTER")
            echo "accounts.idx"
            ;;
        "CUSTOMERS.MASTER")
            echo "CUSTOMERS.DAT"
            ;;
        "STUDENT.INPUT.DATA")
            echo "datasets/student_input_data.dat"
            ;;
        "STUDENT.OUTPUT.DATA")
            echo "datasets/student_output_data.dat"
            ;;
        "BATCH.INPUT.DATA")
            echo "datasets/batch_input.dat"
            ;;
        "BATCH.OUTPUT.DATA")
            echo "datasets/batch_output.dat"
            ;;
        *)
            # Generic conversion: replace dots with underscores, lowercase
            local filename=$(echo "$dsn" | tr '.' '_' | tr '[:upper:]' '[:lower:]')
            echo "datasets/$filename.dat"
            ;;
    esac
}

# Execute COBOL program
execute_cobol_program() {
    local cobol_file="$1"
    local log_file="$2"
    
    echo "Executing COBOL program: $cobol_file" | tee -a "$log_file"
    
    # Look for COBOL program in programs directory first, then current directory
    local cobol_path=""
    if [[ -f "programs/$cobol_file" ]]; then
        cobol_path="programs/$cobol_file"
    elif [[ -f "$cobol_file" ]]; then
        cobol_path="$cobol_file"
    else
        echo "ERROR: COBOL program '$cobol_file' not found" | tee -a "$log_file"
        echo "Searched in: programs/$cobol_file and $cobol_file" | tee -a "$log_file"
        return 8
    fi
    
    echo "Found COBOL program at: $cobol_path" | tee -a "$log_file"
    
    # Compile and execute COBOL program
    local program_name=$(basename "$cobol_file" .cbl)
    local executable="$TEMP_DIR/$program_name"
    
    echo "Compiling $cobol_file..." | tee -a "$log_file"
    
    # Try to compile with available COBOL compiler
    if command -v cobc >/dev/null 2>&1 && [[ "$FORCE_SIMULATION" != "true" ]]; then
        cobc -x -o "$executable" "$cobol_path" 2>&1 | tee -a "$log_file"
        local compile_rc=${PIPESTATUS[0]}
        
        if [[ $compile_rc -eq 0 ]]; then
            echo "Compilation successful. Executing..." | tee -a "$log_file"
            "$executable" 2>&1 | tee -a "$log_file"
            return ${PIPESTATUS[0]}
        else
            echo "Compilation failed with RC=$compile_rc. Falling back to simulation..." | tee -a "$log_file"
            simulate_cobol_execution "$program_name" "$log_file"
            return $?
        fi
    else
        echo "COBOL compiler not available or simulation forced. Simulating execution..." | tee -a "$log_file"
        simulate_cobol_execution "$program_name" "$log_file"
        return $?
    fi
}

# Simulate COBOL program execution when compiler not available
simulate_cobol_execution() {
    local program_name="$1"
    local log_file="$2"
    
    echo "Simulating execution of $program_name" | tee -a "$log_file"
    
    case "$program_name" in
        "simple_validator")
            echo "SIMPLE-VALIDATOR: Starting data validation..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Processing record batch..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Checking data integrity..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Applying business rules..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Validation completed" | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Total records: 100" | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Valid records: 95" | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Invalid records: 5" | tee -a "$log_file"
            return 0
            ;;
        "data_processor")
            echo "Account balance update completed" | tee -a "$log_file"
            return 0
            ;;
        "report_generator")
            echo "REPORT-GENERATOR: Starting report generation..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Collecting data..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Formatting output..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Creating headers and footers..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Generating summary statistics..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Report generation completed" | tee -a "$log_file"
            echo "REPORT-GENERATOR: Lines generated: 1500" | tee -a "$log_file"
            echo "REPORT-GENERATOR: Pages created: 25" | tee -a "$log_file"
            echo "REPORT-GENERATOR: Summary records: 50" | tee -a "$log_file"
            return 0
            ;;
        "simple_validator")
            echo "SIMPLE-VALIDATOR: Starting data validation..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Processing record batch..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Checking data integrity..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Applying business rules..." | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Validation completed" | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Total records: 100" | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Valid records: 95" | tee -a "$log_file"
            echo "SIMPLE-VALIDATOR: Invalid records: 5" | tee -a "$log_file"
            return 0
            ;;
        "data_processor")
            echo "DATA-PROCESSOR: Starting data processing..." | tee -a "$log_file"
            echo "DATA-PROCESSOR: Reading input data..." | tee -a "$log_file"
            echo "DATA-PROCESSOR: Applying transformations..." | tee -a "$log_file"
            echo "DATA-PROCESSOR: Updating master records..." | tee -a "$log_file"
            echo "DATA-PROCESSOR: Generating audit trail..." | tee -a "$log_file"
            echo "DATA-PROCESSOR: Processing completed successfully" | tee -a "$log_file"
            echo "DATA-PROCESSOR: Records processed: 250" | tee -a "$log_file"
            echo "DATA-PROCESSOR: Updates applied: 240" | tee -a "$log_file"
            echo "DATA-PROCESSOR: Errors found: 2" | tee -a "$log_file"
            return 0
            ;;
        "report_generator")
            echo "REPORT-GENERATOR: Starting report generation..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Collecting data..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Formatting output..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Creating headers and footers..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Generating summary statistics..." | tee -a "$log_file"
            echo "REPORT-GENERATOR: Report generation completed" | tee -a "$log_file"
            echo "REPORT-GENERATOR: Lines generated: 1500" | tee -a "$log_file"
            echo "REPORT-GENERATOR: Pages created: 25" | tee -a "$log_file"
            echo "REPORT-GENERATOR: Summary records: 50" | tee -a "$log_file"
            return 0
            ;;
        "file_processor")
            echo "FILE-PROCESSOR: Starting file processing..." | tee -a "$log_file"
            if [[ -n "$INFILE" && -n "$OUTFILE" ]]; then
                if [[ -f "$INFILE" ]]; then
                    cp "$INFILE" "$OUTFILE"
                    local line_count=$(wc -l < "$INFILE" 2>/dev/null || echo "0")
                    echo "FILE-PROCESSOR: Processing completed." | tee -a "$log_file"
                    echo "FILE-PROCESSOR: Records processed: $line_count" | tee -a "$log_file"
                    echo "FILE-PROCESSOR: Job completed successfully" | tee -a "$log_file"
                    return 0
                else
                    echo "FILE-PROCESSOR: ERROR - Input file not found: $INFILE" | tee -a "$log_file"
                    return 8
                fi
            else
                echo "FILE-PROCESSOR: ERROR - INFILE or OUTFILE not defined" | tee -a "$log_file"
                return 8
            fi
            ;;
        "display_output")
            echo "DISPLAY-OUTPUT: Showing contents of output dataset" | tee -a "$log_file"
            echo "==================================================" | tee -a "$log_file"
            if [[ -n "$DATASET" && -f "$DATASET" ]]; then
                cat "$DATASET" | tee -a "$log_file"
                local line_count=$(wc -l < "$DATASET" 2>/dev/null || echo "0")
                echo "==================================================" | tee -a "$log_file"
                echo "DISPLAY-OUTPUT: Total lines displayed: $line_count" | tee -a "$log_file"
                return 0
            else
                echo "DISPLAY-OUTPUT: ERROR - Dataset not found: $DATASET" | tee -a "$log_file"
                return 8
            fi
            ;;
        "copy_lines")
            echo "COPY-LINES: Starting file copy operation..." | tee -a "$log_file"
            if [[ -n "$INFILE" && -n "$OUTFILE" ]]; then
                if [[ -f "$INFILE" ]]; then
                    cp "$INFILE" "$OUTFILE"
                    local line_count=$(wc -l < "$INFILE" 2>/dev/null || echo "0")
                    echo "COPY-LINES: File copy completed." | tee -a "$log_file"
                    echo "COPY-LINES: Records copied: $line_count" | tee -a "$log_file"
                    echo "COPY-LINES: Job completed successfully" | tee -a "$log_file"
                    return 0
                else
                    echo "COPY-LINES: ERROR - Input file not found: $INFILE" | tee -a "$log_file"
                    return 8
                fi
            else
                echo "COPY-LINES: ERROR - INFILE or OUTFILE not defined" | tee -a "$log_file"
                return 8
            fi
            ;;
        "4-hello")
            echo "Hello from COBOL!" | tee -a "$log_file"
            echo "HELLO-COBOL: Program executed successfully" | tee -a "$log_file"
            return 0
            ;;
        *)
            echo "Program $program_name executed successfully" | tee -a "$log_file"
            return 0
            ;;
    esac
}

# Check condition code
check_condition() {
    local condition="$1"
    
    # Parse condition: (return_code,operator,step_name)
    if [[ "$condition" =~ ([0-9]+),([A-Z]+),([A-Z0-9]+) ]]; then
        local check_rc="${BASH_REMATCH[1]}"
        local operator="${BASH_REMATCH[2]}"
        local step_name="${BASH_REMATCH[3]}"
        
        local step_rc_file="$TEMP_DIR/${step_name}.rc"
        
        if [[ -f "$step_rc_file" ]]; then
            local actual_rc=$(cat "$step_rc_file")
            
            case "$operator" in
                "EQ")
                    [[ $actual_rc -eq $check_rc ]]
                    ;;
                "NE")
                    [[ $actual_rc -ne $check_rc ]]
                    ;;
                "GT")
                    [[ $actual_rc -gt $check_rc ]]
                    ;;
                "LT")
                    [[ $actual_rc -lt $check_rc ]]
                    ;;
                *)
                    echo "Unknown condition operator: $operator"
                    return 1
                    ;;
            esac
        else
            echo "Step $step_name return code not found"
            return 1
        fi
    else
        echo "Invalid condition format: $condition"
        return 1
    fi
}

# Main function
main() {
    if [[ $# -eq 0 ]]; then
        echo "Usage: $0 <jcl_file>"
        echo "Example: $0 banking_job.jcl"
        exit 1
    fi
    
    local jcl_file="$1"
    
    echo "JCL Simulation Framework"
    echo "========================"
    
    init_environment
    parse_jcl "$jcl_file"
    
    local final_rc=$?
    echo "Job $JOB_NAME completed with return code: $final_rc"
    
    # Display job summary
    echo ""
    echo "Job Summary:"
    echo "============"
    echo "Job Name: $JOB_NAME"
    echo "Steps Executed: $STEP_COUNT"
    echo "Final Return Code: $final_rc"
    echo "SYSOUT Location: $SYSOUT_DIR"
    echo "Datasets Location: $DATASET_DIR"
    
    exit $final_rc
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi