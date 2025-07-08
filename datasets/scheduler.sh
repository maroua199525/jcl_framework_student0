#!/bin/bash
#########################################################################
# JCL Job Scheduler - Simulates IBM TWS/OPC job scheduling
# Manages job submission, dependencies, and scheduling
#########################################################################

SCHEDULER_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JOB_QUEUE_DIR="$SCHEDULER_DIR/queue"
JOB_HISTORY_DIR="$SCHEDULER_DIR/history"
JOB_SCHEDULE_FILE="$SCHEDULER_DIR/schedule.conf"
PARSER_SCRIPT="$SCHEDULER_DIR/jcl_parser.sh"

# Initialize scheduler environment
init_scheduler() {
    mkdir -p "$JOB_QUEUE_DIR" "$JOB_HISTORY_DIR"
    
    if [[ ! -f "$JOB_SCHEDULE_FILE" ]]; then
        create_default_schedule
    fi
    
    echo "JCL Job Scheduler initialized"
    echo "Queue directory: $JOB_QUEUE_DIR"
    echo "History directory: $JOB_HISTORY_DIR"
    echo "Schedule file: $JOB_SCHEDULE_FILE"
}

# Create default schedule configuration
create_default_schedule() {
    cat > "$JOB_SCHEDULE_FILE" << 'EOF'
# JCL Job Schedule Configuration
# Format: SCHEDULE|JOB_NAME|JCL_FILE|CRON_EXPRESSION|DEPENDENCIES
# 
# CRON_EXPRESSION format: minute hour day month weekday
# DEPENDENCIES: comma-separated list of job names that must complete first
#
# Examples:
SCHEDULE|BANKDLY|jobs/banking_daily.jcl|0 2 * * *|
SCHEDULE|SECAUDIT|jobs/security_audit.jcl|0 3 * * *|BANKDLY
SCHEDULE|MONTHRPT|jobs/monthly_report.jcl|0 4 1 * *|BANKDLY,SECAUDIT
EOF
    
    echo "Default schedule configuration created"
}

# Submit job to queue
submit_job() {
    local jcl_file="$1"
    local job_name="$2"
    local priority="${3:-5}"
    
    if [[ ! -f "$jcl_file" ]]; then
        echo "ERROR: JCL file '$jcl_file' not found"
        return 1
    fi
    
    if [[ -z "$job_name" ]]; then
        job_name=$(basename "$jcl_file" .jcl | tr '[:lower:]' '[:upper:]')
    fi
    
    local job_id="${job_name}_$(date +%Y%m%d_%H%M%S)"
    local queue_file="$JOB_QUEUE_DIR/${priority}_${job_id}.job"
    
    cat > "$queue_file" << EOF
JOB_ID=$job_id
JOB_NAME=$job_name
JCL_FILE=$jcl_file
PRIORITY=$priority
SUBMIT_TIME="$(date '+%Y-%m-%d %H:%M:%S')"
STATUS=QUEUED
DEPENDENCIES=
EOF
    
    echo "Job submitted: $job_id"
    echo "Queue file: $queue_file"
    return 0
}

# Execute job from queue
execute_job() {
    local queue_file="$1"
    
    if [[ ! -f "$queue_file" ]]; then
        echo "ERROR: Queue file '$queue_file' not found"
        return 1
    fi
    
    # Source job information
    source "$queue_file"
    
    echo "Executing job: $JOB_ID"
    echo "JCL file: $JCL_FILE"
    
    # Update job status
    sed -i 's/STATUS=QUEUED/STATUS=RUNNING/' "$queue_file"
    echo "START_TIME=\"$(date '+%Y-%m-%d %H:%M:%S')\"" >> "$queue_file"
    
    # Execute the JCL
    local job_log="$JOB_HISTORY_DIR/${JOB_ID}.log"
    local return_code=0
    
    if [[ -f "$PARSER_SCRIPT" ]]; then
        "$PARSER_SCRIPT" "$JCL_FILE" > "$job_log" 2>&1
        return_code=$?
    else
        echo "ERROR: JCL parser not found: $PARSER_SCRIPT" | tee "$job_log"
        return_code=8
    fi
    
    # Update job status
    echo "END_TIME=\"$(date '+%Y-%m-%d %H:%M:%S')\"" >> "$queue_file"
    echo "RETURN_CODE=$return_code" >> "$queue_file"
    
    if [[ $return_code -eq 0 ]]; then
        sed -i 's/STATUS=RUNNING/STATUS=COMPLETED/' "$queue_file"
        echo "Job $JOB_ID completed successfully"
    else
        sed -i 's/STATUS=RUNNING/STATUS=FAILED/' "$queue_file"
        echo "Job $JOB_ID failed with return code: $return_code"
    fi
    
    # Move to history
    mv "$queue_file" "$JOB_HISTORY_DIR/"
    
    return $return_code
}

# Process job queue
process_queue() {
    echo "Processing job queue..."
    
    # Process jobs by priority (lower number = higher priority)
    for queue_file in $(ls "$JOB_QUEUE_DIR"/*.job 2>/dev/null | sort); do
        if [[ -f "$queue_file" ]]; then
            source "$queue_file"
            
            # Check dependencies
            if check_dependencies "$DEPENDENCIES"; then
                echo "Dependencies satisfied for job: $JOB_NAME"
                execute_job "$queue_file"
            else
                echo "Dependencies not satisfied for job: $JOB_NAME"
            fi
        fi
    done
}

# Check job dependencies
check_dependencies() {
    local deps="$1"
    
    if [[ -z "$deps" ]]; then
        return 0  # No dependencies
    fi
    
    IFS=',' read -ra DEP_ARRAY <<< "$deps"
    for dep in "${DEP_ARRAY[@]}"; do
        dep=$(echo "$dep" | xargs)  # Trim whitespace
        
        # Check if dependency job completed successfully
        local dep_completed=false
        for history_file in "$JOB_HISTORY_DIR"/${dep}_*.job; do
            if [[ -f "$history_file" ]]; then
                source "$history_file"
                if [[ "$STATUS" == "COMPLETED" && "$RETURN_CODE" == "0" ]]; then
                    dep_completed=true
                    break
                fi
            fi
        done
        
        if [[ "$dep_completed" != true ]]; then
            echo "Dependency not satisfied: $dep"
            return 1
        fi
    done
    
    return 0
}

# Install cron jobs from schedule
install_cron_jobs() {
    echo "Installing cron jobs from schedule..."
    
    local temp_cron="/tmp/jcl_scheduler_cron"
    
    # Preserve existing cron jobs (excluding our scheduler jobs)
    crontab -l 2>/dev/null | grep -v "# JCL_SCHEDULER" > "$temp_cron"
    
    # Add scheduler jobs
    while IFS='|' read -r type job_name jcl_file cron_expr dependencies; do
        # Skip comments and empty lines
        [[ "$type" =~ ^[[:space:]]*# ]] && continue
        [[ -z "$type" ]] && continue
        
        if [[ "$type" == "SCHEDULE" ]]; then
            local full_jcl_path="$SCHEDULER_DIR/$jcl_file"
            echo "$cron_expr $0 submit '$full_jcl_path' '$job_name' # JCL_SCHEDULER:$job_name" >> "$temp_cron"
        fi
    done < "$JOB_SCHEDULE_FILE"
    
    # Install new crontab
    crontab "$temp_cron"
    rm "$temp_cron"
    
    echo "Cron jobs installed successfully"
    echo "Current crontab:"
    crontab -l | grep "JCL_SCHEDULER"
}

# Remove cron jobs
remove_cron_jobs() {
    echo "Removing JCL scheduler cron jobs..."
    
    local temp_cron="/tmp/jcl_scheduler_cron"
    crontab -l 2>/dev/null | grep -v "# JCL_SCHEDULER" > "$temp_cron"
    crontab "$temp_cron"
    rm "$temp_cron"
    
    echo "Cron jobs removed"
}

# Show job status
show_status() {
    echo "JCL Job Scheduler Status"
    echo "========================"
    
    echo ""
    echo "Queued Jobs:"
    echo "------------"
    if ls "$JOB_QUEUE_DIR"/*.job >/dev/null 2>&1; then
        for queue_file in "$JOB_QUEUE_DIR"/*.job; do
            # Safely read job file without executing it
            local job_name=$(grep "^JOB_NAME=" "$queue_file" | cut -d'=' -f2)
            local job_id=$(grep "^JOB_ID=" "$queue_file" | cut -d'=' -f2)
            local priority=$(grep "^PRIORITY=" "$queue_file" | cut -d'=' -f2)
            local status=$(grep "^STATUS=" "$queue_file" | cut -d'=' -f2)
            echo "  $job_name ($job_id) - Priority: $priority, Status: $status"
        done
    else
        echo "  No jobs in queue"
    fi
    
    echo ""
    echo "Recent Job History:"
    echo "-------------------"
    if ls "$JOB_HISTORY_DIR"/*.job >/dev/null 2>&1; then
        for history_file in $(ls -t "$JOB_HISTORY_DIR"/*.job | head -10); do
            # Safely read job file without executing it
            local job_name=$(grep "^JOB_NAME=" "$history_file" | cut -d'=' -f2)
            local job_id=$(grep "^JOB_ID=" "$history_file" | cut -d'=' -f2)
            local status=$(grep "^STATUS=" "$history_file" | cut -d'=' -f2)
            local return_code=$(grep "^RETURN_CODE=" "$history_file" | cut -d'=' -f2)
            echo "  $job_name ($job_id) - Status: $status, RC: ${return_code:-N/A}"
        done
    else
        echo "  No job history"
    fi
    
    echo ""
    echo "Scheduled Jobs:"
    echo "---------------"
    while IFS='|' read -r type job_name jcl_file cron_expr dependencies; do
        [[ "$type" =~ ^[[:space:]]*# ]] && continue
        [[ -z "$type" ]] && continue
        
        if [[ "$type" == "SCHEDULE" ]]; then
            echo "  $job_name: $cron_expr (deps: ${dependencies:-none})"
        fi
    done < "$JOB_SCHEDULE_FILE"
}

# Show help
show_help() {
    cat << 'EOF'
JCL Job Scheduler - Usage

Commands:
  init                    Initialize scheduler environment
  submit <jcl_file> [name] [priority]
                         Submit job to queue
  process                Process job queue
  status                 Show scheduler status
  install-cron           Install cron jobs from schedule
  remove-cron            Remove cron jobs
  help                   Show this help

Examples:
  ./scheduler.sh init
  ./scheduler.sh submit jobs/banking_daily.jcl BANKDLY 1
  ./scheduler.sh process
  ./scheduler.sh status
  ./scheduler.sh install-cron

Schedule Configuration:
  Edit schedule.conf to define recurring jobs
  Format: SCHEDULE|JOB_NAME|JCL_FILE|CRON_EXPRESSION|DEPENDENCIES

Cron Expression Format:
  minute hour day month weekday
  Examples:
    0 2 * * *     - Daily at 2:00 AM
    0 3 * * 1     - Weekly on Monday at 3:00 AM
    0 4 1 * *     - Monthly on 1st at 4:00 AM
EOF
}

# Main function
main() {
    case "${1:-help}" in
        "init")
            init_scheduler
            ;;
        "submit")
            if [[ $# -lt 2 ]]; then
                echo "Usage: $0 submit <jcl_file> [job_name] [priority]"
                exit 1
            fi
            submit_job "$2" "$3" "$4"
            ;;
        "process")
            process_queue
            ;;
        "status")
            show_status
            ;;
        "install-cron")
            install_cron_jobs
            ;;
        "remove-cron")
            remove_cron_jobs
            ;;
        "help"|*)
            show_help
            ;;
    esac
}

# Run main function
main "$@"