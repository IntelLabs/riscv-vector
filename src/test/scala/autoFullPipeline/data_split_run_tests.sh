#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 <instruction to be tested>"
    exit 1
fi

N_SPLITS=8
TEST_INST="${1:-}"
PROJECT_AUTOTEST_ROOT="./src/test/scala/auto/AutoTestRes"

cd "./AutoTestRes"
foldername=Test_data_split_$(date +'%Y-%m-%d_%H-%M-%S')
mkdir "$foldername" && cd "$foldername"

echo "========"
echo "Tests start. All outputs and split instruction text files will be saved to AutoTestRes/$foldername"
echo "========"

function wait_exit() {
    children=("$@")
    EXIT_CODE=0
    for job in "${children[@]}"; do
       echo "Waiting for ${job}.."
       CODE=0;
       wait ${job} || CODE=$?
       if [[ "${CODE}" != "0" ]]; then
           echo "${job}, test failed with ${CODE}" ;
           EXIT_CODE=1;
       fi
   done
}

children_pids=()

function intfunc() {
    pkill -P "${children_pids[@]}"
    kill -SIGKILL "${children_pids[@]}"
    echo "Closed .. $?"
    exit 1
}
trap intfunc 2

incorrect_inst_file_path="$PROJECT_AUTOTEST_ROOT/$foldername/all_incorrect_insts.txt"
done_inst_file_path="$PROJECT_AUTOTEST_ROOT/$foldername/done_insts.txt"
for i in $(seq 0 "$((N_SPLITS - 1))"); do
    inst_file="${TEST_INST}_${i}"
    # inst_file_path="$PROJECT_AUTOTEST_ROOT/$foldername/$inst_file"
    out_file_path1="$PROJECT_AUTOTEST_ROOT/$foldername/$inst_file.out.txt"
    out_file_path2="$PROJECT_AUTOTEST_ROOT/$foldername/$inst_file.stderr_out.txt"

    pushd . > /dev/null
    cd ../../../../../..
    sbt -J-XX:+UseG1GC -J-Xms1g -J-Xmx4g \
        -DincorrInsts="$incorrect_inst_file_path" \
        -DdoneInsts="$done_inst_file_path" \
        -DdataSplitInst="$TEST_INST" -DdataSplitIx="$i" -DdataSplitN="$N_SPLITS" \
        "testOnly darecreek.vfuAutotest.alu.VAluSpec" \
        1> "$out_file_path1" 2> "$out_file_path2" &   # run the command via bash in subshell
    popd > /dev/null

    process_id="$!"
    children_pids+=($process_id)
    echo "Starting test process $i (pid $process_id): Testing $inst_file.."
    sleep 10
done

echo "pids: ${children_pids[@]}"

wait_exit "${children_pids[@]}"

echo "EXIT_CODE => $EXIT_CODE"
exit "$EXIT_CODE"
# end