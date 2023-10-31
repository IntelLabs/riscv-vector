#!/bin/bash

N_PROCESSES=4

if [ -z "$1" ]; then
    echo "Usage: $0 <file of instruction names>"
    exit 1
fi

filename="$1"

if [ ! -f "$filename" ]; then
    echo "$filename not found."
    exit 1
fi

cd "./AutoTestRes"
foldername=Test_$(date +'%Y-%m-%d_%H-%M-%S')
mkdir "$foldername" && cd "$foldername"
mkdir "incorrectData"

echo "========"
echo "Tests start. All outputs and split instruction text files will be saved to AutoTestRes/$foldername"
echo "========"

# split instruction file
splitNLines=$(( $(wc -l < "../../$filename") / $N_PROCESSES ))
splitNLinesMod=$(( $(wc -l < "../../$filename") % $N_PROCESSES ))
if [[ $splitNLinesMod -ne 0 ]]; then
    splitNLines=$(( $splitNLines + 1 ))
fi
split -l $splitNLines -a 1 -d --additional-suffix=.txt "../../$filename" instructions_


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

incorrect_inst_file_path="./src/test/scala/auto/AutoTestRes/$foldername/all_incorrect_insts.txt"
done_inst_file_path="./src/test/scala/auto/AutoTestRes/$foldername/done_insts.txt"
for i in $(seq 0 "$N_PROCESSES"); do
    inst_file="instructions_$i.txt"
    inst_file_path="./src/test/scala/auto/AutoTestRes/$foldername/$inst_file"
    out_file_path1="./src/test/scala/auto/AutoTestRes/$foldername/$inst_file.out.txt"
    out_file_path2="./src/test/scala/auto/AutoTestRes/$foldername/$inst_file.stderr_out.txt"
    # echo "$inst_file"
    if [ -f "$inst_file" ]; then
        pushd . > /dev/null
        cd ../../../../../..
        sbt -J-XX:+UseG1GC -J-Xms4g -J-Xmx8g -Dinsfile="$inst_file_path" \
            -DincorrInsts="$incorrect_inst_file_path" \
            -DdoneInsts="$done_inst_file_path" \
            -DincorrDataDire="./src/test/scala/auto/AutoTestRes/$foldername/incorrectData/" \
            "testOnly darecreek.vfuAutotest.alu.VAluSpec" \
            1> "$out_file_path1" 2> "$out_file_path2" &   # run the command via bash in subshell
        popd > /dev/null

        process_id="$!"
        children_pids+=($process_id)
        echo "Starting test process $i (pid $process_id): Testing instructions in $inst_file.."
        sleep 10
    fi
done

echo "pids: ${children_pids[@]}"

wait_exit "${children_pids[@]}"

echo "EXIT_CODE => $EXIT_CODE"
exit "$EXIT_CODE"
# end