# Compile compiler
# make

errors=()

################################################################################
# add test
################################################################################

printf "add\n"
./main.native examples/basics/add.rkt

res=$(echo "10" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('add: expected result to equal 0')
fi

################################################################################
# neg test
################################################################################

printf "neg\n"
./main.native examples/basics/neg.rkt

res=$(./output)
if [ "$res" -eq "-45" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=('neg: expected result to equal -45')
fi

################################################################################
# uniquify test
################################################################################

printf "uniquify\n"
./main.native examples/basics/uniquify.rkt

res=$(./output)
if [ "$res" -eq "18" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=('uniquify: expected result to equal 18')
fi

################################################################################
# liveness test
################################################################################

printf "liveness\n"
./main.native examples/reg-alloc/liveness.rkt

res=$(./output)
if [ "$res" -eq "42" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=('liveness: expected result to equal 42')
fi

################################################################################
# and test
################################################################################

printf "and\n"
./main.native examples/control-flow/and.rkt

res=$(echo "10" | ./output)
if [ "$res" -eq "1" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('and: expected result to equal 1')
fi

res=$(echo "5" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'\n"
    errors+=('and: expected result to equal 0')
fi

################################################################################
# not test
################################################################################

printf "not\n"
./main.native examples/control-flow/not.rkt

res=$(./output)
if [ "$res" -eq "0" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=('not: expected result to equal 0')
fi

################################################################################
# eq? test
################################################################################

printf "eq?\n"
./main.native examples/control-flow/eq.rkt

res=$(echo "10" | ./output)
if [ "$res" -eq "1" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('eq?: expected result to equal 1')
fi

res=$(echo "5" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'"
    errors+=('eq?: expected result to equal 0')
fi

################################################################################
# gt test
################################################################################

printf "gt\n"
./main.native examples/control-flow/gt.rkt

res=$(echo "15" | ./output)
if [ "$res" -eq "1" ]; then
    printf "    passed - '15'\n"
else
    printf "    failed - '15'\n"
    errors+=('gt: expected result to equal 1')
fi

res=$(echo "10" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('gt: expected result to equal 0')
fi

################################################################################
# gte test
################################################################################

printf "gte\n"
./main.native examples/control-flow/gte.rkt

res=$(echo "10" | ./output)
if [ "$res" -eq "1" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('gte: expected result to equal 1')
fi

res=$(echo "5" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'\n"
    errors+=('gte: expected result to equal 0')
fi

################################################################################
# if test
################################################################################

printf "if\n"
./main.native examples/control-flow/if.rkt

res=$(echo "20" | ./output)
if [ "$res" -eq "30" ]; then
    printf "    passed - '20'\n"
else
    printf "    failed - '20'\n"
    errors+=('if: expected result to equal 30')
fi

res=$(echo "10" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('if: expected result to equal 0')
fi

################################################################################
# lt test
################################################################################

printf "lt\n"
./main.native examples/control-flow/lt.rkt

res=$(echo "5" | ./output)
if [ "$res" -eq "1" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'\n"
    errors+=('lt: expected result to equal 1')
fi

res=$(echo "10" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('lt: expected result to equal 0')
fi

################################################################################
# lte test
################################################################################

printf "lte\n"
./main.native examples/control-flow/lte.rkt

res=$(echo "10" | ./output)
if [ "$res" -eq "1" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=('lte: expected result to equal 1')
fi

res=$(echo "15" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '15'\n"
else
    printf "    failed - '15'\n"
    errors+=('lte: expected result to equal 0')
fi


# Report results
if [ ${#errors[@]} -ne 0 ]; then
    printf '\nErrors:\n%s\n\n' "${errors[@]}"
else
    printf '\nNo Errors\n'
fi
