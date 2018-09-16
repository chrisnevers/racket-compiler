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
    errors+=("add: expected result to equal 0, but received $res")
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
    errors+=("neg: expected result to equal -45, but received $res")
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
    errors+=("uniquify: expected result to equal 18, but received $res")
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
    errors+=("liveness: expected result to equal 42, but received $res")
fi

################################################################################
# and test
################################################################################

printf "and\n"
./main.native examples/control-flow/and.rkt

res=$(echo "10" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("and: expected result to equal #t, but received $res")
fi

res=$(echo "5" | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'\n"
    errors+=("and: expected result to equal #f, but received $res")
fi

################################################################################
# or test
################################################################################

printf "or\n"
./main.native examples/control-flow/or.rkt

res=$(echo "10" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("or: expected result to equal #t, but received $res")
fi

res=$(echo "0" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '0'\n"
else
    printf "    failed - '0'\n"
    errors+=("or: expected result to equal #t, but received $res")
fi

res=$(echo "5" | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'\n"
    errors+=("or: expected result to equal #f, but received $res")
fi

################################################################################
# not test
################################################################################

printf "not\n"
./main.native examples/control-flow/not.rkt

res=$(./output)
if [ "$res" == "#f" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=("not: expected result to equal #f, but received $res")
fi

################################################################################
# eq? test
################################################################################

printf "eq?\n"
./main.native examples/control-flow/eq.rkt

res=$(echo "10" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("eq?: expected result to equal #t, but received $res")
fi

res=$(echo "5" | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'"
    errors+=("eq?: expected result to equal #f, but received $res")
fi

################################################################################
# gt test
################################################################################

printf "gt\n"
./main.native examples/control-flow/gt.rkt

res=$(echo "15" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '15'\n"
else
    printf "    failed - '15'\n"
    errors+=("gt: expected result to equal #t, but received $res")
fi

res=$(echo "10" | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("gt: expected result to equal #f, but received $res")
fi

################################################################################
# gte test
################################################################################

printf "gte\n"
./main.native examples/control-flow/gte.rkt

res=$(echo "10" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("gte: expected result to equal #t, but received $res")
fi

res=$(echo "5" | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'\n"
    errors+=("gte: expected result to equal #f, but received $res")
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
    errors+=("if: expected result to equal 30, but received $res")
fi

res=$(echo "10" | ./output)
if [ "$res" -eq "0" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("if: expected result to equal 0, but received $res")
fi

################################################################################
# lt test
################################################################################

printf "lt\n"
./main.native examples/control-flow/lt.rkt

res=$(echo "5" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '5'\n"
else
    printf "    failed - '5'\n"
    errors+=("lt: expected result to equal #t, but received $res")
fi

res=$(echo "10" | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("lt: expected result to equal #f, but received $res")
fi

################################################################################
# lte test
################################################################################

printf "lte\n"
./main.native examples/control-flow/lte.rkt

res=$(echo "10" | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed - '10'\n"
else
    printf "    failed - '10'\n"
    errors+=("lte: expected result to equal #t, but received $res")
fi

res=$(echo "15" | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed - '15'\n"
else
    printf "    failed - '15'\n"
    errors+=("lte: expected result to equal #f, but received $res")
fi

################################################################################
# pos? test
################################################################################

printf "pos?\n"
./main.native examples/control-flow/pos?.rkt

res=$(./output)
if [ "$res" == "#t" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=("pos?: expected result to equal #t, but received $res")
fi

################################################################################
# neg? test
################################################################################

printf "neg?\n"
./main.native examples/control-flow/neg?.rkt

res=$(./output)
if [ "$res" == "#t" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=("neg?: expected result to equal #t, but received $res")
fi

################################################################################
# zero? test
################################################################################

printf "zero?\n"
./main.native examples/control-flow/zero?.rkt

res=$(echo '0' | ./output)
if [ "$res" == "#t" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=("zero?: expected result to equal #t, but received $res")
fi

res=$(echo '5' | ./output)
if [ "$res" == "#f" ]; then
    printf "    passed\n"
else
    printf "    failed\n"
    errors+=("zero?: expected result to equal #f, but received $res")
fi

# Report results
printf '\nErrors:\n'
if [ ${#errors[@]} -ne 0 ]; then
    printf '%s\n' "${errors[@]}"
else
    printf 'none\n'
fi
