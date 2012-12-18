#!/bin/sh -e

debian/testbuild >/dev/null 2>&1 || true

cat <<EOF
#!/bin/sh -e

rm -rf tests/
mkdir tests
EOF



for x in tests/testcase.*; do
    n=${x#*.}
    echo "cat >tests/testcase.$n <<EOF"
    cat tests/testcase.$n
    echo "EOF"
    echo "cat >tests/up.$n <<EOF"
    cat tests/up-res.$n
    echo "EOF"
    echo
done   

cat <<EOF
result=true
for test in 1 2 3 4 5 6; do
        args="\$(cat tests/testcase.\$test | sed -n 's/^# RUN: //p')"
        ./ifup -nv --force -i tests/testcase.\$test \$args \\
                >tests/up-res-out.\$test 2>tests/up-res-err.\$test || 
                true
        (echo "====stdout===="; cat tests/up-res-out.\$test
         echo "====stderr===="; cat tests/up-res-err.\$test) > tests/up-res.\$test

        echo "Testcase \$test: \$args"
        
        if diff -ub tests/up.\$test tests/up-res.\$test; then
                echo "(okay)"
        else
                echo "(failed)"
                result=false
        fi
        echo "=========="
done

if \$result; then
        echo "(okay overall)"
        exit 0
else
        echo "(failed overall)"
        exit 1
fi
EOF
