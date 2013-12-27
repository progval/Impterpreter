rm -f _build/test_output.txt

for filename in $(ls tests/*.imp)
do
    echo "
$filename
===">> _build/test_output.txt
    cat $filename | ./impterpreter >> _build/test_output.txt
done

diff -u _build/test_output.txt tests/expected_output.txt > _build/tests.diff
RETVAL=$?
[ $RETVAL -eq 0 ] && echo "Succès"
[ $RETVAL -ne 0 ] && echo "Échec (voir _build/tests.diff)"
exit $RETVAL
