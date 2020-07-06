echo "Goedel Test Suite Utility"
echo "Examine Test Results For IN/$1.in ?"
echo "enter 0 to proceed 888 to next, or crtl-c to quit"
read answer
while [ $answer -ne 888 ]
do
    echo "1 $1.in "
    if test -f $1.exp 
    then
	echo "2 $1.exp"
    fi
    if test -f $1.loc 
    then
	echo "3 $1.loc"
    fi
    if test -f ../Tests/REFDIFF/$1.refdiff.tmp
    then
	echo "4 ../Tests/REFDIFF/$1.refdiff.tmp"
	echo "5 ../Tests/REFOUT/$1.refout OUT/$1.out.tmp"
    fi
    if test -f ../Tests/ERRDIFF/$1.errdiff.tmp 
    then
	echo "6 ../Tests/ERRDIFF/$1.errdiff.tmp"
	echo "7 ../Tests/REFERR/$1.referr ERR/$1.err.tmp"
    fi
    echo "11 Approve New $1 Output To Reference"
    echo "16 edit Port.txt"

    echo "888 NEXT FILE SET"
    echo "ctrl-c QUIT"
    echo "select"
    read select
    case $select in
	1) $EDITOR $1.in;;
	2) $EDITOR $1.exp;;
	3) $EDITOR $1.loc;;
	4) $EDITOR ../Tests/REFDIFF/$1.refdiff.tmp;;
	5) $EDITOR ../Tests/REFOUT/$1.refout ../Tests/OUT/$1.out.tmp;;
	6) $EDITOR ../Tests/ERRDIFF/$1.errdiff.tmp;;
	7) $EDITOR ../Tests/REFERR/$1.referr ../Tests/ERR/$1.err.tmp;;
	11) ./approve_out.sh $1;;
	16) $EDITOR Port.txt;;
	888) exit 2;;
    esac
done


