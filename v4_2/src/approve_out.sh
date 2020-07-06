if test -f ../Tests/OUT/$1.out.tmp
then
    echo cp ../Tests/OUT/$1.out.tmp ../Tests/REFOUT/$1.refout
    cp ../Tests/OUT/$1.out.tmp ../Tests/REFOUT/$1.refout
else
    rm ../Tests/REFOUT/$1.refout
    touch ../Tests/REFOUT/$1.refout
fi
if test -f ../Tests/ERR/$1.err.tmp
then
    echo cp ../Tests/ERR/$1.err.tmp ../Tests/REFERR/$1.referr
    cp ../Tests/ERR/$1.err.tmp ../Tests/REFERR/$1.referr
else
    rm ../Tests/REFERR/$1.referr
    touch ../Tesdts/REFERR/$1.referr
fi
echo "$1 approved" >../Tests/APPROVED/$1.approved
echo "$1 approved"

