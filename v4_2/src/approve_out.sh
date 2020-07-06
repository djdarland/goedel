if test -f ../Tests/OUT/$1.out.tmp
then
    echo cp ../Tests/OUT/$1.out.tmp ../Tests/REFOUT/$1.refout
    cp ../Tests/OUT/$1.out.tmp ../Tests/REFOUT/$1.refout
else
    rm ../Tests/REFOUT/$1.refout
    touch ../Tests/REFOUT/$1.refout
fi
# if test -f ERR/$1.err.tmp
# then
#    cp ERR/$1.err.tmp REFERR/$1.referr
# else
#    rm REFERR/$1.referr
#    touch REFERR/$1.referr
# fi
echo "$1 approved" >../Tests/APPROVED/$1.approved
echo "$1 approved"

