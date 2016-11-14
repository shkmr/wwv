##
##
run()
{
    cp /dev/null info.log
    for i in data/*.wav; do
        run1 $i
    done
}

run1()
{
    i=$1
    echo $i
    b=$(basename $i .wav)
    gosh demod-100Hz.scm < $i | tee $b.dat | grep '^## decode: '
    sed -n '/detect/s/^## //p' $b.dat > $b.det
    sed -n '/codes:/s/^## //p' $b.dat > $b.codes
    (echo "File: $i Run: $(date)";
     sed -n '/## wav:/s/^## wav: //p' $b.dat;
     echo; echo) >> info.log
}

clean()
{
    rm -f *~ *.det *.dat *.log *.codes
}

while [ $1 ]; do
    case $1 in
	all)   run;;
	clean) clean;;
        *) run1 $1;;
    esac
    shift
done
