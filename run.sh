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
    gosh demod-100Hz.scm < $i > $b.dat
    sed -n '/detect/s/^## //p' $b.dat > $b.det
    (echo "File: $i Run: $(date)";
     sed -n '/## wav:/s/^## wav: //p' $b.dat;
     echo; echo) >> info.log
}

clean()
{
    rm -f *~ *.det *.dat *.log
}

while [ $1 ]; do
    case $1 in
	all)   run;;
	clean) clean;;
        *) run1 $1;;
    esac
    shift
done
