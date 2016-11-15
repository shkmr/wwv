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

make_ref()
{
    gosh demod-100Hz.scm < data/wwv9.wav > data/wwv9.ref
}

check()
{
    gosh demod-100Hz.scm < data/wwv9.wav | tee wwv9.chk | grep '^## decode: '
    diff wwv9.chk data/wwv9.ref
}

sox()
{
    rec -c 1 -b 16 -r 8000 -t wav - 2>/dev/null \
        |  gosh ./demod-100Hz.scm \
        |  grep '\(wav\|det\|dec\|codes\)' 
}

clean()
{
    rm -f *~ *.det *.dat *.log *.codes *.chk
}

while [ $1 ]; do
    case $1 in
	all)   run;;
        sox)   sox;;
        make_ref) make_ref;;
        check) check;;
	clean) clean;;
        *) run1 $1;;
    esac
    shift
done
