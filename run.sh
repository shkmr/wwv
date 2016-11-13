##
##
for i in data/*.wav; do
    echo $i
    b=$(basename $i .wav)
    gosh demod-100Hz.scm < $i > $b.dat
    sed -n '/detect/s/^## //p' $b.dat > $b.det
done
