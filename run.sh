for i in *.wav; do
    echo $i
    b=$(basename $i .wav)
    gosh demod-100Hz.scm < $i > $b.dat
    sed -n '/CORR/s/^## //p' $b.dat > $b.cor
done
