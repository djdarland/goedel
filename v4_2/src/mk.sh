cd /media/ramdisk
if [ -d "Temp" ] ; then
    chmod -R u+w Temp
    rm -r Temp
fi
mkdir Temp
cd ~/src/mine/mng_git
cp -r goedel /media/ramdisk/Temp
cd /media/ramdisk/Temp/goedel/v4_2/src
make >~/src/mine/mng_git/goedel/v4_2/src/djd.out 2>~/src/mine/mng_git/goedel/v4_2/src/djd.err
cd
$EDITOR djd.out
$EDITOT djd.err

