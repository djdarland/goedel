cd /media/ramdisk
if [ -d "Temp" ] ; then
    chmod -R u+w Temp
    rm -r Temp
fi
mkdir Temp
cd ~/src/mine/mng_git
cp -r goedel /media/ramdisk/Temp
cd /media/ramdisk/Temp/goedel/v1_3_27/src
make 
