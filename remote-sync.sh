pushd ../ &&\
echo "Remove local ircbd.zip" &&\
rm -rf ircdb.zip &&\
echo "Zip new files" &&\
zip -r  --exclude=*.git* --exclude=*.cabal-sandbox* --exclude=*dist* --exclude=*cabal.sandbox.config* ircdb.zip ircdb &> /dev/null &&\
echo "Delete olivine zip" &&\
ssh olivine "rm -rf ~/ircdb.zip ~/ircdb" &&\
echo "Upload to olivine" &&\
scp ircdb.zip olivine:. &&\
echo "Run sync command" &&\
ssh -t olivine "unzip -qq ircdb.zip && cd ircdb && sh run.sh && sudo sh sync.sh"
if [[ $? -ne 0 ]] ; then
    echo "Failure---------------------------------------------------------------------"
else
    echo "Success+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
fi
popd
