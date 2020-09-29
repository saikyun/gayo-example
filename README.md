# gayo

## Installation
```
$ yarn
$ shadow-cljs watch bro
```

Then browse to [localhost:8021](http://localhost:8021)









\
\
\
\
\
\


### Ignore these

export NODE_OPTIONS=--max_old_space_size=4096

rm -r node_modules/ .shadow-cljs/ targets/expo/node_modules/ targets/expo/.shadow-cljs/ targets/browser/public/js/ targets/expo/app/




cd ~/spel/gayo-example
rm -r targets/browser/public/assets
rm -r targets/browser/public/js
cp -r assets targets/browser/public/
shadow-cljs release bro
surge targets/browser/public/ hard-range.surge.sh
rm -r targets/browser/public/assets/
ln -s /Users/test/spel/gayo-example/assets targets/browser/public
