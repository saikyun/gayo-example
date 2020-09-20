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




rm -r targets/browser/public/js/cljs-runtime
rm -r targets/browser/public/assets/
cp -r assets targets/browser/public/
surge targets/browser/public/
