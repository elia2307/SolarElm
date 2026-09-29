case $1 in 
    production) elm make src/Main.elm --output=main.js --optimize
        ;;
    *) elm make src/Main.elm --output=main.js
        ;;
esac

