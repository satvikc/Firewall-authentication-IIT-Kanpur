# IITK Authentication in Haskell

Add your username password as stated in line 51 eg
```haskell
parse [] = return ("username","password")
```
comment the line
```haskell
-- parse _ = usage >> exit
```

compile using 
```sh
cabal install
```

then simply run 

```sh
hwall-auth-iitk
```


