# IITK Authentication in Haskell


Install using
```sh
cabal install
```

then simply run

```sh
hwall-auth-iitk
```

You can also supply username and password as command line arguments.

```sh
hwall-auth-iitk "username" "password"
```


### Hardcoding Username and Password in the Executable

Add your username and password as stated in line 65 eg

```haskell
parse [] = return ("username","password")
```

comment the line

```haskell
-- parse _ = readInput
```

### Contributors

[Satvik Chauhan](https://github.com/satvikc)

[Jayesh Kumar Gupta](https://github.com/rejuvyesh)
