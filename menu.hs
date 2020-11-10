main = do
  print("1 - Serie Fibonacci")
  print("2 - Presentar numeros del 1 al 10")
  print("3 - Factorial")
  print("4 - Desaparece numeros")
  print("5 - Palindromos")
  print("6 - Menu de calculadora")
  print("7 - Salir")
  n <- getLine
  casos (read n)

casos n = do 
 case n of
   1 -> do
        putStrLn("¿Que posicion deseas?")
        p <- getLine
        serie 0 1 1 (read p)
   2 -> unoAdiez 1
   3 -> factorial
   4 -> desaparecer [1,2,3,4,5,6,7,8]
   5 -> palindromo
   6 -> calculadora
   7 -> putStrLn("Adios :)")


-- 1 - 10
unoAdiez n = do
  if n<= 10
    then do 
    print n
    unoAdiez (n+1)
  else 
     main


-- Factorial
factorial  = do
 print("Ingresa el numero: ")
 n <- getLine
 print ((product [1..(read n)]))
 main

-- Palindromo
palindromo  = do 
 print("Ingresa los elementos: ")
 p <- getLine
 if p == reverse p
  then do 
   print("Es palindromo")
 else 
   print("No es palindromo")
 main

-- Desaparecer numeros
desaparecer n = do 
 if length n >1
  then do
  print (n)
  desaparecer (take ((length n)-1) n )
 else 
  print(n)
 main


-- calculadora
calculadora = do
  print("1 - Suma")
  print("2 - Resta")
  print("3 - Multiplicar")
  print("4 - Dividr")
  print("5 Salir")
  n <- getLine
  menuCalc (read n)


menuCalc n = do 
 case n of
   1 -> sumar
   2 -> restar
   3 -> multiplicar
   4 -> dividir
   5 -> main
 
sumar = do
 putStrLn ("Ingresa numero 1")
 a <- getLine
 putStrLn ("Ingresa numero 2")
 b <- getLine 
 putStrLn ("= "++show(read a + read b))
 calculadora

restar = do
 putStrLn ("Ingresa numero 1")
 a <- getLine
 putStrLn ("Ingresa numero 2")
 b <- getLine 
 putStrLn ("= "++show(read a - read b))
 calculadora

multiplicar = do
 putStrLn ("Ingresa numero 1")
 a <- getLine
 putStrLn ("Ingresa numero 2")
 b <- getLine 
 putStrLn ("= "++show(read a * read b))
 calculadora


dividir = do
 putStrLn ("Ingresa numero 1")
 a <- getLine
 putStrLn ("Ingresa numero 2")
 b <- getLine 
 putStrLn ("= "++show(read a / read b))
 calculadora



-- SERIE
serie a b c l = do
 if l > c
  then do
   serie b (a+b) (c+1) l
 else 
   do
   print("La posicion es: ")
   print(a)
   main