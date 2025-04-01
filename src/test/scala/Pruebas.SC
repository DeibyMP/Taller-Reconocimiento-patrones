import ManiobrasTrenes._

val e1 = (List('a', 'b', 'c', 'd'), Nil, Nil)
val e2 = aplicarMovimiento(e1, Uno(2))
val e3 = aplicarMovimiento(e2, Dos(3))
val e4 = aplicarMovimiento(e3, Dos(-1))
val e5 = aplicarMovimiento(e4, Uno(-2))

println(e1)
println(e2)
println(e3)
println(e4)
println(e5)
println(aplicarMovimientos(e1, List(Uno(2), Dos(3), Dos(-1), Uno(-2), Dos(-1))))
val e = (List('a', 'b','c', 'd'), List('d', 'b', 'c', 'a'))
println(e)

//Pruebas definirManiobra

val e6 = definirManiobra(List('a', 'b','c', 'd'), List('c', 'b', 'a', 'd'))
val e7 = definirManiobra(List('x', 'y','z'), List('y', 'z', 'x'))
val e8 = definirManiobra(List('x', 'y','z'), List('z', 'x', 'y'))
val e9 = definirManiobra(List('1', '2','3', '4'), List('4', '2', '1', '3'))
val e10 = definirManiobra(List('1', '2','3', '4'), List('2', '4', '3', '1'))
println(e6)
println(e7)
println(e8)
println(e9)
println(e10)

//Pruebas aplicarMovimiento

val e11 = (List('x', 'y', 'z'), Nil, Nil)
val e12 = aplicarMovimiento(e11, Dos(1))
val e13 = aplicarMovimiento(e11, Dos(2))
val e14 = aplicarMovimiento(e12, Uno(2))
val e15 = aplicarMovimiento(e14, Dos(1))

println(e11)
println(e12)
println(e13)
println(e14)
println(e15)

//Pruebas aplicarMovimientos

val e16 = (List('1', '2', '3', '4', '5'), Nil, Nil)
val e17 = aplicarMovimientos(e16,List(Uno(2), Dos(3), Dos(-1), Uno(-1)))
val e18 = aplicarMovimientos(e16, List(Uno(3), Dos(1), Dos(-1), Uno(-1)))
val e19 = aplicarMovimientos(e16, List(Uno(2), Dos(2), Dos(-1), Uno(-2)))
val e20 = aplicarMovimientos(e16, List(Uno(1), Dos(1), Dos(-1), Uno(-1)))

println(e16)
println(e17)
println(e18)
println(e19)
println(e20)