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
val e6t = (List('a', 'b','c', 'd'), Nil, Nil)
val e6f = aplicarMovimientos(e6t, List(Uno(1), Uno(1), Uno(1), Dos(1), Uno(-1), Dos(1), Uno(-1), Dos(-1), Dos(-1), Uno(-1)))
val e7 = definirManiobra(List('x', 'y','z'), List('y', 'z', 'x'))
val e7t = (List('x', 'y','z'), Nil,Nil)
val e7f = aplicarMovimientos(e7t, List(Uno(1), Uno(1), Dos(1), Uno(-1), Uno(-1), Dos(-1)))
val e8 = definirManiobra(List('x', 'y','z'), List('z', 'x', 'y'))
val e8t = (List('x', 'y','z'), Nil, Nil)
val e8f = aplicarMovimientos(e8t, List(Uno(1), Dos(1), Dos(1), Uno(-1), Dos(-1), Dos(-1)))
val e9 = definirManiobra(List('1', '2','3', '4'), List('4', '2', '1', '3'))
val e9t = (List('1', '2','3', '4'), Nil, Nil)
val e9f = aplicarMovimientos(e9t, List(Uno(1), Dos(1), Uno(1), Dos(1), Uno(-1), Dos(1), Uno(-1), Dos(-1), Dos(-1), Dos(-1)))
val e10 = definirManiobra(List('1', '2','3', '4'), List('2', '4', '3', '1'))
val e10t = (List('1', '2','3', '4'), Nil, Nil)
val e10f = aplicarMovimientos(e10t, List(Uno(1), Uno(1), Uno(1), Dos(1), Uno(-1), Uno(-1), Dos(1), Uno(-1), Dos(-1), Dos(-1)))
println(e6)
println(e6f) //Se espera: List('c', 'b', 'a', 'd')
println(e7) 
println(e7f) //Se espera: List('y', 'z', 'x')
println(e8)
println(e8f) //Se espera: List('z', 'x', 'y')
println(e9)
println(e9f) //Se espera: List('4', '2', '1', '3')
println(e10)
println(e10f) //Se espera: List('2', '4', '3', '1')

//Pruebas aplicarMovimiento


val e11 = (List('x', 'y', 'z'), Nil, Nil)
val e12 = aplicarMovimiento(e11, Dos(1))
val e13 = aplicarMovimiento(e11, Dos(2))
val e14 = aplicarMovimiento(e12, Uno(2))
val e15 = aplicarMovimiento(e14, Dos(1))
println(e11) // Se espera: (List(x, y, z),List(),List()
println(e12) // Se espera: (List(x, y),List(),List(z))
println(e13) // Se espera: (List(x),List(),List(y, z))
println(e14) // Se espera: (List(),List(x, y),List(z))
println(e15) // Se espera: (List(),List(x, y),List(z))


//Pruebas aplicarMovimientos

val e16 = (List('1', '2', '3', '4', '5'), Nil, Nil)
val e17 = aplicarMovimientos(e16,List(Uno(2), Dos(3), Dos(-1), Uno(-1)))
val e18 = aplicarMovimientos(e16, List(Uno(3), Dos(1), Dos(-1), Uno(-1)))
val e19 = aplicarMovimientos(e16, List(Uno(2), Dos(2), Dos(-1), Uno(-2)))
val e20 = aplicarMovimientos(e16, List(Uno(1), Dos(1), Dos(-1), Uno(-1)))
println(e16) // Se espera: (List(1, 4),List(5),List(2, 3))
println(e17) // Se espera: (List(1, 2, 3),List(4, 5),List())
println(e18) // Se espera: (List(1, 2, 4, 5),List(),List(3))
println(e19) // Se espera: (List(1, 2, 4, 5),List(),List(3))
println(e20) // Se espera: (List(1, 2, 3, 4, 5),List(),List())
