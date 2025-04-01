package object ManiobrasTrenes {
  trait Movimiento

  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)

  case class Uno(n: Int) extends Movimiento

  case class Dos(n: Int) extends Movimiento

  type Maniobra = List[Movimiento]
  
  def aplicarMovimiento(e: Estado, m: Movimiento): Estado = m match {
    // Mover n vagones del final del principal al uno
    case Uno(n) if n > 0 =>
      val (principal, uno, dos) = e
      val toMove = math.min(n, principal.length)
      //Basicamente lo que se modifico fue la forma en que se movian los trenes de una via a otra
      //Usamos .takeRight para tomar el ultimo valor de la via principal, y lo movemos al principio de la via Uno
      val mover = principal.takeRight(toMove)
      //Usamos .dropRight para dejar en la via principal los trenes que no se mueven de esa via
      val quedan = principal.dropRight(toMove)
      (quedan, mover ++ uno, dos)

    // Mover n vagones del inicio del uno al principal
    case Uno(n) if n < 0 =>
      val (principal, uno, dos) = e
      val toMove = math.min(-n, uno.length)
      //Basicamente lo que se modifico fue la forma en que se movian los trenes de una via a otra
      //Usamos .take para tomar el primer valor de la via uno, y lo movemos al final de la via principal
      val mover = uno.take(toMove)
      //Usamos .drop para dejar en la via Uno los trenes que no se mueven de esa via
      val quedan = uno.drop(toMove)
      (principal ++ mover, quedan, dos)

    // Mover n vagones del final del principal al dos
    case Dos(n) if n > 0 =>
      val (principal, uno, dos) = e
      val toMove = math.min(n, principal.length)
      //Basicamente lo que se modifico fue la forma en que se movian los trenes de una via a otra
      //Usamos .takeRight para tomar el ultimo valor de la via principal, y lo movemos al principio de la via Dos
      val mover = principal.takeRight(toMove)
      //Usamos .dropRight para dejar en la via principal los trenes que no se mueven de esa via
      val quedan = principal.dropRight(toMove)
      (quedan, uno, mover ++ dos)

    // Mover n vagones del inicio del dos al principal
    case Dos(n) if n < 0 =>
      val (principal, uno, dos) = e
      val toMove = math.min(-n, dos.length)
      //Basicamente lo que se modifico fue la forma en que se movian los trenes de una via a otra
      //Usamos .take para tomar el primer valor de la via Dos, y lo movemos al final de la via principal
      val mover = dos.take(toMove)
      //Usamos .drop para dejar en la via Dos los trenes que no se mueven de esa via
      val quedan = dos.drop(toMove)
      (principal ++ mover, uno, quedan)

    // Movimiento nulo
    case _ => e
  }
  
  def aplicarMovimientos(e: Estado, movs: Maniobra): List[Estado] = {
    movs.foldLeft(List(e)) { (estados, mov) =>
      aplicarMovimiento(estados.head, mov) :: estados
    }.reverse
  }

  def definirManiobra(t1: Tren, t2: Tren): Maniobra = {
    require(t1.toSet == t2.toSet, "Los trenes deben contener los mismos vagones")

    def buscarSolucion(pendientes: List[(Estado, Maniobra)], visitados: Set[Estado]): Option[Maniobra] = pendientes match {
      case Nil => None
      case (estado@(principal, _, _), movimientos) :: resto =>
        if (principal == t2) Some(movimientos.reverse)
        else {
          val nuevosEstados = List(Uno(1), Uno(-1), Dos(1), Dos(-1))
            .map(m => (aplicarMovimiento(estado, m), m :: movimientos))
            .filter { case (nuevoEstado, _) => !visitados.contains(nuevoEstado) }

          buscarSolucion(resto ++ nuevosEstados, visitados ++ nuevosEstados.map(_._1))
        }
    }

    buscarSolucion(List(((t1, Nil, Nil), Nil)), Set((t1, Nil, Nil))).getOrElse(Nil)
  }
}