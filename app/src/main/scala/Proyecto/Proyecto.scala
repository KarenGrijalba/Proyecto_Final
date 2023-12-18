/**
  * Taller 3 - Programación Funcional
  * Autores: <Carlos Stiven Ruiz Rojas 2259629, Karen Grijalba Ortiz 2259626, Jhony Fernando Duque 2259398 >
  * Profesor: Carlos A Delgado
  */
package Proyecto

import com.sun.tools.javac.Main
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import common._

object Proyecto {

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  /////////////// CADENA INGENUO PARALELO ///////////////

  def reconstruirCadenaParalelo(n: Int, o: Oraculo): Seq[Char] = {

    def generarCadenas(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq())
      else
        alfabeto flatMap { elemento =>
          generarCadenas(n - 1) map { combinacion =>
            elemento +: combinacion
          }
        }
    }

    def reconstruirCadena(): Seq[Char] = {
      val cadenas = generarCadenas(n)

      if (n % 2 == 0) {
        val (cadena1, cadena2) = parallel(cadenas.take(n / 2), (cadenas.drop(n / 2)))

        val resultado1: Seq[Char] = {
          cadena1 flatMap { cadena =>
            if (o(cadena)) cadena // Si la secuencia cumple con la condición, se agrega a la lista
            else Seq.empty[Char] // Si no cumple, se agrega una lista vacía
          }
        }
        val resultado2: Seq[Char] = {
          cadena2 flatMap { cadena =>
            if (o(cadena)) cadena // Si la secuencia cumple con la condición, se agrega a la lista
            else Seq.empty[Char] // Si no cumple, se agrega una lista vacía
          }

        }
        resultado1 ++ resultado2
      } else {
        val (cadena1, cadena2) = parallel(cadenas.take((n + 1) / 2), cadenas.drop((n - 1) / 2))
        val resultado1: Seq[Char] = {
          cadena1 flatMap { cadena =>
            if (o(cadena)) cadena // Si la secuencia cumple con la condición, se agrega a la lista
            else Seq.empty[Char] // Si no cumple, se agrega una lista vacía
          }
        }
        val resultado2: Seq[Char] = {
          cadena2 flatMap { cadena =>
            if (o(cadena)) cadena // Si la secuencia cumple con la condición, se agrega a la lista
            else Seq.empty[Char] // Si no cumple, se agrega una lista vacía
          }
        }
        resultado1 ++ resultado2
      }
    }

    reconstruirCadena()
  }

  //////////CADENA INGENUO SECUENCIAL ////////////////////

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

    def generarCadenas(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq())
      else
        alfabeto flatMap { elemento =>
          generarCadenas(n - 1) map { combinacion =>
            elemento +: combinacion
          }
        }
    }


    def reconstruirCadena(): Seq[Char] = {
      val cadenas = generarCadenas(n) // Genera todas las cadenas de longitud n

      //println("cadenas: " + cadenas)
      def resultado(): Seq[Char] = {
        cadenas flatMap { cadena =>
          if (o(cadena)) cadena // Si la secuencia cumple con la condición, se agrega a la lista
          else Seq.empty[Char] // Si no cumple, se agrega una lista vacía
        }
      }

      resultado()
    }

    reconstruirCadena()
  }

  /////////// CADENA MEJORADO SECUENCIAL ///////////////

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {

    def generarCadenasCandidatas(k: Int, candidatas: Seq[Seq[Char]]): Seq[Char] = { // Función que genera las cadenas candidatas
      if (k == n) {
        candidatas.head
      } else {
        val nuevasCandidatas = candidatas.flatMap { candidata =>
          alfabeto.flatMap { elemento =>
            if (o(candidata :+ elemento)) Seq(candidata :+ elemento)
            else Seq.empty[Seq[Char]]
          }
        }
        generarCadenasCandidatas(k + 1, nuevasCandidatas) // Se llama recursivamente con las nuevas cadenas candidatas
      }
    }

    def reconstruirCadena(): Seq[Char] = {
      def cadenasCandidatas(): Seq[Char] = {
        val cadenas = alfabeto.map(Seq(_)) // Genera las cadenas iniciales de longitud 1
        generarCadenasCandidatas(1, cadenas)
      }

      cadenasCandidatas()
    }

    reconstruirCadena()
  }

  /////////// CADENA MEJORADO PARALELO ///////////////
  def reconstruirCadenaMejoradoParalela(n: Int, o: Oraculo): Seq[Char] = {

    def generarCadenasCandidatas(k: Int, candidatas: Seq[Seq[Char]]): Seq[Char] = { // Función que genera las cadenas candidatas
      if (k == n) {
        candidatas.head
      } else {
        if (k % 2 == 0) {
          val (nuevasCandidatas1, nuevasCandidatas2) = parallel(candidatas.take(k / 2).flatMap { candidata =>
            alfabeto.flatMap { elemento =>
              if (o(candidata :+ elemento)) Seq(candidata :+ elemento)
              else Seq.empty[Seq[Char]]
            }
          }, candidatas.drop(k / 2).flatMap { candidata =>
            alfabeto.flatMap { elemento =>
              if (o(candidata :+ elemento)) Seq(candidata :+ elemento)
              else Seq.empty[Seq[Char]]
            }

          })
          val candidatasfinal = nuevasCandidatas1 ++ nuevasCandidatas2
          generarCadenasCandidatas(k + 1, candidatasfinal) // Se llama recursivamente con las nuevas cadenas candidatas
        } else {
          val (nuevasCandidatas1, nuevasCandidatas2) = parallel(candidatas.take((k + 1) / 2).flatMap { candidata =>
            alfabeto.flatMap { elemento =>
              if (o(candidata :+ elemento)) Seq(candidata :+ elemento)
              else Seq.empty[Seq[Char]]
            }
          }, candidatas.drop((k - 1) / 2).flatMap { candidata =>
            alfabeto.flatMap { elemento =>
              if (o(candidata :+ elemento)) Seq(candidata :+ elemento)
              else Seq.empty[Seq[Char]]
            }

          })
          val candidatasfinal = nuevasCandidatas1 ++ nuevasCandidatas2
          generarCadenasCandidatas(k + 1, candidatasfinal) // Se llama recursivamente con las nuevas cadenas candidatas
        }

      }
    }

    def reconstruirCadena(): Seq[Char] = {
      def cadenasCandidatas(): Seq[Char] = {
        val cadenas = alfabeto.map(Seq(_)) // Genera las cadenas iniciales de longitud 1
        generarCadenasCandidatas(1, cadenas)
      }

      cadenasCandidatas()
    }

    reconstruirCadena()
  }

  /////////// CADENA TURBO SECUENCIAL ///////////////

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    def construirSubcadena(k: Int, subcadena: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) subcadena.head
      else {
        val nuevoSubcadena = subcadena.flatMap { secuencia =>
          subcadena.flatMap { elemento =>
            val nuevaSecuencia = secuencia ++ elemento
            if (o(nuevaSecuencia)) Seq(nuevaSecuencia)
            else Seq.empty[Seq[Char]]
          }
        }
        construirSubcadena(k * 2, nuevoSubcadena)
      }
    }

    val cadenas = alfabeto.map(Seq(_))
    val subcadena = construirSubcadena(1, cadenas)
    subcadena
  }

  /////////// CADENA TURBO PARALELO ///////////////

  def reconstruirCadenaTurboParalela(n: Int, o: Oraculo): Seq[Char] = {
    def construirSubcadena(k: Int, subcadena: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) subcadena.head
      else {
        if (k % 2 == 0) {
          val(nuevasCandidatas1,nuevasCandidatas2) = parallel(subcadena.take(k / 2).flatMap { candidata =>
            subcadena.flatMap { elemento =>
              val nuevaSecuencia = candidata ++ elemento
              if (o(nuevaSecuencia)) Seq(nuevaSecuencia)
              else Seq.empty[Seq[Char]]
            }
          }, subcadena.drop(k / 2).flatMap { candidata =>
            subcadena.flatMap { elemento =>
              val nuevaSecuencia = candidata ++ elemento
              if (o(nuevaSecuencia)) Seq(nuevaSecuencia)
              else Seq.empty[Seq[Char]]
            }

          })
          val candidatasfinal = nuevasCandidatas1 ++ nuevasCandidatas2
          construirSubcadena(k * 2, candidatasfinal)
        } else {
          val(nuevasCandidatas1,nuevasCandidatas2) = parallel(subcadena.take((k + 1) / 2).flatMap { candidata =>
            subcadena.flatMap { elemento =>
              val nuevaSecuencia = candidata ++ elemento
              if (o(nuevaSecuencia)) Seq(nuevaSecuencia)
              else Seq.empty[Seq[Char]]
            }
          }, subcadena.drop((k - 1) / 2).flatMap { candidata =>
            subcadena.flatMap { elemento =>
              val nuevaSecuencia = candidata ++ elemento
              if (o(nuevaSecuencia)) Seq(nuevaSecuencia)
              else Seq.empty[Seq[Char]]
            }

          })
          val candidatasfinal = nuevasCandidatas1 ++ nuevasCandidatas2
          construirSubcadena(k * 2, candidatasfinal)
        }
      }
    }

    val cadenas = alfabeto.map(Seq(_))
    val subcadena = construirSubcadena(1, cadenas)
    subcadena
  }

  /////////// CADENA TURBO MEJORADO SECUENCIAL ///////////////

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {

    def filtrar(subCadena: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      subCadena.flatMap(s1 => subCadena.map(s2 => s1 ++ s2)) // Generar todas las subcadenas posibles
        .filter(s => (0 to s.length - k).forall(i => subCadena.exists(subSeq => s.drop(i).take(k) == subSeq)) && o(s)) // Filtrar las subcadenas que no cumplan con la condición
    }

    def construirSubcadena(k: Int, subCadena: Seq[Seq[Char]]): Seq[Char] = {
      if (k > n) subCadena.head
      else {
        val nuevoSubcadena = filtrar(subCadena, k / 2)
        construirSubcadena(k * 2, nuevoSubcadena)
      }
    }

    val cadena = alfabeto.map(Seq(_))
    val subcadena = construirSubcadena(2, cadena)
    subcadena
  }

  /////////// CADENA TURBO MEJORADO PARALELO ///////////////

  /*
  def reconstruirCadenaTurboMejoradaParalela(n: Int, o: Oraculo): Seq[Char] = {

      def filtrar(subCadena: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
        subCadena.flatMap(s1 => subCadena.map(s2 => s1 ++ s2)) // Generar todas las subcadenas posibles
          .filter(s => (0 to s.length - k).forall(i => subCadena.exists(subSeq => s.drop(i).take(k) == subSeq)) && o(s)) // Filtrar las subcadenas que no cumplan con la condición
      }

      def construirSubcadena(k: Int, subCadena: Seq[Seq[Char]]): Seq[Char] = {
        if (k > n) subCadena.head
        else {
          if (k % 2 == 0) {
            val(nuevasCandidatas1,nuevasCandidatas2) = parallel(filtrar(subCadena.take(k / 2), k / 2), filtrar(subCadena.drop(k / 2), k / 2))
            val candidatasfinal = nuevasCandidatas1 ++ nuevasCandidatas2
            construirSubcadena(k * 2, candidatasfinal)
          } else {
            val(nuevasCandidatas1,nuevasCandidatas2) = parallel(filtrar(subCadena.take((k + 1) / 2), (k + 1) / 2), filtrar(subCadena.drop((k - 1) / 2), (k - 1) / 2))
            val candidatasfinal = nuevasCandidatas1 ++ nuevasCandidatas2
            construirSubcadena(k * 2, candidatasfinal)
          }
        }
      }

      val cadena = alfabeto.map(Seq(_))
      val subcadena = construirSubcadena(2, cadena)
      subcadena
  }
  */
}


