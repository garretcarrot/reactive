package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    def testProbability(rate: Double): Boolean = randomBelow(100) < 100 * rate
    val prevalence = 0.01
    val transmissibility = 0.4
    val death = 0.25
    val airTrafficProbability = 0.0
    val mobility = 1.0
    val chosenFew = 0.0
  }

  import SimConfig._

  val persons: List[Person] = (1 to population).toList map (new Person(_))
  for (i <- 1 to (persons.size * prevalence).toInt) {
    persons(randomBelow(persons.size)).infect()
  }
  for (i <- 1 to (persons.size * chosenFew).toInt) {
    var index = randomBelow(persons.size)
    while (persons(index).infected || persons(index).vaccinated)
      index = randomBelow(persons.size)
    persons(index).vaccinate()
  }
  // println(s"infected: ${persons.count(_.infected)}")
  // println(s"vaccinated: ${persons.count(_.vaccinated)}")

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var vaccinated = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    afterDelay(randomBelow(6))(move())

    def move(): Unit = {
      if (!dead) {
        if (testProbability(airTrafficProbability)) {
          flyAway()
        } else {
          moveAdjacent()
        }
        transmit()
      }
      afterDelay(randomBelow(6))(move)
    }

    def flyAway(): Unit = {
      var rowIndex = randomBelow(roomRows)
      var colIndex = randomBelow(roomColumns)
      while (row == rowIndex && col == colIndex) {
        rowIndex = randomBelow(roomRows)
        colIndex = randomBelow(roomColumns)
      }
      row = randomBelow(rowIndex)
      col = randomBelow(colIndex)
    }

    def moveAdjacent(): Unit = {
      val sickPersons = persons.filter(_.sick).map(p => (p.row, p.col))
      val possibleRooms = List(
        ((row + 1) % roomRows, col),
        (if (row == 0) roomRows - 1 else row - 1, col),
        (row, (col + 1) % roomColumns),
        (row, if (col == 0) roomColumns - 1 else col - 1)
      ) filterNot sickPersons.contains

      if (possibleRooms.nonEmpty) {
        val (newRow, newCol) = possibleRooms(randomBelow(possibleRooms.size))
        if (testProbability(mobility)) {
          if (sick && testProbability(mobility)) {
            row = newRow
            col = newCol
          } else {
            row = newRow
            col = newCol
          }
        }
      }
    }

    def transmit(): Unit = {
      if (!infected && !immune) {
        val roomPersons = persons.filter(p => p.row == row && p.col == col)
        val isTransmitted = testProbability(transmissibility)
        if (roomPersons.exists(_.infected) && isTransmitted) {
          infect()
        }
      }
    }

    def infect(): Unit = {
      if (!vaccinated) {
        infected = true
        afterDelay(6) {
          sick = true
        }
        afterDelay(14) {
          if (testProbability(death)) dead = true
        }
        afterDelay(16) {
          if (!dead) {
            sick = false; immune = true
          }
        }
        afterDelay(18) {
          if (!dead) {
            infected = false; immune = false
          }
        }
      }
    }

    def vaccinate(): Unit = {
      vaccinated = true
    }

  }
}
