package org.openmole.plugin.method.evolution.data

case class SavedData(
  generation: Long,
  frequency:  Option[Long],
  name:       String,
  last:       Boolean)

object EvolutionMetadata {
  def method = "evolution"
  case object none extends EvolutionMetadata

  case class StochasticNSGA2(
    genome:    Seq[GenomeBoundData],
    objective: Seq[ObjectiveData.NoisyObjective],
    mu:        Int,
    sample:    Int,
    saved:     SavedData) extends EvolutionMetadata

}

sealed trait EvolutionMetadata

object GenomeBoundData {
  case class DoubleBound(value: String, low: Double, high: Double) extends GenomeBoundData
  case class IntBound(value: String, low: Int, high: Int) extends GenomeBoundData
  case class DoubleSequenceBound(value: String, low: Array[Double], high: Array[Double]) extends GenomeBoundData
  case class IntSequenceBound(value: String, low: Array[Int], high: Array[Int]) extends GenomeBoundData
  case class Enumeration(value: String, values: Seq[String]) extends GenomeBoundData
}

sealed trait GenomeBoundData

object ObjectiveData {
  case class NoisyObjective(
    name:     String,
    delta:    Option[Double],
    negative: Boolean)
}
