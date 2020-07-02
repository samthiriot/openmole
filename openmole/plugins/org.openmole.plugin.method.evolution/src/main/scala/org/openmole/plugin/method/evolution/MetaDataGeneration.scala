package org.openmole.plugin.method.evolution

import org.openmole.plugin.method.evolution.data._
import org.openmole.core.expansion.FromContext
import io.circe._
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration
import org.openmole.core.exception.InternalProcessingError
import org.openmole.core.tools.io.Prettifier._
import Genome.GenomeBound
import org.openmole.plugin.hook.omr.MethodData

object MetadataGeneration {

  import EvolutionMetadata._

  def genomeData(g: Genome) =
    FromContext { p ⇒
      import p._
      g.map(boundData).map(_.from(context))
    }

  def boundData(b: GenomeBound) = FromContext { p ⇒
    import p._

    b match {
      case b: GenomeBound.ScalarDouble             ⇒ GenomeBoundData.DoubleBound(b.v.name, b.low.from(context), b.high.from(context))
      case b: GenomeBound.ScalarInt                ⇒ GenomeBoundData.IntBound(b.v.name, b.low.from(context), b.high.from(context))
      case b: GenomeBound.SequenceOfDouble         ⇒ GenomeBoundData.DoubleSequenceBound(b.v.name, b.low.from(context), b.high.from(context))
      case b: GenomeBound.SequenceOfInt            ⇒ GenomeBoundData.IntSequenceBound(b.v.name, b.low.from(context), b.high.from(context))
      case b: GenomeBound.Enumeration[_]           ⇒ GenomeBoundData.Enumeration(b.v.name, b.values.map(_.prettify()))
      case b: GenomeBound.SequenceOfEnumeration[_] ⇒ GenomeBoundData.Enumeration(b.v.name, b.values.map(_.prettify()))
    }
  }

  def noisyObjectiveData(o: NoisyObjective[_]) = ObjectiveData.NoisyObjective(o.as.getOrElse(o.prototype.name), o.delta, o.negative)

  def fromString(s: String): EvolutionMetadata =
    decode[EvolutionMetadata](s) match {
      case Left(e)  ⇒ throw new InternalProcessingError(s"Error parsing metadata $s", e)
      case Right(m) ⇒ m
    }
}
