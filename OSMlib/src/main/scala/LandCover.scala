package org.seacourt.osm

import scala.collection.{mutable, immutable}

trait LandCoverType
{
    def score : Double
}


object LandCoverType
{
    // Scores are somewhat arbitrary, other than urban == bad, rural == good
    case object IndustrialOrCommercial      extends LandCoverType { val score = 0.2 }
    case object RoadAndRail                 extends LandCoverType { val score = 0.2 }
    case object Airports                    extends LandCoverType { val score = 0.2 }
    case object DumpSites                   extends LandCoverType { val score = 0.2 }
    case object ConstructionSites           extends LandCoverType { val score = 0.2 }
    case object MineralExtraction           extends LandCoverType { val score = 0.2 }

    case object ContinuousUrbanFabric       extends LandCoverType { val score = 0.3 }
    case object DiscontinuousUrbanFabric    extends LandCoverType { val score = 0.3 }
    case object PortAreas                   extends LandCoverType { val score = 0.4 }
    
    case object SportAndLeisure             extends LandCoverType { val score = 0.5 }
    
    case object GreenUrbanAreas             extends LandCoverType { val score = 0.7 }
    
    case object NonIrrigatedArableLand      extends LandCoverType { val score = 0.7 }
    case object PermanentlyIrrigatedLand    extends LandCoverType { val score = 0.7 }
    case object RiceFields                  extends LandCoverType { val score = 0.7 }
    case object PermanentAnnualCrops        extends LandCoverType { val score = 0.7 }
    case object ComplexCultivation          extends LandCoverType { val score = 0.7 }
    case object ArigultureAndNatural        extends LandCoverType { val score = 0.7 }
    
    case object BurntAreas                  extends LandCoverType { val score = 0.8 }
    case object AgroForestry                extends LandCoverType { val score = 0.8 }
    
    case object Vineyards                   extends LandCoverType { val score = 0.9 }
    case object FruitAndBerryPlantations    extends LandCoverType { val score = 0.9 }
    case object OliveGroves                 extends LandCoverType { val score = 0.9 }
    case object Pastures                    extends LandCoverType { val score = 0.9 }
    
    case object BroadLeavedForest           extends LandCoverType { val score = 1.0 }
    case object ConiferousForest            extends LandCoverType { val score = 1.0 }
    case object MixedForest                 extends LandCoverType { val score = 1.0 }
    case object NaturalGrasslands           extends LandCoverType { val score = 1.0 }
    case object MoorsAndHeathland           extends LandCoverType { val score = 1.0 }
    case object SclerophyllousVegetation    extends LandCoverType { val score = 1.0 }
    case object TransitionalWoodlandScrub   extends LandCoverType { val score = 1.0 }
    case object BeachesDunes                extends LandCoverType { val score = 1.0 }
    case object BareRocks                   extends LandCoverType { val score = 1.0 }
    case object SparselyVegetatedAreas      extends LandCoverType { val score = 1.0 }
    case object GlaciersAndPerpetualSnow    extends LandCoverType { val score = 1.0 }

    case object InlandMarshes               extends LandCoverType { val score = 1.0 }
    case object PeatBogs                    extends LandCoverType { val score = 1.0 }
    case object SaltMarshes                 extends LandCoverType { val score = 1.0 }
    case object Salines                     extends LandCoverType { val score = 1.0 }
    case object IntertidalFlats             extends LandCoverType { val score = 1.0 }
    case object WaterCourses                extends LandCoverType { val score = 1.0 }
    case object WaterBodies                 extends LandCoverType { val score = 1.0 }
    case object CoastalLagoons              extends LandCoverType { val score = 1.0 }
    case object Estuaries                   extends LandCoverType { val score = 1.0 }
    case object SeaAndOcean                 extends LandCoverType { val score = 1.0 }
    
    case object NoData                      extends LandCoverType { val score = 0.5 }
    case object UnclassifiedLandSurface     extends LandCoverType { val score = 0.5 }
    case object UnclassifiedWaterBodies     extends LandCoverType { val score = 0.5 }
    case object Unclassified                extends LandCoverType { val score = 0.5 }
        
        
    lazy val typeMap =
    {
        val data = mutable.ArrayBuffer[(Int, LandCoverType)]()
        
        data.append( (1 -> ContinuousUrbanFabric) )
        data.append( (2 -> DiscontinuousUrbanFabric) )
        data.append( (3 -> IndustrialOrCommercial) )
        data.append( (4 -> RoadAndRail) )
        data.append( (5 -> PortAreas) )
        data.append( (6 -> Airports) )
        data.append( (7 -> MineralExtraction) )
        data.append( (8 -> DumpSites) )
        data.append( (9 -> ConstructionSites) )
        data.append( (10 -> GreenUrbanAreas) )
        data.append( (11 -> SportAndLeisure) )
        data.append( (12 -> NonIrrigatedArableLand) )
        data.append( (13 -> PermanentlyIrrigatedLand) )
        data.append( (14 -> RiceFields) )
        data.append( (15 -> Vineyards) )
        data.append( (16 -> FruitAndBerryPlantations) )
        data.append( (17 -> OliveGroves) )
        data.append( (18 -> Pastures) )
        data.append( (19 -> PermanentAnnualCrops) )
        data.append( (20 -> ComplexCultivation) )
        data.append( (21 -> ArigultureAndNatural) )
        data.append( (22 -> AgroForestry) )
        data.append( (23 -> BroadLeavedForest) )
        data.append( (24 -> ConiferousForest) )
        data.append( (25 -> MixedForest) )
        data.append( (26 -> NaturalGrasslands) )
        data.append( (27 -> MoorsAndHeathland) )
        data.append( (28 -> SclerophyllousVegetation) )
        data.append( (29 -> TransitionalWoodlandScrub) )
        data.append( (30 -> BeachesDunes) )
        data.append( (31 -> BareRocks) )
        data.append( (32 -> SparselyVegetatedAreas) )
        data.append( (33 -> BurntAreas) )
        data.append( (34 -> GlaciersAndPerpetualSnow) )

        data.append( (35 -> InlandMarshes) )
        data.append( (36 -> PeatBogs) )
        data.append( (37 -> SaltMarshes) )
        data.append( (38 -> Salines) )
        data.append( (39 -> IntertidalFlats) )
        data.append( (40 -> WaterCourses) )
        data.append( (41 -> WaterBodies) )
        data.append( (42 -> CoastalLagoons) )
        data.append( (43 -> Estuaries) )
        data.append( (44 -> SeaAndOcean) )
        data.append( (48 -> NoData) )
        data.append( (49 -> UnclassifiedLandSurface) )
        data.append( (50 -> UnclassifiedWaterBodies) )
        data.append( (255 -> Unclassified) )
        
        data.toMap
    }
}

