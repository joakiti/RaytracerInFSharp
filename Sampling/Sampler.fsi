namespace Sampling

module Sampler =
    [<Sealed>]
    type Sampler =
        member getNext : float * float
        member getNextUH : float * float * float
        member mapPointsToUH : e:float -> Sampler
        member mapPointsToUD : Sampler
        member randomSet : (float * float) array

    type SampleSet
     
    val mkRegularSampler : n:int -> Sampler
    val mkRandomSampler : n:int -> sets:int -> Sampler
    val mkJitteredSampler : n:int -> sets:int -> Sampler
    val mkNRooksSampler : n:int -> sets:int -> Sampler
    val mkMultiJitteredSampler : n:int -> sets:int -> Sampler

    val getNext : s:Sampler -> float * float
    val getNextUH : s:Sampler -> float * float * float
    val mapPointsToUH : s:Sampler -> e:float -> Sampler
    val mapPointsToUD : s:Sampler -> Sampler