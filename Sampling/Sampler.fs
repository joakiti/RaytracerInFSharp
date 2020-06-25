namespace Sampling

module Sampler =
    open System
    open System.Collections.Concurrent
    open System.Threading

    type SampleSet =
        |SS of (float * float) array
        |UH of (float * float * float) array
        
    type Sampler =
        { Sets : SampleSet array;
          mutable idMap : ConcurrentDictionary<int, (int * int)> //used to map ThreadID to which set it is using and which samplepoint in that set it is using.
          setSize : int //number of samplepoints in a set
          numSets : int //number of sets in the sampler
          rng : ThreadLocal<Random>
        }                           
        
    //------------------------------------------HELPER FUNCTIONS--------------------------------------------

    //creates a new random number generator, used by several samplers and the shuffle array function
    let rng = new Random()

    //creates an array with N evenly spaced numbers from 0 -> 1
    let create1DGrid n =
        let div = 1. / float((n + 1))
        let divMax = float(n) * div
        let array = [|for x in div .. div .. divMax + div/2. -> x|] //+ div/2. to counter floating point division error
        array

    //creates an array where each number is in the center of an evenly spaced "grid"
    let create1DcenteredGrid n =
        let div = 1. / float(n)
        let array = [|for x in div/2. .. div .. 1. -> x|]
        array

    //creates every tuple combination of each element in a single array.
    let combine input = Array.foldBack (fun x state -> Array.append (Array.map (fun y -> (x,y)) input) state) input [||]

    //exchange 2 elements in an array, used for shuffle
    let swap a f s =
        let temp = Array.get a f
        Array.set a f (Array.get a s)
        Array.set a s temp

    //shuffle array based on Tores shuffling algorithm
    let shuffle a =
        for i = 1 to Array.length a - 1 do
            let j = rng.Next(0, i + 1)
            swap a i j
        a
    
    //shuffles array in index range n -> m
    let shuffleInRange (a:float array) n m =
        for i = n to m do
            let j = rng.Next(n, i + 1)
            swap a i j
        a

    //calculate a random (float) in range min -> max
    let rngInRange (min : float) (max : float) =
        let a = rng.NextDouble() * (max - min) + min
        a

    //----------------------------------------CREATE SAMPLER FUNCTIONS--------------------------------------------

    //creates a regular sampler with n * n samples evenly spaced in a unit square
    let mkRegularSampler (n : int) =
        if n < 1 then failwith "n has to be bigger then 0"
        else
        let oneDimension = create1DGrid n
        let combined = combine oneDimension
        let combined = shuffle combined
        let sampleSet = [|SS(combined)|]
        { Sets = sampleSet;
         idMap = new ConcurrentDictionary<int, (int * int)>();
         setSize = n*n;
         numSets = 1;
         rng = new ThreadLocal<Random>(fun () -> new Random(Guid.NewGuid().GetHashCode())) //threadsafe random number generator
        }

    //creates a random sampler with n samples randomly spaced in a unit square
    let mkRandomSampler (n : int) (sets : int) =
        if n < 1 || sets < 1 then failwith "n and number of sets has to be bigger then 0"
        else
        let sampleSets = Array.zeroCreate sets
        for i = 0 to sets - 1 do
            let sampleSet = [|for _ in 0 .. n - 1 -> (rng.NextDouble(), rng.NextDouble())|]
            sampleSets.[i] <- SS(sampleSet)
        { Sets = sampleSets;
         idMap = new ConcurrentDictionary<int, (int * int)>();
         setSize = n;
         numSets = sets;
         rng = new ThreadLocal<Random>(fun () -> new Random(Guid.NewGuid().GetHashCode()))}

    //creates a NRooks sampler based on shuffling 2 arrays and then jittering each sample point
    let mkNRooksSampler (n : int) (sets : int) =
        if n < 0 || sets < 0 then failwith "n and number of sets has to be bigger then 0"
        else
        let sampleSets = Array.zeroCreate sets
        let divHalf = (1. / float(2 * n))
        for i in 0 .. sets - 1 do
            let x = create1DcenteredGrid n |> shuffle
            let y = create1DcenteredGrid n |> shuffle
            for i in 0 .. n - 1 do
                x.[i] <- rngInRange (x.[i]-divHalf) (x.[i]+divHalf)
                y.[i] <- rngInRange (y.[i]-divHalf) (y.[i]+divHalf)
            let set = Array.zip x y
            let set = shuffle set
            sampleSets.[i] <- SS(set)
        { Sets = sampleSets;
         idMap = new ConcurrentDictionary<int, (int * int)>();
         setSize = n;
         numSets = sets;
         rng = new ThreadLocal<Random>(fun () -> new Random(Guid.NewGuid().GetHashCode()))}

    //creates a Jittered sampler where each point in a cell is randomized/jittered 
    let mkJitteredSampler (n : int) (sets : int) =
        if n < 0 || sets < 0 then failwith "n and number of sets has to be bigger then 0"
        else
        let sampleSets = Array.zeroCreate sets
        let divHalf = (1. / float(2 * n))
        for i in 0 .. sets - 1 do
            let x = create1DcenteredGrid n
            let xy = combine x
            for i in 0 .. (n * n - 1) do
                match xy.[i] with
                | (x, y) -> xy.[i] <- ((rngInRange (x-divHalf) (x+divHalf)), (rngInRange (y-divHalf) (y+divHalf)))
            let set = shuffle xy
            sampleSets.[i] <- SS(set)
        { Sets = sampleSets;
         idMap = new ConcurrentDictionary<int, (int * int)>();
         setSize = n * n;
         numSets = sets;
         rng = new ThreadLocal<Random>(fun () -> new Random(Guid.NewGuid().GetHashCode()))}

    //creates a multiJittered Sampler by creating 2 arrays of n * n size, and then shuffling the sub arrays of size n, in 2 dimensions
    let mkMultiJitteredSampler (n : int) (sets : int) =
        if n < 0 || sets < 0 then failwith "n and number of sets has to be bigger then 0"
        else
        let sampleSets = Array.zeroCreate sets
        let divHalf = (1. / float(2 * n*n))
        let x = create1DcenteredGrid (n*n)
        let y = create1DcenteredGrid (n*n)
        for j in 0 .. sets-1 do
            for i in 0 .. n*n - 1 do
                x.[i] <- rngInRange (x.[i]-divHalf) (x.[i]+divHalf)
                y.[i] <- rngInRange (y.[i]-divHalf) (y.[i]+divHalf)
            for i in 0 .. n .. (n*n-1) do
                shuffleInRange x i (i+n-1)
            let yn = Array.ofSeq(List.fold(fun acc t -> Seq.append acc (seq{ t .. n .. n*n-1 })) Seq.empty [0 .. n-1])
            let random = [|for i in yn -> x.[i]|]
            for i in 0 .. n .. (n*n-1) do
                shuffleInRange random i (i+n-1)
            let set = Array.zip random y |> shuffle
            sampleSets.[j] <- SS(set)
        { Sets = sampleSets;
         idMap = new ConcurrentDictionary<int, (int * int)>();
         setSize = n * n;
         numSets = sets;
         rng = new ThreadLocal<Random>(fun () -> new Random(Guid.NewGuid().GetHashCode()))}

    //----------------------------------------MAPPING TO UNIT HEMISPHERE--------------------------------------------

    //calculating Phi
    let calcPhi a = 2. * System.Math.PI * a

    //calculating Theta
    let calcTheta e b =
        let a = 1. - b
        let c = 1. / (e + 1.)
        let d = a**c
        let theta = System.Math.Acos d
        theta
    
    //mapping a single sample point (a,b) to a 3 dimensional sample point (x,y,z) in a unit hemisphere
    let mapToUH coords e =
        let (a,b) = coords
        let cos = System.Math.Cos
        let sin = System.Math.Sin
        let theta = calcTheta e b
        let phi = calcPhi a
        ((sin theta) * (cos phi), (sin theta) * (sin phi), (cos theta))
    
    //traversing sampler and mapping all points to a Unit Hemisphere
    let mapPointsToUH (s:Sampler) (e:float) =
        let a = s.Sets
        let mapSampleSet sampleSet =
            let (SS(b)) = sampleSet
            UH(Array.map (fun coord -> mapToUH coord e) b)
        { Sets = Array.map (mapSampleSet) a;
         idMap = new ConcurrentDictionary<int, (int * int)>(); 
         setSize = s.setSize;
         numSets = s.numSets;
         rng = new ThreadLocal<Random>(fun () -> new Random(Guid.NewGuid().GetHashCode()))}

    //----------------------------------------MAPPING TO UNIT DISC--------------------------------------------
    
    //doubles the size of a sample point in unit square, a point in center 0.5 in the unitsquare is now in point 0.0
    let doubleSquare (a, b) = (2.*a-1., 2.*b-1.) 

    //calculates R and Theta based on which quarter our point is in
    let calcRandTheta (a, b) =
        match (a, b) with
        |(0.,0.) -> (0.,0.)
        |_ when a >= b && a > -b -> 
            let r = a
            let theta = (Math.PI/4.) * (b/a)
            (r, theta)
        |_ when a <= b && a > -b ->
            let r = b
            let theta = (Math.PI/4.) * (2.-(a/b))
            (r, theta)
        |_ when a <= b && a < -b ->
            let r = -a
            let theta = (Math.PI/4.) * (4.+(b/a))
            (r, theta)
        |_ when a >= b && a < -b ->
            let r = -b
            let theta = (Math.PI/4.) * (6.-(a/b))
            (r, theta)
        |(_,_) -> (0.,0.)
    
    //mapping a single sample point (a,b) to asample point (x,y) in a Unit Disc
    let mapToUD (x, y) = 
        let (a, b) = doubleSquare (x, y) 
        let (r, theta) = calcRandTheta (a, b)
        let a = r*Math.Cos(theta)
        let b = r*Math.Sin(theta)
        (a, b)
    
    //traversing sampler and mapping all points to a Unit Disc
    let mapPointsToUD (s:Sampler) =
        let a = s.Sets
        let mapSampleSet sampleSet =
            let (SS(b)) = sampleSet
            SS(Array.map (fun coord -> mapToUD coord) b)
        { Sets = Array.map (mapSampleSet) a;
         idMap = new ConcurrentDictionary<int, (int * int)>(); 
         setSize = s.setSize;
         numSets = s.numSets;
         rng = new ThreadLocal<Random>(fun () -> new Random());}

    //------------------------------------------TRAVERSE SAMPLER---------------------------------------------

    //gets next samplepoint (x,y) in sampler for each thread, the function finds a new sampleSet if the current sampleSet is empty. 
    let getNext (s:Sampler) = 
         let id = System.Threading.Thread.CurrentThread.ManagedThreadId
         let setIndex, sampleIndex = s.idMap.GetOrAdd(id, (s.rng.Value.Next(0, s.numSets), 0))
         if sampleIndex >= s.setSize
         then
            let newSetIndex = s.rng.Value.Next(0, s.numSets)
            let newSampleIndex = 1
            s.idMap.TryUpdate(id, (newSetIndex, newSampleIndex), (setIndex, sampleIndex)) |> ignore
            let (SS(set)) = s.Sets.[newSetIndex]
            set.[0]
         else 
            let newSampleIndex = sampleIndex + 1
            s.idMap.TryUpdate(id, (setIndex, newSampleIndex), (setIndex, sampleIndex)) |> ignore
            let (SS(set)) = s.Sets.[setIndex]
            set.[sampleIndex]

    //gets next samplepoint (x,y,z) in sampler for each thread, the function finds a new sampleSet if the current sampleSet is empty.  
    let getNextUH (s:Sampler) = 
         let id = System.Threading.Thread.CurrentThread.ManagedThreadId
         let setIndex, sampleIndex = s.idMap.GetOrAdd(id, (s.rng.Value.Next(0, s.numSets), 0))

         if sampleIndex >= s.setSize
         then
            let newSetIndex = s.rng.Value.Next(0, s.numSets)
            let newSampleIndex = 1
            s.idMap.TryUpdate(id, (newSetIndex, newSampleIndex), (setIndex, sampleIndex)) |> ignore
            let (UH(set)) = s.Sets.[newSetIndex]
            set.[0]
         else 
            let newSampleIndex = sampleIndex + 1
            s.idMap.TryUpdate(id, (setIndex, newSampleIndex), (setIndex, sampleIndex)) |> ignore
            let (UH(set)) = s.Sets.[setIndex]
            set.[sampleIndex]

    //returns a random set in the sampler, this function is used by the cameras as they need a new random set for each pixel we can traverse. 
    let getRandomSet s =
        let a = s.Sets
        let length = Array.length a
        let r = rng.Next (0, length)
        let (SS(set)) = a.[r]
        set

    type Sampler with
        member this.getNext = getNext this
        member this.getNextUH = getNextUH this
        member this.mapPointsToUH(e) = mapPointsToUH this e
        member this.mapPointsToUD = mapPointsToUD this
        member this.randomSet = getRandomSet this